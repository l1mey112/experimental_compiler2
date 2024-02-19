#include "all.h"

FILE *gfile;

#define gprintf(...) fprintf(gfile, __VA_ARGS__)
#define gprintf_stmt(...) do { gtabs(); gprintf(__VA_ARGS__); } while (0)
#define gprintf_stmt_debug(loc, ...) do { gdebug(loc); gprintf_stmt(__VA_ARGS__); } while (0)

static u32 _tabs = 0;
static u32 branch_level = 0;

static void gdebug(loc_t loc) {
	// unlikely, will be called on stmt base which will usually have a location
	if (!abi.debug_symbols || loc.len == 0) {
		return;
	}

	static fs_rfile_t last_file = (u16)-1;

	lineinfo_t lineinfo = fs_reconstruct_lineinfo(loc); // TODO: no memo will make this slow

	gprintf("# %u", lineinfo.line_nr + 1);

	if (last_file != lineinfo.file) {
		gprintf(" \"%s\"", fs_files_queue[lineinfo.file].fp);
		last_file = lineinfo.file;
	}

	gprintf("\n");
}

static void gtabs(void) {
	for (u32 i = 0; i < _tabs; i++) {
		gprintf("\t");
	}
}

void gmangle_local(ir_desc_t *desc, rlocal_t local) {
	istr_t name = desc->locals[local].name;
	
	if (name != ISTR_NONE) {
		gprintf("%s_%u", sv_from(name), local);
	} else {
		gprintf("_%u", local);
	}
}

void gmangle_symbol(rsym_t symbol) {
	sym_t *sym = &symbols[symbol];

	const char *s = sv_from(sym->key);
	size_t len = strlen(s);

	for (size_t i = 0; i < len; i++) {
		char c = s[i];
		if (c == '.') {
			gprintf("__");
		} else if (c == ':') {
			gprintf("_");
		} else {
			gprintf("%c", c);
		}
	}
}

void ghir_expr(ir_desc_t *desc, hir_expr_t *expr) {
	switch (expr->kind) {
		case EXPR_INTEGER_LIT: {
			// TODO: int max and proper suffix
			gprintf("%lu", expr->d_integer);
			break;
		}
		case EXPR_BOOL_LIT: {
			gprintf("%s", expr->d_bool_lit ? "true" : "false");
			break;
		}
		case EXPR_LOCAL: {
			gmangle_local(desc, expr->d_local);
			break;
		}
		case EXPR_SYM: {
			gmangle_symbol(expr->d_sym);
			break;
		}
		case EXPR_INFIX: {
			gprintf("(");
			ghir_expr(desc, expr->d_infix.lhs);
			gprintf(" %s ", tok_op_str(expr->d_infix.kind));
			ghir_expr(desc, expr->d_infix.rhs);
			gprintf(")");
			break;
		}
		case EXPR_CALL: {
			ghir_expr(desc, expr->d_call.f);
			gprintf("(");
			for (u32 i = 0, c = arrlenu(expr->d_call.args); i < c; i++) {
				if (i > 0) {
					gprintf(", ");
				}
				ghir_expr(desc, &expr->d_call.args[i]);
			}
			gprintf(")");
			break;
		}
		case EXPR_PREFIX: {
			gprintf("%s", expr->d_prefix.op == EXPR_K_NOT ? "!" : "-");
			ghir_expr(desc, expr->d_prefix.expr);
			break;
		}
		default: {
			eprintf("\n\nkind: %u\n\n", expr->kind);
			assert_not_reached();
		}
	}
}

void gstmts(ir_desc_t *desc, hir_expr_t *stmts) {
	for (u32 i = 0, c = arrlenu(stmts); i < c; i++) {
		hir_expr_t *expr = &stmts[i];

		switch (expr->kind) {
			case EXPR_RETURN: {
				gprintf_stmt_debug(expr->loc, "");
				if (expr->d_return.expr->kind == EXPR_TUPLE_UNIT) {
					gprintf("return;\n");
					break;
				}				
				gprintf("return ");
				ghir_expr(desc, expr->d_return.expr);
				gprintf(";\n");
				break;
			}
			case EXPR_ASSIGN: {
				gprintf_stmt_debug(expr->loc, "");
				ghir_expr(desc, expr->d_assign.lhs);
				gprintf(" = ");
				ghir_expr(desc, expr->d_assign.rhs);
				gprintf(";\n");
				break;
			}
			case EXPR_IF: {
				gprintf_stmt_debug(expr->loc, "if (");
				ghir_expr(desc, expr->d_if.cond);
				assert(expr->d_if.then->kind == EXPR_DO_BLOCK);
				gprintf(") {\n");
				_tabs++;
				gstmts(desc, expr->d_if.then->d_do_block.exprs);
				_tabs--;

				if (expr->d_if.els == NULL) {
					gprintf_stmt("}\n");
					break;
				} else {
					assert(expr->d_if.els->kind == EXPR_DO_BLOCK);
					gprintf_stmt("} else {\n");
					_tabs++;
					gstmts(desc, expr->d_if.els->d_do_block.exprs);
					_tabs--;
					gprintf_stmt("}\n");
				}
				break;
			}
			case EXPR_DO_BLOCK: {
				if (expr->d_do_block.backward) {
					_tabs--;
					gprintf_stmt_debug(expr->loc, "_bL%u:\n", branch_level);
					_tabs++;
				}

				if (!expr->d_do_block.no_branch) {
					branch_level++;
				}
				
				gstmts(desc, expr->d_do_block.exprs);

				if (!expr->d_do_block.no_branch) {
					branch_level--;
				}
				
				if (expr->d_do_block.forward) {
					_tabs--;
					gprintf_stmt("_fL%u:\n", branch_level);
					_tabs++;
				}
				break;
			}
			case EXPR_BREAK: {
				gprintf_stmt_debug(expr->loc, "goto _fL%u;\n", branch_level - 1 - expr->d_break.branch_level);
				break;
			}
			case EXPR_CONTINUE: {
				gprintf_stmt_debug(expr->loc, "goto _bL%u;\n", branch_level - 1 - expr->d_continue.branch_level);
				break;
			}
			default: {
				gprintf_stmt_debug(expr->loc, "");
				ghir_expr(desc, expr);
				gprintf(";\n");
				break;
			}
		}
	}
}

void gtype(type_t type) {
	type = type_underlying(type);
	assert(!type_is_diverging(type));

	u32 size = type_sizeof(&abi, type);
	assert(size != 0);

	switch (type_kind(type)) {
		case TYPE_I8: gprintf("int8_t"); break;
		case TYPE_I16: gprintf("int16_t"); break;
		case TYPE_I32: gprintf("int32_t"); break;
		case TYPE_I64: gprintf("int64_t"); break;
		case TYPE_ISIZE: gprintf("intptr_t"); break;
		case TYPE_U8: gprintf("uint8_t"); break;
		case TYPE_U16: gprintf("uint16_t"); break;
		case TYPE_U32: gprintf("uint32_t"); break;
		case TYPE_U64: gprintf("uint64_t"); break;
		case TYPE_USIZE: gprintf("uintptr_t"); break;
		case TYPE_F32: gprintf("float"); break;
		case TYPE_F64: gprintf("double"); break;
		case TYPE_BOOL: gprintf("bool"); break;
		case TYPE_PTR: {
			tinfo_t *tinfo = type_get(type);
			gtype(tinfo->d_ptr.ref);
			gprintf("*");
			break;
		}
		default: {
			assert_not_reached();
		}
	}
}

// int __REDIRECT(main$Foo$$new, (int num), main\.Foo\:new);
// int main$Foo$$new(int num) {};

// returns if diverging
bool gproc_def_ret(proc_t *proc) {
	type_t ret = type_underlying(type_get(proc->type)->d_fn.ret);
	bool is_diverging = type_is_diverging(ret);

	if (is_diverging || type_sizeof(&abi, ret) == 0) {
		gprintf("void");
	} else {
		gtype(ret);
	}

	return is_diverging;
}

void gproc_def_args(proc_t *proc) {
	for (u32 i = 0, c = arrlenu(proc->arguments); i < c; i++) {
		if (i > 0) {
			gprintf(", ");
		}
		rlocal_t rlocal = proc->arguments[i];
		local_t *local = &proc->desc.locals[rlocal];
		gtype(local->type);
		gprintf(" ");
		gmangle_local(&proc->desc, rlocal);
	}
}

void gredirect(sym_t *sym) {
	const char *s;

	if (sym->is_extern) {
		s = sv_from(sym->extern_symbol);
	} else {
		s = sv_from(sym->key);
	}

	size_t len = strlen(s);

	for (size_t i = 0; i < len; i++) {
		char c = s[i];
		if (c == ':') {
			gprintf("\\:");
		} else {
			gprintf("%c", c);
		}
		/* else if (c == '"') {
			gprintf("\\\"");
		} */
	}
}

void gproc_def(rsym_t rsym, proc_t *proc) {
	sym_t *sym = &symbols[rsym];

	if (!sym->is_extern) {
		gprintf("static ");
	}

	bool is_diverging = gproc_def_ret(proc);
	gprintf(" ");
	gmangle_symbol(rsym);
	gprintf("(");
	gproc_def_args(proc);
	gprintf(") LINK(");
	gredirect(sym);
	gprintf(")");

	if (is_diverging) {
		gprintf(" DIVERGING");
	}

	gprintf(";\n");
}

void gglobal_def(rsym_t rsym, global_t *global) {
	sym_t *sym = &symbols[rsym];

	if (!sym->is_extern) {
		gprintf("static ");
	}

	gtype(global->type);
	gprintf(" ");
	gmangle_symbol(rsym);
	gprintf(" LINK(");
	gredirect(sym);
	gprintf(")");

	if (global->constant) {
		gprintf(" = ");
		ghir_expr(&global->desc, global->constant);
	}

	gprintf(";\n");
}

// forward declare defs
void gdefs(void) {
	for (u32 i = 0, c = arrlenu(symbols_po); i < c; i++) {
		rsym_t rsym = symbols_po[i];
		sym_t *sym = &symbols[rsym];

		switch (sym->kind) {
			case SYMBOL_PROC: {
				gproc_def(rsym, &sym->d_proc);
				break;
			}
			case SYMBOL_GLOBAL: {
				gglobal_def(rsym, &sym->d_global);
				break;
			}
			default: {
				assert_not_reached();
			}
		}
	}
}

// already_def can be NULL
void gdesc_predef_locals(ir_desc_t *desc, rlocal_t *already_def) {
	// def locals on the same line if ones were defined before
	// don't bother creating a optimised in order list

	type_t before = TYPE_INFER;
	for (u32 i = 0, c = arrlenu(desc->locals); i < c; i++) {
		for (u32 j = 0, c = arrlenu(already_def); j < c; j++) {
			if (already_def[j] == i) {
				goto next;
			}
		}

		local_t *local = &desc->locals[i];

		if (before != TYPE_INFER && local->type == before) {
			gprintf(", ");
		} else {
			if (before != TYPE_INFER) {
				gprintf(";\n");
			}
			gtabs();
			gtype(local->type);
			gprintf(" ");
			before = local->type;
		}
		
		gmangle_local(desc, i);
	next:;
	}

	if (arrlenu(desc->locals) > 0) {
		gprintf(";\n");
	}
}

void gproc(rsym_t rsym) {
	sym_t *sym = &symbols[rsym];

	proc_t *proc = &sym->d_proc;
	hir_expr_t *body = proc->desc.hir;

	// extern def
	if (body == NULL) {
		return;
	}

	assert(body->kind == EXPR_DO_BLOCK);

	gproc_def_ret(&sym->d_proc);
	gprintf(" ");
	gmangle_symbol(rsym);
	gprintf("(");
	gproc_def_args(&sym->d_proc);
	gprintf(") {\n");
	gprintf_stmt_debug(sym->loc, ""); // fn start
	_tabs++;
	gdesc_predef_locals(&sym->d_proc.desc, proc->arguments);
	gstmts(&sym->d_proc.desc, body->d_do_block.exprs);
	_tabs--;
	gprintf("}\n");
}

void gsyms(void) {
	for (u32 i = 0, c = arrlenu(symbols_po); i < c; i++) {
		rsym_t rsym = symbols_po[i];
		sym_t *sym = &symbols[rsym];

		if (sym->kind == SYMBOL_TYPE || sym->kind == SYMBOL_IMPORT_ASSERTION) {
			continue;
		}

		switch (sym->kind) {
			case SYMBOL_PROC: {
				gproc(rsym);
				break;
			}
			case SYMBOL_GLOBAL: {
				// globals will already have initialisers, else they'll be initialised
				// at runtime in a seperate function called before `main.main`
				break;
			}
			default: {
				assert_not_reached();
			}
		}

		if (i < c - 1) {
			gprintf("\n");
		}
	}
}

void hir_cgen(FILE *file) {
	gfile = file;

	gprintf("#include <stdint.h>\n");
	gprintf("#include <stdbool.h>\n");

	gprintf("\n");

	gprintf("// %s\n", build_token);
	gprintf("_Static_assert(%u == sizeof(void*), \"abi sanity\");\n", abi.ptr_size);
	gprintf("_Static_assert(%u == sizeof(uintptr_t), \"abi sanity\");\n", abi.ptr_size);

	gprintf("\n");

	gprintf("#define DIVERGING __attribute__((noreturn))\n");
	gprintf("#define LINK(x) __asm__(#x)\n");

	gprintf("\n");

	gdefs();
	gsyms();
}
