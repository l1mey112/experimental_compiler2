#include "all.h"

FILE *gfile;

#define gprintf(...) fprintf(gfile, __VA_ARGS__)
#define gprintf_stmt(...) do { gtabs(); gprintf(__VA_ARGS__); } while (0)
static u32 _tabs = 0;

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

		gtabs();

		switch (expr->kind) {
			case EXPR_RETURN: {
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
				ghir_expr(desc, expr->d_assign.lhs);
				gprintf(" = ");
				ghir_expr(desc, expr->d_assign.rhs);
				gprintf(";\n");
				break;
			}
			case EXPR_IF: {
				gprintf("if (");
				ghir_expr(desc, expr->d_if.cond);
				assert(expr->d_if.then->kind == EXPR_DO_BLOCK);
				assert(expr->d_if.els->kind == EXPR_DO_BLOCK);
				gprintf(") {\n");
				_tabs++;
				gstmts(desc, expr->d_if.then->d_do_block.exprs);
				_tabs--;
				gprintf_stmt("} else {\n");
				_tabs++;
				gstmts(desc, expr->d_if.els->d_do_block.exprs);
				_tabs--;
				gprintf_stmt("}\n");
				break;
			}
			default: {
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

void gproc_def_ret(proc_t *proc) {
	type_t ret = type_underlying(type_get(proc->type)->d_fn.ret);
	bool is_diverging = type_is_diverging(ret);

	if (is_diverging || type_sizeof(&abi, ret) == 0) {
		gprintf("void");
	} else {
		gtype(ret);
	}
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

void gproc_def(rsym_t rsym, proc_t *proc) {
	sym_t *sym = &symbols[rsym];

	if (!sym->is_extern) {
		gprintf("static ");
	}

	gproc_def_ret(proc);
	gprintf(" __REDIRECT(");
	gmangle_symbol(rsym);
	gprintf(", (");
	gproc_def_args(proc);
	gprintf("), ");

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
	}

	gprintf(");\n");
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
				assert_not_reached();
			}
			case SYMBOL_TYPE:
			case SYMBOL_IMPORT_ASSERTION: {
				break;
			}
			default: {
				assert_not_reached();
			}
		}
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
	_tabs++;
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
				assert_not_reached();
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
	gprintf("#include <stddef.h>\n");
	gprintf("#include <features.h>\n");

	gprintf("\n");

	gprintf("// %s\n", build_token);
	gprintf("_Static_assert(%u == sizeof(void*), \"abi sanity\");\n", abi.ptr_size);

	gprintf("\n");

	gdefs();
	gsyms();
}
