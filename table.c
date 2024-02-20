#include "all.h"
#include "hir.h"
//
// global symbol (hash) table
//
// the parser will insert eagerly into a global symbol table, returning a reference
// to a symbol. a symbol's "fully qualified name" is completely unique.
//
// no symbol (function, type, global, etc) can have the same fully qualified name.
// sometimes there may be conflicting modules with different filesystem paths but
// whilst sharing conflicting module paths, we will raise an error in this case.
//
// the symbol table works with a sort of "eventual consistency" where on references
// to a symbol we just insert a placeholder and then later on we fill in the details.
// after the parsing stage we go one by one and ensure each symbol isn't a placeholder
// and if it is, we raise an error
//

sym_t *symbols;
rsym_t *symbols_po;

rsym_t table_resolve(fs_rmod_t mod, istr_t short_name, loc_t onerror) {
	istr_t qualified_name = fs_module_symbol(mod, short_name);
	ptrdiff_t sym = hmgeti(symbols, qualified_name);

	// if you have access to the symbol anyway you've `import_main`ed anyway
	if (sym == __main_init && mod != main_module) {
		err_with_pos(onerror, "`main.init` can only be used through `import_main`");
	}

	if (sym != -1) {
		return sym;
	}

	sym_t desc = {
		.key = qualified_name,
		.short_name = short_name,
		.kind = _SYMBOL_PLACEHOLDER,
	};

	hmputs(symbols, desc);

	return (rsym_t)hmlenu(symbols) - 1;
}

rsym_t table_resolve_qualified_opt(istr_t qualified_name) {
	ptrdiff_t sym = hmgeti(symbols, qualified_name);

	if (sym != -1) {
		return sym;
	}

	return RSYM_NONE;
}

// this is annoying, i wish i could insert symbols into the hash
// table to then have it never be resolvable again.
// having all the data in the same place is what i want
istr_t table_anon_symbol(void) {
	static u32 anon_count = 0;

	char buf[128];
	u32 v = snprintf(buf, sizeof(buf), "<anon_%u>", anon_count++);

	return sv_intern((u8*)buf, v);
}

rsym_t table_register(sym_t desc) {
	if (desc.key == sv_move("main.init")) {
		err_with_pos(desc.loc, "`main.init` is a reserved symbol");
	}

	ptrdiff_t sym = hmgeti(symbols, desc.key);

	if (sym != -1 && symbols[sym].kind != _SYMBOL_PLACEHOLDER) {
		// TODO: better error message, probably module local/context
		//       no need to print the whole qualified name
		err_with_pos(desc.loc, "symbol `%s` already defined", sv_from(desc.key));
	}

	hmputs(symbols, desc);

	// would have been updated
	if (sym != -1) {
		return sym;
	}

	return (rsym_t)hmlenu(symbols) - 1;
}

// i8..! and symbols get qualified names
istr_t table_type_qualified_name(type_t type) {
	ti_kind kind = type_kind(type);

	switch (kind) {
		#define X(name, lit) \
			case name: return sv_move(lit);
		TYPE_X_CONCRETE_LIST
		#undef X
		case TYPE_SYMBOL: {
			return symbols[type_get(type)->d_symbol].key;
		}
		default: {
			return ISTR_NONE;
		}
	}
}

rsym_t table_resolve_method(type_t bare_type, istr_t field) {
	assert(bare_type == type_strip_muls(bare_type));

	istr_t qualified_name;

	// qualified_name = i32:method
	if ((qualified_name = table_type_qualified_name(bare_type)) == ISTR_NONE) {
		return RSYM_NONE;
	}

	istr_t selector = fs_module_symbol_selector(qualified_name, field);

	return table_resolve_qualified_opt(selector);
}

rlocal_t ir_local_new(ir_desc_t *desc, local_t local) {
	u32 idx = arrlenu(desc->locals);
	arrpush(desc->locals, local);
	return idx;
}

static void _dump_function(sym_t *sym);
static void _dump_global(sym_t *sym);
static void _dump_type(sym_t *sym);

void table_dump(sym_t *sym) {
	if (sym->kind == SYMBOL_IMPORT_ASSERTION) {
		return;
	}
	
	void *_ = alloc_scratch(0);

	if (sym->is_extern) {
		eprintf("extern \"%s\" ", sv_from(sym->extern_symbol));
	}

	if (sym->is_pub) {
		eprintf("pub ");
	}

	switch (sym->kind) {
		case SYMBOL_PROC: {
			_dump_function(sym);
			break;
		}
		case SYMBOL_GLOBAL: {
			_dump_global(sym);
			break;
		}
		case SYMBOL_TYPE: {
			_dump_type(sym);
			break;
		}
		default: {
			assert_not_reached();
		}
	}
	alloc_reset(_);
}

static u32 _tabs = 0;

void _print_tabs(void) {
	for (u32 i = 0; i < _tabs; i++) {
		eprintf("  ");
	}
}

void table_dump_po(void) {
	for (u32 i = 0, c = arrlenu(symbols_po); i < c; i++) {
		sym_t *sym = &symbols[i];

		if (sym->kind == _SYMBOL_PLACEHOLDER) {
			continue;
		}
		
		table_dump(sym);
	}
}

static void _dump_type(sym_t *sym) {
	eprintf("type %s = %s\n", sv_from(sym->key), type_dbg_str(sym->d_type.type));
}

static void _print_local(ir_desc_t *desc, rlocal_t local) {
	local_t *localp = &desc->locals[local];

	if (localp->name != ISTR_NONE) {
		eprintf("%s_", sv_from(localp->name));
	} else {
		eprintf("_");
	}
	eprintf("%u", local);
}

// : type
static void _print_local_type(ir_desc_t *desc, rlocal_t local) {
	local_t *localp = &desc->locals[local];
	eprintf(": %s", type_dbg_str(localp->type));
}

static void _print_sym(ir_desc_t *desc, rsym_t sym) {
	eprintf("%s", sv_from(symbols[sym].key));
}

static void _print_pattern(ir_desc_t *desc, pattern_t *pattern) {
	switch (pattern->kind) {
		case PATTERN_TUPLE: {
			eprintf("(");
			for (u32 i = 0, c = arrlenu(pattern->d_tuple); i < c; i++) {
				pattern_t *elem = &pattern->d_tuple[i];
				_print_pattern(desc, elem);
				if (i + 1 < c) {
					eprintf(", ");
				}
			}
			eprintf(")");
			break;
		}
		case PATTERN_ARRAY: {
			eprintf("[");
			if (pattern->d_array.match && pattern->d_array.match_lhs) {
				_print_pattern(desc, pattern->d_array.match);
				eprintf("..., ");
			}
			for (u32 i = 0, c = arrlenu(pattern->d_array.elems); i < c; i++) {
				pattern_t *elem = &pattern->d_array.elems[i];
				_print_pattern(desc, elem);
				if (i + 1 < c) {
					eprintf(", ");
				}
			}
			if (pattern->d_array.match && !pattern->d_array.match_lhs) {
				eprintf(", ...");
				_print_pattern(desc, pattern->d_array.match);
			}
			eprintf("]");
			break;
		}
		case PATTERN_LOCAL: {
			_print_local(desc, pattern->d_local);
			break;
		}
		case PATTERN_TUPLE_UNIT: {
			eprintf("()");
			break;
		}
		case PATTERN_INTEGER_LIT: {
			eprintf("%s", sv_from(pattern->d_integer_lit));
			break;
		}
		case PATTERN_BOOL_LIT: {
			eprintf("%s", pattern->d_bool_lit ? "true" : "false");
			break;
		}
		case PATTERN_UNDERSCORE: {
			eprintf("_");
			break;
		}
		default: {
			assert_not_reached();
		}
	}
}

static void _print_blk_id_space(u8 blk_id) {
	if (blk_id != BLK_ID_NONE) {
		eprintf(":%u ", blk_id);
	}
}

static void _print_expr(ir_desc_t *desc, hir_expr_t *expr) {
	switch (expr->kind) {
		case EXPR_LOCAL: {
			_print_local(desc, expr->d_local);
			break;
		}
		case EXPR_SYM: {
			_print_sym(desc, expr->d_sym);
			break;
		}
		case EXPR_INTEGER_LIT: {
			eprintf("%lu", expr->d_integer);
			break;
		}
		case EXPR_BOOL_LIT: {
			eprintf("%s", expr->d_bool_lit ? "true" : "false");
			break;
		}
		case EXPR_POSTFIX: {
			_print_expr(desc, expr->d_postfix.expr);
			eprintf("%s", expr->d_postfix.op == EXPR_K_INC ? "++" : "--");
			break;
		}
		case EXPR_ASSIGN: {
			_print_expr(desc, expr->d_assign.lhs);
			eprintf(" %s ", tok_op_str(expr->d_assign.kind));
			_print_expr(desc, expr->d_assign.rhs);
			break;
		}
		case EXPR_INFIX: {
			eprintf("(");
			_print_expr(desc, expr->d_infix.lhs);
			eprintf(" %s ", tok_op_str(expr->d_infix.kind));
			_print_expr(desc, expr->d_infix.rhs);
			eprintf(")");
			break;
		}
		case EXPR_PREFIX: {
			eprintf("%s", expr->d_prefix.op == EXPR_K_NOT ? "!" : "-");
			_print_expr(desc, expr->d_prefix.expr);
			break;
		}
		case EXPR_CALL: {
			_print_expr(desc, expr->d_call.f);
			eprintf("(");
			for (int i = 0, c = arrlen(expr->d_call.args); i < c; i++) {
				_print_expr(desc, &expr->d_call.args[i]);
				if (i != c - 1) {
					eprintf(", ");
				}
			}
			eprintf(")");
			break;
		}
		case EXPR_LOOP: {
			eprintf("loop ");
			if (expr->d_loop.forward || expr->d_loop.backward) {
				eprintf("<");
				if (expr->d_loop.forward) {
					eprintf("f");
				}
				if (expr->d_loop.backward) {
					eprintf("b");
				}
				eprintf("> ");
			}
			_print_expr(desc, expr->d_loop.expr);
			break;
		}
		case EXPR_DO_BLOCK: {
			eprintf("do");
			if (expr->d_do_block.no_branch || expr->d_do_block.forward || expr->d_do_block.backward) {
				eprintf(" <");
				if (expr->d_do_block.no_branch) {
					eprintf("n");
				}
				if (expr->d_do_block.forward) {
					eprintf("f");
				}
				if (expr->d_do_block.backward) {
					eprintf("b");
				}
				eprintf("> ");
			}
			eprintf("\n");
			_tabs++;
			for (int i = 0, c = arrlen(expr->d_do_block.exprs); i < c; i++) {
				_print_tabs();
				_print_expr(desc, &expr->d_do_block.exprs[i]);
				if (i != c - 1) {
					eprintf("\n"); // expr automatically prints newline
				}
			}
			_tabs--;
			break;
		}
		case EXPR_TUPLE_UNIT: {
			eprintf("()");
			break;
		}
		case EXPR_TUPLE: {
			eprintf("(");
			for (int i = 0, c = arrlen(expr->d_tuple); i < c; i++) {
				_print_expr(desc, &expr->d_tuple[i]);
				if (i != c - 1) {
					eprintf(", ");
				}
			}
			eprintf(")");
			break;
		}
		case EXPR_FIELD: {
			_print_expr(desc, expr->d_field.expr);
			if (expr->d_field.field != ISTR_NONE) {
				eprintf(".%s", sv_from(expr->d_field.field));
			} else {
				eprintf(".%u", expr->d_field.field_idx);
			}
			break;
		}
		case EXPR_STRUCT: {
			//_print_expr(desc, expr->d_struct.expr);
			eprintf("%s{", type_dbg_str(expr->type));
			for (int i = 0, c = arrlen(expr->d_struct.fields); i < c; i++) {
				eprintf("%s: ", sv_from(expr->d_struct.fields[i].field));
				_print_expr(desc, expr->d_struct.fields[i].expr);
				if (i != c - 1) {
					eprintf(", ");
				}
			}
			eprintf("}");
			break;
		}
		case EXPR_STRUCT_POSITIONAL: {
			eprintf("%s{", type_dbg_str(expr->type));
			for (int i = 0, c = arrlen(expr->d_struct_positional.exprs); i < c; i++) {
				_print_expr(desc, &expr->d_struct_positional.exprs[i]);
				if (i != c - 1) {
					eprintf(", ");
				}
			}
			eprintf("}");
			break;
		}
		case EXPR_BREAK: {
			eprintf("brk :%u ", expr->d_break.branch_level);
			_print_expr(desc, expr->d_break.expr);
			break;
		}
		case EXPR_CONTINUE: {
			eprintf("rep :%u", expr->d_continue.branch_level);
			break;
		}
		case EXPR_RETURN: {
			eprintf("ret ");
			_print_expr(desc, expr->d_return.expr);
			break;
		}
		case EXPR_CAST: {
			_print_expr(desc, expr->d_cast.expr);
			eprintf(": %s", type_dbg_str(expr->type));
			break;
		}
		case EXPR_IF: {
			eprintf("if (");
			_print_expr(desc, expr->d_if.cond);
			eprintf(") ");
			_print_expr(desc, expr->d_if.then);
			if (expr->d_if.els) {
				eprintf(" else ");
				_print_expr(desc, expr->d_if.els);
			}
			break;
		}
		case EXPR_ARRAY: {
			eprintf("[");
			for (int i = 0, c = arrlen(expr->d_array); i < c; i++) {
				_print_expr(desc, &expr->d_array[i]);
				if (i != c - 1) {
					eprintf(", ");
				}
			}
			eprintf("]");
			break;
		}
		/* case EXPR_LAMBDA: {
			// \x y z -> x + y + z

			eprintf("\\");
			for (u32 i = 0, c = arrlenu(expr->d_lambda.args); i < c; i++) {
				_hir_dump_var_w_type(expr->d_lambda.args[i]);
				if (i != c - 1) {
					eprintf(" ");
				}
			}

			eprintf(" -> ");

			_hir_dump_expr(modp, expr->d_lambda.scope, expr->d_lambda.expr);
			break;
		} */
		/* case EXPR_MATCH: {
			// match expr
			//    (x, y) -> ...

			eprintf("match ");
			_print_expr(desc, expr->d_match.expr);
			eprintf("\n");

			_tabs++;
			for (u32 i = 0, c = arrlenu(expr->d_match.exprs); i < c; i++) {
				_print_tabs();
				_print_pattern(desc, &expr->d_match.patterns[i]);
				eprintf(" -> ");
				_print_expr(desc, &expr->d_match.exprs[i]);
				if (i != c - 1) {
					eprintf("\n"); // expr automatically prints newline
				}
			}
			_tabs--;
			break;
		} */
		case EXPR_VOIDING: {
			eprintf("void ");
			_print_expr(desc, expr->d_voiding);
			break;
		}
		case EXPR_DEREF: {
			_print_expr(desc, expr->d_deref);
			eprintf(".*");
			break;
		}
		case EXPR_ADDR_OF: {
			eprintf("&");
			if (expr->d_addr_of.is_mut) {
				eprintf("'");
			}
			_print_expr(desc, expr->d_addr_of.ref);
			break;
		}
		/* case EXPR_SIZEOF_TYPE: {
			eprintf("sizeof(%s)", type_dbg_str(expr->d_sizeof_type));
			break;
		} */
		case EXPR_INDEX: {
			_print_expr(desc, expr->d_index.expr);
			eprintf("[");
			_print_expr(desc, expr->d_index.index);
			eprintf("]");
			break;
		}
		case EXPR_SLICE: {
			_print_expr(desc, expr->d_slice.expr);
			eprintf("[");
			if (expr->d_slice.lo) {
				_print_expr(desc, expr->d_slice.lo);
			}
			eprintf("..");
			if (expr->d_slice.hi) {
				_print_expr(desc, expr->d_slice.hi);
			}
			eprintf("]");
			break;
		}
		case EXPR_LET: {
			eprintf("let ");
			_print_pattern(desc, &expr->d_let.pattern);
			eprintf(" = ");
			_print_expr(desc, expr->d_let.expr);
			break;
		}
		case EXPR_UNREACHABLE: {
			switch (expr->d_unreachable.kind) {
				case UNREACHABLE_ASSERTION: {
					eprintf("unreachable");
					break;
				}
				case UNREACHABLE_HEURISTIC: {
					eprintf("unreachable(heuristic)");
					break;
				}
				case UNREACHABLE_UD2: {
					eprintf("unreachable(ud2)");
					break;
				}
			}
			break;
		}
		default: {
			eprintf("\nunknown expr kind %d\n", expr->kind);
			print_hint_with_pos(expr->loc, "LOC HERE");
			assert_not_reached();
		}
	}
}

static void _dump_function(sym_t *sym) {
	proc_t *proc = &sym->d_proc;
	ir_desc_t *desc = &proc->desc;

	eprintf("%s(", sv_from(sym->key));

	// print args
	for (u32 i = 0, c = arrlenu(proc->arguments); i < c; i++) {
		rlocal_t local = proc->arguments[i];
		local_t *localp = &desc->locals[local];

		if (localp->kind == LOCAL_MUT) {
			eprintf("'");
		}
		_print_local(desc, local);
		_print_local_type(desc, local);

		if (i + 1 < c) {
			eprintf(", ");
		}
	}

	eprintf("): %s", type_dbg_str(proc->ret_type));

	if (desc->hir) {
		eprintf(" hir = ");
		if (desc->hir->kind == EXPR_DO_BLOCK) {
			_print_expr(desc, desc->hir);
		} else {
			eprintf("\n");
			_tabs++;
			_print_tabs();
			_print_expr(desc, desc->hir);
			_tabs--;
		}
	}
	eprintf("\n\n");
}

static void _dump_global(sym_t *sym) {
	global_t *global = &sym->d_global;
	ir_desc_t *desc = &global->desc;

	eprintf("%s", sv_from(sym->key));

	if (global->is_mut) {
		eprintf("'");
	}

	eprintf(": %s", type_dbg_str(global->type));

	if (desc->hir) {
		eprintf(" hir = ");
		_print_expr(desc, desc->hir);
		eprintf("\n");
	}

	if (global->constant) {
		eprintf("  (const: ");
		_print_expr(desc, global->constant);
		eprintf(")\n");
	}
}
