#include "all.h"
#include <assert.h>
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

rsym_t table_resolve(rmod_t mod, istr_t short_name) {
	istr_t qualified_name = fs_module_symbol_sv(mod, short_name);
	ptrdiff_t sym = hmgeti(symbols, qualified_name);
	
	if (sym != -1) {
		return sym;
	}

	sym_t desc = {
		.key = qualified_name,
		.short_name = short_name,
		.is_placeholder = true,
	};

	hmputs(symbols, desc);

	return (rsym_t)hmlenu(symbols) - 1;
}

rsym_t table_register(sym_t desc) {
	ptrdiff_t sym = hmgeti(symbols, desc.key);

	if (sym != -1 && !symbols[sym].is_placeholder) {
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

rlocal_t ir_local_new(ir_desc_t *desc, local_t local) {
	u32 idx = arrlenu(desc->locals);
	arrpush(desc->locals, local);
	return idx;
}

static void _dump_function(sym_t *sym);
static void _dump_global(sym_t *sym);
static void _dump_type(sym_t *sym);

void table_dump(sym_t *sym) {
	assert(!sym->is_placeholder);

	void *_ = alloc_scratch(0);

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
		printf("  ");
	}
}

void table_dump_all(void) {
	for (u32 i = 0, c = hmlenu(symbols); i < c; i++) {
		sym_t *sym = &symbols[i];

		if (sym->is_placeholder) {
			continue;
		}
		
		table_dump(sym);
	}
}

static void _dump_type(sym_t *sym) {
	tsymbol_t *typeinfo = &sym->d_type;

	switch (typeinfo->kind) {
		case TYPESYMBOL_STRUCT: {
			if (arrlenu(typeinfo->d_struct.fields) == 0) {
				printf("struct %s {}\n", sv_from(sym->key));
				break;
			}
			
			printf("struct %s {\n", sv_from(sym->key));
			for (u32 i = 0, c = arrlenu(typeinfo->d_struct.fields); i < c; i++) {
				tsymbol_sf_t *f = &typeinfo->d_struct.fields[i];
				printf("  %s: %s\n", sv_from(f->field), type_dbg_str(f->type));
			}
			printf("}\n");
			break;
		}
		default: {
			assert_not_reached();
		}
	}
	
	//printf(" = %s\n", type_dbg_str(sym->type));
}

static void _print_local(ir_desc_t *desc, rlocal_t local) {
	local_t *localp = &desc->locals[local];

	if (localp->name != ISTR_NONE) {
		printf("%s.", sv_from(localp->name));
	} else {
		printf("_");
	}
	printf("%u", local);
}

// : type
static void _print_local_type(ir_desc_t *desc, rlocal_t local) {
	local_t *localp = &desc->locals[local];
	printf(": %s", type_dbg_str(localp->type));
}

static void _print_sym(ir_desc_t *desc, rsym_t sym) {
	printf("{%s}", sv_from(symbols[sym].key));
}

static void _print_pattern(ir_desc_t *desc, pattern_t *pattern) {
	switch (pattern->kind) {
		case PATTERN_TUPLE: {
			printf("(");
			for (u32 i = 0, c = arrlenu(pattern->d_tuple); i < c; i++) {
				pattern_t *elem = &pattern->d_tuple[i];
				_print_pattern(desc, elem);
				if (i + 1 < c) {
					printf(", ");
				}
			}
			printf(")");
			break;
		}
		case PATTERN_ARRAY: {
			printf("[");
			if (pattern->d_array.match && pattern->d_array.match_lhs) {
				_print_pattern(desc, pattern->d_array.match);
				printf("..., ");
			}
			for (u32 i = 0, c = arrlenu(pattern->d_array.elems); i < c; i++) {
				pattern_t *elem = &pattern->d_array.elems[i];
				_print_pattern(desc, elem);
				if (i + 1 < c) {
					printf(", ");
				}
			}
			if (pattern->d_array.match && !pattern->d_array.match_lhs) {
				printf(", ...");
				_print_pattern(desc, pattern->d_array.match);
			}
			printf("]");
			break;
		}
		case PATTERN_LOCAL: {
			_print_local(desc, pattern->d_local);
			break;
		}
		case PATTERN_TUPLE_UNIT: {
			printf("()");
			break;
		}
		case PATTERN_INTEGER_LIT: {
			printf("%s", sv_from(pattern->d_integer_lit));
			break;
		}
		case PATTERN_BOOL_LIT: {
			printf("%s", pattern->d_bool_lit ? "true" : "false");
			break;
		}
		case PATTERN_UNDERSCORE: {
			printf("_");
			break;
		}
		default: {
			assert_not_reached();
		}
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
			printf("%s", sv_from(expr->d_integer_lit));
			break;
		}
		case EXPR_BOOL_LIT: {
			printf("%s", expr->d_bool_lit ? "true" : "false");
			break;
		}
		case EXPR_POSTFIX: {
			_print_expr(desc, expr->d_postfix.expr);
			printf("%s", expr->d_postfix.kind == TOK_INC ? "++" : "--");
			break;
		}
		case EXPR_ASSIGN: {
			_print_expr(desc, expr->d_assign.lhs);
			printf(" = ");
			_print_expr(desc, expr->d_assign.rhs);
			break;
		}
		case EXPR_INFIX: {
			printf("(%s ", tok_op_str(expr->d_infix.kind));
			_print_expr(desc, expr->d_infix.lhs);
			printf(" ");
			_print_expr(desc, expr->d_infix.rhs);
			printf(")");
			break;
		}
		case EXPR_PREFIX: {
			printf("%s", tok_op_str(expr->d_prefix.kind));
			_print_expr(desc, expr->d_prefix.expr);
			break;
		}
		case EXPR_CALL: {
			_print_expr(desc, expr->d_call.f);
			printf("(");
			for (int i = 0, c = arrlen(expr->d_call.args); i < c; i++) {
				_print_expr(desc, &expr->d_call.args[i]);
				if (i != c - 1) {
					printf(", ");
				}
			}
			printf(")");
			break;
		}
		case EXPR_LOOP: {
			printf(":%u loop ", expr->d_loop.blk_id);
			_print_expr(desc, expr->d_loop.expr);
			break;
		}
		case EXPR_DO_BLOCK: {
			printf(":%u do\n", expr->d_do_block.blk_id);
			_tabs++;
			for (int i = 0, c = arrlen(expr->d_do_block.exprs); i < c; i++) {
				_print_tabs();
				_print_expr(desc, &expr->d_do_block.exprs[i]);
				if (i != c - 1) {
					printf("\n"); // expr automatically prints newline
				}
			}
			_tabs--;
			break;
		}
		case EXPR_TUPLE_UNIT: {
			printf("()");
			break;
		}
		case EXPR_TUPLE: {
			printf("(");
			for (int i = 0, c = arrlen(expr->d_tuple); i < c; i++) {
				_print_expr(desc, &expr->d_tuple[i]);
				if (i != c - 1) {
					printf(", ");
				}
			}
			printf(")");
			break;
		}
		case EXPR_FIELD: {
			_print_expr(desc, expr->d_field.expr);
			if (expr->d_field.field != ISTR_NONE) {
				printf(".%s", sv_from(expr->d_field.field));
			} else {
				printf(".%u", expr->d_field.field_idx);
			}
			break;
		}
		case EXPR_BREAK: {
			printf("brk :%u ", expr->d_break.blk_id);
			_print_expr(desc, expr->d_break.expr);
			break;
		}
		case EXPR_CONTINUE: {
			printf("rep :%u", expr->d_continue.blk_id);
			break;
		}
		case EXPR_RETURN: {
			printf("ret ");
			_print_expr(desc, expr->d_return);
			break;
		}
		case EXPR_CAST: {
			_print_expr(desc, expr->d_cast.expr);
			printf(": %s", type_dbg_str(expr->type));
			break;
		}
		case EXPR_IF: {
			printf("if (");
			_print_expr(desc, expr->d_if.cond);
			printf(") ");
			_print_expr(desc, expr->d_if.then);
			printf(" else ");
			_print_expr(desc, expr->d_if.els);
			break;
		}
		/* case EXPR_UNDEFINED: {
			printf("undefined");
			break;
		} */
		case EXPR_ARRAY: {
			printf("[");
			for (int i = 0, c = arrlen(expr->d_array); i < c; i++) {
				_print_expr(desc, &expr->d_array[i]);
				if (i != c - 1) {
					printf(", ");
				}
			}
			printf("]");
			break;
		}
		/* case EXPR_LAMBDA: {
			// \x y z -> x + y + z

			printf("\\");
			for (u32 i = 0, c = arrlenu(expr->d_lambda.args); i < c; i++) {
				_hir_dump_var_w_type(expr->d_lambda.args[i]);
				if (i != c - 1) {
					printf(" ");
				}
			}

			printf(" -> ");

			_hir_dump_expr(modp, expr->d_lambda.scope, expr->d_lambda.expr);
			break;
		} */
		case EXPR_MATCH: {
			// match expr
			//    (x, y) -> ...

			printf("match ");
			_print_expr(desc, expr->d_match.expr);
			printf("\n");

			_tabs++;
			for (u32 i = 0, c = arrlenu(expr->d_match.exprs); i < c; i++) {
				_print_tabs();
				_print_pattern(desc, &expr->d_match.patterns[i]);
				printf(" -> ");
				_print_expr(desc, &expr->d_match.exprs[i]);
				if (i != c - 1) {
					printf("\n"); // expr automatically prints newline
				}
			}
			_tabs--;
			break;
		}
		case EXPR_VOIDING: {
			printf("void ");
			_print_expr(desc, expr->d_voiding);
			break;
		}
		case EXPR_DEREF: {
			_print_expr(desc, expr->d_deref);
			printf(".*");
			break;
		}
		case EXPR_ADDR_OF: {
			printf("&");
			if (expr->d_addr_of.is_mut) {
				printf("'");
			}
			_print_expr(desc, expr->d_addr_of.ref);
			break;
		}
		/* case EXPR_SIZEOF_TYPE: {
			printf("sizeof(%s)", type_dbg_str(expr->d_sizeof_type));
			break;
		} */
		case EXPR_INDEX: {
			_print_expr(desc, expr->d_index.expr);
			printf("[");
			_print_expr(desc, expr->d_index.index);
			printf("]");
			break;
		}
		case EXPR_SLICE: {
			_print_expr(desc, expr->d_slice.expr);
			printf("[");
			if (expr->d_slice.lo) {
				_print_expr(desc, expr->d_slice.lo);
			}
			printf("..");
			if (expr->d_slice.hi) {
				_print_expr(desc, expr->d_slice.hi);
			}
			printf("]");
			break;
		}
		case EXPR_LET: {
			printf("let ");
			_print_pattern(desc, &expr->d_let.pattern);
			printf(" = ");
			_print_expr(desc, expr->d_let.expr);
			break;
		}
		default: {
			printf("\nunknown expr kind %d\n", expr->kind);
			print_hint_with_pos(expr->loc, "LOC HERE");
			assert_not_reached();
		}
	}
}

static void _dump_function(sym_t *sym) {
	proc_t *proc = &sym->d_proc;
	ir_desc_t *desc = &proc->desc;

	printf("%s(", sv_from(sym->key));

	// print args
	for (u32 i = 0, c = proc->arguments; i < c; i++) {
		local_t *localp = &desc->locals[i];

		if (localp->kind == LOCAL_MUT) {
			printf("'");
		}
		_print_local(desc, i);
		_print_local_type(desc, i);

		if (i + 1 < c) {
			printf(", ");
		}
	}

	printf(") -> %s hir = ", type_dbg_str(type_get(proc->type)->d_fn.ret));

	_print_expr(desc, &desc->hir);
	printf("\n");
}

static void _dump_global(sym_t *sym) {
	global_t *global = &sym->d_global;
	ir_desc_t *desc = &global->desc;

	printf("%s", sv_from(sym->key));

	if (global->is_mut) {
		printf("'");
	}

	printf(" hir = ");
	_print_expr(desc, &desc->hir);
	printf("\n");
}
