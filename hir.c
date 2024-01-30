#include "all.h"

hir_expr_t *hir_dup(hir_expr_t expr) {
	hir_expr_t *dup = malloc(sizeof(hir_expr_t));
	*dup = expr;
	return dup;
}

static void _print_local(proc_t *proc, rlocal_t local) {
	local_t *localp = &proc->locals[local];

	if (localp->name != ISTR_NONE) {
		printf("%s.", sv_from(localp->name));
	} else {
		printf("_");
	}
	printf("%u", local);
}

// : type
static void _print_local_type(proc_t *proc, rlocal_t local) {
	local_t *localp = &proc->locals[local];
	printf(": %s", type_dbg_str(localp->type));
}

static void _print_pattern(proc_t *proc, pattern_t *pattern) {
	switch (pattern->kind) {
		case PATTERN_TUPLE: {
			printf("(");
			for (u32 i = 0, c = arrlenu(pattern->d_tuple.elems); i < c; i++) {
				pattern_t *elem = &pattern->d_tuple.elems[i];
				_print_pattern(proc, elem);
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
				_print_pattern(proc, pattern->d_array.match);
				printf("..., ");
			}
			for (u32 i = 0, c = arrlenu(pattern->d_array.elems); i < c; i++) {
				pattern_t *elem = &pattern->d_array.elems[i];
				_print_pattern(proc, elem);
				if (i + 1 < c) {
					printf(", ");
				}
			}
			if (pattern->d_array.match && !pattern->d_array.match_lhs) {
				printf(", ...");
				_print_pattern(proc, pattern->d_array.match);
			}
			printf("]");
			break;
		}
		case PATTERN_LOCAL: {
			_print_local(proc, pattern->d_local);
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

void _print_sym(proc_t *proc, rsym_t sym) {
	printf("{%s}", sv_from(symbols[sym].key));
}

static u32 _tabs = 0;

void _print_tabs(void) {
	for (u32 i = 0; i < _tabs; i++) {
		printf("  ");
	}
}


static void _print_expr(proc_t *proc, hir_expr_t *expr) {
	switch (expr->kind) {
		case EXPR_LOCAL: {
			_print_local(proc, expr->d_local);
			break;
		}
		case EXPR_SYM: {
			_print_sym(proc, expr->d_sym);
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
			_print_expr(proc, expr->d_postfix.expr);
			printf("%s", expr->d_postfix.kind == TOK_INC ? "++" : "--");
			break;
		}
		case EXPR_ASSIGN: {
			_print_expr(proc, expr->d_assign.lhs);
			printf(" = ");
			_print_expr(proc, expr->d_assign.rhs);
			break;
		}
		case EXPR_INFIX: {
			printf("(%s ", tok_op_str(expr->d_infix.kind));
			_print_expr(proc, expr->d_infix.lhs);
			printf(" ");
			_print_expr(proc, expr->d_infix.rhs);
			printf(")");
			break;
		}
		case EXPR_PREFIX: {
			printf("%s", tok_op_str(expr->d_prefix.kind));
			_print_expr(proc, expr->d_prefix.expr);
			break;
		}
		case EXPR_CALL: {
			_print_expr(proc, expr->d_call.f);
			printf("(");
			for (int i = 0, c = arrlen(expr->d_call.args); i < c; i++) {
				_print_expr(proc, &expr->d_call.args[i]);
				if (i != c - 1) {
					printf(", ");
				}
			}
			printf(")");
			break;
		}
		case EXPR_LOOP: {
			printf(":%u loop ", expr->d_loop.blk_id);
			_print_expr(proc, expr->d_loop.expr);
			break;
		}
		case EXPR_DO_BLOCK: {
			printf(":%u do\n", expr->d_do_block.blk_id);
			_tabs++;
			for (int i = 0, c = arrlen(expr->d_do_block.exprs); i < c; i++) {
				_print_tabs();
				_print_expr(proc, &expr->d_do_block.exprs[i]);
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
			for (int i = 0, c = arrlen(expr->d_tuple.elems); i < c; i++) {
				_print_expr(proc, &expr->d_tuple.elems[i]);
				if (i != c - 1) {
					printf(", ");
				}
			}
			printf(")");
			break;
		}
		case EXPR_FIELD: {
			_print_expr(proc, expr->d_field.expr);
			if (expr->d_field.field != ISTR_NONE) {
				printf(".%s", sv_from(expr->d_field.field));
			} else {
				printf(".%u", expr->d_field.field_idx);
			}
			break;
		}
		case EXPR_BREAK: {
			printf("brk :%u ", expr->d_break.blk_id);
			_print_expr(proc, expr->d_break.expr);
			break;
		}
		case EXPR_CONTINUE: {
			printf("rep :%u", expr->d_continue.blk_id);
			break;
		}
		case EXPR_RETURN: {
			printf("ret ");
			_print_expr(proc, expr->d_return);
			break;
		}
		case EXPR_CAST: {
			_print_expr(proc, expr->d_cast);
			printf(":%s", type_dbg_str(expr->type));
			break;
		}
		case EXPR_IF: {
			printf("if (");
			_print_expr(proc, expr->d_if.cond);
			printf(") ");
			_print_expr(proc, expr->d_if.then);
			printf(" else ");
			_print_expr(proc, expr->d_if.els);
			break;
		}
		/* case EXPR_UNDEFINED: {
			printf("undefined");
			break;
		} */
		case EXPR_ARRAY: {
			printf("[");
			for (int i = 0, c = arrlen(expr->d_array.elems); i < c; i++) {
				_print_expr(proc, &expr->d_array.elems[i]);
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
			_print_expr(proc, expr->d_match.expr);
			printf("\n");

			_tabs++;
			for (u32 i = 0, c = arrlenu(expr->d_match.exprs); i < c; i++) {
				_print_tabs();
				_print_pattern(proc, &expr->d_match.patterns[i]);
				printf(" -> ");
				_print_expr(proc, &expr->d_match.exprs[i]);
				if (i != c - 1) {
					printf("\n"); // expr automatically prints newline
				}
			}
			_tabs--;
			break;
		}
		case EXPR_VOIDING: {
			printf("void ");
			_print_expr(proc, expr->d_voiding);
			break;
		}
		case EXPR_DEREF: {
			_print_expr(proc, expr->d_deref);
			printf(".*");
			break;
		}
		case EXPR_ADDR_OF: {
			printf("&");
			if (expr->d_addr_of.is_mut) {
				printf("'");
			}
			_print_expr(proc, expr->d_addr_of.ref);
			break;
		}
		case EXPR_SIZEOF_TYPE: {
			printf("sizeof(%s)", type_dbg_str(expr->d_sizeof_type));
			break;
		}
		case EXPR_INDEX: {
			_print_expr(proc, expr->d_index.expr);
			printf("[");
			_print_expr(proc, expr->d_index.index);
			printf("]");
			break;
		}
		case EXPR_SLICE: {
			_print_expr(proc, expr->d_slice.expr);
			printf("[");
			if (expr->d_slice.lo) {
				_print_expr(proc, expr->d_slice.lo);
			}
			printf("..");
			if (expr->d_slice.hi) {
				_print_expr(proc, expr->d_slice.hi);
			}
			printf("]");
			break;
		}
		case EXPR_LET: {
			printf("let ");
			_print_pattern(proc, &expr->d_let.pattern);
			printf(" = ");
			_print_expr(proc, expr->d_let.expr);
			break;
		}
		default: {
			printf("\nunknown expr kind %d\n", expr->kind);
			print_hint_with_pos(expr->loc, "LOC HERE");
			assert_not_reached();
		}
	}
}

void hir_dump_function(sym_t *sym) {
	assert(sym->kind == SYMBOL_PROC);
	void *_ = alloc_scratch(0);

	proc_t *proc = &sym->d_proc;
	printf("function %s(", sv_from(sym->key));

	// print args
	for (u32 i = 0, c = proc->arguments; i < c; i++) {
		local_t *localp = &proc->locals[i];

		if (localp->kind == LOCAL_MUT) {
			printf("'");
		}
		_print_local(proc, i);
		_print_local_type(proc, i);

		if (i + 1 < c) {
			printf(", ");
		}
	}

	printf(") -> %s hir = ", type_dbg_str(type_get(sym->type)->d_fn.ret));

	_print_expr(proc, &proc->hir);
	printf("\n");

	alloc_reset(_);
}
