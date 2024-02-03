#include "lower.h"
#include "all.h"
#include "hir.h"
#include "lir.h"

// a.field[0] = 50

// lvalue = 50

// {a [field, index: 0 ]} = 50

void lhir_lvalue_impl(lir_lvalue_t *lvalue, lir_stmt_t **stmts, hir_expr_t *expr) {
	switch (expr->kind) {
		case EXPR_LOCAL: {
			lvalue->local = expr->d_local;
			lvalue->loc = expr->loc;
			break;
		}
		default: {
			assert_not_reached();
		}
	}
}

lir_lvalue_t lhir_lvalue(lir_stmt_t **stmts, hir_expr_t *expr) {
	lir_lvalue_t lvalue = {};
	lhir_lvalue_impl(&lvalue, stmts, expr);
	return lvalue;
}

lir_value_t lhir_expr(ir_desc_t *desc, lir_stmt_t **stmts, hir_expr_t *expr) {
	switch (expr->kind) {
		case EXPR_INTEGER_LIT: {
			return (lir_value_t){
				.kind = VALUE_INTEGER_LIT,
				.loc = expr->loc,
				.d_integer_lit = expr->d_integer_lit,
			};
		}
		case EXPR_BOOL_LIT: {
			return (lir_value_t){
				.kind = VALUE_BOOL_LIT,
				.loc = expr->loc,
				.d_bool_lit = expr->d_bool_lit,
			};
		}
		case EXPR_ARRAY: {
			lir_value_t *values = NULL;

			for (u32 i = 0, c = arrlenu(expr->d_array); i < c; i++) {
				lir_value_t value = lhir_expr(desc, stmts, &expr->d_array[i]);
				arrpush(values, value);
			}

			return (lir_value_t){
				.kind = VALUE_ARRAY,
				.loc = expr->loc,
				.d_array = values,
			};
		}
		case EXPR_TUPLE_UNIT: {
			return (lir_value_t){
				.kind = VALUE_TUPLE_UNIT,
				.loc = expr->loc,
			};
		}
		case EXPR_TUPLE: {
			lir_value_t *values = NULL;

			for (u32 i = 0, c = arrlenu(expr->d_tuple); i < c; i++) {
				lir_value_t value = lhir_expr(desc, stmts, &expr->d_tuple[i]);
				arrpush(values, value);
			}

			return (lir_value_t){
				.kind = VALUE_TUPLE,
				.loc = expr->loc,
				.d_tuple = values,
			};
		}
		/* case EXPR_DO_BLOCK: {

		} */
		/* case EXPR_LOOP: {

		} */
		/* case EXPR_IF: {

		} */
		case EXPR_ASSIGN: {
			// lhs = rhs
			lir_lvalue_t lhs = lhir_lvalue(stmts, expr->d_assign.lhs);
			lir_value_t rhs = lhir_expr(desc, stmts, expr->d_assign.rhs);

			lir_stmt(stmts, (lir_stmt_t){
				.kind = STMT_ASSIGN,
				.dest = lhs,
				.d_value = rhs,
			});
			
			return (lir_value_t){
				.kind = VALUE_LVALUE,
				.loc = expr->loc,
				.d_lvalue = (lir_lvalue_t){
					.local = lhs.local,
					.loc = expr->loc,
				},
			};
		}
		case EXPR_INFIX: {
			lir_value_t lhs = lhir_expr(desc, stmts, expr->d_infix.lhs);
			lir_value_t rhs = lhir_expr(desc, stmts, expr->d_infix.rhs);

			static const u8 tok_to_op[] = {
				[TOK_ADD] = VALUE_ADD,
				[TOK_SUB] = VALUE_SUB,
				[TOK_MUL] = VALUE_MUL,
				[TOK_DIV] = VALUE_DIV,
				[TOK_MOD] = VALUE_MOD,
				[TOK_EQ] = VALUE_EQ,
				[TOK_NE] = VALUE_NE,
				[TOK_LT] = VALUE_LE,
				[TOK_GT] = VALUE_LT,
				[TOK_LE] = VALUE_GE,
				[TOK_GE] = VALUE_GT,
				[TOK_AND] = VALUE_AND,
				[TOK_OR] = VALUE_OR,
			};

			return (lir_value_t){
				.kind = tok_to_op[expr->d_infix.kind],
				.loc = expr->loc,
				.d_infix = {
					.lhs = lir_dup(lhs),
					.rhs = lir_dup(rhs),
				},
			};
		}
		/* case EXPR_POSTFIX: {

		}
		case EXPR_PREFIX: {

		}
		case EXPR_DEREF: {

		}
		case EXPR_ADDR_OF: {

		}
		case EXPR_INDEX: {

		}
		case EXPR_SLICE: {

		} */
		case EXPR_LOCAL: {
			return (lir_value_t){
				.kind = VALUE_LVALUE,
				.loc = expr->loc,
				.d_lvalue = (lir_lvalue_t){
					.local = expr->d_local,
					.loc = expr->loc,
				},
			};
		}
		case EXPR_SYM: {
			return (lir_value_t){
				.kind = VALUE_LVALUE,
				.loc = expr->loc,
				.d_lvalue = (lir_lvalue_t){
					.symbol = expr->d_sym,
					.is_sym = true,
					.loc = expr->loc,
				},
			};
		}
		/* case EXPR_CAST: {

		} */
		case EXPR_CALL: {
			lir_value_t f = lhir_expr(desc, stmts, expr->d_call.f);
			lir_value_t *args = NULL;

			for (u32 i = 0, c = arrlenu(expr->d_call.args); i < c; i++) {
				lir_value_t arg = lhir_expr(desc, stmts, &expr->d_call.args[i]);
				arrpush(args, arg);
			}

			return (lir_value_t){
				.kind = VALUE_CALL,
				.loc = expr->loc,
				.d_call = {
					.f = lir_dup(f),
					.args = args,
				},
			};
		}
		/* case EXPR_BREAK: {

		}
		case EXPR_CONTINUE: {

		} */
		/* case EXPR_RETURN: {
			// TODO: warning about `brk_id` we might need it
			lir_value_t value = lhir_expr(desc, stmts, expr->d_return.expr);
			lir_stmt(stmts, (lir_stmt_t){
				.kind = STMT_RETURN,
				.d_value = value,
			});

			// TODO: what do we return??

			return 
		} */
		case EXPR_VOIDING: {
			lir_value_t value = lhir_expr(desc, stmts, expr->d_voiding);

			lir_stmt(stmts, (lir_stmt_t){
				.kind = STMT_DISCARD,
				.d_value = value,
			});
			return (lir_value_t){
				.kind = VALUE_TUPLE_UNIT,
				.loc = expr->loc,
			};
		}
		/* case EXPR_FIELD: {

		}
		case EXPR_LET: {

		} */
		default: {
			assert_not_reached();
		}
	}
}

void lproc(proc_t *proc) {
	lir_value_t value = lhir_expr(&proc->desc, &proc->desc.lir.stmts, &proc->desc.hir);
	lir_stmt(&proc->desc.lir.stmts, (lir_stmt_t){
		.kind = STMT_RETURN,
		.d_value = value,
	});
}

void lglobal(global_t *global) {
	assert_not_reached();
}

void llower_all_symbols(rsym_t *po) {
	for (u32 i = 0, c = arrlenu(po); i < c; i++) {
		rsym_t rsym = po[i];
		sym_t *sym = &symbols[rsym];

		printf("check: %s\n", sv_from(sym->key));

		switch (sym->kind) {
			case SYMBOL_PROC: {
				lproc(&sym->d_proc);
				break;
			}
			case SYMBOL_GLOBAL: {
				lglobal(&sym->d_global);
				break;
			} 
			case SYMBOL_TYPE: {
				break;
			}
			default: {
				assert_not_reached();
			}
		}
	}
}
