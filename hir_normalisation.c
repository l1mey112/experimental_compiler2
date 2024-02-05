#include "all.h"
#include "hir.h"

enum : u8 {
	ST_NONE,
	ST_DIVERGING,
};

enum : u8 {
	RC_NONE,
	RC_DISCARD,
};

#define DIVERGING(expr) if ((expr) == ST_DIVERGING) { return ST_DIVERGING; }

u8 nhir_expr(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr, u8 rc) {
	switch (expr->kind) {
		case EXPR_SYM:
		case EXPR_INTEGER_LIT:
		case EXPR_BOOL_LIT:
		case EXPR_TUPLE_UNIT: {
			break;
		}
		case EXPR_LOCAL: {
			// TODO: ! inside locals???
			break;
		}
		case EXPR_INFIX: {
			// perform reassociation, but not constprop/eval.
			// that is left to further passes where compile errors are raised,
			// this is just to arrange expressions to catch overflow errors early.
			// we'll have futher reassociation passes in later IRs.
			//
			// overflow errors don't make sense for floating point numbers,
			// so ignore them. IEEE floats don't hold properties in the same way
			// as integers.
			//

			// transform lhs and rhs
			DIVERGING(nhir_expr(desc, stmts, expr->d_infix.lhs, rc));
			DIVERGING(nhir_expr(desc, stmts, expr->d_infix.rhs, rc));

			// RULES: `t` are terms, `c` are constants, constants are also terms
			//
			// 1. t + c  = c + t
			// 2. t * c  = c * t
			// 3. t - c  = (-c) + t
			// 4. t1 + (t2 + t3) = (t1 + t2) + t3
			// 5. t1 * (t2 * t3) = (t1 * t2) * t3
			//

			hir_expr_t *lhs = expr->d_infix.lhs;
			hir_expr_t *rhs = expr->d_infix.rhs;

			// rule 1, 2, 3
			if (rhs->kind == EXPR_INTEGER_LIT) {
				switch (expr->d_infix.kind) {
					// TODO: disable rule 3
					case TOK_SUB: {
						// transform to -c + t
						rhs = hir_dup((hir_expr_t){
							.kind = EXPR_PREFIX,
							.type = rhs->type,
							.loc = rhs->loc,
							.d_prefix = {
								.op = EXPR_K_SUB,
								.expr = rhs,
							},
						});
						expr->d_infix.kind = TOK_ADD;

						// fallthrough
					}
					case TOK_ADD:
					case TOK_MUL: {
						// swap lhs and rhs
						hir_expr_t *tmp = lhs;
						lhs = rhs;
						rhs = tmp;
						break;
					}
					default: {
						break;
					}
				}
			}

			// rule 4, 5
			if (rhs->kind == EXPR_INFIX && ((expr->d_infix.kind == TOK_ADD && rhs->d_infix.kind == TOK_ADD) || (expr->d_infix.kind == TOK_MUL && rhs->d_infix.kind == TOK_MUL))) {
				hir_expr_t *t1 = lhs;
				hir_expr_t *t2 = rhs->d_infix.lhs;
				hir_expr_t *t3 = rhs->d_infix.rhs;

				// transform to (t1 + t2) + t3
				lhs = hir_dup((hir_expr_t){
					.kind = EXPR_INFIX,
					.type = expr->type,
					.loc = expr->loc,
					.d_infix = {
						.kind = expr->d_infix.kind,
						.lhs = t1,
						.rhs = t2,
					},
				});

				rhs = t3;
			}

			expr->d_infix.lhs = lhs;
			expr->d_infix.rhs = rhs;
			break;
		}
		case EXPR_RETURN: {
			DIVERGING(nhir_expr(desc, stmts, expr->d_return.expr, RC_NONE));
			arrpush(*stmts, *expr);
			return ST_DIVERGING;
		}
		case EXPR_ASSIGN: {
			DIVERGING(nhir_expr(desc, stmts, expr->d_assign.lhs, RC_NONE));
			DIVERGING(nhir_expr(desc, stmts, expr->d_assign.rhs, RC_NONE));
			arrpush(*stmts, *expr);
			// <- lhs
			*expr = *expr->d_assign.lhs;
			break;
		}
		case EXPR_IF: {
			DIVERGING(nhir_expr(desc, stmts, expr->d_if.cond, RC_NONE));

			// intermediary local
			rlocal_t local = ir_local_new(desc, (local_t){
				.name = ISTR_NONE, // tmp name
				.kind = LOCAL_IMM,
				.type = expr->type,
				.loc = expr->loc,
			});

			hir_expr_t *then_stmts = NULL;
			hir_expr_t *else_stmts = NULL;

			// generate assignments
			if (nhir_expr(desc, &then_stmts, expr->d_if.then, rc) != ST_DIVERGING) {
				hir_expr_t assign = {
					.kind = EXPR_ASSIGN,
					.type = TYPE_BOTTOM,
					.loc = expr->d_if.then->loc,
					.d_assign = {
						.lhs = hir_dup((hir_expr_t){
							.kind = EXPR_LOCAL,
							.d_local = local,
							.type = expr->type,
						}),
						.rhs = expr->d_if.then,
					},
				};
				arrpush(then_stmts, assign);
			}
			if (nhir_expr(desc, &else_stmts, expr->d_if.els, rc) != ST_DIVERGING) {
				hir_expr_t assign = {
					.kind = EXPR_ASSIGN,
					.type = TYPE_BOTTOM,
					.loc = expr->d_if.els->loc,
					.d_assign = {
						.lhs = hir_dup((hir_expr_t){
							.kind = EXPR_LOCAL,
							.d_local = local,
							.type = expr->type,
						}),
						.rhs = expr->d_if.els,
					},
				};
				arrpush(else_stmts, assign);
			}

			hir_expr_t if_stmt = {
				.kind = EXPR_IF,
				.type = TYPE_BOTTOM,
				.loc = expr->loc,
				.d_if = {
					.cond = expr->d_if.cond,
					.then = hir_dup((hir_expr_t){
						.kind = EXPR_DO_BLOCK,
						.type = TYPE_BOTTOM,
						.d_do_block = {
							.exprs = then_stmts,
							.blk_id = BLK_ID_NONE,
						},
					}),
					.els = hir_dup((hir_expr_t){
						.kind = EXPR_DO_BLOCK,
						.type = TYPE_BOTTOM,
						.d_do_block = {
							.exprs = else_stmts,
							.blk_id = BLK_ID_NONE,
						},
					}),
				},
			};

			arrpush(*stmts, if_stmt);

			*expr = (hir_expr_t){
				.kind = EXPR_LOCAL,
				.type = expr->type,
				.d_local = local,
				.loc = expr->loc,
			};
			break;
		}
		case EXPR_CALL: {
			DIVERGING(nhir_expr(desc, stmts, expr->d_call.f, RC_NONE));
			// transform args
			for (u32 i = 0, c = arrlenu(expr->d_call.args); i < c; i++) {
				DIVERGING(nhir_expr(desc, stmts, &expr->d_call.args[i], RC_NONE));
			}
			break;
		}
		case EXPR_DO_BLOCK: {
			// TODO: need to norm properly
			//       for all exprs push RC_DISCARD, a brk will ignore this
			// TODO: remove all brks to a block all

			/* if (rc == RC_DISCARD) {
			} */

			hir_expr_t *do_stmts = NULL;

			for (u32 i = 0, c = arrlenu(expr->d_do_block.exprs); i < c; i++) {
				hir_expr_t *nexpr = &expr->d_do_block.exprs[i];

				// trim exprs up to idx
				if (nhir_expr(desc, &do_stmts, nexpr, RC_DISCARD) == ST_DIVERGING) {
					break;
				}

				arrpush(do_stmts, *nexpr);
			}

			expr->d_do_block.exprs = do_stmts;
			break;
		}
		case EXPR_LOOP: {
			// TODO: duplicate below deletions
			DIVERGING(nhir_expr(desc, stmts, expr->d_loop.expr, RC_DISCARD));
			break;
		}
		case EXPR_BREAK: {
			// TODO: comment in do block above
			//       do i perform DCE here or inside HIR simpliciation???
			//       probably not honestly
			//
			//       remove RC_DISCARD ??
			DIVERGING(nhir_expr(desc, stmts, expr->d_break.expr, RC_DISCARD));
			break;
		}
		case EXPR_VOIDING: {
			// no presence of voiding
			DIVERGING(nhir_expr(desc, stmts, expr->d_voiding, RC_DISCARD));
			arrpush(*stmts, *expr->d_voiding);
			*expr = (hir_expr_t){
				.kind = EXPR_TUPLE_UNIT,
				.type = TYPE_UNIT,
				.loc = expr->loc,
			};
			break;
		}
		case EXPR_LET: {
			// normalise/simplify let
			//  let a = ...
			//  let _ = ...

			pattern_t *pat = &expr->d_let.pattern;
			hir_expr_t *rhs = expr->d_let.expr;

			DIVERGING(nhir_expr(desc, stmts, rhs, RC_DISCARD));

			// let is statement ONLY
			switch (pat->kind) {
				case PATTERN_UNDERSCORE: {
					*expr = *rhs;
					break;
				}
				case PATTERN_LOCAL: {
					hir_expr_t assign = {
						.kind = EXPR_ASSIGN,
						.type = TYPE_UNIT,
						.loc = expr->loc,
						.d_assign = {
							.lhs = hir_dup((hir_expr_t){
								.kind = EXPR_LOCAL,
								.d_local = pat->d_local,
								.type = rhs->type,
							}),
							.rhs = rhs,
						},
					};

					*expr = assign;
					break;
				}
				default: {
					// TODO: else, simplify let to match
					//       convert below to match
					//
					//   let (a, b) = ...
					//
					//   match ...
					//     (a, b) = ()
					
					assert_not_reached();
				}
			}

			break;
		}
		case EXPR_POSTFIX: {
			DIVERGING(nhir_expr(desc, stmts, expr->d_postfix.expr, RC_NONE));
			break;
		}
		/* 
		case EXPR_ARRAY:
		case EXPR_TUPLE:
		case EXPR_LOOP:
		case EXPR_POSTFIX:
		case EXPR_PREFIX:
		case EXPR_DEREF:
		case EXPR_ADDR_OF:
		case EXPR_INDEX:
		case EXPR_SLICE:
		case EXPR_SYM:
		case EXPR_CAST:
		case EXPR_CONTINUE:
		case EXPR_VOIDING:
		case EXPR_FIELD:
		case EXPR_LET: */
		default: {
			printf("\n\nkind: %u\n\n", expr->kind);
			assert_not_reached();
		}
	}
	return ST_NONE;
}

void nproc(proc_t *proc) {
	hir_expr_t *stmts = NULL;
	hir_expr_t *hir = hir_dup(proc->desc.hir);

	// construct return value
	if (nhir_expr(&proc->desc, &stmts, hir, RC_NONE) != ST_DIVERGING) {
		hir_expr_t ret = {
			.kind = EXPR_RETURN,
			.type = TYPE_BOTTOM,
			.d_return = {
				.expr = hir,
			},
		};
		arrpush(stmts, ret);
	}

	proc->desc.hir = (hir_expr_t){
		.kind = EXPR_DO_BLOCK,
		.type = TYPE_BOTTOM,
		.d_do_block = {
			.exprs = stmts,
			.blk_id = BLK_ID_NONE,
		},
	};
}

void nglobal(global_t *global) {
	assert_not_reached();
}

void hir_normalisation(void) {
	for (u32 i = 0, c = arrlenu(symbols_po); i < c; i++) {
		rsym_t rsym = symbols_po[i];
		sym_t *sym = &symbols[rsym];

		switch (sym->kind) {
			case SYMBOL_PROC: {
				nproc(&sym->d_proc);
				break;
			}
			case SYMBOL_GLOBAL: {
				nglobal(&sym->d_global);
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
