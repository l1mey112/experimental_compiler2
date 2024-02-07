#include "all.h"
#include "hir.h"

enum : u8 {
	ST_NONE,
	ST_DIVERGING,
};

// i hate rebuilding these data structures, it's a joke

#define UNIT(loc_v) (hir_expr_t){ .kind = EXPR_TUPLE_UNIT, .type = TYPE_UNIT, .loc = loc_v }

typedef struct nblk_t nblk_t;

struct nblk_t {
	rlocal_t dest;
};

nblk_t blks[128];

#define DIVERGING(expr) if ((expr) == ST_DIVERGING) { return ST_DIVERGING; }

u8 nhir_expr(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr);
void nhir_discard_expr(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr);

u8 nhir_loop(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr) {
	// TODO: duplicate below deletions
	nblk_t* blk = &blks[expr->d_do_block.blk_id];
	*blk = (nblk_t){
		.dest = (rlocal_t)-1,
	};

	type_t loop_type = expr->type;

	if (loop_type != TYPE_UNIT && loop_type != TYPE_BOTTOM) {
		blk->dest = ir_local_new(desc, (local_t){
			.name = ISTR_NONE,
			.kind = LOCAL_IMM,
			.type = loop_type,
			.loc = expr->loc,
		});
	}

	hir_expr_t *loop_expr = expr->d_loop.expr;
	hir_expr_t *loop_stmts = NULL;

	// convert `loop expr` into:
	//
	// :0 do
	//     expr
	//     rep :0

	hir_expr_t do_res = {
		.kind = EXPR_DO_BLOCK,
		.type = loop_type != TYPE_BOTTOM ? TYPE_UNIT : TYPE_BOTTOM,
		.loc = expr->loc,
		.d_do_block = {
			.exprs = NULL,
			.blk_id = expr->d_do_block.blk_id,
		},
	};

	DIVERGING(nhir_expr(desc, &loop_stmts, loop_expr));
	nhir_discard_expr(desc, stmts, loop_expr);

	// loop to top
	hir_expr_t do_rep = {
		.kind = EXPR_CONTINUE,
		.type = TYPE_BOTTOM,
		.loc = expr->loc,
		.d_continue = {
			.blk_id = expr->d_do_block.blk_id,
		},
	};

	arrpush(loop_stmts, do_rep);

	do_res.d_do_block.exprs = loop_stmts;

	arrpush(*stmts, do_res);

	if (loop_type == TYPE_BOTTOM) {
		return ST_DIVERGING;
	}

	if (loop_type == TYPE_UNIT) {
		*expr = UNIT(expr->loc);
	} else {
		*expr = (hir_expr_t){
			.kind = EXPR_LOCAL,
			.type = loop_type,
			.d_local = blk->dest,
			.loc = expr->loc,
		};
	}

	return ST_NONE;
}

u8 nhir_do2(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr) {
	hir_expr_t do_expr = *expr;

	nblk_t* blk = &blks[do_expr.d_do_block.blk_id];
	*blk = (nblk_t){};

	bool single_brk = do_expr.d_do_block.is_single_expr;
	u32 c = arrlenu(do_expr.d_do_block.exprs);

	if (single_brk) {
		assert(do_expr.d_do_block.exprs[c - 1].kind == EXPR_BREAK);
	}

	// what a silly switch case
	switch (do_expr.type) {
		default: {
			if (!single_brk) {
				blk->dest = ir_local_new(desc, (local_t){
					.name = ISTR_NONE,
					.kind = LOCAL_IMM,
					.type = do_expr.type,
					.loc = do_expr.loc,
				});
			}
			// fallthrough
		}
		case TYPE_UNIT:
		case TYPE_BOTTOM: {
			blk->dest = (rlocal_t)-1;
			break;
		}
	}

	// used if `do` block is recreated
	hir_expr_t *sep_stmts = NULL;
	hir_expr_t **nstmts = single_brk ? stmts : &sep_stmts;
	u8 status;

	for (u32 i = 0; i < c; i++) {
		hir_expr_t *nexpr = &do_expr.d_do_block.exprs[i];

		if (i + 1 == c && single_brk) {
			*expr = *nexpr->d_break.expr;
			status = nhir_expr(desc, nstmts, expr);

			// will be discarded by further code down the line and replaced with ()
			if (status != ST_DIVERGING && do_expr.type == TYPE_UNIT) {
				nhir_discard_expr(desc, nstmts, expr);
			}

			break;
		}

		status = nhir_expr(desc, nstmts, nexpr);

		if (status == ST_DIVERGING) {
			break;
		}

		// append
		nhir_discard_expr(desc, nstmts, nexpr);
	}

	// do -> stmts
	if (!single_brk) {
		print_hint_with_pos(do_expr.loc, "teste");
		do_expr.d_do_block.exprs = sep_stmts;
		do_expr.type = do_expr.type == TYPE_BOTTOM ? TYPE_BOTTOM : TYPE_UNIT;

		arrpush(*stmts, do_expr);
	}

	// construct ret1
	if (do_expr.type == TYPE_UNIT) {
		*expr = UNIT(do_expr.loc);
		status = ST_NONE;
	} else if (do_expr.type == TYPE_BOTTOM) {
		status = ST_DIVERGING;
	} else if (!single_brk) {
		*expr = (hir_expr_t){
			.kind = EXPR_LOCAL,
			.type = do_expr.type,
			.loc = do_expr.loc,
			.d_local = blk->dest,
		};
	}

	return status;
}

// TODO: call these at discard roots on expressions that may have side effects,
//       unwrapping such into the stmts. return false if expr needs to go,
//       return true if expr needs to stay

// TODO: discard with effects will forward all side effects to a stmt and
//       RETURN NOTHING, DISCARD EXPR ITSELF ENTIRELY

// consume expr. will spill to stmts all effects to statements
// TODO: impl more, this is just for show
void nhir_discard_expr(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr) {
	switch (expr->kind) {
		case EXPR_SYM:
		case EXPR_INTEGER_LIT:
		case EXPR_BOOL_LIT:
		case EXPR_TUPLE_UNIT:
		case EXPR_LOCAL: {
			return;
		}
		default: {
			arrpush(*stmts, *expr); // TODO: we don't know for sure, just keep it.
		}
	}
}

u8 nhir_expr(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr) {
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
			DIVERGING(nhir_expr(desc, stmts, expr->d_infix.lhs));
			DIVERGING(nhir_expr(desc, stmts, expr->d_infix.rhs));

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
					case TOK_SUB: {
						// don't negate unsigned numbers
						if (type_is_unsigned(expr->type)) {
							break;
						}
						
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
			DIVERGING(nhir_expr(desc, stmts, expr->d_return.expr));
			arrpush(*stmts, *expr);
			return ST_DIVERGING;
		}
		case EXPR_ASSIGN: {
			DIVERGING(nhir_expr(desc, stmts, expr->d_assign.lhs));
			DIVERGING(nhir_expr(desc, stmts, expr->d_assign.rhs));
			arrpush(*stmts, *expr);
			// <- lhs
			*expr = *expr->d_assign.lhs;
			break;
		}
		case EXPR_IF: {
			// TODO: extract to seperate function passing in variable to convert
			//       if () else if () else if -> share same variable no duplicate `do` stmts

			DIVERGING(nhir_expr(desc, stmts, expr->d_if.cond));

			bool has_else = expr->d_if.els != NULL;
			bool has_tmpvar = has_else && !(expr->type == TYPE_UNIT || expr->type == TYPE_BOTTOM);

			// intermediary local
			rlocal_t local;
			
			if (has_tmpvar) {
				local = ir_local_new(desc, (local_t){
					.name = ISTR_NONE, // tmp name
					.kind = LOCAL_IMM,
					.type = expr->type,
					.loc = expr->loc,
				});
			}

			hir_expr_t *then_stmts = NULL;
			hir_expr_t *else_stmts = NULL;

			// generate assignments
			if (nhir_expr(desc, &then_stmts, expr->d_if.then) != ST_DIVERGING && has_tmpvar) {
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
			if (has_else && nhir_expr(desc, &else_stmts, expr->d_if.els) != ST_DIVERGING && has_tmpvar) {
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

			hir_expr_t *new_els = NULL;

			// if (c) <empty> else <empty>
			// if (c) <empty>
			// TODO: will DELETE effectful expressions!
			if (arrlenu(then_stmts) == 0 && (!has_else || arrlenu(else_stmts) == 0)) {
				nhir_discard_expr(desc, stmts, expr->d_if.cond);
				*expr = UNIT(expr->loc);
				break;
			}

			if (has_else) {
				new_els = hir_dup((hir_expr_t){
					.kind = EXPR_DO_BLOCK,
					.type = TYPE_BOTTOM,
					.d_do_block = {
						.exprs = else_stmts,
						.blk_id = BLK_ID_NONE,
					},
				});
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
					.els = new_els,
				},
			};

			arrpush(*stmts, if_stmt);

			if (has_tmpvar) {
				*expr = (hir_expr_t){
					.kind = EXPR_LOCAL,
					.type = expr->type,
					.d_local = local,
					.loc = expr->loc,
				};
			} else if (expr->type == TYPE_BOTTOM) {
				return ST_DIVERGING;
			} else {
				*expr = UNIT(expr->loc);
			}
			break;
		}
		case EXPR_CALL: {
			DIVERGING(nhir_expr(desc, stmts, expr->d_call.f));
			// transform args
			for (u32 i = 0, c = arrlenu(expr->d_call.args); i < c; i++) {
				DIVERGING(nhir_expr(desc, stmts, &expr->d_call.args[i]));
			}
			break;
		}
		case EXPR_DO_BLOCK: {
			return nhir_do2(desc, stmts, expr);
		}
		case EXPR_LOOP: {
			return nhir_loop(desc, stmts, expr);
		}
		case EXPR_CONTINUE: {
			arrpush(*stmts, *expr);
			return ST_DIVERGING;
		}
		case EXPR_BREAK: {
			nblk_t *blk = &blks[expr->d_break.blk_id];
			
			DIVERGING(nhir_expr(desc, stmts, expr->d_break.expr));

			if (blk->dest != (rlocal_t)-1) {
				assert(expr->d_break.expr->type != TYPE_UNIT || expr->d_break.expr->type != TYPE_BOTTOM);

				// transform break expr
				//
				// brk expr -> _0 = expr; brk ()

				hir_expr_t assign = {
					.kind = EXPR_ASSIGN,
					.type = TYPE_UNIT,
					.loc = expr->loc,
					.d_assign = {
						.lhs = hir_dup((hir_expr_t){
							.kind = EXPR_LOCAL,
							.d_local = blk->dest,
							.type = expr->d_break.expr->type,
						}),
						.rhs = expr->d_break.expr,
					},
				};
				
				arrpush(*stmts, assign);
			} else {
				nhir_discard_expr(desc, stmts, expr->d_break.expr);
			}

			hir_expr_t brk_unit = {
				.kind = EXPR_BREAK,
				.type = TYPE_BOTTOM,
				.loc = expr->loc,
				.d_break = {
					.blk_id = expr->d_break.blk_id,
					.expr = hir_dup(UNIT(expr->loc)),
				},
			};

			arrpush(*stmts, brk_unit);
			return ST_DIVERGING;
		}
		case EXPR_VOIDING: {
			// no presence of voiding
			DIVERGING(nhir_expr(desc, stmts, expr->d_voiding));
			nhir_discard_expr(desc, stmts, expr->d_voiding);
			*expr = UNIT(expr->loc);
			break;
		}
		case EXPR_LET: {
			// normalise/simplify let
			//  let a = ...
			//  let _ = ...

			pattern_t *pat = &expr->d_let.pattern;
			hir_expr_t *rhs = expr->d_let.expr;

			DIVERGING(nhir_expr(desc, stmts, rhs));

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
			// TODO: cannot v++ -> v = v + 1 as v can be ANY lvalue
			//       preferable we perform this transformation
			// TODO: spill lvalue to lvalue if possible
			DIVERGING(nhir_expr(desc, stmts, expr->d_postfix.expr));
			break;
		}
		case EXPR_PREFIX: {
			// -v !v
			DIVERGING(nhir_expr(desc, stmts, expr->d_prefix.expr));
			break;
		}
		/* 
		case EXPR_ARRAY:
		case EXPR_TUPLE:
		case EXPR_LOOP:
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
		case EXPR_CAST: {
			// TODO: `value: !` IS illegal, not currently though we don't have good casting rules/matrix
			DIVERGING(nhir_expr(desc, stmts, expr->d_cast.expr));
			break;
		}
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
	if (nhir_expr(&proc->desc, &stmts, hir) != ST_DIVERGING) {
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
