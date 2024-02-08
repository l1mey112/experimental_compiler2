#include "all.h"
#include "hir.h"

enum : u8 {
	ST_NONE,
	ST_DIVERGING,
};

#define UNIT(loc_v) (hir_expr_t){ .kind = EXPR_TUPLE_UNIT, .type = TYPE_UNIT, .loc = loc_v }

typedef struct nblk_t nblk_t;

struct nblk_t {
	rlocal_t dest;
};

nblk_t blks[128];

#define DIVERGING(expr) if ((expr) == ST_DIVERGING) { return ST_DIVERGING; }

static u8 nhir_expr(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr);
static u8 nhir_expr_target(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr, rlocal_t target);

static u8 nhir_loop_target(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr, rlocal_t target) {
	// TODO: duplicate below deletions
	nblk_t* blk = &blks[expr->d_do_block.blk_id];
	*blk = (nblk_t){
		.dest = target,
	};

	type_t loop_type = expr->type;

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

	// discard
	DIVERGING(nhir_expr_target(desc, &loop_stmts, loop_expr, (rlocal_t)-1));

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

static u8 nhir_loop(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr) {
	type_t loop_type = expr->type;

	rlocal_t local;

	if (loop_type != TYPE_UNIT && loop_type != TYPE_BOTTOM) {
		local = ir_local_new(desc, (local_t){
			.name = ISTR_NONE,
			.kind = LOCAL_IMM,
			.type = loop_type,
			.loc = expr->loc,
		});
	} else {
		local = (rlocal_t)-1;
	}

	return nhir_loop_target(desc, stmts, expr, local);
}

static u8 nhir_do_target(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr, rlocal_t target) {
	hir_expr_t do_expr = *expr;

	nblk_t* blk = &blks[do_expr.d_do_block.blk_id];
	*blk = (nblk_t){
		.dest = target,
	};

	bool single_brk = do_expr.d_do_block.is_single_expr;
	u32 c = arrlenu(do_expr.d_do_block.exprs);

	if (single_brk) {
		assert(do_expr.d_do_block.exprs[c - 1].kind == EXPR_BREAK);
	}

	// used if `do` block is recreated
	hir_expr_t *sep_stmts = NULL;
	hir_expr_t **nstmts = single_brk ? stmts : &sep_stmts;
	u8 status;

	for (u32 i = 0; i < c; i++) {
		hir_expr_t *nexpr = &do_expr.d_do_block.exprs[i];

		if (i + 1 == c && single_brk) {
			*expr = *nexpr->d_break.expr;

			// will be discarded by further code down the line and replaced with ()
			if (do_expr.type == TYPE_UNIT || do_expr.type == TYPE_BOTTOM) {
				status = nhir_expr_target(desc, nstmts, expr, (rlocal_t)-1);
			} else {
				status = nhir_expr(desc, nstmts, expr);
			}

			break;
		}

		status = nhir_expr_target(desc, nstmts, nexpr, (rlocal_t)-1);

		if (status == ST_DIVERGING) {
			break;
		}
	}

	// construct ret
	if (do_expr.type == TYPE_UNIT) {
		*expr = UNIT(do_expr.loc);
		status = ST_NONE;
	} else if (do_expr.type == TYPE_BOTTOM) {
		assert(status == ST_DIVERGING);
	} else if (!single_brk) {
		*expr = (hir_expr_t){
			.kind = EXPR_LOCAL,
			.type = do_expr.type,
			.loc = do_expr.loc,
			.d_local = blk->dest,
		};
		status = ST_NONE;
	} else {
		// expr is already set, it's the last expression
	}

	// do -> stmts
	if (!single_brk) {
		do_expr.d_do_block.exprs = sep_stmts;
		do_expr.type = do_expr.type == TYPE_BOTTOM ? TYPE_BOTTOM : TYPE_UNIT;

		arrpush(*stmts, do_expr);
	}

	return status;
}

static u8 nhir_do(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr) {
	rlocal_t local;
	bool single_brk = expr->d_do_block.is_single_expr;

	// what a silly switch case
	switch (expr->type) {
		default: {
			if (!single_brk) {
				local = ir_local_new(desc, (local_t){
					.name = ISTR_NONE,
					.kind = LOCAL_IMM,
					.type = expr->type,
					.loc = expr->loc,
				});
				break;
			}
			// fallthrough
		}
		case TYPE_UNIT:
		case TYPE_BOTTOM: {
			local = (rlocal_t)-1;
			break;
		}
	}

	return nhir_do_target(desc, stmts, expr, local);
}

static u8 nhir_if_target(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr, rlocal_t target) {
	bool has_else = expr->d_if.els != NULL;
	bool has_tmpvar = target != (rlocal_t)-1;

	hir_expr_t *then_stmts = NULL;
	hir_expr_t *else_stmts = NULL;

	u8 status;

	// normalise `else` and `then` first before `cond`

	// don't rely on `!` as a means for diverging, it's possible the checker misses some propagation
	status = nhir_expr_target(desc, &then_stmts, expr->d_if.then, target);
	if (has_else) {
		status &= nhir_expr_target(desc, &else_stmts, expr->d_if.els, target);
	} else {
		status = ST_NONE;
	}

	// if (c) <empty> else <empty>
	// if (c) <empty>
	if (arrlenu(then_stmts) == 0 && (!has_else || arrlenu(else_stmts) == 0)) {
		DIVERGING(nhir_expr_target(desc, stmts, expr->d_if.cond, (rlocal_t)-1));
		*expr = UNIT(expr->loc);
		return ST_NONE;
	} else {
		DIVERGING(nhir_expr(desc, stmts, expr->d_if.cond));
	}

	hir_expr_t *new_els = NULL;

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
			.d_local = target,
			.loc = expr->loc,
		};
	} else if (expr->type == TYPE_BOTTOM) {
		assert(status == ST_DIVERGING);
	} else {
		*expr = UNIT(expr->loc);
	}
	return status;
}

static u8 nhir_if(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr) {
	bool has_else = expr->d_if.els != NULL;
	bool has_tmpvar = has_else && !(expr->type == TYPE_UNIT || expr->type == TYPE_BOTTOM);

	// intermediary local
	rlocal_t target;

	if (has_tmpvar) {
		target = ir_local_new(desc, (local_t){
			.name = ISTR_NONE, // tmp name
			.kind = LOCAL_IMM,
			.type = expr->type,
			.loc = expr->loc,
		});
	} else {
		target = (rlocal_t)-1;
	}

	return nhir_if_target(desc, stmts, expr, target);
}

static u8 nhir_expr(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr) {
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
			// TODO: SIDE EFFECTING LVALUES!
			DIVERGING(nhir_expr(desc, stmts, expr->d_assign.lhs));
			DIVERGING(nhir_expr(desc, stmts, expr->d_assign.rhs));
			arrpush(*stmts, *expr);
			// <- lhs
			*expr = *expr->d_assign.lhs;
			break;
		}
		case EXPR_IF: {
			return nhir_if(desc, stmts, expr);
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
			return nhir_do(desc, stmts, expr);
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

			// `blk->dest` can be -1
			DIVERGING(nhir_expr_target(desc, stmts, expr->d_break.expr, blk->dest));

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
			DIVERGING(nhir_expr_target(desc, stmts, expr->d_voiding, (rlocal_t)-1));
			*expr = UNIT(expr->loc);
			break;
		}
		case EXPR_LET: {
			// normalise/simplify let
			//  let a = ...
			//  let _ = ...

			pattern_t *pat = &expr->d_let.pattern;
			hir_expr_t *rhs = expr->d_let.expr;

			// let is statement ONLY
			switch (pat->kind) {
				case PATTERN_UNDERSCORE: {
					DIVERGING(nhir_expr_target(desc, stmts, rhs, (rlocal_t)-1));
					break;
				}
				case PATTERN_LOCAL: {
					DIVERGING(nhir_expr_target(desc, stmts, rhs, pat->d_local));
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

			// let evaluates to (), is always a statement though
			*expr = UNIT(expr->loc);
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

// don't call directly, call `nhir_expr_target` with -1 for best results
// consume expr. will spill to stmts all effects to statements
// TODO: impl more, this is just for show
static void nhir_discard_expr(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr) {
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

// forward the result of expressions to targets
// used to reduce the amount of temporary variables
static u8 nhir_expr_target(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr, rlocal_t target) {
	// target == -1, discard

	// forwarding invalid targets also avoids creating a
	// new temporary that would go unused anyway

	switch (expr->kind) {
		case EXPR_IF: {
			// unlike `loop` and `do`, values are returned through the target without `brk`
			bool has_else = expr->d_if.els != NULL;
			bool has_tmpvar = has_else && !(expr->type == TYPE_UNIT || expr->type == TYPE_BOTTOM);
			if (has_tmpvar) {
				return nhir_if_target(desc, stmts, expr, target);
			}
			goto ndefault;
		}
		case EXPR_LOOP: {
			return nhir_loop_target(desc, stmts, expr, target);
		}
		case EXPR_DO_BLOCK: {
			return nhir_do_target(desc, stmts, expr, target);
		}
		ndefault: default: {
			DIVERGING(nhir_expr(desc, stmts, expr));

			if (target == (rlocal_t)-1) {
				nhir_discard_expr(desc, stmts, expr);
			} else {
				hir_expr_t assign = {
					.kind = EXPR_ASSIGN,
					.type = TYPE_UNIT,
					.loc = expr->loc,
					.d_assign = {
						.lhs = hir_dup((hir_expr_t){
							.kind = EXPR_LOCAL,
							.d_local = target,
							.type = expr->type,
						}),
						.rhs = expr,
					},
				};

				arrpush(*stmts, assign);
			}
			break;
		}
	}

	return ST_NONE;
}

static void nproc(proc_t *proc) {
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

static void nglobal(global_t *global) {
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
