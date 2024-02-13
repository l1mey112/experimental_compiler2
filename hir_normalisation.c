#include "all.h"
#include "hir.h"

enum : u8 {
	ST_NONE,
	ST_DIVERGING,
};

enum : rlocal_t {
	TARGET_DISCARD = (rlocal_t)-1,
	TARGET_RETURN = (rlocal_t)-2,
};

#define UNIT(loc_v) (hir_expr_t){ .kind = EXPR_TUPLE_UNIT, .type = TYPE_UNIT, .loc = loc_v }

typedef struct nblk_t nblk_t;

struct nblk_t {
	rlocal_t dest;
};

nblk_t blks[128];

// a ZST is a type with a single value.
// this doesn't fit in our imperative vision after normalisation.
// after normalisation, all mentions of ZSTs are removed.
// no shuffling, no construction, no nothing
static bool is_trivial_zst(type_t type) {
	if (type_is_diverging(type)) {
		return true;
	}

	u32 s = type_sizeof(&target.arch, type);
	return s == 0;
}

#define DIVERGING(expr) if ((expr) == ST_DIVERGING) { return ST_DIVERGING; }

static u8 nhir_expr(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr);
static u8 nhir_expr_target(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr, rlocal_t target);
static u8 nhir_discard_expr(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr);

static u8 nhir_loop_target(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr, rlocal_t target) {
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
	DIVERGING(nhir_discard_expr(desc, &loop_stmts, loop_expr));

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

// needs var
static bool nhir_loop_nv(hir_expr_t *expr) {
	return expr->type != TYPE_UNIT && expr->type != TYPE_BOTTOM;
}

static u8 nhir_loop(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr) {
	rlocal_t local = TARGET_DISCARD;

	if (nhir_loop_nv(expr)) {
		local = ir_local_new(desc, (local_t){
			.name = ISTR_NONE,
			.kind = LOCAL_IMM,
			.type = expr->type,
			.loc = expr->loc,
		});
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
				status = nhir_discard_expr(desc, nstmts, expr);
			} else {
				status = nhir_expr(desc, nstmts, expr);
			}

			break;
		}

		status = nhir_discard_expr(desc, nstmts, nexpr);

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

static bool nhir_do_nv(hir_expr_t *expr) {
	return expr->type != TYPE_UNIT && expr->type != TYPE_BOTTOM && !expr->d_do_block.is_single_expr;
}

static u8 nhir_do(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr) {
	rlocal_t local = TARGET_DISCARD;

	if (nhir_do_nv(expr)) {
		local = ir_local_new(desc, (local_t){
			.name = ISTR_NONE,
			.kind = LOCAL_IMM,
			.type = expr->type,
			.loc = expr->loc,
		});
	}

	return nhir_do_target(desc, stmts, expr, local);
}

static u8 nhir_if_target(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr, rlocal_t target) {
	bool has_else = expr->d_if.els != NULL;
	bool has_tmpvar = target != TARGET_DISCARD;

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
		DIVERGING(nhir_discard_expr(desc, stmts, expr->d_if.cond));
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

static bool nhir_if_nv(hir_expr_t *expr) {
	rlocal_t target = TARGET_DISCARD;

	bool has_else = expr->d_if.els != NULL;
	bool has_tmpvar = has_else && !(expr->type == TYPE_UNIT || expr->type == TYPE_BOTTOM);

	return has_tmpvar;
}

static u8 nhir_if(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr) {
	rlocal_t local = TARGET_DISCARD;

	if (nhir_if_nv(expr)) {
		local = ir_local_new(desc, (local_t){
			.name = ISTR_NONE, // tmp name
			.kind = LOCAL_IMM,
			.type = expr->type,
			.loc = expr->loc,
		});
	}

	return nhir_if_target(desc, stmts, expr, local);
}

static void nhir_assign(hir_expr_t **stmts, loc_t loc, hir_expr_t *lhs, hir_expr_t *rhs) {
	hir_expr_t assign = {
		.kind = EXPR_ASSIGN,
		.type = TYPE_UNIT,
		.loc = loc,
		.d_assign = {
			.lhs = lhs,
			.rhs = rhs,
			.kind = TOK_ASSIGN,
		},
	};

	arrpush(*stmts, assign);
}

static hir_expr_t *nhir_local_ptr(ir_desc_t *desc, loc_t loc, rlocal_t local) {
	hir_expr_t *lhs = hir_dup((hir_expr_t){
		.kind = EXPR_LOCAL,
		.d_local = local,
		.type = desc->locals[local].type,
		.loc = loc,
	});

	return lhs;
}

static void nhir_assign_local(ir_desc_t *desc, hir_expr_t **stmts, loc_t loc, rlocal_t local, hir_expr_t *rhs) {
	hir_expr_t *lhs = nhir_local_ptr(desc, loc, local);

	nhir_assign(stmts, loc, lhs, rhs);
}

// will unwrap possible side effecting lvalue to a pure expression, running the side effects once
// call if you're using an lvalue multiple times
// converting: p[k++] goes into
//
// _0 = &p[k++]
// <- *_0
static hir_expr_t *nhir_lvalue(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr) {
	// catch non side effecting expressions
	switch (expr->kind) {
		case EXPR_LOCAL: {
			return expr;
		}
		// TODO: everything else
		default: {
			break;
		}
	}

	type_t ptr_type = type_new_inc_mul(expr->type, false);

	rlocal_t _0 = ir_local_new(desc, (local_t){
		.name = ISTR_NONE,
		.kind = LOCAL_IMM,
		.type = ptr_type,
		.loc = expr->loc,
	});

	hir_expr_t *ref = hir_dup((hir_expr_t){
		.kind = EXPR_ADDR_OF,
		.type = ptr_type,
		.loc = expr->loc,
		.d_addr_of = {
			.ref = hir_dup(*expr),
			.is_mut = true,
		},
	});

	// _0 = &expr
	nhir_assign_local(desc, stmts, expr->loc, _0, ref);

	// <- *_0
	return hir_dup((hir_expr_t){
		.kind = EXPR_DEREF,
		.type = expr->type,
		.loc = expr->loc,
		.d_deref = nhir_local_ptr(desc, expr->loc, _0),
	});
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
			// infix
			DIVERGING(nhir_expr(desc, stmts, expr->d_infix.lhs));
			DIVERGING(nhir_expr(desc, stmts, expr->d_infix.rhs));

			// perform reassociation, but not constprop/eval.
			// that is left to further passes where compile errors are raised,
			// this is just to arrange expressions to catch overflow errors early.
			// we'll have futher reassociation passes in later IRs.
			//
			// overflow errors don't make sense for floating point numbers,
			// so ignore them. IEEE floats don't hold properties in the same way
			// as integers.
			//
			// RULES: `t` are terms, `c` are constants, constants are also terms
			//
			// 1. t + c  = c + t
			// 2. t * c  = c * t
			// 3. t - c  = (-c) + t
			// 4. t1 + (t2 + t3) = (t1 + t2) + t3
			// 5. t1 * (t2 * t3) = (t1 * t2) * t3
			//

			// TODO: don't perform reassoc yet, we don't currently check constant overflow

			/* hir_expr_t *lhs = expr->d_infix.lhs;
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
			expr->d_infix.rhs = rhs; */
			break;
		}
		case EXPR_RETURN: {
			DIVERGING(nhir_expr_target(desc, stmts, expr->d_return.expr, TARGET_RETURN));
			return ST_DIVERGING;
		}
		case EXPR_ASSIGN: {
			// += to +, etc. watch out for side effecting lvalues

			tok_t kind = expr->d_assign.kind;

			bool is_infix_op = kind != TOK_ASSIGN;

			DIVERGING(nhir_expr(desc, stmts, expr->d_assign.lhs));
			expr->d_assign.lhs = nhir_lvalue(desc, stmts, expr->d_assign.lhs);

			hir_expr_t *lhs = expr->d_assign.lhs;
			hir_expr_t *rhs = expr->d_assign.rhs;

			if (is_infix_op) {
				kind = tok_assign_op_to_op[kind];

				// x += 1 -> x = x + 1

				*rhs = (hir_expr_t){
					.kind = EXPR_INFIX,
					.type = expr->type,
					.loc = expr->loc,
					.d_infix = {
						.kind = kind,
						.lhs = hir_dup(*lhs),
						.rhs = hir_dup(*rhs),
					},
				};

				expr->d_assign.kind = TOK_ASSIGN;
			}

			DIVERGING(nhir_expr(desc, stmts, rhs));
			arrpush(*stmts, *expr);

			// <- lhs
			*expr = *lhs;
			break;
		}
		case EXPR_IF: {
			return nhir_if(desc, stmts, expr);
		}
		case EXPR_CALL: {
			DIVERGING(nhir_expr(desc, stmts, expr->d_call.f));

			tinfo_t *finfo = type_get(expr->d_call.f->type);
			assert(finfo->kind == TYPE_FUNCTION);
			type_t ret_type = finfo->d_fn.ret;

			hir_expr_t *nargs = NULL;

			// transform args and remove ZSTs
			for (u32 i = 0, c = arrlenu(expr->d_call.args); i < c; i++) {
				hir_expr_t *arg = &expr->d_call.args[i];
				DIVERGING(nhir_expr(desc, stmts, arg));

				if (is_trivial_zst(arg->type)) {
					nhir_discard_expr(desc, stmts, arg);
				} else {
					arrpush(nargs, *arg);
				}
			}

			expr->d_call.args = nargs;

			// __builtin_unreachable()
			if (ret_type == TYPE_BOTTOM) {
				hir_expr_t unreachable = {
					.kind = EXPR_UNREACHABLE,
					.type = TYPE_BOTTOM,
					.loc = expr->loc,
					.d_unreachable = {
						.kind = UNREACHABLE_HEURISTIC,
					},
				};

				arrpush(*stmts, *expr);
				arrpush(*stmts, unreachable);

				return ST_DIVERGING;
			}
			break;
		}
		case EXPR_ARRAY: {
			// transform elements
			for (u32 i = 0, c = arrlenu(expr->d_array); i < c; i++) {
				DIVERGING(nhir_expr(desc, stmts, &expr->d_array[i]));
			}
			break;
		}
		case EXPR_TUPLE: {
			// transform elements
			for (u32 i = 0, c = arrlenu(expr->d_tuple); i < c; i++) {
				DIVERGING(nhir_expr(desc, stmts, &expr->d_tuple[i]));
			}
			break;
		}
		case EXPR_INDEX: {
			DIVERGING(nhir_expr(desc, stmts, expr->d_index.expr));
			DIVERGING(nhir_expr(desc, stmts, expr->d_index.index));
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
			DIVERGING(nhir_discard_expr(desc, stmts, expr->d_voiding));
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
					DIVERGING(nhir_discard_expr(desc, stmts, rhs));
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

			// convert: v++
			// _0 = v + 1
			// v = _0
			// <- _0

			hir_expr_t *v = nhir_lvalue(desc, stmts, expr->d_postfix.expr);

			rlocal_t _0 = ir_local_new(desc, (local_t){
				.name = ISTR_NONE,
				.kind = LOCAL_IMM,
				.type = expr->type,
				.loc = expr->loc,
			});

			hir_expr_t v_op_1 = {
				.kind = EXPR_INFIX,
				.type = expr->type,
				.loc = expr->loc,
				.d_infix = {
					.kind = expr->d_postfix.op == EXPR_K_INC ? TOK_ADD : TOK_SUB,
					.lhs = v /* hir_dup(*v) */,
					.rhs = hir_dup((hir_expr_t){
						.kind = EXPR_INTEGER_LIT,
						.type = expr->type,
						.loc = expr->loc,
						.d_integer = 1,
					}),
				},
			};

			// _0 = v + 1
			nhir_assign_local(desc, stmts, expr->loc, _0, hir_dup(v_op_1));

			// v = _0
			nhir_assign(stmts, expr->loc, v, nhir_local_ptr(desc, expr->loc, _0));

			// <- _0
			*expr = (hir_expr_t){
				.kind = EXPR_LOCAL,
				.d_local = _0,
				.type = expr->type,
				.loc = expr->loc,
			};
			break;
		}
		case EXPR_PREFIX: {
			// -v !v
			DIVERGING(nhir_expr(desc, stmts, expr->d_prefix.expr));
			break;
		}
		/* 
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
		case EXPR_STRUCT: {
			// TODO: check if this is okay

			// lhs is assumed to be symbol, these are okay
			for (u32 i = 0, c = arrlenu(expr->d_struct.fields); i < c; i++) {
				DIVERGING(nhir_expr(desc, stmts, expr->d_struct.fields[i].expr));
			}
			break;
		}
		case EXPR_FIELD: {
			DIVERGING(nhir_expr(desc, stmts, expr->d_field.expr));
			break;
		}
		case EXPR_DEREF: {
			DIVERGING(nhir_expr(desc, stmts, expr->d_deref));
			break;
		}
		default: {
			printf("\n\nkind: %u\n\n", expr->kind);
			assert_not_reached();
		}
	}
	return ST_NONE;
}

static u8 nhir_discard_expr(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr) {
	return nhir_expr_target(desc, stmts, expr, TARGET_DISCARD);
}

// don't call directly, call `nhir_expr_target` with -1 for best results
// consume expr. will spill to stmts all effects to statements
// TODO: impl more, this is just for show
static void nhir_discard_pure(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr) {
	switch (expr->kind) {
		case EXPR_SYM:
		case EXPR_INTEGER_LIT:
		case EXPR_BOOL_LIT:
		case EXPR_TUPLE_UNIT:
		case EXPR_LOCAL: {
			return;
		}
		case EXPR_DEREF: {
			nhir_discard_pure(desc, stmts, expr->d_deref);
			break;
		}
		default: {
			arrpush(*stmts, *expr); // TODO: we don't know for sure, just keep it.
		}
	}
}

// forward the result of expressions to targets
// used to reduce the amount of temporary variables
static u8 nhir_expr_target(ir_desc_t *desc, hir_expr_t **stmts, hir_expr_t *expr, rlocal_t target) {
	// forwarding invalid targets also avoids creating a
	// new temporary that would go unused anyway

	rlocal_t otarget = target;
	bool zst = is_trivial_zst(expr->type);

	// discard all
	if (zst) {
		target = TARGET_DISCARD;
	}

	switch (expr->kind) {
		case EXPR_IF: {
			if (nhir_if_nv(expr)) {
				return nhir_if_target(desc, stmts, expr, target);
			}
			goto ndefault;
		}
		case EXPR_LOOP: {
			if (nhir_loop_nv(expr)) {
				return nhir_loop_target(desc, stmts, expr, target);
			}
			goto ndefault;
		}
		case EXPR_DO_BLOCK: {
			if (nhir_do_nv(expr)) {
				return nhir_do_target(desc, stmts, expr, target);
			}
			goto ndefault;
		}
		ndefault: default: {
			DIVERGING(nhir_expr(desc, stmts, expr));

			switch (target) {
				case TARGET_DISCARD: {
					nhir_discard_pure(desc, stmts, expr);
					break;
				}
				case TARGET_RETURN: {
					hir_expr_t ret = {
						.kind = EXPR_RETURN,
						.type = TYPE_BOTTOM,
						.loc = expr->loc,
						.d_return = {
							.expr = expr,
							.blk_id = BLK_ID_NONE,
						}
					};

					arrpush(*stmts, ret);
					return ST_DIVERGING;
				}
				default: {
					nhir_assign_local(desc, stmts, expr->loc, target, expr);
					break;
				}
			}
			break;
		}
	}

	if (zst && otarget == TARGET_RETURN) {
		hir_expr_t ret = {
			.kind = EXPR_RETURN,
			.type = TYPE_BOTTOM,
			.loc = expr->loc,
			.d_return = {
				.expr = hir_dup(UNIT(expr->loc)),
				.blk_id = BLK_ID_NONE,
			},
		};

		arrpush(*stmts, ret);
		return ST_DIVERGING;
	}

	return ST_NONE;
}

static void nproc(proc_t *proc) {
	hir_expr_t *stmts = NULL;
	hir_expr_t *hir = hir_dup(proc->desc.hir);

	bool diverging = false;

	rlocal_t *narguments = NULL;

	for (u32 i = 0, c = arrlenu(proc->arguments); i < c; i++) {
		local_t *local = &proc->desc.locals[proc->arguments[i]];

		if (type_is_diverging(local->type)) {
			diverging = true;
			local->kind = _LOCAL_ZST_DELETED;
			continue;
		}

		// can't call sizeof on diverging types
		u32 s = type_sizeof(&target.arch, local->type);

		if (s == 0) {
			local->kind = _LOCAL_ZST_DELETED;
		} else {
			arrpush(narguments, proc->arguments[i]);
		}
	}

	proc->arguments = narguments;

	// none(v: !) -> unreachable

	if (!diverging) {
		nhir_expr_target(&proc->desc, &stmts, hir, TARGET_RETURN);
	} else {
		hir_expr_t unreachable = {
			.kind = EXPR_UNREACHABLE,
			.type = TYPE_BOTTOM,
			.loc = hir->loc,
			.d_unreachable = {
				.kind = UNREACHABLE_UD2,
			},
		};

		arrpush(stmts, unreachable);
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
	// globals don't contain `ret`
	// for consteval after normalisation, we'll have to add a `ret` to the end of the block

	hir_expr_t *stmts = NULL;
	hir_expr_t *hir = hir_dup(global->desc.hir);

	nhir_expr_target(&global->desc, &stmts, hir, TARGET_RETURN);

	global->desc.hir = (hir_expr_t){
		.kind = EXPR_DO_BLOCK,
		.type = TYPE_BOTTOM,
		.d_do_block = {
			.exprs = stmts,
			.blk_id = BLK_ID_NONE,
		},
	};

	/* // eval
	extern void hir_eval_global(global_t *global);

	hir_eval_global(global); */
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
