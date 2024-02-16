#include "check.h"
#include "all.h"
#include "hir.h"

// TODO: in the future, use a zero cost invariant/assert marker
//
//     cdo(desc: *ir_desc_t, upvalue: type_t, expr: *hir_expr_t): ()
//         invariant expr.kind == EXPR_DO_BLOCK
//     = {
//         ...
//     }
//

// sets expr->type
void cdo(ir_desc_t *desc, type_t upvalue, hir_expr_t *expr) {
	u32 blk_id = expr->d_do_block.blk_id;
	cblk_t *blk = &c.blocks[blk_id];

	// all do blocks have at least one expr

	*blk = (cblk_t){
		.brk_type = TYPE_INFER,
		.upvalue = upvalue,
	};

	u32 stmts_len = arrlenu(expr->d_do_block.exprs);
	for (u32 i = 0, c = stmts_len; i < c; i++) {
		hir_expr_t *stmt = &expr->d_do_block.exprs[i];
		// nonsensical to pass upvalue
		type_t type = cexpr(desc, TYPE_INFER, stmt, BM_RVALUE);
		if (type == TYPE_BOTTOM && i + 1 < c) {
			err_with_pos(expr->d_do_block.exprs[i + 1].loc, "unreachable code");
		}
	}

	hir_expr_t *stmt_last = &expr->d_do_block.exprs[stmts_len - 1];

	// singular exit brk, and no rep
	expr->d_do_block.is_single_expr = blk->brk_type == TYPE_INFER && !blk->is_rep;

	// TODO: what happens on `stmt_last !` ???

	// brk () || no brk && expr: ()
	//if (stmt_last->type != TYPE_BOTTOM) {
	if (blk->brk_type == TYPE_UNIT || (blk->brk_type == TYPE_INFER && upvalue == TYPE_UNIT)) {
		hir_expr_t brk_unit = (hir_expr_t){
			.kind = EXPR_BREAK,
			.loc = stmt_last->loc,
			.type = TYPE_BOTTOM,
			.d_break = {
				.blk_id = blk_id,
				.expr = hir_dup((hir_expr_t){
					.kind = EXPR_TUPLE_UNIT,
					.loc = stmt_last->loc,
					.type = TYPE_UNIT,
				}),
			},
		};

		blk->brk_type = TYPE_UNIT;
		arrpush(expr->d_do_block.exprs, brk_unit);
	} else {
		if (blk->brk_type != TYPE_INFER) {
			ctype_unify(blk->brk_type, stmt_last);
		} else {
			blk->brk_type = stmt_last->type;
		}

		hir_expr_t brk_last = (hir_expr_t){
			.kind = EXPR_BREAK,
			.loc = stmt_last->loc,
			.type = TYPE_BOTTOM,
			.d_break = {
				.blk_id = blk_id,
				.expr = NULL,
			},
		};

		// these should compile into eachother, a `let` "expr" is supposed to
		// be basically a statement, evaluating to ()
		//
		// do
		//     let a = ...
		//
		// :0 do
		//     let a = ...
		//     brk :0 ()
		//
		if (stmt_last->kind == EXPR_LET) {
			brk_last.d_break.expr = hir_dup((hir_expr_t){
				.kind = EXPR_TUPLE_UNIT,
				.loc = stmt_last->loc,
				.type = TYPE_UNIT,
			});

			arrpush(expr->d_do_block.exprs, brk_last);
		} else {
			brk_last.d_break.expr = hir_dup(*stmt_last);

			*stmt_last = brk_last;
		}
	}
	//}

	expr->type = blk->brk_type;
}

// sets expr->type
void cloop(ir_desc_t *desc, type_t upvalue, hir_expr_t *expr) {
	cblk_t *blk = &c.blocks[expr->d_loop.blk_id];

	*blk = (cblk_t){
		.brk_type = TYPE_INFER,
		.upvalue = upvalue,
	};

	(void)cexpr(desc, TYPE_UNIT, expr->d_loop.expr, BM_RVALUE);

	// if a loop block has no breaks, it loops forever
	if (blk->brk_type == TYPE_INFER) {
		blk->brk_type = TYPE_BOTTOM;
	}

	expr->type = blk->brk_type;
}

// assumes !bool_op and !cmp_op
// assumes checked RHS
void cinfix_rhs_arith_op(ir_desc_t *desc, loc_t onerror, type_t lhs_t, tok_t kind, hir_expr_t *rhs) {

	bool is_ptr_arith = false;

	ti_kind lhs_kind = type_kind(lhs_t);	
	type_t rhs_t = rhs->type;

	// TODO: will need to add in *opaque pointers, no arith on these
	// TODO: introduce proper bounded ptr types for such too
	// TODO: better errors and proper semantics, look at real compilers

	if (lhs_kind == TYPE_PTR && (kind == TOK_ADD || kind == TOK_SUB)) {
		is_ptr_arith = true;
	} else if (lhs_kind == TYPE_PTR) {
		err_with_pos(onerror, "invalid operation `%s` on pointer type `%s`", tok_op_str(kind), type_dbg_str(lhs_t));
	}

	assert(!is_ptr_arith && "TODO: ptr arith proper types");

	if (lhs_t != TYPE_BOTTOM) {
		if (!is_ptr_arith && !type_is_number(lhs_t)) {
			err_with_pos(onerror, "invalid operation `%s` on non numeric type `%s`", tok_op_str(kind), type_dbg_str(lhs_t));
		}
	}

	if (rhs_t != TYPE_BOTTOM) {
		if (!type_is_number(rhs_t)) {
			err_with_pos(onerror, "invalid operation `%s` on non numeric type `%s`", tok_op_str(kind), type_dbg_str(rhs_t));
		}
	}
}

void cinfix(ir_desc_t *desc, type_t upvalue, hir_expr_t *expr) {
	hir_expr_t *lhs = expr->d_infix.lhs;
	hir_expr_t *rhs = expr->d_infix.rhs;
	tok_t kind = expr->d_infix.kind;

	bool is_bool_op = kind == TOK_AND || kind == TOK_OR;
	bool is_cmp_op = kind == TOK_EQ || kind == TOK_NE || kind == TOK_LT || kind == TOK_GT || kind == TOK_LE || kind == TOK_GE;

	type_t lhs_t = cexpr(desc, upvalue, lhs, BM_RVALUE);
	type_t rhs_t = cexpr(desc, lhs_t, rhs, BM_RVALUE);

	if (is_bool_op) {
		// TODO: ! && _ -> !
		// TODO: ! || _ -> !

		(void)ctype_unify(TYPE_BOOL, lhs);
		(void)ctype_unify(TYPE_BOOL, rhs);

		expr->type = TYPE_BOOL;
	} else {
		(void)ctype_unify(lhs_t, rhs);

		if (is_cmp_op) {
			expr->type = TYPE_BOOL;
		} else {
			cinfix_rhs_arith_op(desc, expr->loc, lhs_t, kind, rhs);
			expr->type = lhs_t;
		}
	}
}

// TODO: doesn't support `immutable` properly
//       let g; g = 50 should work, but it doesn't
void clvalue(ir_desc_t *desc, hir_expr_t *expr, bool is_mutable) {
	switch (expr->kind) {
		case EXPR_LOCAL: {
			local_t *local = &desc->locals[expr->d_local];
			if (is_mutable && local->kind != LOCAL_MUT) {
				// TODO: then pull a rust and suggest on a loc_t?
				print_err_with_pos(expr->loc, "cannot mutate immutable variable");
				print_hint_with_pos(local->loc, "variable `%s` declared here", sv_from(local->name));
				err_unwind();
			}
			break;
		}
		case EXPR_SYM: {
			sym_t *symbol = &symbols[expr->d_sym];

			switch (symbol->kind) {
				case SYMBOL_PROC: {
					if (is_mutable) {
						err_with_pos(expr->loc, "cannot mutate function `%s`", sv_from(symbol->short_name));
					}
					break;
				}
				case SYMBOL_GLOBAL: {
					if (is_mutable && !symbol->d_global.is_mut) {
						err_with_pos(expr->loc, "cannot mutate immutable global variable `%s`", sv_from(symbol->short_name));
					}
					break;
				}
				default: {
					assert_not_reached();
				}
			}
			break;
		}
		case EXPR_DEREF: {
			type_t ptr_type = expr->d_deref->type;
			assert(type_kind(ptr_type) == TYPE_PTR);
			tinfo_t *info = type_get(ptr_type);

			if (is_mutable && !info->d_ptr.is_mut) {
				err_with_pos(expr->loc, "cannot mutate immutable pointer `%s`", type_dbg_str(ptr_type));
			}
			break;
		}
		case EXPR_INDEX: {
			// TODO: mutability of slices and so on...
			// TODO: really need to standardise pointer types
			assert_not_reached();
			break;
		}
		// TODO: field accesses since we already support them
		default: {
			err_with_pos(expr->loc, "this expression is not an lvalue");
		}
	}
}

void cassign(ir_desc_t *desc, hir_expr_t *expr) {
	tok_t kind = expr->d_assign.kind;
	bool is_infix_op = kind != TOK_ASSIGN;

	hir_expr_t *lhs = expr->d_infix.lhs;
	hir_expr_t *rhs = expr->d_infix.rhs;

	type_t lhs_type;

	// let g; g = ...
	if (lhs->kind == EXPR_LOCAL && desc->locals[lhs->d_local].type == TYPE_INFER) {
		type_t rhs_type = cexpr(desc, TYPE_INFER, rhs, BM_RVALUE);
		desc->locals[lhs->d_local].type = rhs_type;
		// assign local stuffs
		lhs_type = cexpr(desc, TYPE_INFER, lhs, BM_LVALUE);
	} else {
		lhs_type = cexpr(desc, TYPE_INFER, lhs, BM_LVALUE);
		(void)cexpr(desc, lhs_type, rhs, BM_RVALUE);
	}

	clvalue(desc, lhs, true);

	// a += 20 -> a = a + 20
	if (is_infix_op) {
		cinfix_rhs_arith_op(desc, expr->loc, lhs_type, tok_assign_op_to_op[kind], rhs);
	}

	// lhs = rhs
	ctype_unify(lhs_type, rhs);

	expr->type = lhs_type;
}

// check trivial irrefutable patterns, leave everything else up to dataflow analysis
void cpattern(ir_desc_t *desc, pattern_t *pattern, type_t type) {
	switch (pattern->kind) {
		case PATTERN_TUPLE: {
			if (type_kind(type) != TYPE_TUPLE) {
				err_with_pos(pattern->loc, "type mismatch: expected tuple, got `%s`", type_dbg_str(type));
			}
			tinfo_t *tinfo = type_get(type);
			if (arrlenu(tinfo->d_tuple) != arrlenu(pattern->d_tuple)) {
				// TODO: printing pattern???
				err_with_pos(pattern->loc, "cannot match tuple pattern against tuple type `%s` of different length", type_dbg_str(type));
			}
			for (size_t i = 0; i < arrlenu(pattern->d_tuple); i++) {
				cpattern(desc, &pattern->d_tuple[i], tinfo->d_tuple[i]);
			}
			break;
		}
		case PATTERN_TUPLE_UNIT: {
			if (type != TYPE_UNIT) {
				err_with_pos(pattern->loc, "type mismatch: expected `()`, got `%s`", type_dbg_str(type));
			}
			break;
		}
		case PATTERN_LOCAL: {
			local_t *local = &desc->locals[pattern->d_local];
			if (local->type == TYPE_INFER) {
				local->type = type;
			} else {
				(void)ctype_unify_type(local->type, type, pattern->loc);
			}
			break;
		}
		case PATTERN_INTEGER_LIT: {
			// TODO: check integer is inbounds for type
			if (type_is_number(type)) {
				err_with_pos(pattern->loc, "type mismatch: expected integer, got `%s`", type_dbg_str(type));
			}
			break;
		}
		case PATTERN_UNDERSCORE: {
			break;
		}
		case PATTERN_ARRAY: {
			// [xs..., x] and [x, ...xs]
			//
			// never [xs...] and [...xs]
			//
			// []T and [3]T match [a, b, c]
			// []T and [3]T match [a, b, ...c]

			type_t elem_type;
			size_t elems = 0; // can't be 0

			switch (type_kind(type)) {
				case TYPE_SLICE: {
					elem_type = type_get(type)->d_slice.elem;
					break;
				}
				case TYPE_ARRAY: {
					elem_type = type_get(type)->d_array.elem;
					elems = type_get(type)->d_array.length;
					break;
				}
				default: {
					err_with_pos(pattern->loc, "cannot match array pattern against non-array type `%s`", type_dbg_str(type));
				}
			}

			size_t len = arrlenu(pattern->d_array.elems);

			if (pattern->d_array.match) {
				type_t make_slice = type_array_or_slice_to_slice(type);
				cpattern(desc, pattern->d_array.match, make_slice);

				// error on:
				//   [3]T -> [a, b, c, ...xs]
				//   [3]T -> [xs..., a, b, c]
				len++;
			}

			for (u32 i = 0, c = arrlenu(pattern->d_array.elems); i < c; i++) {
				cpattern(desc, &pattern->d_array.elems[i], elem_type);
			}

			if (elems != 0 && len != elems) {
				err_with_pos(pattern->loc, "cannot match array pattern against array type `%s` of different length", type_dbg_str(type));
			}
			break;
		}
		default: {
			assert_not_reached();
		}
	}
}

static void integer_fits_in(loc_t onerror, type_t type, u64 lit) {
	if (!type_integer_fits_in(&abi, lit, type)) {
		err_with_pos(onerror, "integer literal cannot fit inside type `%s`", type_dbg_str(type));
	}
}

static void cinteger_lit(type_t upvalue, hir_expr_t *expr) {
	if (upvalue != TYPE_INFER && type_is_integer(upvalue)) {
		expr->type = upvalue;
	} else {
		expr->type = TYPE_I32;
	}
}

type_t cexpr(ir_desc_t *desc, type_t upvalue, hir_expr_t *expr, u8 cfg) {
	switch (expr->kind) {
		case EXPR_INTEGER_LIT: {
			cinteger_lit(upvalue, expr);
			integer_fits_in(expr->loc, expr->type, expr->d_integer);
			break;
		}
		case EXPR_BOOL_LIT: {
			expr->type = TYPE_BOOL;
			break;
		}
		case EXPR_ARRAY: {
			assert(expr->d_array != NULL); // TODO: handle []

			// never implicit, always demote to slice when upvalue only 
			//
			type_t elem_upvale = TYPE_INFER;
			bool is_slice = false;
			if (upvalue != TYPE_INFER) {
				switch (type_kind(upvalue)) {
					case TYPE_ARRAY: {
						elem_upvale = type_get(upvalue)->d_array.elem;
						break;
					}
					case TYPE_SLICE: {
						elem_upvale = type_get(upvalue)->d_slice.elem;
						is_slice = true;
						break;
					}
					default: {
						break;
					}
				}
			}

			type_t elem_type = cexpr(desc, elem_upvale, &expr->d_array[0], BM_RVALUE);
			// []elem_type

			u32 len = arrlenu(expr->d_array);

			for (u32 i = 1; i < len; i++) {
				hir_expr_t *elem = &expr->d_array[i];

				type_t type = cexpr(desc, elem_type, elem, BM_RVALUE);
				ctype_unify(type, elem);
			}

			tinfo_t typeinfo;
			if (is_slice) {
				typeinfo = (tinfo_t){
					.kind = TYPE_SLICE,
					.d_slice = {
						.elem = elem_type,
					}
				};
			} else {
				typeinfo = (tinfo_t){
					.kind = TYPE_ARRAY,
					.d_array = {
						.elem = elem_type,
						.length = len,
					}
				};
			}

			expr->type = type_new(typeinfo);
			break;
		}
		case EXPR_TUPLE_UNIT: {
			expr->type = TYPE_UNIT;
			break;
		}
		case EXPR_TUPLE: {
			// unpack upvalue if applicable
			type_t *upvales = NULL;
			if (upvalue != TYPE_INFER && type_kind(upvalue) == TYPE_TUPLE) {
				upvales = type_get(upvalue)->d_tuple;
				if (arrlenu(upvales) != arrlenu(expr->d_tuple)) {
					upvales = NULL;
				}
			}

			type_t *types = NULL;

			for (u32 i = 0, c = arrlenu(expr->d_tuple); i < c; i++) {
				type_t upvalue = TYPE_INFER;
				if (upvales) {
					upvalue = upvales[i];
				}
				hir_expr_t *inner = &expr->d_tuple[i];
				type_t type = cexpr(desc, upvalue, inner, BM_RVALUE);
				ctype_unify(type, inner);
				arrpush(types, type);
			}

			tinfo_t info = (tinfo_t){
				.kind = TYPE_TUPLE,
				.d_tuple = types,
			};

			expr->type = type_new(info);
			break;
		}
		/* case EXPR_MATCH: {

		} */
		case EXPR_DO_BLOCK: {
			cdo(desc, upvalue, expr);
			break;
		}
		case EXPR_LOOP: {
			cloop(desc, upvalue, expr);
			break;
		}
		case EXPR_IF: {
			// TODO: if (!) ... else ... = !
			// TODO: a then: ! unify els: T -> ! when in reality the !
			//       should be stamped out. ctype unify should ignore !
			// TODO: formalise unification semantics, possibly introduce
			//       a seperate function for propagating !

			(void)cexpr(desc, TYPE_BOOL, expr->d_if.cond, BM_RVALUE);
			(void)ctype_unify(TYPE_BOOL, expr->d_if.cond);

			type_t then_type = cexpr(desc, upvalue, expr->d_if.then, BM_RVALUE);
			if (expr->d_if.els) {
				type_t else_type = cexpr(desc, upvalue, expr->d_if.els, BM_RVALUE);
				expr->type = ctype_unify(then_type, expr->d_if.els);				
			} else {
				// fake construction of type unit will be unwrapped by normalisation passes
				expr->type = TYPE_UNIT;
			}
			break;
		}
		case EXPR_ASSIGN: {
			cassign(desc, expr);
			break;
		}
		case EXPR_INFIX: {
			cinfix(desc, upvalue, expr);
			break;
		}
		case EXPR_POSTFIX: {
			// v++ | v--
			type_t type = cexpr(desc, upvalue, expr->d_postfix.expr, BM_LVALUE);
			clvalue(desc, expr->d_postfix.expr, true);
			if (!type_is_number(type)) {
				err_with_pos(expr->loc, "type mismatch: expected numeric type, got `%s`", type_dbg_str(type));
			}
			expr->type = type;
			break;
		}
		case EXPR_PREFIX: {
			// don't raise error on (-128):i8 through it recursing and
			// checking (128):i8 without the negation. check seperately
			type_t type;
			if (expr->d_prefix.op == EXPR_K_SUB && expr->d_prefix.expr->kind == EXPR_INTEGER_LIT) {
				cinteger_lit(upvalue, expr->d_prefix.expr);				
				type = expr->d_prefix.expr->type;
				integer_fits_in(expr->loc, type, -expr->d_prefix.expr->d_integer);
			} else {
				type = cexpr(desc, upvalue, expr->d_prefix.expr, BM_RVALUE);
			}

			switch (expr->d_prefix.op) {
				case EXPR_K_NOT: {
					(void)ctype_unify(TYPE_BOOL, expr->d_prefix.expr);
					expr->type = TYPE_BOOL;
					break;
				}
				case EXPR_K_SUB: {
					if (!type_is_number(type)) {
						err_with_pos(expr->loc, "type mismatch: expected numeric type, got `%s`", type_dbg_str(type));
					}
					expr->type = type;
					break;
				}
				default: {
					assert_not_reached();
				}
			}
			break;
		}
		case EXPR_DEREF: {
			// lvalue needs to propagate cfg
			type_t type = cexpr(desc, TYPE_INFER, expr->d_deref, cfg);
			if (type_kind(type) != TYPE_PTR) {
				err_with_pos(expr->loc, "type mismatch: expected pointer type, got `%s`", type_dbg_str(type));
			}
			expr->type = type_get(type)->d_ptr.ref;
			break;
		}
		case EXPR_ADDR_OF: {
			// possibly below.
			
			// ??? allowed to take the address of absolutely ANYTHING
			// normalisation pass will desugar into explicit seperate locals
			// cannot &'temporary. no point.

			// TODO: taking address of functions should only be possible on locals and symbols
			//       fuck this is complicated.. what about &structfield.function ??
			// TODO: &&v -> will be parsed as boolean && not ref-ref
			// TODO: is_literal() to check if it's fine

			bool is_mut_ref = expr->d_addr_of.is_mut;
			hir_expr_t *ref = expr->d_addr_of.ref;

			type_t type = cexpr(desc, TYPE_INFER, ref, BM_LVALUE);
			clvalue(desc, ref, is_mut_ref);

			// &function -> function
			if (type_kind(type) == TYPE_FUNCTION) {
				if (is_mut_ref) {
					err_with_pos(expr->loc, "can't take mutable address of function");
				}
				*expr = *expr->d_addr_of.ref;
			} else {
				type = type_new_inc_mul(type, is_mut_ref);
			}

			expr->type = type;
			break;
		}
		case EXPR_INDEX: {
			// lvalue needs to propagate cfg
			type_t type = cexpr(desc, TYPE_INFER, expr->d_index.expr, cfg);

			if (type_kind(type) != TYPE_ARRAY && type_kind(type) != TYPE_SLICE) {
				err_with_pos(expr->d_index.expr->loc, "type mismatch: expected array or slice type, got `%s`", type_dbg_str(type));
			}

			type_t index_type = cexpr(desc, TYPE_USIZE, expr->d_index.index, BM_RVALUE);
			ctype_unify(TYPE_USIZE, expr->d_index.index);

			expr->type = type_array_or_slice_elem(type);
			break;
		}
		case EXPR_SLICE: {
			// possibly? lvalue needs to propagate cfg
			type_t type = cexpr(desc, TYPE_INFER, expr->d_slice.expr, cfg);

			if (type_kind(type) != TYPE_ARRAY && type_kind(type) != TYPE_SLICE) {
				err_with_pos(expr->d_slice.expr->loc, "type mismatch: expected array or slice type, got `%s`", type_dbg_str(type));
			}

			if (expr->d_slice.lo) {
				type_t index_type = cexpr(desc, TYPE_USIZE, expr->d_slice.lo, BM_RVALUE);
				ctype_unify(TYPE_USIZE, expr->d_slice.lo);
			}

			if (expr->d_slice.hi) {
				type_t index_type = cexpr(desc, TYPE_USIZE, expr->d_slice.hi, BM_RVALUE);
				ctype_unify(TYPE_USIZE, expr->d_slice.hi);
			}

			expr->type = type_array_or_slice_to_slice(type);
			break;
		}
		case EXPR_LOCAL: {
			// TODO: nullary `f` doesn't take an () argument
			//       to get the addr of function, you must do `&f`
			
			local_t *local = &desc->locals[expr->d_local];

			if (local->type == TYPE_INFER) {
				err_with_pos(expr->loc, "can't infer type");
			}

			expr->type = local->type;

			cufcs_autocall(desc, expr, cfg); // promote and recheck if rvalue
			break;
		}
		case EXPR_SYM: {
			// TODO: comment about local above
			//       will need distinction about using as an lvalue or not
			
			sym_t *symbol = &symbols[expr->d_sym];
			type_t type;

			// the check reorder pass should assert that all symbols
			// pass a sanity check and all uses make sense.
			
			switch (symbol->kind) {
				case SYMBOL_PROC: {
					type = symbol->d_proc.type;
					break;
				}
				case SYMBOL_GLOBAL: {
					type = symbol->d_global.type;
					break;
				}
				default: {
					// SYMBOL_TYPE is unreachable
					assert_not_reached();
				}
			}

			assert(type != TYPE_INFER);

			expr->type = type;

			cufcs_autocall(desc, expr, cfg); // promote and recheck if rvalue
			break;
		}
		case EXPR_CAST: {
			type_t type = cexpr(desc, expr->type, expr->d_cast.expr, BM_RVALUE);

			// remove cast, it's already the right type
			if (type == expr->type) {
				*expr = *expr->d_cast.expr;
			}

			// TODO: check if this cast is even possible.
			//       good places to start is just reading source code of major compilers.

			// expr->type is cast type already
			break;	
		}
		case EXPR_CALL: {
			ccall(desc, upvalue, expr);
			break;
		}
		case EXPR_BREAK: {
			cblk_t *blk = &c.blocks[expr->d_break.blk_id]; // 1:1 correspondence

			type_t brk_type = cexpr(desc, blk->upvalue, expr->d_break.expr, BM_RVALUE);

			// no ! precedence
			if (blk->brk_type == TYPE_INFER || blk->brk_type == TYPE_BOTTOM) {
				blk->brk_type = brk_type;
				blk->brk_loc = expr->loc;				
			} else {
				// ignore the return value, don't want ! precedence
				(void)ctype_unify(blk->brk_type, expr->d_break.expr);

				// print_err_with_pos(expr->loc, "type mismatch: expected `%s`, got `%s`", type_dbg_str(blk->brk_type), type_dbg_str(brk_type));
				// print_hint_with_pos(blk->brk_loc, "type `%s` deduced here", type_dbg_str(blk->brk_type));
				// err_unwind();
			}

			expr->type = TYPE_BOTTOM;
			break;
		}
		case EXPR_CONTINUE: {
			cblk_t *blk = &c.blocks[expr->d_continue.blk_id];
			blk->is_rep = true;
			expr->type = TYPE_BOTTOM;
			break;
		}
		// TODO: need for inference as well, still need brk_type
		case EXPR_RETURN: {
			cblk_t *blk = &c.blocks[expr->d_return.blk_id]; // 1:1 correspondence

			type_t brk_type = cexpr(desc, blk->upvalue, expr->d_return.expr, BM_RVALUE);

			// if `blk->upvalue` is set, that is the type of the entire function
			if (blk->upvalue != TYPE_INFER) {
				(void)ctype_unify(blk->upvalue, expr->d_return.expr);
			} else {
				// no ! precedence
				if (blk->brk_type == TYPE_INFER || blk->brk_type == TYPE_BOTTOM) {
					blk->brk_type = brk_type;
					blk->brk_loc = expr->loc;				
				} else {
					// ignore the return value, don't want ! precedence
					(void)ctype_unify(blk->brk_type, expr->d_return.expr);
				}
			}

			expr->type = TYPE_BOTTOM;
			break;
		}
		case EXPR_VOIDING: {
			// TODO: _ = discard
			(void)cexpr(desc, TYPE_UNIT, expr->d_voiding, BM_RVALUE);
			expr->type = TYPE_UNIT;
			break;
		}
		case EXPR_FIELD: {
			type_t type = cexpr(desc, TYPE_INFER, expr->d_field.expr, BM_RVALUE);
			
			// x.field       (field: "field", field_idx: -1)
			// x.0           (field: ISTR_NONE, field_idx: 0)

			if (expr->d_field.field != ISTR_NONE) {
				cnamed_field(desc, type, expr, NULL, cfg);
			} else {
				type_t type_use = type_underlying(type);
				ti_kind kind = type_kind(type_use);

				if (kind != TYPE_TUPLE) {
					err_with_pos(expr->loc, "type mismatch: expected tuple type, got `%s`", type_dbg_str(type));
				}

				type_t *elems = type_get(type)->d_tuple;
				u32 field = expr->d_field.field_idx;

				if (field >= arrlenu(elems)) {
					err_with_pos(expr->loc, "index out of range");
				}
				expr->type = elems[field];
			}
			break;
		}
		case EXPR_STRUCT: {
			type_t underlying = type_underlying(expr->type);
			if (type_kind(underlying) != TYPE_STRUCT) {
				err_with_pos(expr->loc, "not a struct type");
			}

			// assume no duplicate fields, parser has checked that

			tinfo_t *info = type_get(underlying);
			tinfo_sf_t *fields = info->d_struct.fields;
			hir_sf_t *expr_fields = expr->d_struct.fields;

			// O(n^2) but n is small
			for (u32 i = 0, c = arrlenu(expr_fields); i < c; i++) {
				tinfo_sf_t *field = NULL;
				hir_sf_t *expr_field = &expr_fields[i];

				for (u32 j = 0, c = arrlenu(fields); j < c; j++) {
					if (fields[j].field == expr_field->field) {
						field = &fields[j];
						break;
					}
				}

				if (field == NULL) {
					err_with_pos(expr_field->field_loc, "field `%s` not found in struct `%s`", sv_from(expr->d_struct.fields[i].field), type_dbg_str(expr->type));
				}

				(void)cexpr(desc, field->type, expr_field->expr, BM_RVALUE);
				(void)ctype_unify(field->type, expr_field->expr);
			}

			// expr->type is already set in parser, validated in reorder pass
			break;
		}
		case EXPR_STRUCT_POSITIONAL: {
			type_t underlying = type_underlying(expr->type);
			if (type_kind(underlying) != TYPE_STRUCT) {
				err_with_pos(expr->loc, "not a struct type");
			}

			tinfo_t *info = type_get(underlying);
			tinfo_sf_t *fields = info->d_struct.fields;

			u32 c = arrlenu(fields);
			u32 k = arrlenu(expr->d_struct_positional.exprs);

			if (k > c) {
				err_with_pos(expr->loc, "more positional fields than fields in struct");
			}
			if (k < c) {
				err_with_pos(expr->loc, "less positional fields than fields in struct");
			}
			// just right

			for (u32 i = 0; i < c; i++) {
				tinfo_sf_t *field = &fields[i];
				hir_expr_t *expr_field = &expr->d_struct_positional.exprs[i];

				(void)cexpr(desc, field->type, expr_field, BM_RVALUE);
				(void)ctype_unify(field->type, expr_field);
			}

			// TODO: empty structs? assert that
			// TODO: conversion to EXPR_STRUCT ?

			// expr->type is already set in parser, validated in reorder pass
			break;
		}
		case EXPR_LET: {
			// let non_trivial_pattern = expr

			// TODO: let p = !
			//       <- !

			// treat `let v = ...` as `v = ...`

			// duplcation of cpattern for locals, but `ctype_unify()`
			if (expr->d_let.pattern.kind == PATTERN_LOCAL) {
				local_t *local = &desc->locals[expr->d_let.pattern.d_local];

				// forward type
				type_t type = cexpr(desc, local->type, expr->d_let.expr, BM_RVALUE);

				if (local->type == TYPE_INFER) {
					local->type = type;
				} else {
					ctype_unify(local->type, expr->d_let.expr);
				}
			} else {
				type_t type = cexpr(desc, TYPE_INFER, expr->d_let.expr, BM_RVALUE);

				cpattern(desc, &expr->d_let.pattern, type);
			}

			expr->type = TYPE_UNIT;
			break;
		}
		default: {
			assert_not_reached();
		}
	}

	assert(expr->type != TYPE_INFER);
	return expr->type;
}
