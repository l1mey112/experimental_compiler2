#include "check.h"
#include "all.h"
#include "hir.h"
#include <assert.h>

cctx_t c;

type_t cexpr(ir_desc_t *desc, type_t upvalue, hir_expr_t *expr);

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
	u8 blk_id = c.blocks_len++;
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
		type_t type = cexpr(desc, TYPE_INFER, stmt);
		if (type == TYPE_BOTTOM && i + 1 < c) {
			err_with_pos(expr->d_do_block.exprs[i + 1].loc, "unreachable code");
		}
	}
	c.blocks_len--;

	hir_expr_t *stmt_last = &expr->d_do_block.exprs[stmts_len - 1];

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
		hir_expr_t brk_last = (hir_expr_t){
			.kind = EXPR_BREAK,
			.loc = stmt_last->loc,
			.type = TYPE_BOTTOM,
			.d_break = {
				.blk_id = blk_id,
				.expr = stmt_last,
			},
		};

		if (blk->brk_type != TYPE_INFER) {
			ctype_unify(blk->brk_type, stmt_last);
		} else {
			blk->brk_type = stmt_last->type;
		}
		arrpush(expr->d_do_block.exprs, brk_last);
	}
	//}

	expr->type = blk->brk_type;
}

// sets expr->type
void cloop(ir_desc_t *desc, type_t upvalue, hir_expr_t *expr) {
	cblk_t *blk = &c.blocks[c.blocks_len++];

	*blk = (cblk_t){
		.brk_type = TYPE_INFER,
		.upvalue = upvalue,
	};

	(void)cexpr(desc, TYPE_INFER, expr->d_loop.expr);
	c.blocks_len--;

	// if a loop block has no breaks, it loops forever
	if (blk->brk_type == TYPE_INFER) {
		blk->brk_type = TYPE_BOTTOM;
	}

	expr->type = blk->brk_type;
}

void cinfix(ir_desc_t *desc, type_t upvalue, hir_expr_t *expr) {
	hir_expr_t *lhs = expr->d_infix.lhs;
	hir_expr_t *rhs = expr->d_infix.rhs;
	tok_t kind = expr->d_infix.kind;

	bool is_bool_op = kind == TOK_AND || kind == TOK_OR;
	bool is_cmp_op = kind == TOK_EQ || kind == TOK_NE || kind == TOK_LT || kind == TOK_GT || kind == TOK_LE || kind == TOK_GE;
	bool is_assign_op = kind == TOK_ASSIGN_ADD || kind == TOK_ASSIGN_SUB || kind == TOK_ASSIGN_MUL || kind == TOK_ASSIGN_DIV || kind == TOK_ASSIGN_MOD;
	bool is_ptr_arith = false;

	type_t lhs_t = cexpr(desc, upvalue, lhs);
	type_t rhs_t = cexpr(desc, lhs_t, rhs);

	ti_kind lhs_kind = type_kind(lhs_t);

	// TODO: will need to add in *opaque pointers, no arith on these
	// TODO: introduce proper bounded ptr types for such too
	// TODO: better errors and proper semantics, look at real compilers

	if (lhs_kind == TYPE_PTR && (kind == TOK_ADD || kind == TOK_SUB)) {
		is_ptr_arith = true;
	} else if (lhs_kind == TYPE_PTR) {
		err_with_pos(expr->loc, "invalid operation `%s` on pointer type `%s`", tok_op_str(kind), type_dbg_str(lhs_t));
	}

	// TODO: ! && _ -> !
	// TODO: ! || _ -> !

	if (is_bool_op) {
		(void)ctype_unify(TYPE_BOOL, lhs);
		(void)ctype_unify(TYPE_BOOL, rhs);

		expr->type = TYPE_BOOL;
	} else {
		type_t type = ctype_unify(lhs_t, rhs);

		if (is_cmp_op) {
			expr->type = TYPE_BOOL;
		} else {
			if (!type_is_number(lhs_t)) {
				err_with_pos(expr->loc, "invalid operation `%s` on non numeric type `%s`", tok_op_str(kind), type_dbg_str(lhs_t));
			}
			if (!type_is_number(rhs_t)) {
				err_with_pos(expr->loc, "invalid operation `%s` on non numeric type `%s`", tok_op_str(kind), type_dbg_str(rhs_t));
			}
			expr->type = type;
		}
	}
}

type_t cexpr(ir_desc_t *desc, type_t upvalue, hir_expr_t *expr) {
	switch (expr->kind) {
		case EXPR_INTEGER_LIT: {
			if (upvalue != TYPE_INFER && type_is_integer(upvalue)) {
				expr->type = upvalue;
			} else {
				expr->type = TYPE_I32;
			}
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

			type_t elem_type = cexpr(desc, elem_upvale, &expr->d_array[0]);
			// []elem_type

			u32 len = arrlenu(expr->d_array);

			for (u32 i = 1; i < len; i++) {
				hir_expr_t *elem = &expr->d_array[i];

				type_t type = cexpr(desc, elem_type, elem);
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
				type_t type = cexpr(desc, upvalue, inner);
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

			(void)cexpr(desc, TYPE_BOOL, expr->d_if.cond);
			(void)ctype_unify(TYPE_BOOL, expr->d_if.cond);

			type_t then_type = cexpr(desc, upvalue, expr->d_if.then);
			type_t else_type = cexpr(desc, upvalue, expr->d_if.els);
			expr->type = ctype_unify(then_type, expr->d_if.els);
			break;
		}
		/* case EXPR_ASSIGN: {
			
		} */
		case EXPR_INFIX: {
			cinfix(desc, upvalue, expr);
			break;
		}
		case EXPR_POSTFIX: {

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

		}
		case EXPR_LOCAL: {
			// TODO: nullary `f` doesn't take an () argument
			//       to get the addr of function, you must do `&f`
			
			local_t *local = &desc->locals[expr->d_local];

			if (local->type == TYPE_INFER) {
				err_with_pos(expr->loc, "can't infer type");
			}

			expr->type = local->type;
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
			break;
		}
		case EXPR_CAST: {
			type_t type = cexpr(desc, expr->type, expr->d_cast.expr);

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
			// we don't use a curried form internally to not waste analysis checking
			// for partial application. most function calls are complete calls anyway.
			//
			// `f x g` will compile into a merged call, you'll need to separate them.
			//
			//     f(x: T): T -> ()          (TODO: update fn type syntax, functions can be 0 arity now)
			//
			//     f(x, g) -> f(x)(g)
			//

			// TODO: separate calls

			type_t f_type = cexpr(desc, TYPE_INFER, expr->d_call.f);
			if (type_kind(f_type) != TYPE_FUNCTION) {
				err_with_pos(expr->loc, "type mismatch: expected function type, got `%s`", type_dbg_str(f_type));
			}

			// iterate one by one over the args
			// for every arg, if it exhausts the current arguments of a function type
			// set that return value to the list of argument types, and so on.

			tinfo_t *fn_info = type_get(f_type);
			
			type_t *args = fn_info->d_fn.args;
			u32 cursor = 0;
			for (u32 i = 0, c = arrlenu(expr->d_call.args); i < c; i++, cursor++) {
				if (cursor >= arrlenu(args)) {
					// extract the return value, using the type as a function itself
					assert_not_reached();
				}

				hir_expr_t *arg_expr = &expr->d_call.args[i];
				type_t arg_upvalue = args[cursor];

				(void)cexpr(desc, arg_upvalue, arg_expr);
				(void)ctype_unify(arg_upvalue, arg_expr);
			}

			u32 papp = arrlenu(expr->d_call.args) - cursor;
			assert(papp == 0); // TODO: partial apply

			expr->type = fn_info->d_fn.ret;
			break;
		}
		case EXPR_BREAK: {
			cblk_t *blk = &c.blocks[expr->d_break.blk_id]; // 1:1 correspondence

			type_t brk_type = cexpr(desc, blk->upvalue, expr->d_break.expr);

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
			// no need to pass or inspect anything
			expr->type = TYPE_BOTTOM;
			break;
		}
		// TODO: need for inference as well, still need brk_type
		/* case EXPR_RETURN: {
			cblk_t *blk = &c.blocks[expr->d_return.blk_id]; // 1:1 correspondence
			
			(void)cexpr(desc, blk->upvalue, expr->d_return.expr);
			(void)ctype_unify(blk->upvalue, expr->d_return.expr);

			expr->type = TYPE_BOTTOM;
			break;
		} */
		case EXPR_VOIDING: {
			// TODO: _ = discard
			(void)cexpr(desc, TYPE_UNIT, expr->d_voiding);
			expr->type = TYPE_UNIT;
			break;
		}
		case EXPR_FIELD: {

		}
		case EXPR_LET: {

		}
		default: {
			assert_not_reached();
		}
	}

	assert(expr->type != TYPE_INFER);
	return expr->type;
}

void cproc(proc_t *proc) {
	c = (cctx_t){};

	cblk_t *blk = &c.blocks[c.blocks_len++];

	*blk = (cblk_t){
		.upvalue = proc->ret_type,
		.brk_type = TYPE_INFER,
	};

	type_t ret = cexpr(&proc->desc, proc->ret_type, &proc->desc.hir);

	c.blocks_len--;

	// TODO: unification properly
	if (proc.)

	printf("type %s\n", type_dbg_str(ret));

	assert_not_reached();
}

void cglobal(global_t *global) {
	c = (cctx_t){};

	type_t ret = cexpr(&global->desc, global->type, &global->desc.hir);

	printf("type %s\n", type_dbg_str(ret));
	assert_not_reached();
	// cdesc(&proc->desc);
	// assert_not_reached();	
}

void compiler_check(void) {
	rsym_t *po = creorder_po_and_sanity();

	for (u32 i = 0, c = arrlenu(po); i < c; i++) {
		rsym_t rsym = po[i];
		sym_t *sym = &symbols[rsym];
		switch (sym->kind) {
			case SYMBOL_PROC: {
				cproc(&sym->d_proc);
				break;
			}
			case SYMBOL_GLOBAL: {
				cglobal(&sym->d_global);
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