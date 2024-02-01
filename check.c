#include "check.h"
#include "all.h"
#include "hir.h"

cctx_t c;

type_t cexpr(ir_desc_t *desc, type_t upvalue, hir_expr_t *expr);

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
		/* case EXPR_LOOP: {
			cloop(desc, expr, upvalue);
			break;
		} */
		case EXPR_IF: {

		}
		case EXPR_ASSIGN: {

		}
		case EXPR_INFIX: {

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

		}
		case EXPR_SYM: {

		}
		case EXPR_CAST: {

		}
		case EXPR_CALL: {
			assert_not_reached();
		}
		case EXPR_BREAK: {
			cblk_t *blk = &c.blocks[expr->d_break.blk_id]; // 1:1 correspondence

			type_t brk_type = cexpr(desc, blk->upvalue, expr->d_break.expr);

			// ! encompasses all types

			if (blk->brk_type )
			
			/* if (blk->brk_type == TYPE_INFER || blk->brk_type == TYPE_BOTTOM) {
				blk->brk_type = brk_type;
				blk->brk_loc = expr->loc;
			} else if (brk_type != TYPE_BOTTOM && blk->brk_type != brk_type) {
				print_err_with_pos(expr->loc, "type mismatch: expected `%s`, got `%s`", type_dbg_str(blk->brk_type), type_dbg_str(brk_type));
				print_hint_with_pos(blk->brk_loc, "type `%s` deduced here", type_dbg_str(blk->brk_type));
				err_unwind();
			} */

			break;
		}
		case EXPR_CONTINUE: {

		}
		case EXPR_RETURN: {

		}
		case EXPR_VOIDING: {

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
	/* c = (cctx_t){};

	tinfo_t *typeinfo = type_get(proc->type);
	assert(typeinfo->kind == TYPE_FUNCTION);

	type_t ret = cexpr(&proc->desc, typeinfo->d_fn.ret, &proc->desc.hir);

	printf("type %s\n", type_dbg_str(ret)); */

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