#include "all.h"
#include "hir.h"

void gchir_mutate_tree(hir_expr_t *expr, u32 gln) {
	switch (expr->kind) {
		case EXPR_INTEGER_LIT:
		case EXPR_BOOL_LIT:
		case EXPR_ARRAY:
		case EXPR_TUPLE_UNIT:
		case EXPR_TUPLE:
		case EXPR_SYM:
		case EXPR_CONTINUE:
		case EXPR_UNREACHABLE: {
			break;
		}
		//
		case EXPR_DO_BLOCK: {
			for (u32 i = 0, c = arrlenu(expr->d_do_block.exprs); i < c; i++) {
				gchir_mutate_tree(&expr->d_do_block.exprs[i], gln);
			}
			break;
		}
		case EXPR_LOOP: {
			gchir_mutate_tree(expr->d_loop.expr, gln);
			break;
		}
		case EXPR_IF: {
			gchir_mutate_tree(expr->d_if.cond, gln);
			gchir_mutate_tree(expr->d_if.then, gln);
			if (expr->d_if.els) {
				gchir_mutate_tree(expr->d_if.els, gln);
			}
			break;
		}
		case EXPR_ASSIGN: {
			gchir_mutate_tree(expr->d_assign.lhs, gln);
			gchir_mutate_tree(expr->d_assign.rhs, gln);
			break;
		}
		case EXPR_INFIX: {
			gchir_mutate_tree(expr->d_infix.lhs, gln);
			gchir_mutate_tree(expr->d_infix.rhs, gln);
			break;
		}
		case EXPR_PREFIX: {
			gchir_mutate_tree(expr->d_prefix.expr, gln);
			break;
		}
		case EXPR_DEREF: {
			gchir_mutate_tree(expr->d_deref, gln);
			break;
		}
		case EXPR_ADDR_OF: {
			gchir_mutate_tree(expr->d_addr_of.ref, gln);
			break;
		}
		case EXPR_INDEX: {
			gchir_mutate_tree(expr->d_index.expr, gln);
			gchir_mutate_tree(expr->d_index.index, gln);
			break;
		}
		case EXPR_CAST: {
			gchir_mutate_tree(expr->d_cast.expr, gln);
			break;
		}
		case EXPR_CALL: {
			gchir_mutate_tree(expr->d_call.f, gln);
			for (u32 i = 0, c = arrlenu(expr->d_call.args); i < c; i++) {
				gchir_mutate_tree(&expr->d_call.args[i], gln);
			}
			break;
		}
		case EXPR_BREAK: {
			gchir_mutate_tree(expr->d_break.expr, gln);
			break;
		}
		case EXPR_RETURN: {
			gchir_mutate_tree(expr->d_return.expr, gln);
			break;
		}
		case EXPR_FIELD: {
			gchir_mutate_tree(expr->d_field.expr, gln);
			break;
		}
		// case EXPR_STRUCT_POSITIONAL:
		// case EXPR_STRUCT:
		case EXPR_LOCAL: {
			expr->d_local += gln;
			break;
		}
		default: {
			assert_not_reached();
		}
	}
}

static void gclower(rsym_t rsym_global, global_t *global) {
	sym_t *sym_global = &symbols[rsym_global];
	sym_t *sym_init = &symbols[__main_init];
	assert(sym_init->kind == SYMBOL_PROC);
	assert(sym_init->d_proc.desc.hir->kind == EXPR_DO_BLOCK);
	proc_t *init = &sym_init->d_proc;

	// main.test = do <n>
	//     ret 1

	// normalised globals only have a single `ret`
	// in the last position

	// main.init() = do <n>
	//     main.test = 1

	// 1. convert the `ret` to assignment of global
	// 2. append locals to `main.init` and shift
	//    indices of the locals in the global

	// append new globals
	u32 oln = arrlenu(init->desc.locals);
	u32 gln = arrlenu(global->desc.locals);
	local_t *lp = arraddnptr(init->desc.locals, gln);
	memcpy(lp, global->desc.locals, gln * sizeof(local_t));
	
	hir_expr_t *hir = global->desc.hir;

	// inc indices
	gchir_mutate_tree(hir, oln);

	assert(hir->kind == EXPR_DO_BLOCK);
	u32 gstmtn = arrlenu(hir->d_do_block.exprs);
	hir_expr_t *last = &hir->d_do_block.exprs[gstmtn - 1];
	assert(last->kind == EXPR_RETURN);

	// convert `ret` to assignment
	*last = (hir_expr_t) {
		.kind = EXPR_ASSIGN,
		.d_assign = {
			.lhs = hir_dup((hir_expr_t){
				.kind = EXPR_SYM,
				.type = global->type,
				.loc = sym_global->loc,
				.d_sym = rsym_global,
			}),
			.rhs = last->d_return.expr,
			.kind = TOK_ASSIGN,
		}
	};

	// append all to `main.init`
	hir_expr_t *ep = arraddnptr(init->desc.hir->d_do_block.exprs, gstmtn);
	memcpy(ep, hir->d_do_block.exprs, gstmtn * sizeof(hir_expr_t));

	// gone
	global->desc.locals = NULL;
	global->desc.hir = NULL;
}

void hir_global_lower(void) {
	for (u32 i = 0, c = arrlenu(symbols_po); i < c; i++) {
		rsym_t rsym = symbols_po[i];
		sym_t *sym = &symbols[rsym];

		// a used global and not constant known
		if (sym->kind == SYMBOL_GLOBAL && !sym->d_global.constant) {
			gclower(rsym, &sym->d_global);
		}
	}
}
