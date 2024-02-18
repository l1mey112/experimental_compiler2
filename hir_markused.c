#include "all.h"

// run over the HIR and mark symbols if used by a parent symbol
// all "visible" symbols are ones that are externally visible
// will also order symbols in postorder, like hir_reorder.
//
// this is also a sort of correctness pass, further passes like cgen
// won't be able to generate certain symbols that go unused
//
// as much as i would like to take the traversal code from hir_reorder
// it's too specialised for this case

typedef struct mu_entry_t mu_entry_t;

struct mu_entry_t {
	rsym_t key;
};

static mu_entry_t *mu_map = NULL;
static rsym_t *symbols_marked = NULL;

void msymbol_dfs(rsym_t rsym);

void mhir_expr(hir_expr_t *expr) {
	switch (expr->kind) {
		case EXPR_SYM: {
			msymbol_dfs(expr->d_sym);
			break;
		}
		//
		case EXPR_ARRAY: {
			for (u32 i = 0, c = arrlenu(expr->d_array); i < c; i++) {
				mhir_expr(&expr->d_array[i]);
			}
			break;
		}
		case EXPR_TUPLE: {
			for (u32 i = 0, c = arrlenu(expr->d_tuple); i < c; i++) {
				mhir_expr(&expr->d_tuple[i]);
			}
			break;
		}
		case EXPR_DO_BLOCK: {
			for (u32 i = 0, c = arrlenu(expr->d_do_block.exprs); i < c; i++) {
				mhir_expr(&expr->d_do_block.exprs[i]);
			}
			break;
		}
		case EXPR_LOOP: {
			mhir_expr(expr->d_loop.expr);
			break;
		}
		case EXPR_IF: {
			mhir_expr(expr->d_if.cond);
			mhir_expr(expr->d_if.then);
			if (expr->d_if.els) {
				mhir_expr(expr->d_if.els);
			}
			break;
		}
		case EXPR_ASSIGN: {
			mhir_expr(expr->d_assign.lhs);
			mhir_expr(expr->d_assign.rhs);
			break;
		}
		case EXPR_INFIX: {
			mhir_expr(expr->d_infix.lhs);
			mhir_expr(expr->d_infix.rhs);
			break;
		}
		case EXPR_POSTFIX: {
			mhir_expr(expr->d_postfix.expr);
			break;
		}
		case EXPR_PREFIX: {
			mhir_expr(expr->d_prefix.expr);
			break;
		}
		case EXPR_DEREF: {
			mhir_expr(expr->d_deref);
			break;
		}
		//
		case EXPR_CAST: {
			mhir_expr(expr->d_cast.expr);
			break;
		}
		case EXPR_STRUCT: {
			for (u32 i = 0, c = arrlenu(expr->d_struct.fields); i < c; i++) {
				mhir_expr(expr->d_struct.fields[i].expr);
			}
			break;
		}
		case EXPR_STRUCT_POSITIONAL: {
			for (u32 i = 0, c = arrlenu(expr->d_struct_positional.exprs); i < c; i++) {
				mhir_expr(&expr->d_struct_positional.exprs[i]);
			}
			break;
		}
		//
		case EXPR_CALL: {
			mhir_expr(expr->d_call.f);
			for (u32 i = 0, c = arrlenu(expr->d_call.args); i < c; i++) {
				mhir_expr(&expr->d_call.args[i]);
			}
			break;
		}
		case EXPR_BREAK: {
			mhir_expr(expr->d_break.expr);
			break;
		}
		case EXPR_RETURN: {
			mhir_expr(expr->d_return.expr);
			break;
		}
		case EXPR_VOIDING: {
			mhir_expr(expr->d_voiding);
			break;
		}
		case EXPR_LET: {
			mhir_expr(expr->d_let.expr);
			break;
		}
		//
		case EXPR_INDEX: {
			// &v[0]
			mhir_expr(expr->d_index.expr);
			mhir_expr(expr->d_index.index);
			break;
		}
		case EXPR_SLICE: {
			// &v[0..1]
			mhir_expr(expr->d_slice.expr);
			if (expr->d_slice.lo) {
				mhir_expr(expr->d_slice.lo);
			}
			if (expr->d_slice.hi) {
				mhir_expr(expr->d_slice.hi);
			}
			break;
		}
		case EXPR_FIELD: {
			mhir_expr(expr->d_field.expr);
			break;
		}
		case EXPR_ADDR_OF: {
			mhir_expr(expr->d_addr_of.ref);
			break;
		}
		//
		case EXPR_LOCAL:
		case EXPR_CONTINUE:
		case EXPR_INTEGER_LIT:
		case EXPR_BOOL_LIT:
		case EXPR_TUPLE_UNIT: {
			break; // empty
		}
		default: {
			assert_not_reached();
		}
	}
}

void msymbol_dfs(rsym_t rsym) {
	// have i visited this symbol before?
	if (hmgeti(mu_map, rsym) != -1) {
		return;
	}

	// mark visited
	hmputs(mu_map, (mu_entry_t) { .key = rsym });

	sym_t *sym = &symbols[rsym];

	switch (sym->kind) {
		case SYMBOL_PROC: {
			if (sym->d_proc.desc.hir) {
				mhir_expr(sym->d_proc.desc.hir);
			}
			break;
		}
		case SYMBOL_GLOBAL: {
			if (sym->d_global.desc.hir) {
				mhir_expr(sym->d_global.desc.hir);
			}
			break;
		}
		default: {
			assert_not_reached();
		}
	}

	// add to postorder
	arrpush(symbols_marked, rsym);
}

void hir_markused(void) {
	// iterate over all symbols, externally visible are roots
	for (rsym_t rsym = 0; rsym < hmlenu(symbols); rsym++) {
		sym_t *sym = &symbols[rsym];

		if (sym->is_extern) {
			msymbol_dfs(rsym);
		}
	}

	symbols_po = symbols_marked;
}
