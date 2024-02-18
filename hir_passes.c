#include "all.h"

void hir_reorder(void);
void hir_type_proc(sym_t *sym, proc_t *proc);
void hir_type_global(sym_t *sym, global_t *global);
void hir_type_import(sym_t *sym, imas_t *imas);
void hir_normalise_global(global_t *global);
void hir_normalise_proc(proc_t *proc);
void hir_eval_global(sym_t *sym, global_t *global);
void hir_markused(void);

// TODO: this is damn hacky, but simple. "hacky" is in the eye of the beholder anyway
//       though it seems it would be better handled in the checker looking at places
//       where type annotations could exist, which are casts and let bindings
void hir_global_constexpr(rsym_t rsym, global_t *global) {
	// search entire type table for references to this global
	// assumed that only one unique global exists

	sym_t *sym = &symbols[rsym];

	tinfo_t *info = NULL;
	for (u32 i = 0; i < type_len; i++) {
		tinfo_t *i_info = &types[i];

		if (i_info->kind == TYPE_ARRAY && i_info->d_array.is_symbol) {
			if (i_info->d_array.d_symbol == rsym) {
				info = i_info;
				break;
			}
		}
	}

	if (info == NULL) {
		return;
	}

	// 1. is this global a constant
	// 2. is this global a positive integer
	// 3. is this global a non-zero integer

	if (!global->constant) {
		err_with_pos(sym->loc, "not a constant expression");
	}

	// there are no negative integers in the HIR,
	// only positive ones prepended with unary minus

	hir_expr_t *expr = global->constant;
	if (!type_is_integer(expr->type)) {
		err_with_pos(sym->loc, "not an integer");
	}

	// no negatives allowed
	if (expr->kind == EXPR_PREFIX && expr->d_prefix.op == EXPR_K_SUB) {
		err_with_pos(sym->loc, "can't have negative array size");
	}

	assert(expr->kind == EXPR_INTEGER_LIT);

	// no zeros allowed
	u64 value = expr->d_integer;

	if (value == 0) {
		err_with_pos(sym->loc, "can't have zero array size");
	}

	info->d_array.is_symbol = false;
	info->d_array.d_length = value;
}

void hir_passes(void) {
    hir_reorder();

    for (u32 i = 0, c = arrlenu(symbols_po); i < c; i++) {
		rsym_t rsym = symbols_po[i];
		sym_t *sym = &symbols[rsym];

        // after checking, it's assumed that only extern symbols can have no body

		switch (sym->kind) {
			case SYMBOL_PROC: {
                hir_type_proc(sym, &sym->d_proc);
                if (sym->d_proc.desc.hir == NULL) {
					continue;
				}
                hir_normalise_proc(&sym->d_proc);
				break;
			}
			case SYMBOL_GLOBAL: {
                hir_type_global(sym, &sym->d_global);
                if (sym->d_global.desc.hir == NULL) {
                    continue;
                }
                hir_normalise_global(&sym->d_global);
				hir_eval_global(sym, &sym->d_global);
				hir_global_constexpr(rsym, &sym->d_global);
				break;
			} 
			case SYMBOL_TYPE: {
				break;
			}
			case SYMBOL_IMPORT_ASSERTION: {
				hir_type_import(sym, &sym->d_imas);
				break;
			}
			default: {
				assert_not_reached();
			}
		}
	}

	hir_markused();

	// remove aggregate fields etc of ()
	// it's safe to do this after normalisation
	/* for (u32 i = 0; i < type_len; i++) {
		tinfo_t *i_info = &types[i];

		if (i_info->kind == TYPE_ARRAY && i_info->d_array.is_symbol) {
			if (i_info->d_array.d_symbol == rsym) {
				info = i_info;
				break;
			}
		}
	} */
}
