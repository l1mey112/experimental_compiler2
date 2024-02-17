#include "all.h"

void hir_reorder(void);
void hir_type_proc(sym_t *sym, proc_t *proc);
void hir_type_global(sym_t *sym, global_t *global);
void hir_normalise_global(global_t *global);
void hir_normalise_proc(proc_t *proc);
    
// reorder
//
// symbol -> hir_typing -> hir_normalisation ------------------>
//                                          ?global -> hir_eval
//
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
