#include "check.h"
#include "all.h"

// typecheck a list of topologically ordered symbols

void cproc(lir_rsym_t rsym) {
	lir_sym_t *sym = &symbols[rsym];
	assert(sym->kind == SYMBOL_PROC);

}

void lir_check(void) {
    creorder();

	for (lir_rsym_t i = 0; i < arrlenu(creorder_sorted); i++) {
		lir_sym_t *sym = &symbols[creorder_sorted[i]];

		switch (sym->kind) {
			case SYMBOL_PROC: {
				cproc(i);
				break;
			}
			case SYMBOL_GLOBAL:
			case SYMBOL_TYPE: {
				assert_not_reached();
			}
		}
	}
}