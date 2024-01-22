#include "check.h"
#include "all.h"

// typecheck a list of topologically ordered symbols

void lir_check(void) {
    creorder();

	for (u32 i = 0; i < arrlenu(creorder_sorted); i++) {
		lir_sym_t *sym = &symbols[creorder_sorted[i]];

		switch (sym->kind) {
			case SYMBOL_PROC: {
				break;
			}
			case SYMBOL_GLOBAL:
			case SYMBOL_TYPE: {
				assert_not_reached();
			}
		}
	}
}