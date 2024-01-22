#include "check.h"

// typecheck symbols

void lir_check(void) {
    creorder();

	for (u32 i = 0; i < arrlenu(creorder_sorted); i++) {
		lir_sym_t *sym = &symbols[creorder_sorted[i]];
		printf("%s\n", sv_from(sym->key));
	}
}