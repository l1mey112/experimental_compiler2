#include "check.h"
#include "all.h"

// run passes on a list of topologically ordered symbols

void cproc(rsym_t rsym) {
	

}

void compiler_check(void) {
	rsym_t *po = creorder_po_and_sanity();

	// RPO
	for (u32 i = arrlenu(po); i-- > 0;) {
		rsym_t rsym = po[i];
		sym_t *sym = &symbols[rsym];
		printf("%u: %s\n", i, sv_from(sym->key));
	}
}