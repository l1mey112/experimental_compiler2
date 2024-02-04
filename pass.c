#include "all.h"
#include "pass.h"

void compiler_pass_all(void) {
	rsym_t *po = pass_reorder_sanity();
	pass_check_all(po);
	pass_normalise_all(po);
	pass_debug_cgen(po);
}
