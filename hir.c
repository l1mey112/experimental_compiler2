#include "all.h"

hir_expr_t *hir_dup(hir_expr_t expr) {
	hir_expr_t *dup = malloc(sizeof(hir_expr_t));
	*dup = expr;
	return dup;
}
