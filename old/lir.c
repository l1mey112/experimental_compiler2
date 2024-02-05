#include "all.h"
#include "lir.h"

lir_value_t *lir_dup(lir_value_t value) {
    lir_value_t *dup = malloc(sizeof(lir_value_t));
    *dup = value;
    return dup;
}

void lir_stmt(lir_stmt_t **stmts, lir_stmt_t stmt) {
    arrpush(*stmts, stmt);
}
