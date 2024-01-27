#include "all.h"

// shared checker defs

void creorder_and_type(void);
void ctype_proc(lir_sym_t *sym, lir_proc_t *proc);

type_t ctype_unify(type_t lhs_t, type_t rhs_t, loc_t onerror);