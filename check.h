#include "all.h"

// shared checker defs

// return a postorder sorted list of symbols, reverse for proper ordering
rsym_t *creorder_po_and_sanity(void);

void ctype_proc(sym_t *sym, lir_proc_t *proc);
type_t ctype_unify(type_t lhs_t, type_t rhs_t, loc_t onerror);