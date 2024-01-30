#include "all.h"
#include <assert.h>
//
// global symbol (hash) table
//
// the parser will insert eagerly into a global symbol table, returning a reference
// to a symbol. a symbol's "fully qualified name" is completely unique.
//
// no symbol (function, type, global, etc) can have the same fully qualified name.
// sometimes there may be conflicting modules with different filesystem paths but
// whilst sharing conflicting module paths, we will raise an error in this case.
//
// the symbol table works with a sort of "eventual consistency" where on references
// to a symbol we just insert a placeholder and then later on we fill in the details.
// after the parsing stage we go one by one and ensure each symbol isn't a placeholder
// and if it is, we raise an error
//

sym_t *symbols;

rsym_t table_resolve(istr_t qualified_name) {
	ptrdiff_t sym = hmgeti(symbols, qualified_name);
	
	if (sym != -1) {
		return sym;
	}

	sym_t desc = {
		.key = qualified_name,
		.is_placeholder = true,
	};

	hmputs(symbols, desc);

	return (rsym_t)hmlenu(symbols) - 1;
}

rsym_t table_register(sym_t desc) {
	ptrdiff_t sym = hmgeti(symbols, desc.key);

	if (sym != -1 && !symbols[sym].is_placeholder) {
		// TODO: better error message, probably module local/context
		//       no need to print the whole qualified name
		err_with_pos(desc.loc, "symbol `%s` already defined", sv_from(desc.key));
	}

	hmputs(symbols, desc);

	// would have been updated
	if (sym != -1) {
		return sym;
	}

	return (rsym_t)hmlenu(symbols) - 1;
}

rlocal_t proc_local_new(proc_t *proc, local_t local) {
	u32 idx = arrlenu(proc->locals);
	arrpush(proc->locals, local);
	return idx;
}

// impl module dump and so on

// TODO: need to unify them together
//       1. they'll print the same function
//       2. the same locals
//       3. but instead subst for `lir` and `hir`
//
// TODO: print hir expr
// TOOD: normal print pattern though, shared

extern void hir_dump_function(sym_t *sym);

void table_dump(sym_t *sym) {
	assert(!sym->is_placeholder);

	switch (sym->kind) {
		case SYMBOL_PROC: {
			hir_dump_function(sym);
			break;
		}
		default: {
			assert_not_reached();
		}
	}
}

void table_dump_all(void) {
	for (u32 i = 0, c = hmlenu(symbols); i < c; i++) {
		sym_t *sym = &symbols[i];

		if (sym->is_placeholder) {
			continue;
		}
		
		table_dump(sym);
	}
}
