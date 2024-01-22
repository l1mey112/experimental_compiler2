#include "all.h"
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

lir_sym_t *symbols;

lir_rsym_t table_resolve(istr_t qualified_name) {
	ptrdiff_t sym = hmgeti(symbols, qualified_name);
	
	if (sym != -1) {
		return sym;
	}

	lir_sym_t desc = {
		.key = qualified_name,
		.is_placeholder = true,
	};

	hmputs(symbols, desc);

	return (lir_rsym_t)hmlenu(symbols) - 1;
}

lir_rsym_t table_register(lir_sym_t desc) {
	ptrdiff_t sym = hmgeti(symbols, desc.key);

	if (sym != -1 && !symbols[sym].is_placeholder) {
		// TODO: better error message, probably module local/context
		//       no need to print the whole qualified name
		err_with_pos(desc.loc, "symbol `%s` already defined", desc.key);
	}

	hmputs(symbols, desc);

	// would have been updated
	if (sym != -1) {
		return sym;
	}

	return (lir_rsym_t)hmlenu(symbols) - 1;
}