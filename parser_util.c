#include "all.h"
#include "hir.h"
#include "parser.h"
#include <errno.h>

bool pprev_next_to(void) {
	return p.line_prev.pos + p.line_prev.len == p.line_token.pos;
}

bool ppeek_next_to(void) {
	return p.line_token.pos + p.line_token.len == p.line_peek.pos;
}

#define DEFAULT_DBG_TOK(expected) (token_t){.kind = expected, .lit = ISTR_NONE}

void pcheck(tok_t expected) {
	if (p.token.kind == TOK_EOF) {
		err_with_pos(p.token.loc, "unexpected EOF, expected %s", tok_dbg_str(DEFAULT_DBG_TOK(expected)));
	} else if (p.token.kind != expected) {
		err_with_pos(p.token.loc, "unexpected %s, expected %s", tok_dbg_str(p.token), tok_dbg_str(DEFAULT_DBG_TOK(expected)));
	}
}

void pexpect(tok_t expected) {
	if (p.token.kind == expected) {
		pnext();
	} else if (p.token.kind == TOK_EOF) {
		err_with_pos(p.token.loc, "unexpected EOF, expected %s", tok_dbg_str(DEFAULT_DBG_TOK(expected)));
	} else {
		err_with_pos(p.token.loc, "unexpected %s, expected %s", tok_dbg_str(p.token), tok_dbg_str(DEFAULT_DBG_TOK(expected)));
	}
}

void NORETURN punexpected(const char *err) {
	err_with_pos(p.token.loc, "unexpected %s, %s", tok_dbg_str(p.token), err);
}

// TYPE_INFER on error
type_t ptok_to_type(tok_t kind) {
	switch (kind) {
		case TOK_I8:    return TYPE_I8;
		case TOK_I16:   return TYPE_I16;
		case TOK_I32:   return TYPE_I32;
		case TOK_I64:   return TYPE_I64;
		case TOK_ISIZE: return TYPE_ISIZE;
		case TOK_U8:    return TYPE_U8;
		case TOK_U16:   return TYPE_U16;
		case TOK_U32:   return TYPE_U32;
		case TOK_U64:   return TYPE_U64;
		case TOK_USIZE: return TYPE_USIZE;
		case TOK_F32:   return TYPE_F32;
		case TOK_F64:   return TYPE_F64;
		case TOK_BOOL:  return TYPE_BOOL;
		default:
			return TYPE_INFER;
	}
}

u64 pparse_int(token_t token) {
	u64 lit = strtoull(sv_from(token.lit), NULL, 10);

	// we won't get any other error
	if (errno == ERANGE) {
		err_with_pos(token.loc, "integer literal too large to fit in 64 bits");
	}

	return lit;
}

enum : u8 {
	PREC_UNKNOWN,
	PREC_CALL,
	PREC_UNARY,
};

u8 ptype_prec(void) {
	switch (p.token.kind) {
		case TOK_COLON: return PREC_CALL;
		case TOK_PIPE: return PREC_CALL; // TODO: same prec?
		default: return PREC_UNKNOWN;
	}
}

// parse const expression and create anonymous global
rsym_t pconst_expression(void) {
	loc_t oloc = p.token.loc;

	ir_desc_t desc = {};

	// TODO: mask scopes
	hir_expr_t expr = pexpr(&desc, 0);

	desc.hir = hir_dup(expr);

	sym_t sym = {
		.key = table_anon_symbol(),
		.mod = p.mod,
		.short_name = ISTR_NONE,
		.loc = oloc,
		.kind = SYMBOL_GLOBAL,
		.d_global = {
			.desc = desc,
			.type = TYPE_INFER,
			.type_loc = (loc_t){0},
			.is_mut = false,
		},
		.is_pub = false,
		.is_extern = false,
		.extern_symbol = ISTR_NONE,
	};

	return table_register(sym);
}

type_t ptype_expr(u8 prec) {
	type_t type;
	switch (p.token.kind) {
		case TOK_MUL: {
			// *T
			pnext();

			bool is_mut = false;
			// *'T
			if (p.token.kind == TOK_TACK) {
				pnext();
				is_mut = true;
			}
			type = ptype_expr(PREC_UNARY);
			type = type_new_inc_mul(type, is_mut);
			break;
		}
		case TOK_OSQ: {
			pnext();
			// [5]i32
			// []i32

			bool is_array = false;
			bool is_array_symbol = false;
			u64 int_size_val;
			rsym_t const_sym;

			if (p.token.kind != TOK_CSQ) {
				// one of two
				// 1. integer literal
				// 2. constant initialiser

				is_array = true;

				// TODO: symbol forwarding

				if (p.token.kind == TOK_INTEGER && p.peek.kind == TOK_CSQ) {
					int_size_val = pparse_int(p.token);
					pnext();
				} else {
					is_array_symbol = true;
					const_sym = pconst_expression();
				}
			}
			pexpect(TOK_CSQ);
			
			// []i32
			//   ^^^

			type_t elem = ptype_expr(PREC_UNARY);

			if (is_array) {
				tinfo_t info = {
					.kind = TYPE_ARRAY,
					.d_array.elem = elem,
				};

				if (is_array_symbol) {
					info.d_array.is_symbol = true;
					info.d_array.d_symbol = const_sym;
				} else {
					info.d_array.d_length = int_size_val;
				}

				type = type_new(info);
			} else {
				type = type_new((tinfo_t){
					.kind = TYPE_SLICE,
					.d_slice.elem = elem,
				});
			}
			break;
		}
		case TOK_IDENT: {
			istr_t lit = p.token.lit;
			loc_t loc = p.token.loc;

			// Type
			// module.Type

			rsym_t typesym;

			int id;
			if ((id = pimport_ident(lit)) != -1) {
				pnext();
				// TODO: duplicated node inside pident_wstruc()
				if (p.token.kind != TOK_DOT) {
					print_err_with_pos(p.token.loc, "expected `.` after import name `%s`", sv_from(p.is[id].name));
					print_hint_with_pos(loc, "import name `%s` used here", sv_from(p.is[id].name));
					err_unwind();
				}
				pnext();
				pcheck(TOK_IDENT);
				istr_t lit = p.token.lit;
				pnext();

				typesym = table_resolve(p.is[id].mod, lit);
			} else {
				// TODO: perform search of local scope, incase of scoped defs

				// main.Foo
				typesym = table_resolve(p.mod, lit);
				pnext();
			}
			
			type = type_new((tinfo_t){
				.kind = TYPE_SYMBOL,
				.d_symbol = typesym,
			});
			break;
		}
		case TOK_NOT: {
			pnext();
			type = TYPE_BOTTOM;
			break;
		}
		case TOK_OPAR: {
			// tuples and functions
			//
			// TODO: #() -> i32 is a function pointer, not closure
			//bool is_hash = false; // is fn ptr
			
			type_t *elems = NULL;
			type_t single = (type_t)-1;
			bool first = true;
			pnext();
			// (...)
			//  ^^^
			while (p.token.kind != TOK_CPAR) {
				type_t type = ptype();
				// (i32, ...)
				//     ^
				
				if (first) {
					single = type;
				} else {
					if (elems == NULL) {
						arrpush(elems, single);
						single = (type_t)-1;
					}
					arrpush(elems, type);					
				}

				if (p.token.kind == TOK_COMMA) {
					pnext();
				} else if (p.token.kind != TOK_CPAR) {
					punexpected("expected `,` or `)`");
				}

				first = false;
			}
			pnext();

			if (single != (type_t)-1) {
				type = single;
				break;
			} else if (elems == NULL) {
				type = TYPE_UNIT;
				break;
			}

			type = type_new((tinfo_t){
				.kind = TYPE_TUPLE,
				.d_tuple = elems,
			});
			break;
		}
		default: {
			if ((type = ptok_to_type(p.token.kind)) != TYPE_INFER) {
				pnext();
				break;
			}
			punexpected("expected type");
		}
	}

	while (prec < ptype_prec()) {
		switch (p.token.kind) {
			case TOK_COLON: {
				pnext();
				type_t ret = ptype_expr(PREC_CALL);

				// args: ret
				// (i32, i32): i32

				// TODO: syntax ambiguity
				//
				// (i32, i32): (i32)                 (two arguments)
				// ((i32, i32)): (i32)               (one argument)
				// ((((i32, i32)))): (i32)           (one argument)

				type_t *args;

				tinfo_t *info;
				if (TI_GUARD(type, TYPE_TUPLE, info)) {
					args = info->d_tuple;
				} else if (type == TYPE_UNIT) {
					args = NULL;
				} else {
					args = arr(type_t, type);
				}

				type = type_new((tinfo_t){
					.kind = TYPE_FUNCTION,
					.d_fn.args = args,
					.d_fn.ret = ret,
				});
				continue;
			}
			case TOK_PIPE: {
				// case TOK_ARROW: {
				//     no merging as that would break type equivalence/immutability
				//
				//     TODO: we alloc + copy every single damn time, this is very bad,
				//           however, i don't care at the moment.
				//           in the future, extract this code out so that it doesn't
				//           waste any memory to constant duplications. O(n^2) roughly
				//
				//           on second thought, this is really really weird code
				//           refactor ASAP, but leave for later honestly
				//
				//           will get even weirder with regions assigned to each ptr arg
				//
				//     i32 -> i32           (parse into single fn call)
				//     i32 -> i32 -> i32    (merge into single fn call)
				// }
				//
				// TODO: same todo as above

				pnext();
				type_t *elems = NULL;
				type_t elem = ptype_expr(PREC_CALL);
				
				tinfo_t *info;
				if (TI_GUARD(type, TYPE_SUM, info)) {
					u32 len = arrlenu(info->d_sum.elems);
					type_t *nptr = arraddnptr(elems, len);
					memcpy(nptr, info->d_sum.elems, len * sizeof(type_t));
					elems = nptr;
				} else {
					arrpush(elems, type);
				}

				arrpush(elems, elem);

				type = type_new((tinfo_t){
					.kind = TYPE_SUM,
					.d_sum.elems = elems,
				});
				continue;
			}
			default: {}
		}
		break;
	}

	return type;
}

// TODO: *'i32 change to *i32' instead ??
type_t ptype(void) {
	return ptype_expr(0);
}

void pimport_insert_validate(loc_t oloc, fs_rmod_t mod, istr_t module_ident) {
	for (u32 i = 0; i < p.is_len; i++) {
		pimport_t *is = &p.is[i];
		if (is->mod == mod) {
			err_with_pos(oloc, "import `%s` already imported", sv_from(fs_module_symbol(is->mod, module_ident)));
		} else if (is->name == module_ident) {
			err_with_pos(oloc, "import name `%s` already used", sv_from(module_ident));
		}
	}

	p.is[p.is_len++] = (pimport_t){
		.mod = mod,
		.name = module_ident,
		.loc = oloc,
	};
}

void pimport(void) {
	if (p.has_done_imports) {
		punexpected("import declaration must come before code");
	}
	
	loc_t oloc = p.peek.loc;
	istr_t fields[64]; // no one is gonna do this... right?
	u32 fields_len = 0;

	// import a.b.c
	do {
		pnext();
		istr_t field = p.token.lit;
		pexpect(TOK_IDENT);
		fields[fields_len++] = field;
	} while(p.token.kind == TOK_DOT);

	// TODO: import k as v

	// TODO: also would be a nice error message if it could
	//       tell you "up to" where it matched the module path

	fs_rmod_t mod = fs_register_import(p.mod, fields, fields_len, oloc);
	istr_t module_ident = fields[fields_len - 1];

	pimport_insert_validate(oloc, mod, module_ident);
}

// import_main -> symbol

void pimport_main(void) {
	if (p.has_done_imports) {
		punexpected("import declaration must come before code");
	}

	// import_main {}

	istr_t main_ident = sv_move("main");

	loc_t oloc = p.token.loc;
	if (fs_mod_arena[p.mod].kind == MOD_MAIN) {
		err_with_pos(oloc, "main module cannot import itself");
	}
	pimport_insert_validate(oloc, main_module, main_ident);
	pnext();
	//
	// import_main {
	//             ^
	//     main: (): ()
	// }
	pexpect(TOK_OCBR);

	imas_entry_t *entries = NULL;

	while (p.token.kind != TOK_CCBR) {
		pcheck(TOK_IDENT);
		istr_t ident = p.token.lit;
		loc_t loc = p.token.loc;
		pnext();
		pexpect(TOK_COLON);
		loc_t type_loc = p.token.loc;
		type_t type = ptype();

		imas_entry_t entry = {
			.symbol = table_resolve(main_module, ident),
			.symbol_loc = loc,
			.type = type,
			.type_loc = type_loc,
		};

		arrpush(entries, entry);
	}
	pnext();

	sym_t sym = {
		.key = table_anon_symbol(),
		.mod = p.mod,
		.short_name = ISTR_NONE,
		.loc = oloc,
		.kind = SYMBOL_IMPORT_ASSERTION,
		.d_imas = {
			.entries = entries,
		},
		.is_pub = false,
		.is_extern = false,
		.extern_symbol = ISTR_NONE,
	};

	table_register(sym);
}

// -1 for not found
int pimport_ident(istr_t name) {
	for (u32 i = 0; i < p.is_len; i++) {
		if (p.is[i].name == name) {
			return i;
		}
	}
	return -1;
}

// register a symbol, raise error if already exists
void pscope_register(pscope_entry_t to_register) {
	if (to_register.kind == PS_DEBUG_REFERENCE) {
		goto success;
	}
	
	// 1. can't shadow imports
	int idx;
	if ((idx = pimport_ident(to_register.name)) != -1) {
		print_err_with_pos(to_register.loc, "variable `%s` cannot shadow import `%s`", sv_from(to_register.name), sv_from(to_register.name));
		print_hint_with_pos(p.is[idx].loc, "import declared here");
		err_unwind();
	}

	if (p.scope_len == 0) {
		goto success;
	}

	// 2. no duplicate vars in the same scope
	// 3. can't use var before def in same scope
	u32 lo = p.scope[p.scope_len - 1];
	u32 hi = p.scope_entries_len;
	for (u32 i = lo; i < hi; i++) {
		pscope_entry_t *entry = &p.scope_entries[i];

		// still check masked entries

		if (entry->kind == PS_DEBUG_REFERENCE) {
			print_err_with_pos(entry->loc, "variable `%s` used before definition in the same scope", sv_from(to_register.name));
			print_hint_with_pos(to_register.loc, "previous declaration here");
			err_unwind();
		}

		if (entry->name == to_register.name) {
			print_err_with_pos(to_register.loc, "variable `%s` already exists in this scope", sv_from(to_register.name));
			print_hint_with_pos(entry->loc, "previous declaration here");
			err_unwind();
		}
	}

success:
	p.scope_entries[p.scope_entries_len++] = to_register;
}

void ppush_scope(void) {
	p.scope[p.scope_len++] = p.scope_entries_len;
}

void ppop_scope(void) {
	assert(p.scope_len > 0);
	p.scope_len--;
	p.scope_entries_len = p.scope[p.scope_len];
}

void pmask_scope(u32 entries_lo, u32 entries_hi, bool mask) {
	for (u32 i = entries_lo; i < entries_hi; i++) {
		pscope_entry_t *entry = &p.scope_entries[i];
		entry->is_masked = mask;
	}
}

static pattern_t *pattern_dup(pattern_t pattern) {
	pattern_t *ptr = malloc(sizeof(pattern_t));
	*ptr = pattern;
	return ptr;
}

// the only place where user variables are created
pattern_t ppattern(ir_desc_t *desc) {
	switch (p.token.kind) {
		case TOK_UNDERSCORE: {
			pnext();
			return (pattern_t){
				.kind = PATTERN_UNDERSCORE,
			};
		}
		case TOK_IDENT: {
			bool is_mut = false;

			istr_t name = p.token.lit;
			loc_t name_loc = p.token.loc;
			pnext();

			// v' as mut now

			if (p.token.kind == TOK_TACK) {
				is_mut = true;
				pnext();
			}

			rlocal_t local = ir_local_new(desc, (local_t){
				.kind = is_mut ? LOCAL_MUT : LOCAL_IMM,
				.type = TYPE_INFER,
				.loc = name_loc,
				.name = name,
			});

			(void)pscope_register((pscope_entry_t){
				.name = name,
				.loc = name_loc,
				.kind = PS_LOCAL,
				.d_local = { local },
			});

			return (pattern_t){
				.loc = name_loc,
				.kind = PATTERN_LOCAL,
				.d_local = local,
			};
		}
		case TOK_INTEGER: {
			istr_t val = p.token.lit;
			loc_t loc = p.token.loc;
			pnext();
			return (pattern_t){
				.loc = loc,
				.kind = PATTERN_INTEGER_LIT,
				.d_integer_lit = val,
			};
		}
		case TOK_OPAR: {
			loc_t oloc = p.token.loc;
			pnext();
			if (p.token.kind == TOK_CPAR) {
				pnext();
				return (pattern_t){
					.kind = PATTERN_TUPLE_UNIT,
					.loc = oloc,
				};
			}
			bool first = true;
			pattern_t *elems = NULL;
			pattern_t pattern;
			while (p.token.kind != TOK_CPAR) {
				if (first) {
					pattern = ppattern(desc);
				} else {
					if (elems == NULL) {
						arrpush(elems, pattern);
					}
					pattern = ppattern(desc);
					arrpush(elems, pattern);					
				}

				if (p.token.kind == TOK_COMMA) {
					pnext();
				} else if (p.token.kind != TOK_CPAR) {
					punexpected("expected `,` or `)`");
				}

				first = false;
			}
			pnext();
			if (elems != NULL) {
				pattern = (pattern_t){
					.kind = PATTERN_TUPLE,
					.loc = oloc,
					.d_tuple = elems,
				};
			}
			return pattern;
		}
		case TOK_OSQ: {
			// [1, a, b]
			// ^
			loc_t oloc = p.token.loc;
			pattern_t *elems = NULL;

			pattern_t pattern = {
				.kind = PATTERN_ARRAY,
				.loc = oloc,
			};

			// [x, ...xs]
			// [xs..., x]
			//
			// [a..., b, c, d]
			// [a, b, c, ...d]
			//
			// [a..., b, ...c]     (impossible and ambiguous)
			//
			pnext();
			while (p.token.kind != TOK_CSQ) {
				// [x, ...xs]
				if (p.token.kind == TOK_TRIPLE_DOTS) {
					if (pattern.d_array.match) {
						err_with_pos(p.token.loc, "cannot have multiple `...` in array pattern");
					}
					loc_t oloc = p.token.loc;
					pnext();
					pattern.d_array.match = pattern_dup(ppattern(desc));
					pattern.d_array.match_lhs = false;
					if (p.token.kind != TOK_CSQ) {
						// TODO: dbg str for patterns instead of printing `xs...`
						err_with_pos(oloc, "a `...xs` in array pattern must reside at the end");
					}
				} else {
					pattern_t elem = ppattern(desc);

					// [xs..., x]
					if (p.token.kind == TOK_TRIPLE_DOTS) {
						if (pattern.d_array.match) {
							err_with_pos(p.token.loc, "cannot have multiple `...` in array pattern");
						}
						if (arrlenu(elems) != 0) {
							// TODO: dbg str for patterns instead of printing `xs...`
							err_with_pos(p.token.loc, "a `xs...` in array pattern must reside at the start");
						}
						pnext();
						pattern.d_array.match = pattern_dup(elem);
						pattern.d_array.match_lhs = true;
					} else {
						arrpush(elems, elem);
					}
				}
				
				if (p.token.kind == TOK_COMMA) {
					pnext();
				} else if (p.token.kind != TOK_CSQ) {
					punexpected("expected `,` or `]`");
				}
			}
			pnext();

			// [xs...] and [...xs] not allowed
			if (arrlenu(elems) == 0 && pattern.d_array.match) {
				err_with_pos(pattern.d_array.match->loc, "a `...` in array pattern must not be the only pattern");
				// TODO: print patterns
				// hint: use `xs` instead
			}

			// set
			pattern.d_array.elems = elems;

			return pattern;
		}
		default: {
			punexpected("expected pattern");
		}
	}
}

// if `opt_label != ISTR_NONE` then `onerror` points to label otherwise the `brk` expr
// returns wasm like branch index
u32 pblk_locate_label(istr_t opt_label, loc_t onerror) {
	for (u32 i = p.blks_len; i-- > 0;) {
		pblk_t *blk = &p.blks[i];

		if (!blk->always_brk && opt_label == ISTR_NONE) {
			continue;
		}
		if (blk->label == opt_label) {
			return p.blks_len - i - 1;
		}
	}

	// makes more sense, need to specialise the error message regardless on state of blks
	if (opt_label != ISTR_NONE) {
		err_with_pos(onerror, "label `%s` not found", sv_from(opt_label));
	} else {
		err_with_pos(onerror, "not inside a loop");
	}
}
