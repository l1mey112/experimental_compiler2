#include "all.h"
#include "parser.h"

static bool is_id_begin(u8 ch) {
	return isalpha(ch) || ch == '_';
}

static bool is_id(u8 ch) {
	return isalpha(ch) || ch == '_' || isdigit(ch);
}

token_t plex(void) {
	while (p.pc < p.pend) {
		u8 ch = *p.pc;

		if (isspace(ch)) {
			p.pc++;
			if (ch == '\n') {
				p.plast_nl = p.pc;
				p.line_nr++;
			}
			continue;
		}

		if (ch == ';') {
			p.pc++;
			while (p.pc < p.pend && *p.pc != '\n') {
				p.pc++;
			}
			continue;
		}

		if (is_id_begin(ch)) {
			u8 *start = p.pc;
			bool is_underscore = *start == '_';

			token_t token = {
				.loc.line_nr = p.line_nr,
				.loc.col = p.pc - p.plast_nl,
				.loc.file = p.file,
				.loc.pos = start - p.pstart,
			};
			
			do {
				p.pc++;
			} while (p.pc < p.pend && is_id(*p.pc));

			if (is_underscore && p.pc - start == 1) {
				token.loc.len = 1;
				token.kind = TOK_UNDERSCORE;
				return token;
			}

			// get length and id pointer
			u32 len = p.pc - start;
			token.loc.len = len;

			// TODO: this should be optimised to a static hash table

			if (0);
			#define X(val, lit) \
				else if (ptr_cmp_literal(start, len, lit)) token.kind = val;
			TOK_X_KEYWORDS_LIST
			#undef X
			else {
				istr_t istr = sv_intern(start, len);		
				token.kind = TOK_IDENT;
				token.lit = istr;
			}

			return token;
		} else if (isdigit(ch)) {
			u8 *start = p.pc;

			token_t token = {
				.kind = TOK_INTEGER,
				.loc.line_nr = p.line_nr,
				.loc.col = p.pc - p.plast_nl,
				.loc.file = p.file,
				.loc.pos = start - p.pstart,
			};
			
			do {
				p.pc++;
			} while (p.pc < p.pend && isdigit(*p.pc));

			// get length and id pointer
			u32 len = p.pc - start;

			token.lit = sv_intern(start, len);
			token.loc.len = len;

			return token;
		} else {
			u8 *start = p.pc;
			size_t avail = p.pend - p.pc;

			// the compiler optimiser would be able to optimise and
			// spot locations of compile time known bounds to memcmp
			// ... where it can
			//
			// this isn't perfect, the old switch case impl would be better
			// the macro method reduces complexity on implementation
			// i know well that the compiler will NOT optimise this efficiently

			// TODO: ~~this should be optimised to a static hash table~~
			//       maybe not, it's not that much faster

			token_t token = {
				.loc.line_nr = p.line_nr,
				.loc.col = p.pc - p.plast_nl,
				.loc.file = p.file,
				.loc.pos = start - p.pstart,
			};

			if (0);
			#define X(val, lit) \
				else if (strlen(lit) <= avail && memcmp(start, lit, strlen(lit)) == 0) { \
					token.kind = val; \
					token.loc.len = strlen(lit); \
					p.pc += strlen(lit); \
				}
			TOK_X_OPERATOR_LIST
			#undef X
			else {
				err_with_pos(token.loc, "unexpected character `%c`", ch);
			}

			return token;
		}
	}

	return (token_t){.kind = TOK_EOF};
}

const char *tok_op_str(tok_t tok) {
	switch (tok) {
		#define X(val, lit) \
			case val: return lit;
		TOK_X_OPERATOR_LIST
		#undef X
		default: {
			assert_not_reached();
		}
	}
}

const char *tok_dbg_str(token_t tok) {
	// handle identifiers

	u8 *p;

	bool requires_quotes = true;
	const char *str = NULL;
	u32 len;

	// passing { .lit = -1, .kind = TOK_IDENT } will return "identifier"
	// so you can do something like: "unexpected `x`, expected identifier"
	//                             : "unexpected `x`, expected `+=`"

	if (TOK_HAS_LIT(tok.kind) && tok.lit == ISTR_NONE) {
		requires_quotes = false;
	}
	
	if (TOK_HAS_LIT(tok.kind) && tok.lit != ISTR_NONE) {
		str = sv_from(tok.lit);
		len = strlen(str);
	}
	#define X(val, lit) \
		else if (val == tok.kind) str = lit, len = strlen(lit);
	TOK_X_LIST
	#undef X

	if (requires_quotes) {
		p = malloc(len + 2 + 1);
		sprintf((char *)p, "`%s`", str);
	} else {
		p = malloc(len + 1);
		strcpy((char *)p, str);
	}

	return (const char *)p;
}

void pnext(void) {
	p.prev = p.token;
	p.token = p.peek;
	p.peek = plex();
}

bool pprev_next_to(void) {
	return p.prev.loc.pos + p.prev.loc.len == p.token.loc.pos;
}

bool ppeek_next_to(void) {
	return p.token.loc.pos + p.token.loc.len == p.peek.loc.pos;
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

enum : u8 {
	PREC_UNKNOWN,
	PREC_CALL,
	PREC_UNARY,
};

u8 ptype_prec(void) {
	switch (p.token.kind) {
		case TOK_ARROW: return PREC_CALL;
		default: return PREC_UNKNOWN;
	}
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
			token_t int_size;


			if (p.token.kind == TOK_INTEGER) {
				int_size = p.token;
				is_array = true;
				pnext();
			} else if (p.token.kind != TOK_CSQ) {
				punexpected("expected integer or `]`");
			}
			pexpect(TOK_CSQ);
			
			// []i32
			//   ^^^

			type_t elem = ptype();

			if (is_array) {
				size_t len = strtoull(sv_from(int_size.lit), NULL, 10);

				if (len == 0) {
					err_with_pos(int_size.loc, "array length cannot be zero");
				}
				
				type = type_new((tinfo_t){
					.kind = TYPE_ARRAY,
					.d_array.length = len,
					.d_array.elem = elem,
				}, NULL);
			} else {
				type = type_new((tinfo_t){
					.kind = TYPE_SLICE,
					.d_slice.elem = elem,
				}, NULL);
			}
			break;
		}
		case TOK_IDENT: {
			istr_t initial = p.token.lit;
			pnext();
			if (p.token.kind == TOK_DOT) {
				// TODO: integrate module system later
				assert_not_reached();
			} else {
				assert_not_reached();
			}
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
			bool is_hash = false; // is fn ptr
			
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
				.d_tuple.elems = elems,
			}, NULL);
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
			case TOK_ARROW: {
				// no merging as that would break type equivalence/immutability
				//
				// TODO: we alloc + copy every single damn time, this is very bad,
				//       however, i don't care at the moment.
				//       in the future, extract this code out so that it doesn't
				//       waste any memory to constant duplications. O(n^2) roughly
				//
				//       on second thought, this is really really weird code
				//       refactor ASAP, but leave for later honestly
				//
				//       will get even weirder with regions assigned to each ptr arg
				//
				// i32 -> i32           (parse into single fn call)
				// i32 -> i32 -> i32    (merge into single fn call)
				pnext();
				type_t *args = NULL;
				type_t ret = ptype_expr(PREC_CALL);

				tinfo_t *info;
				if (TI_GUARD(type, TYPE_FUNCTION, info)) {
					u32 len = arrlenu(info->d_fn.args);
					type_t *nptr = arraddnptr(args, len);
					memcpy(nptr, info->d_fn.args, len * sizeof(type_t));
					arrpush(nptr, info->d_fn.ret);
					args = nptr;
				} else {
					arrpush(args, type);
				}

				type = type_new((tinfo_t){
					.kind = TYPE_FUNCTION,
					.d_fn.args = args,
					.d_fn.ret = ret,
				}, NULL);
				continue;
			}
			default: {}
		}
		break;
	}

	return type;
}

type_t ptype(void) {
	return ptype_expr(0);
}

void pimport(void) {
	if (p.has_done_imports) {
		punexpected("import declaration must come before code");
	}
	
	loc_t oloc = p.token.loc;
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

	rmod_t mod = fs_register_import(p.mod, fields, fields_len, oloc);
	istr_t module_ident = fields[fields_len - 1];

	for (u32 i = 0; i < p.is_len; i++) {
		pimport_t *is = &p.is[i];
		if (is->mod == mod) {
			err_with_pos(oloc, "import `%s` already imported", fs_module_symbol_str(is->mod, module_ident));
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

void pscope_register(pscope_entry_t entry) {
	p.scope_entries[p.scope_entries_len++] = entry;
}

void ppush_scope(void) {
	p.scope[p.scope_len++] = p.scope_entries_len;
}

void ppop_scope(void) {
	assert(p.scope_len > 0);
	p.scope_len--;
	p.scope_entries_len = p.scope[p.scope_len];
}

static lir_term_pat_t *pattern_alloc(lir_term_pat_t pattern) {
	lir_term_pat_t *ptr = malloc(sizeof(lir_term_pat_t));
	*ptr = pattern;
	return ptr;
}

// will insert into basic block and register inside the scope
lir_term_pat_t ppattern(lir_proc_t *proc, lir_rblock_t block) {
	bool is_mut = false;
	switch (p.token.kind) {
		case TOK_UNDERSCORE: {
			pnext();
			return (lir_term_pat_t){
				.kind = PATTERN_UNDERSCORE,
			};
		}
		case TOK_TACK: {
			is_mut = true;
			pnext();
			// fallthrough
		}
		case TOK_IDENT: {
			istr_t name = p.token.lit;
			loc_t name_loc = p.token.loc;
			pnext();
			//
			// construct block arg and assign to local
			lir_rblock_t rarg = arrlenu(proc->blocks[block].args);
			lir_rvalue_t arg = lir_block_arg(proc, block, (lir_value_t){
				.type = TYPE_INFER,
				.loc = name_loc,
			});

			lir_rlocal_t local = lir_local_new(proc, name, name_loc, TYPE_INFER, is_mut);
			lir_local_store(proc, block, local, arg);

			(void)pscope_register((pscope_entry_t){
				.name = name,
				.loc = name_loc,
				.kind = PS_LOCAL,
				.d_local = local,
			});

			return (lir_term_pat_t){
				.loc = name_loc,
				.kind = PATTERN_LOCAL,
				.d_local = local,
			};
		}
		case TOK_INTEGER: {
			istr_t val = p.token.lit;
			loc_t loc = p.token.loc;
			pnext();
			return (lir_term_pat_t){
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
				return (lir_term_pat_t){
					.kind = PATTERN_TUPLE_UNIT,
					.loc = oloc,
				};
			}
			bool first = true;
			lir_term_pat_t *elems = NULL;
			lir_term_pat_t pattern;
			while (p.token.kind != TOK_CPAR) {
				if (first) {
					pattern = ppattern(proc, block);
				} else {
					if (elems == NULL) {
						arrpush(elems, pattern);
					}
					pattern = ppattern(proc, block);
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
				pattern = (lir_term_pat_t){
					.kind = PATTERN_TUPLE,
					.loc = oloc,
					.d_tuple.elems = elems,
				};
			}
			return pattern;
		}
		case TOK_OSQ: {
			// [1, a, b]
			// ^
			loc_t oloc = p.token.loc;
			lir_term_pat_t *elems = NULL;

			lir_term_pat_t pattern = {
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
					pattern.d_array.match = pattern_alloc(ppattern(proc, block));
					pattern.d_array.match_lhs = false;
					if (p.token.kind != TOK_CSQ) {
						// TODO: dbg str for patterns instead of printing `xs...`
						err_with_pos(oloc, "a `...xs` in array pattern must reside at the end");
					}
				} else {
					lir_term_pat_t elem = ppattern(proc, block);

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
						pattern.d_array.match = pattern_alloc(elem);
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