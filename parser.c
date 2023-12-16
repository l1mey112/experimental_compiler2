#include "all.h"

#include "stb_ds.h"

typedef struct pctx_t pctx_t;
/* typedef struct pval_t pval_t;
typedef struct pimport_t pimport_t;
typedef enum pvkind_t pkind_t;

struct pimport_t {

};

enum pvkind_t {
	VAL_INST,
	VAL_SYM,
	VAL_LOCAL,
	VAL_INTEGER_LITERAL,
};

struct pval_t {
	pkind_t kind;
	loc_t loc;
	type_t type;

	union {
		//
	};
}; */

/* enum pskind_t {
	SCOPE_FUNCTION,
	SCOPE_BLOCK,
	SCOPE_LOOP,
	SCOPE_SWITCH,
};

struct pscope_t {
	istr_t label; // -1 for none
	pskind_t kind;

	ir_rblk_t bb;   // continue here
	ir_rblk_t pred; // break here

	// var_start
	// var_end
	// var_enclosing (proc_t)
	
	bool is_break;    // TODO: impl
	bool is_continue; // TODO: impl

	union {
		// if expression etc
	};
}; */

struct pctx_t {
	u8 *pstart;
	u8 *pc;
	u8 *pend;
	u8 *plast_nl;
	u32 line_nr;
	token_t token;
	token_t peek;
	// pval_t es[128]; // expr stack
	// u32 es_len;
	// pimport_t is[64]; // import stack
	// u32 is_len;
	fs_rfile_t file;
	fs_rnode_t module;
	bool has_done_imports;
	//
	struct {
		istr_t name; // -1 for none
		type_t type;
		loc_t loc;
	} next_type;
};

pctx_t p;

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

		if (is_id_begin(ch)) {
			u8 *start = p.pc;

			token_t token = {
				.loc.line_nr = p.line_nr,
				.loc.col = p.pc - p.plast_nl,
				.loc.file = p.file,
				.loc.pos = start - p.pstart,
			};
			
			do {
				p.pc++;
			} while (p.pc < p.pend && is_id(*p.pc));

			// get length and id pointer
			u32 len = p.pc - start;
			token.loc.len = len;

			// TODO: this should be optimised to a static hash table

			if (0);
			#define X(val, lit) \
				else if (sv_cmp_literal(start, len, lit)) token.kind = val;
			TOK_X_KEYWORDS_LIST
			#undef X
			else {
				token.kind = TOK_IDENT;
				token.lit = sv_intern(start, len);
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

const char *tok_dbg_str(token_t tok) {
	// handle identifiers

	u8 *p;

	bool requires_quotes = true;
	const char *str = NULL;
	u32 len;

	// passing { .lit = -1, .kind = TOK_IDENT } will return "identifier"
	// so you can do something like: "unexpected `x`, expected identifier"
	//                             : "unexpected `x`, expected `+=`"

	if (TOK_HAS_LIT(tok.kind) && tok.lit == (istr_t)-1) {
		requires_quotes = false;
	}
	
	if (TOK_HAS_LIT(tok.kind) && tok.lit != (istr_t)-1) {
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
	p.token = p.peek;
	p.peek = plex();
}

#define DEFAULT_DBG_TOK(expected) (token_t){.kind = expected, .lit = (istr_t)-1}

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


void pexpr(ir_rscope_t s) {
	assert_not_reached();
}

// function declarations:
// - io <name>            = ...
// - io <name>: <pattern> = ...
// - <name>:    <pattern> = ...

// TODO: figure out internal representation of a pattern match
void ppattern(void) {
	assert_not_reached();
}

// will be filled from main()
istr_t typeinfo_concrete_istr[_TYPE_CONCRETE_MAX];
u32 typeinfo_concrete_istr_size;

type_t ptype_id(istr_t lit) {
	for (u32 i = 0; i < typeinfo_concrete_istr_size; i++) {
		if (typeinfo_concrete_istr[i] == lit) {
			return (type_t)i;
		}
	}

	assert_not_reached();
}

type_t ptype(void) {
	// loc_t initial_pos = p.token.loc;
	type_t type;

	switch (p.token.kind) {
		case TOK_IDENT: {
			istr_t initial = p.token.lit;
			pnext();
			if (p.token.kind == TOK_DOT) {
				// TODO: integrate module system later
				assert_not_reached();
			} else {
				type = ptype_id(initial);
			}
			// terminating condition
			break;
		}
		case TOK_NOT: {
			pnext();
			type = TYPE_BOTTOM;
			// terminating condition
			break;
		}
		case TOK_OPAR: {
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
					if (single != (type_t)-1) {
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
				return single;
			} else if (elems == NULL) {
				return TYPE_UNIT;
			}

			type = type_new((tinfo_t){
				.kind = TYPE_TUPLE,
				.d_tuple.elems = elems,
				.d_tuple.len = arrlen(elems),
			}, NULL);
			// terminating condition
			break;
		}
		default:
			punexpected("expected type in definition");
	}

	// parse curried form
	// essentially a precedence climber, with only one operation which is ->
	// i32 -> i32 -> i32
	// i32 -> (i32 -> i32)
	//
	// TODO: currently a recursive impl when should be using while(not TYPE_FN)
	//       to make it work basically like a pexpr()
	//
	if (p.token.kind == TOK_ARROW) {
		// i32 -> ...
		//     ^^
		pnext();
		type_t ret = ptype();
		type = type_new((tinfo_t){
			.kind = TYPE_FN,
			.d_fn.arg = type,
			.d_fn.ret = ret,
		}, NULL);
	}

	return type;
}

void pproc(ir_rscope_t s) {
	bool is_io = false;

	if (p.token.kind == TOK_IO) {
		is_io = true;
		pnext();
	}

	pcheck(TOK_IDENT);
	istr_t name = p.token.lit;
	loc_t name_loc = p.token.loc;

	// io name
	//    ^^^^

	pnext();

	// name:
	//     ^
	
	if (p.token.kind == TOK_COLON) {
		pnext();
		// TODO: parse pattern
	}

	pexpect(TOK_ASSIGN);
	// TODO: pexpr(s);

	type_t type = TYPE_INFER;
	if (p.next_type.name == name) {
		type = p.next_type.type;
		p.next_type.name = (u32)-1;
	}

	printf("type: %s\n", type_dbg_str(type));
	printf("scope(%u): proc %s\n", s, sv_from(name));
}

void ptypedecl(ir_rscope_t s) {
	// guard in here and in `pstmt()`
	if (p.next_type.name != (u32)-1) {
		err_with_pos(p.next_type.loc, "unused type decl `%s`", sv_from(p.next_type.name));
	}
	
	istr_t name = p.token.lit;
	loc_t name_loc = p.token.loc;

	pnext();
	pnext();
	
	// v :: i32 -> i32 -> ...
	//      ^^^

	type_t type = ptype();

	p.next_type.type = type;
	p.next_type.name = name;
	p.next_type.loc = name_loc;
}

void pstmt(ir_rscope_t s) {
	bool has_next_type = p.next_type.name != (u32)-1;
	
	if (p.token.kind == TOK_IDENT && p.peek.kind == TOK_COLON) {
		pproc(s);
	} else switch (p.token.kind) {
		case TOK_IO: {
			pproc(s);	
			break;
		}
		case TOK_IDENT: {
			if (p.peek.kind == TOK_COLON) {
				pproc(s);
				break;
			}
			if (p.peek.kind == TOK_DOUBLE_COLON) {
				ptypedecl(s);
				break;
			}
			// fall through
		}
		default: {
			pexpr(s);
			break;
		}
	}

	// error if type decl not used
	if (has_next_type && p.next_type.name != (u32)-1) {
		err_with_pos(p.next_type.loc, "unused type decl `%s`", sv_from(p.next_type.name));
	}
}

void ptop_stmt(void) {
	switch (p.token.kind) {
		default: {
			pstmt(SCOPE_ROOT);
			break;		
		}
	}
}

void pentry(fs_rfile_t file) {
	fs_file_t *f = fs_filep(file);
	
	p = (pctx_t){
		.pstart = f->data,
		.pc = f->data,
		.pend = f->data + f->len,
		.plast_nl = f->data,
		.file = file,
		.module = f->module,
		.next_type.name = -1,
	};

	pnext(); // tok
	pnext(); // tok peek

	while (p.token.kind != TOK_EOF) {
		ptop_stmt();
	}
}
