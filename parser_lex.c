#include "all.h"
#include "parser.h"

static bool is_id_begin(u8 ch) {
	return isalpha(ch) || ch == '_';
}

static bool is_id(u8 ch) {
	return isalpha(ch) || ch == '_' || isdigit(ch);
}

static void pstring_literal(token_t *token) {
	u8 *start = p.pc;

	token->kind = TOK_STRING;

	// "here
	//  ^
	while (true) {
		p.pc++;

		if (p.pc >= p.pend) {
			err_with_pos(token->loc, "unfinished string literal");
		}

		u8 ch = *p.pc;
		if (ch == '\n') {
			p.plast_nl = p.pc;
			p.line_nr++;
		}

		if (ch == '"') {
			break;
		}
	}

	p.pc++;

	u32 len = p.pc - start;
	token->loc.len = len;
	token->lit = sv_intern(start + 1, len - 2);
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

		u8 *start = p.pc;
		token_t token = {
			.loc.line_nr = p.line_nr,
			.loc.col = p.pc - p.plast_nl,
			.loc.file = p.file,
			.loc.pos = start - p.pstart,
		};

		if (ch == '"') {
			pstring_literal(&token);
			return token;
		}

		if (is_id_begin(ch)) {
			bool is_underscore = *start == '_';

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
			do {
				p.pc++;
			} while (p.pc < p.pend && isdigit(*p.pc));

			token.kind = TOK_INTEGER;

			// get length and id pointer
			u32 len = p.pc - start;

			token.lit = sv_intern(start, len);
			token.loc.len = len;

			return token;
		} else {
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

	enum : u8 {
		REQ_NONE,
		REQ_QUOTES,
	} status;

	const char *str = NULL;
	u32 len;

	// passing { .lit = -1, .kind = TOK_IDENT } will return "identifier"
	// so you can do something like: "unexpected `x`, expected identifier"
	//                             : "unexpected `x`, expected `+=`"

	// for strings, don't display `expected "this string right now"`

	if (TOK_HAS_LIT(tok.kind) && tok.lit == ISTR_NONE) {
		status = REQ_NONE;
	} else if (tok.kind == TOK_STRING) {
		status = REQ_NONE;
	} else {
		status = REQ_QUOTES;
	}

	if (TOK_HAS_LIT(tok.kind) && tok.lit != ISTR_NONE) {
		str = sv_from(tok.lit);
		len = strlen(str);
	}
	#define X(val, lit) \
		else if (val == tok.kind) str = lit, len = strlen(lit);
	TOK_X_LIST
	#undef X

	if (status == REQ_QUOTES) {
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
