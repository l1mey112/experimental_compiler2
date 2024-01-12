#include "all.h"
#include <assert.h>
#include <stdbool.h>

hir_var_t *hir_vars;

typedef struct pctx_t pctx_t;
typedef struct pimport_t pimport_t;
typedef struct pblk_t pblk_t;
typedef struct pproc_decls_t pproc_decls_t;
typedef struct pscope_desc_t pscope_desc_t;

struct pblk_t {
	istr_t label;
	loc_t loc; // TODO: i don't even think we'll need this?
	bool always_brk; // if false, a `brk` without a label doesn't resolve to this
};

struct pimport_t {
	rmod_t mod;
	istr_t name;
	loc_t loc;
};

// TODO: remove?
struct pscope_desc_t {
};

struct pctx_t {
	u8 *pstart;
	u8 *pc;
	u8 *pend;
	u8 *plast_nl;
	u32 line_nr;
	token_t prev;
	token_t token;
	token_t peek;
	pimport_t is[64]; // import stack
	u32 is_len;
	pblk_t blks[256]; // block stack, idx encoded in a u8
	u32 blks_len;
	pscope_desc_t scope_desc[128]; // scope stack to scope scope level context
	u32 scope_desc_len;
	rfile_t file;
	rmod_t mod;
	mod_t *modp;
	bool has_done_imports;
};

pctx_t p;

void pscope_enter(void) {
	p.scope_desc[p.scope_desc_len++] = (pscope_desc_t){
	};
}

void papply_typedecls(void);

void pscope_leave(void) {
	p.scope_desc_len--;

	// TODO: remove or find better use
	pscope_desc_t *desc = &p.scope_desc[p.scope_desc_len];
	(void)desc;
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

hir_node_t *hir_memdup(hir_node_t node) {
	hir_node_t *ptr = malloc(sizeof(hir_node_t));
	*ptr = node;
	return ptr;
}

hir_pattern_t *pattern_memdup(hir_pattern_t pattern) {
	hir_pattern_t *ptr = malloc(sizeof(hir_pattern_t));
	*ptr = pattern;
	return ptr;
}

static bool hir_name_exists_in(hir_scope_t *scope, istr_t name) {
	if (scope == NULL) {
		scope = &p.modp->toplevel;
	}

	for (u32 i = 0; i < arrlen(scope->locals); i++) {
		hir_rvar_t id = scope->locals[i];
		hir_var_t *var = VAR_PTR(id);

		if (var->name == name) {
			return true;
		}
	}

	return false;
}

static bool hir_var_exists_in(hir_scope_t *scope, hir_rvar_t var) {
	if (scope == NULL) {
		scope = &p.modp->toplevel;
	}

	for (u32 i = 0; i < arrlen(scope->locals); i++) {
		hir_rvar_t id = scope->locals[i];
		if (id == var) {
			return true;
		}
	}

	return false;
}

static hir_var_t *hir_name_in(hir_scope_t *scope, istr_t name) {
	if (scope == NULL) {
		scope = &p.modp->toplevel;
	}

	for (u32 i = 0; i < arrlen(scope->locals); i++) {
		hir_rvar_t id = scope->locals[i];
		hir_var_t *var = VAR_PTR(id);

		if (var->name == name) {
			return var;
		}
	}

	return NULL;
}

void papply_pub(void) {
	// pubs all have types, i don't want to sort modules... global exprs is enough
	for (u32 i = 0, c = arrlenu(p.modp->toplevel.locals); i < c; i++) {
		hir_var_t *varp = VAR_PTR(p.modp->toplevel.locals[i]);
		if (varp->is_pub && varp->type == TYPE_INFER) {
			err_with_pos(varp->loc, "cannot make expression with inferred type public");
		}
	}
}

// find uses in expressions that are apart of the same scope
// don't recurse into deeper scopes
// WILL ONLY FIND SYMBOLS
// warning: possibly unstable pointer
// WILL IGNORE EXISTING VARS RESOLVED FROM AN OLDER SCOPE, ITS JUST NAMES TO NAMES HERE
static hir_node_t *hir_sym_find_use(hir_node_t *expr, istr_t name) {
	hir_node_t *r;
	switch (expr->kind) {
		case NODE_GLOBAL_UNRESOLVED: {
			if (expr->d_global_unresolved == name) {
				return expr;
			}
			return NULL;
		}
		case NODE_VAR: {
			if (VAR_PTR(expr->d_var)->name == name) {
				return expr;
			}
			return NULL;
		}
		case NODE_POSTFIX: {
			return hir_sym_find_use(expr->d_postfix.expr, name);
		}
		case NODE_PREFIX: {
			return hir_sym_find_use(expr->d_prefix.expr, name);
		}
		case NODE_INFIX: {
			if ((r = hir_sym_find_use(expr->d_infix.lhs, name))) {
				return r;
			}
			if ((r = hir_sym_find_use(expr->d_infix.rhs, name))) {
				return r;
			}
			return NULL;
		}
		case NODE_INDEX: {
			if ((r = hir_sym_find_use(expr->d_index.expr, name))) {
				return r;
			}
			if ((r = hir_sym_find_use(expr->d_index.index, name))) {
				return r;
			}
			return NULL;
		}
		case NODE_CALL: {
			if ((r = hir_sym_find_use(expr->d_call.f, name))) {
				return r;
			}
			if ((r = hir_sym_find_use(expr->d_call.arg, name))) {
				return r;
			}
			return NULL;
		}
		case NODE_TUPLE: {
			for (u32 i = 0, c = arrlen(expr->d_tuple.elems); i < c; i++) {
				hir_node_t *exprp = &expr->d_tuple.elems[i];
				if ((r = hir_sym_find_use(exprp, name))) {
					return r;
				}
			}
			return NULL;
		}
		case NODE_ARRAY_LIT: {
			for (u32 i = 0, c = arrlen(expr->d_array_lit.elems); i < c; i++) {
				hir_node_t *exprp = &expr->d_array_lit.elems[i];
				if ((r = hir_sym_find_use(exprp, name))) {
					return r;
				}
			}
			return NULL;
		}
		// even though this doesn't make sense, include for completeness
		case NODE_BREAK: {
			if ((r = hir_sym_find_use(expr->d_break.expr, name))) {
				return r;
			}
			return NULL;
		}
		case NODE_LET_DECL: {
			if ((r = hir_sym_find_use(expr->d_let_decl.expr, name))) {
				return r;
			}
			return NULL;
		}
		case NODE_ADDR_OF: {
			if ((r = hir_sym_find_use(expr->d_addr_of.ref, name))) {
				return r;
			}
			return NULL;
		}
		case NODE_DEREF: {
			if ((r = hir_sym_find_use(expr->d_deref, name))) {
				return r;
			}
			return NULL;
		}
		case NODE_ASSIGN: {
			if ((r = hir_sym_find_use(expr->d_infix.lhs, name))) {
				return r;
			}
			if ((r = hir_sym_find_use(expr->d_infix.rhs, name))) {
				return r;
			}
			return NULL;
		}
		case NODE_CAST: {
			if ((r = hir_sym_find_use(expr->d_cast, name))) {
				return r;
			}
			return NULL;
		}
		case NODE_UNDEFINED:
		case NODE_LAMBDA:
		case NODE_DO_BLOCK:
		case NODE_INTEGER_LIT: {
			return NULL;
		}
		default: {
			// TODO: be sure to be exhaustive
			printf("hir_sym_find_use: unhandled node kind %d\n", expr->kind);
			assert_not_reached();
		}
	}
}

// find uses in expressions that are apart of the same scope
// don't recurse into deeper scopes
// WILL ONLY FIND SYMBOLS
// warning: possibly unstable pointer
static hir_node_t *hir_sym_find_uses(hir_node_t *exprs, istr_t name) {
	if (exprs == NULL) {
		return NULL;
	}
	
	for (u32 i = 0, c = arrlenu(exprs); i < c; i++) {
		hir_node_t *expr = &exprs[i];
		if ((expr = hir_sym_find_use(expr, name))) {
			return expr;
		}
	}

	return NULL;
}

// NULL meaning toplevel
// `scope` MUST refer to `previous_exprs`
static hir_rvar_t hir_new_var(hir_scope_t *scope, istr_t name, loc_t onerror, hir_node_t *previous_exprs) {
	int idx;
	if ((idx = pimport_ident(name)) != -1) {
		print_err_with_pos(onerror, "variable `%s` cannot shadow import `%s`", sv_from(name), sv_from(name));
		print_hint_with_pos(p.is[idx].loc, "import declared here");
		err_unwind();
	}
	
	hir_var_t *ex_var;
	if ((ex_var = hir_name_in(scope, name))) {
		print_err_with_pos(onerror, "variable `%s` already exists in scope", sv_from(name));
		print_hint_with_pos(ex_var->loc, "variable `%s` declared here", sv_from(name));
		err_unwind();
	}

	hir_node_t *use;
	if (scope != NULL && (use = hir_sym_find_uses(previous_exprs, name))) {
		print_err_with_pos(use->loc, "use of variable `%s` before declaration in same scope", sv_from(name));
		print_hint_with_pos(onerror, "variable `%s` declared here", sv_from(name));
		err_unwind();
	}
	
	hir_rvar_t id = arrlen(hir_vars);
	hir_var_t var = {
		.loc = onerror,
		.name = name,
		.type = TYPE_INFER,
	};
	arrpush(hir_vars, var);

	if (scope == NULL) {
		scope = &p.modp->toplevel;
	}

	arrpush(scope->locals, id);
	
	return id;	
}

// NULL meaning toplevel, but that doesn't make sense?
// out can be NULL
// will search parent
// TODO?: this searches the toplevel scope, which should only really
//        be resolved at the end of parsing, so it doesn't really matter
static bool hir_var_resolve_name(hir_scope_t *scope, istr_t name, hir_rvar_t *out) {
	bool is_toplevel = false;
	if (scope == NULL) {
		is_toplevel = true;
		scope = &p.modp->toplevel;
	}

	for (u32 i = 0; i < arrlen(scope->locals); i++) {
		hir_rvar_t id = scope->locals[i];
		hir_var_t *var = VAR_PTR(id);

		if (var->name == name && !var->is_in_decl) {
			if (out) {
				*out = id;
			}
			return true;
		}
	}

	// what an opportunity for tail recursion!
	if (!is_toplevel) {
		return hir_var_resolve_name(scope->parent, name, out);
	} else {
		return false;
	}
}

static hir_scope_t *hir_new_scope_ptr(hir_scope_t *parent) {
	hir_scope_t *ptr = malloc(sizeof(hir_scope_t));
	*ptr = (hir_scope_t){
		.parent = parent,
	};
	return ptr;
}

static hir_scope_t hir_new_scope(hir_scope_t *parent) {
	hir_scope_t scope = {
		.parent = parent,
	};
	return scope;
}

static bool is_id_begin(u8 ch) {
    return isalpha(ch) || ch == '_';
}

static bool is_id(u8 ch) {
    return isalpha(ch) || ch == '_' || isdigit(ch);
}

static token_t plex(void) {
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

// i love prededence climbing
type_t ptype(void);

type_t ptype_unit(void) {
	type_t type;
	
	switch (p.token.kind) {
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

	return type;
}

// will always parse into a type
type_t ptype(void) {
	type_t type;
	
	// TODO: support parsing of function types in parentheses

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
			type = ptype_unit();
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

			type_t elem = ptype_unit();

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
		default: {
			type = ptype_unit();
			break;
		}
	}

	// parse curried form
	// essentially a precedence climber, with only one operation which is ->
	// i32 -> i32 -> i32
	// i32 -> (i32 -> i32)
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

enum : u8 {
	PREC_UNKNOWN, // default
	PREC_ASSIGN,  // = += -= *= /= %=
	PREC_CMP,     // && || (TODO: needs parens)
	PREC_EQ,      // == != < > <= >=
	//PREC_BOR,     // |
	PREC_XOR,     // ^
	//PREC_BAND,    // &
	PREC_ADD,     // + -
	PREC_MUL,     // * / %
	PREC_CALL,    // a b       (determined elsewhere)
	PREC_CAST,    // :
	PREC_PREFIX,  // - * ! &
	PREC_POSTFIX, // ++ --
	PREC_DOT,     // .x
	PREC_INDEX,   // x[]
};

// mut x.thing    -> (mut x).thing
// mut x[y].thing -> (mut x[y]).thing

u8 ptok_prec(tok_t kind) {
	// PREC_PREFIX is determined elsewhere

	switch (kind) {
		case TOK_OSQ:
			return PREC_INDEX;
		case TOK_DOT:
			return PREC_DOT;
		case TOK_INC:
		case TOK_DEC:
			return PREC_POSTFIX;
		case TOK_COLON:
			return PREC_CAST;
		case TOK_MUL:
		case TOK_DIV:
		case TOK_MOD:
			return PREC_MUL;
		case TOK_ADD:
		case TOK_SUB:
			return PREC_ADD;
		/* case TOK_BAND:
			return PREC_BAND; */
		/* case TOK_XOR:
			return PREC_XOR; */
		/* case TOK_BOR:
			return PREC_BOR; */
		case TOK_EQ:
		case TOK_NEQ:
		case TOK_LT:
		case TOK_GT:
		case TOK_GE:
		case TOK_LE:
			return PREC_EQ;
		case TOK_AND:
		case TOK_OR:
			return PREC_CMP;
		case TOK_ASSIGN:
		case TOK_ASSIGN_ADD:
		case TOK_ASSIGN_SUB:
		case TOK_ASSIGN_MUL:
		case TOK_ASSIGN_DIV:
		case TOK_ASSIGN_MOD:
			return PREC_ASSIGN;
		default:
			return PREC_UNKNOWN;
	}
}

// solve syntax ambiguities by looking at whitespace
// 1. f [1, 2, 3] -> parsed as a function call, not index
// 2. f -20       -> parsed as a function call with a negative integer literal, not a subtraction
u8 ptok_prec_ambiguities(void) {
	if (!pprev_next_to()) {
		switch (p.token.kind) {
			case TOK_OSQ: {
				// f [1, 2, 3]
				return 0;
			}
			default: {
				break;
			}
		}
	}

	if (ppeek_next_to()) {
		switch (p.token.kind) {
			case TOK_SUB: {
				// f -20
				return 0;
			}
			default: {
				break;
			}
		}
	}
	
	return ptok_prec(p.token.kind);
}

bool pstmt(hir_node_t *expr, hir_scope_t *s, hir_node_t *previous_exprs);
hir_node_t pexpr(hir_scope_t *s, u8 prec, u8 cfg, hir_node_t *previous_exprs);

// MUST HAVE BLK ENTRY AS TOP BLK IN FIRST CALL
hir_node_t *pindented_do_block(hir_scope_t *s, loc_t oloc) {
	u32 bcol = p.token.loc.col;
	hir_node_t *exprs = NULL;

	// an indented block has columns greater than the column of the original location
	/* if (oloc.col != bcol) {
		punexpected("expected indented block");
	} */

	pscope_enter();
	while (p.token.kind != TOK_EOF) {
		u32 cln = p.token.loc.line_nr;

		hir_node_t expr;
		bool set = pstmt(&expr, s, exprs);
		if (set) {
			arrpush(exprs, expr);
		}

		if (cln != p.token.loc.line_nr && p.token.loc.col < bcol) {
			break;
		}
	}
	pscope_leave();

	if (exprs == NULL) {
		return NULL;
	}

	// wrap last expr in break
	// TODO: don't do this unless the last expr is not a break

	// some last expressions of a do block may possibly be assignments,
	// which is okay, they're still expressions. our repr code chokes up though
	// there isn't really a point to be breaking on those expressions.
	//
	// if the last expression is (), its most likely one of those expressions.

	// okay to invalidate pointers in the parser

	hir_node_t *last = &arrlast(exprs);
	u32 blk_id = p.blks_len - 1; // top blk
	if (last->type == TYPE_UNIT && last->kind != NODE_TUPLE_UNIT) {
		hir_node_t new_last = (hir_node_t){
			.kind = NODE_BREAK_UNIT, // unary shorthand
			.loc = last->loc,
			.type = TYPE_BOTTOM,
			.d_break.blk_id = blk_id,
		};
		arrpush(exprs, new_last);
	} else if (last->kind == NODE_TUPLE_UNIT) {
		*last = (hir_node_t){
			.kind = NODE_BREAK_UNIT, // unary shorthand
			.loc = last->loc,
			.type = TYPE_BOTTOM,
			.d_break.blk_id = blk_id,
			.d_break.expr = NULL,
		};
	} else if (last->type != TYPE_BOTTOM) {
		hir_node_t node = *last;

		*last = (hir_node_t){
			.kind = NODE_BREAK_INFERRED,
			.loc = node.loc,
			.type = TYPE_BOTTOM, // not infer, breaking is a noreturn op
			.d_break.blk_id = blk_id,
			.d_break.expr = hir_memdup(node),
		};
	}

	return exprs;
}

// if `opt_label != ISTR_NONE` then `onerror` points to label otherwise the `brk` expr
u8 pblk_locate(istr_t opt_label, loc_t onerror) {
	for (u32 i = p.blks_len; i-- > 0;) {
		pblk_t *blk = &p.blks[i];
		if (!blk->always_brk && opt_label == ISTR_NONE) {
			continue;
		}
		if (blk->label == opt_label) {
			return i;
		}
	}

	// makes more sense, need to specialise the error message regardless on state of blks
	if (opt_label != ISTR_NONE) {
		err_with_pos(onerror, "label `%s` not found", sv_from(opt_label));
	} else {
		err_with_pos(onerror, "not inside a loop");
	}
}

// naive indentation rules, but works for now
// do
//     ...
//     ...
hir_node_t pdo(hir_scope_t *s, istr_t opt_label, loc_t opt_loc) {
	loc_t oloc = p.token.loc;
	pnext();
	if (p.token.kind == TOK_EOF || p.token.loc.line_nr == oloc.line_nr) {
		err_with_pos(oloc, "expected newline after `do`");
	}

	u8 blk_id = p.blks_len++;
	hir_node_t block = {
		.kind = NODE_DO_BLOCK,
		.loc = oloc,
		.type = TYPE_INFER,
		.d_do_block = {
			.scope = hir_new_scope_ptr(s),
			.exprs = NULL,
			.blk_id = blk_id,
		},
	};

	p.blks[blk_id] = (pblk_t){
		.label = opt_label,
		.loc = opt_loc,
		.always_brk = false,
	};
	hir_node_t *exprs = pindented_do_block(block.d_do_block.scope, oloc);
	p.blks_len--;

	if (exprs == NULL) {
		return (hir_node_t){
			.kind = NODE_TUPLE_UNIT,
			.loc = oloc,
			.type = TYPE_UNIT,
		};
	}

	block.d_do_block.exprs = exprs;

	return block;
}

// loop
//     ...
//     ...
hir_node_t ploop(hir_scope_t *s, u8 expr_cfg, hir_node_t *previous_exprs, istr_t opt_label, loc_t opt_loc) {
	loc_t oloc = p.token.loc;
	pnext();

	u8 blk_id = p.blks_len++;
	hir_node_t loop = {
		.kind = NODE_LOOP,
		.loc = oloc,
		.type = TYPE_INFER,
		.d_loop = {
			.blk_id = blk_id,
		},
	};

	p.blks[blk_id] = (pblk_t){
		.label = opt_label,
		.loc = opt_loc,
		.always_brk = true,
	};
	loop.d_loop.expr = hir_memdup(pexpr(s, 0, expr_cfg, previous_exprs));
	p.blks_len--;

	return loop;
}

// will create vars it cannot find
hir_pattern_t ppattern(hir_scope_t *s, hir_node_t *previous_exprs, bool allow_inlay) {
	bool is_mut = false;
	switch (p.token.kind) {
		case TOK_UNDERSCORE: {
			pnext();
			return (hir_pattern_t){
				.kind = PATTERN_UNDERSCORE,
			};
		}
		case TOK_TACK: {
			is_mut = true;
			pnext();
		}
		case TOK_IDENT: {
			istr_t name = p.token.lit;
			loc_t name_loc = p.token.loc;
			pnext();
			hir_rvar_t var = hir_new_var(s, name, name_loc, previous_exprs);
			VAR_PTR(var)->is_mut = is_mut;

			if (allow_inlay && p.token.kind == TOK_COLON) {
				pnext();
				// parse type inlay

				type_t type = ptype();
				VAR_PTR(var)->type = type;
			}

			return (hir_pattern_t){
				.loc = name_loc,
				.kind = PATTERN_VAR,
				.d_var = var,
			};
		}
		case TOK_INTEGER: {
			istr_t val = p.token.lit;
			loc_t loc = p.token.loc;
			pnext();
			return (hir_pattern_t){
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
				return (hir_pattern_t){
					.kind = PATTERN_TUPLE_UNIT,
					.loc = oloc,
				};
			}
			bool first = true;
			hir_pattern_t *elems = NULL;
			hir_pattern_t pattern;
			while (p.token.kind != TOK_CPAR) {
				if (first) {
					pattern = ppattern(s, previous_exprs, allow_inlay);
				} else {
					if (elems == NULL) {
						arrpush(elems, pattern);
					}
					pattern = ppattern(s, previous_exprs, allow_inlay);
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
				pattern = (hir_pattern_t){
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
			hir_pattern_t *elems = NULL;

			hir_pattern_t pattern = {
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
					pattern.d_array.match = pattern_memdup(ppattern(s, previous_exprs, allow_inlay));
					pattern.d_array.match_lhs = false;
					if (p.token.kind != TOK_CSQ) {
						// TODO: dbg str for patterns instead of printing `xs...`
						err_with_pos(oloc, "a `...xs` in array pattern must reside at the end");
					}
				} else {
					hir_pattern_t elem = ppattern(s, previous_exprs, allow_inlay);

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
						pattern.d_array.match = pattern_memdup(elem);
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

hir_node_t plet(hir_scope_t *s, u8 cfg, hir_node_t *previous_exprs) {
	loc_t oloc = p.token.loc;

	pnext();
	// let x = 20
	//     ^

	// because of how ppattern() creates variables, we can't delay resolving
	// until after, so without this hack the below works:
	//
	// let x = x
	//
	// avoid null refs
	hir_scope_t *pl_scope = s == NULL ? &p.modp->toplevel : s;

	u32 olen = arrlenu(pl_scope->locals);
	hir_pattern_t pattern = ppattern(s, previous_exprs, true);
	u32 rlen = arrlenu(pl_scope->locals);

	// mask variables
	for (u32 i = olen; i < rlen; i++) {
		VAR_PTR(pl_scope->locals[i])->is_in_decl = true;
	}

	pexpect(TOK_ASSIGN);
	// let x = 20
	//         ^^
	hir_node_t rhs = pexpr(s, 0, cfg, previous_exprs);

	// unmask variables
	for (u32 i = olen; i < rlen; i++) {
		VAR_PTR(pl_scope->locals[i])->is_in_decl = false;
	}

	return (hir_node_t){
		.kind = NODE_LET_DECL,
		.loc = oloc,
		.type = TYPE_UNIT,
		.d_let_decl.pattern = pattern,
		.d_let_decl.expr = hir_memdup(rhs),
	};
}

hir_node_t pident(hir_scope_t *s) {
	pcheck(TOK_IDENT);

	int id;
	if ((id = pimport_ident(p.token.lit)) != -1) {
		loc_t oloc = p.token.loc;
		pnext();
		if (p.token.kind != TOK_DOT) {
			print_err_with_pos(p.token.loc, "expected `.` after import name `%s`", sv_from(p.is[id].name));
			print_hint_with_pos(oloc, "import name `%s` used here", sv_from(p.is[id].name));
			err_unwind();
		}
		pnext();
		pcheck(TOK_IDENT);
		istr_t lit = p.token.lit;
		pnext();

		return (hir_node_t){
			.kind = NODE_SYM_UNRESOLVED,
			.type = TYPE_INFER,
			.loc = oloc,
			.d_sym_unresolved = {
				.mod = p.is[id].mod,
				.name = lit,
			},
		};
	}

	hir_node_t node;

	hir_rvar_t var;
	if (hir_var_resolve_name(s, p.token.lit, &var)) {
		node = (hir_node_t){
			.kind = NODE_VAR,
			.type = TYPE_INFER,
			.loc = p.token.loc,
			.d_var = var,
		};
	} else {
		node = (hir_node_t){
			.kind = NODE_GLOBAL_UNRESOLVED,
			.type = TYPE_INFER,
			.loc = p.token.loc,
			.d_global_unresolved = p.token.lit,
		};
	}
	pnext();

	return node;
}

// ----: would probably want to make this a bitfield when more expr cfg are added
//       or not? expressions are nested, you can't get in this superposition of end tokens
//       weird... don't make this change
enum : u8 {
	PEXPR_ET_NONE,
	PEXPR_ET_PAREN,
	PEXPR_ET_ARRAY,
	PEXPR_ET_INDEX_LO,
	PEXPR_ET_INDEX_HI,
	PEXPR_ET_ELSE,
};

// (                           a b                          )
// ^>    cfg = PEXPR_ET_PAREN  ^^^> cfg = PEXPR_ET_PAREN   >^   break 
hir_node_t pexpr(hir_scope_t *s, u8 prec, u8 cfg, hir_node_t *previous_exprs) {
	token_t token = p.token;
	u32 line_nr = token.loc.line_nr;
	hir_node_t node;
	bool is_single = true;
	bool should_continue = true;

	struct {
		istr_t name;
		loc_t name_loc;
	} label;

	label.name = ISTR_NONE;

	while (should_continue) {
		hir_node_t onode = node;
		token_t token = p.token;

		switch (token.kind) {
			case TOK_VOID: {
				pnext();
				// void expr
				//      ^^^^
				hir_node_t expr = pexpr(s, 0, cfg, previous_exprs);
				node = (hir_node_t){
					.kind = NODE_VOIDING,
					.loc = token.loc,
					.type = TYPE_UNIT,
					.d_voiding = hir_memdup(expr),
				};
				break;
			}
			case TOK_UNDEFINED: {
				pnext();
				node = (hir_node_t){
					.kind = NODE_UNDEFINED,
					.type = TYPE_UNDEFINED,
					.loc = token.loc,
				};
				break;
			}
			case TOK_TRUE:
			case TOK_FALSE: {
				pnext();
				node = (hir_node_t){
					.kind = NODE_BOOL_LIT,
					.type = TYPE_BOOL,
					.loc = token.loc,
					.d_bool_lit = token.kind == TOK_TRUE,
				};
				break;
			}
			// TODO: labels
			case TOK_IF: {
				// if (...) ... else ...
				loc_t oloc = token.loc;
				pnext();
				pexpect(TOK_OPAR);
				hir_node_t *cond = hir_memdup(pexpr(s, 0, PEXPR_ET_PAREN, previous_exprs));
				pexpect(TOK_CPAR);

				// if (...) ... else ...
				//          ^^^

				hir_node_t *then = hir_memdup(pexpr(s, 0, PEXPR_ET_ELSE, previous_exprs));

				// if (...) ... else ...
				//              ^^^^
				//            optional

				hir_node_t *els = NULL;
				if (p.token.kind == TOK_ELSE) {
					pnext();
					els = hir_memdup(pexpr(s, 0, 0, previous_exprs));
				} else {
					// desugar to () return using voiding

					// if (t) effect
					//
					// if (t) void effect
					// else   ()

					els = hir_memdup((hir_node_t){
						.kind = NODE_TUPLE_UNIT,
						.loc = oloc,
						.type = TYPE_UNIT,
					});
					then = hir_memdup((hir_node_t){
						.kind = NODE_VOIDING,
						.loc = then->loc,
						.type = TYPE_UNIT,
						.d_voiding = then,
					});
				}

				node = (hir_node_t){
					.kind = NODE_IF,
					.type = TYPE_INFER,
					.loc = token.loc,
					.d_if = {
						.cond = cond,
						.then = then,
						.els = els,
					},
				};
				break;
			}
			// possible syntax ambiguity with `|` operator
			/* case TOK_PIPE: {
				// piecewise functions
				
				// | expr = ...
				// | expr = ...
				// else   = ...
			} */
			case TOK_COLON: {
				// label
				pnext();
				pcheck(TOK_IDENT);
				label.name = p.token.lit;
				label.name_loc = p.token.loc;
				pnext();
				switch (p.token.kind) {
					case TOK_DO:
						break;
					default: {
						err_with_pos(p.token.loc, "expected `do` after label");
					}
				}
				continue;
			}
			case TOK_LOOP: {
				node = ploop(s, cfg, previous_exprs, label.name, label.name_loc);
				label.name = ISTR_NONE;
				break;
			}
			case TOK_DO: {
				node = pdo(s, label.name, label.name_loc); // TODO?: no chaining
				label.name = ISTR_NONE;
				should_continue = false;
				break;
			}
			case TOK_LET: {
				node = plet(s, cfg, previous_exprs);
				should_continue = false; // single expr
				break;
			}
			case TOK_BREAK: {
				istr_t label = ISTR_NONE;
				loc_t onerror = p.token.loc;
				pnext();
				// parse label
				if (p.token.kind == TOK_COLON) {
					pnext();
					pcheck(TOK_IDENT);
					label = p.token.lit;
					onerror = p.token.loc;
					pnext();
				}
				u8 blk_id = pblk_locate(label, onerror);
				hir_node_t expr = pexpr(s, 0, cfg, previous_exprs);
				node = (hir_node_t){
					.kind = NODE_BREAK,
					.loc = token.loc,
					.type = TYPE_BOTTOM,
					.d_break = {
						.blk_id = blk_id,
						.expr = hir_memdup(expr),
					},
				};
				should_continue = false; // single expr
				break;
			}
			case TOK_IDENT: {
				node = pident(s);
				break;
			}
			case TOK_OPAR: {
				loc_t oloc = p.token.loc;
				pnext();
				if (p.token.kind == TOK_CPAR) {
					node = (hir_node_t){
						.kind = NODE_TUPLE_UNIT,
						.loc = oloc,
						.type = TYPE_UNIT,
					};
					pnext();
					break;
				}
				bool first = true;
				hir_node_t *elems = NULL;
				while (p.token.kind != TOK_CPAR) {
					if (first) {
						node = pexpr(s, 0, PEXPR_ET_PAREN, previous_exprs);
					} else {
						if (elems == NULL) {
							arrpush(elems, node);
						}
						node = pexpr(s, 0, PEXPR_ET_PAREN, previous_exprs);
						arrpush(elems, node);					
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
					node = (hir_node_t){
						.kind = NODE_TUPLE,
						.loc = oloc,
						.type = TYPE_INFER,
						.d_tuple = {
							.elems = elems,
						},
					};
				}
				break;
			}
			case TOK_OSQ: {
				loc_t oloc = p.token.loc;
				hir_node_t *nodes = NULL;

				// [ 1, 2, 3, 4, 5 ]
				// ^

				pnext();
				while (p.token.kind != TOK_CSQ) {
					hir_node_t node = pexpr(s, 0, PEXPR_ET_ARRAY, previous_exprs);
					arrpush(nodes, node);				

					if (p.token.kind == TOK_COMMA) {
						pnext();
					} else if (p.token.kind != TOK_CSQ) {
						punexpected("expected `,` or `]`");
					}
				}

				// [ 1, 2, 3, 4, 5 ]
				//                 ^
				pnext();

				node = (hir_node_t){
					.kind = NODE_ARRAY_LIT,
					.loc = oloc,
					.type = TYPE_INFER,
					.d_array_lit = {
						.elems = nodes,
					},
				};
				break;
			}
			case TOK_INTEGER: {
				node = (hir_node_t){
					.kind = NODE_INTEGER_LIT,
					.loc = p.token.loc,
					.type = TYPE_INFER,
					.d_integer_lit = {
						.lit = p.token.lit,
					},
				};
				pnext();
				break;
			}
			default: {
				if (TOK_IS_PREFIX(token.kind)) {
					pnext();
					if (token.kind == TOK_SUB && p.token.kind == TOK_INTEGER) {
						node = (hir_node_t){
							.kind = NODE_INTEGER_LIT,
							.loc = token.loc,
							.type = TYPE_INFER,
							.d_integer_lit = {
								.lit = p.token.lit,
								.negate = true,
							},
						};
						pnext();
					} else {
						if (token.kind == TOK_SINGLE_AND) {
							bool is_mut = false;
							if (p.token.kind == TOK_TACK) {
								// &'v = mut ref
								is_mut = true;
								pnext();
							}
							hir_node_t rhs = pexpr(s, PREC_PREFIX, cfg, previous_exprs);
							node = (hir_node_t){
								.kind = NODE_ADDR_OF,
								.loc = token.loc,
								.type = TYPE_INFER,
								.d_addr_of.ref = hir_memdup(rhs),
								.d_addr_of.is_mut = is_mut,
							};
						} else {
							hir_node_t rhs = pexpr(s, PREC_PREFIX, cfg, previous_exprs);
							node = (hir_node_t){
								.kind = NODE_PREFIX,
								.loc = token.loc,
								.type = TYPE_INFER,
								.d_prefix.expr = hir_memdup(rhs),
								.d_prefix.kind = token.kind,
							};
						}
					}
					break;
				} else {
					punexpected("expected expression");
				}
			}
		}

		switch (cfg) {
			case PEXPR_ET_NONE: break;
			case PEXPR_ET_PAREN: {
				if (p.token.kind == TOK_CPAR || p.token.kind == TOK_COMMA) {
					should_continue = false;
				}
				break;
			}
			case PEXPR_ET_INDEX_LO: {
				if (p.token.kind == TOK_CSQ || p.token.kind == TOK_DOUBLE_DOTS) {
					should_continue = false;
				}
				break;
			}
			case PEXPR_ET_INDEX_HI: {
				if (p.token.kind == TOK_CSQ) {
					should_continue = false;
				}
				break;
			}
			case PEXPR_ET_ARRAY: {
				if (p.token.kind == TOK_CSQ || p.token.kind == TOK_COMMA) {
					should_continue = false;
				}
				break;
			}
			case PEXPR_ET_ELSE: {
				if (p.token.kind == TOK_ELSE) {
					should_continue = false;
				}
				break;
			}
			default: {
				assert_not_reached();
			}
		}

		if (is_single) {
			is_single = false;
		} else {
			node = (hir_node_t){
				.kind = NODE_CALL,
				.loc = onode.loc,
				.type = TYPE_INFER,
				.d_call.f = hir_memdup(onode),
				.d_call.arg = hir_memdup(node),
			};
		}

		// &a b -> &a(b) NOT &(a b)
		if (p.token.kind == TOK_EOF || prec >= PREC_CALL) {
			return node;
		}

		if (ptok_prec_ambiguities() == 0 && p.token.loc.line_nr == line_nr) {
			continue;
		}
		break;
	}

	if (!should_continue) {
		return node;
	}

	while (true) {
		token_t token = p.token;

		if (token.kind == TOK_EOF) {
			return node;
		}

		if (prec < ptok_prec_ambiguities()) {
			switch (token.kind) {
				case TOK_INC:
				case TOK_DEC: {
					pnext();
					node = (hir_node_t){
						.kind = NODE_POSTFIX,
						.loc = token.loc,
						.type = TYPE_INFER,
						.d_postfix.expr = hir_memdup(node),
						.d_postfix.kind = token.kind,
					};
					continue;
				}
				case TOK_OSQ: {
					// x[0..1] and x[0]
					loc_t oloc = p.token.loc;
					pnext();

					hir_node_t *expr0;
					if (p.token.kind == TOK_DOUBLE_DOTS) {
						// x[..1]
						expr0 = NULL;
						pnext();
					} else {
						// x[0]
						expr0 = hir_memdup(pexpr(s, 0, PEXPR_ET_INDEX_LO, previous_exprs));
						if (p.token.kind == TOK_CSQ) {
							pnext();
							node = (hir_node_t){
								.kind = NODE_INDEX,
								.loc = oloc,
								.type = TYPE_INFER,
								.d_index = {
									.expr = hir_memdup(node),
									.index = expr0,
								},
							};
							continue;
						}
					}
					// x[0..]
					//      ^
					// x[0..1]
					//      ^
					if (p.token.kind == TOK_CSQ) {
						pnext();
						node = (hir_node_t){
							.kind = NODE_SLICE,
							.loc = oloc,
							.type = TYPE_INFER,
							.d_slice = {
								.expr = hir_memdup(node),
								.lo = expr0,
								.hi = NULL,
							},
						};
					} else {
						hir_node_t expr1 = pexpr(s, 0, PEXPR_ET_INDEX_HI, previous_exprs);
						pexpect(TOK_CSQ);

						node = (hir_node_t){
							.kind = NODE_SLICE,
							.loc = oloc,
							.type = TYPE_INFER,
							.d_slice = {
								.expr = hir_memdup(node),
								.lo = expr0,
								.hi = hir_memdup(expr1),
							},
						};
					}
					continue;
				}
				default: {
					if (TOK_IS_INFIX(token.kind)) {
						pnext();

						// cast
						if (token.kind == TOK_COLON) {
							type_t type = ptype();
							node = (hir_node_t){
								.kind = NODE_CAST,
								.loc = token.loc,
								.type = type,
								.d_cast = hir_memdup(node),
							};
							continue;
						}

						// postfix deref: v.*
						if (token.kind == TOK_DOT && p.token.kind == TOK_MUL) {
							loc_t oloc = token.loc;
							pnext();
							node = (hir_node_t){
								.kind = NODE_DEREF,
								.loc = oloc,
								.type = TYPE_INFER,
								.d_deref = hir_memdup(node),
							};
							continue;
						}

						tok_t kind = token.kind;

						hir_node_t rhs = pexpr(s, ptok_prec(kind), cfg, previous_exprs);

						bool is_assign_op = kind == TOK_ASSIGN_ADD || kind == TOK_ASSIGN_SUB || kind == TOK_ASSIGN_MUL || kind == TOK_ASSIGN_DIV || kind == TOK_ASSIGN_MOD;

						// sparse array
						static const tok_t assign_op_to_op[] = {
							[TOK_ASSIGN_ADD] = TOK_ADD,
							[TOK_ASSIGN_SUB] = TOK_SUB,
							[TOK_ASSIGN_MUL] = TOK_MUL,
							[TOK_ASSIGN_DIV] = TOK_DIV,
							[TOK_ASSIGN_MOD] = TOK_MOD,
						};

						hir_node_t *p_node = hir_memdup(node);
						hir_node_t *p_rhs = hir_memdup(rhs);

						if (kind == TOK_ASSIGN) {
							node = (hir_node_t){
								.kind = NODE_ASSIGN,
								.loc = token.loc,
								.type = TYPE_INFER,
								.d_assign.lhs = p_node,
								.d_assign.rhs = p_rhs,
							};
						} else if (is_assign_op) {
							// desugar
							//
							// a += b -> a = a + b
							//
							tok_t desugared_infix = assign_op_to_op[kind];

							// seems wildly unsafe to not do a deep copy of `p_node` here
							// but most likely will be okay, since the lhs is used in a very similar context

							hir_node_t op_node = {
								.kind = NODE_INFIX,
								.loc = token.loc,
								.type = TYPE_INFER,
								.d_infix.lhs = p_node,
								.d_infix.rhs = p_rhs,
								.d_infix.kind = desugared_infix,
							};
							node = (hir_node_t){
								.kind = NODE_ASSIGN,
								.loc = token.loc,
								.type = TYPE_INFER,
								.d_assign.lhs = p_node,
								.d_assign.rhs = hir_memdup(op_node),
							};
						} else if (token.kind == TOK_AND) {
							// a && b -> if a then b else false

							hir_node_t false_node = (hir_node_t){
								.kind = NODE_BOOL_LIT,
								.loc = token.loc,
								.type = TYPE_BOOL,
								.d_bool_lit = false,
							};

							node = (hir_node_t){
								.kind = NODE_IF,
								.loc = token.loc,
								.type = TYPE_BOOL, // force bool
								.d_if.cond = p_node,
								.d_if.then = p_rhs,
								.d_if.els = hir_memdup(false_node),
							};
						} else if (token.kind == TOK_OR) {
							// a || b -> if a then true else b

							hir_node_t true_node = (hir_node_t){
								.kind = NODE_BOOL_LIT,
								.loc = token.loc,
								.type = TYPE_BOOL,
								.d_bool_lit = true,
							};

							node = (hir_node_t){
								.kind = NODE_IF,
								.loc = token.loc,
								.type = TYPE_BOOL,
								.d_if.cond = p_node,
								.d_if.then = hir_memdup(true_node),
								.d_if.els = p_rhs,
							};
						} else {
							node = (hir_node_t){
								.kind = NODE_INFIX,
								.loc = token.loc,
								.type = TYPE_INFER,
								.d_infix.lhs = p_node,
								.d_infix.rhs = p_rhs,
								.d_infix.kind = token.kind,
							};
						}

						// NODE_ASSIGN

						// random infix
						
						continue;
					}
				}
			}
		}
		break;
	}

	return node;
}

// desugar `a: x y = x + y` into
//
// ```
// [s0]:let a = [s0<s1]:\_0 _1 -> match (_0, _1)
//     [s0<s1<s2]:(x, y) -> x + y
// ```
//
// function declarations:
// - <name>:    <pattern> = ...
//
// pproc_create() when function doesn't exist
// pproc() when assign a new match overload
//
// the desugared match asserts that the number of arguments are all the same
// i.e x == arrlenu(lambda.args)
void pproc(hir_node_t *out_expr, hir_scope_t *s0, hir_node_t *previous_exprs) {
	pcheck(TOK_IDENT);
	istr_t name = p.token.lit;
	loc_t name_loc = p.token.loc;

	// create new var before evaluation so it can be referenced recursively
	hir_rvar_t proc_var = hir_new_var(s0, name, name_loc, previous_exprs);
	VAR_PTR(proc_var)->is_proc_decl = true; // is a desugared proc decl

	pnext();
	pexpect(TOK_COLON);

	// add: ...
	//      ^^^

	type_t proc_type = ptype();
	VAR_PTR(proc_var)->type = proc_type;

	// add: ...
	// add: ...
	// ^^^

	u32 pattern_len = 0; // zero is invalid, all functions have arity >= 1

	hir_pattern_t *match_patterns = NULL;
	hir_node_t *match_exprs = NULL;
	hir_scope_t *match_scopes = NULL;
	hir_scope_t *s1 = hir_new_scope_ptr(s0);

	if (!(p.token.kind == TOK_IDENT && p.token.lit == name)) {
		err_with_pos(name_loc, "expected function `%s` implementation after type", sv_from(name));
	}

	while (p.token.kind == TOK_IDENT && p.token.lit == name) {
		pnext();
		pexpect(TOK_COLON);

		// add: ...
		//      ^^^ (patterns)

		hir_pattern_t *patterns = NULL;

		// scope of the match arm
		hir_scope_t s2 = hir_new_scope(s1);

		while (true) {
			hir_pattern_t pattern = ppattern(&s2, NULL, false);
			arrpush(patterns, pattern);
			if (p.token.kind == TOK_ASSIGN) {
				break;
			}
		}

		u32 plen = arrlenu(patterns);
		if (pattern_len == 0) {
			pattern_len = plen;
		} else if (plen != pattern_len) {
			err_with_pos(name_loc, "function pattern matching with different number of arguments", sv_from(name));
		}

		hir_pattern_t pattern = {
			.kind = PATTERN_TUPLE,
			.loc = patterns[0].loc,
			.d_tuple = {
				.elems = patterns,
			},
		};

		// add: ... =
		//          ^
		pnext();

		pscope_enter();
		hir_node_t expr = pexpr(&s2, 0, 0, NULL);
		pscope_leave();

		arrpush(match_exprs, expr);
		arrpush(match_scopes, s2);
		arrpush(match_patterns, pattern);
	}

	// captured the types and all patterns, create function here

	hir_node_t tuple_expr = {
		.kind = NODE_TUPLE,
		.type = TYPE_INFER,
		.loc = name_loc,
		.d_tuple.elems = NULL,
	};

	// add: x y = x + y
	//
	// let add = \_0 _1 -> match (_0, _1)
	//     (x, y) -> x + y

	// create variable list
	hir_rvar_t *vars = NULL;
	for (u32 i = 0; i < pattern_len; i++) {
		// convert `i` to string
		char *varname;
		asprintf(&varname, "_%u", i);
		hir_rvar_t var = hir_new_var(s1, sv_move(varname), (loc_t){}, NULL);
		VAR_PTR(var)->is_arg = true; // is an argument
		
		arrpush(vars, var);

		hir_node_t get_var = {
			.kind = NODE_VAR,
			.type = TYPE_INFER,
			.loc = name_loc,
			.d_var = var,
		};

		arrpush(tuple_expr.d_tuple.elems, get_var);
	}

	hir_node_t match = {
		.kind = NODE_MATCH,
		.loc = name_loc,
		.type = TYPE_INFER,
		.d_match = {
			.expr = hir_memdup(tuple_expr),
			.patterns = match_patterns,
			.exprs = match_exprs,
			.scopes = match_scopes,
		},
	};

	hir_node_t *lamdba = hir_memdup((hir_node_t){
		.kind = NODE_LAMBDA,
		.loc = name_loc,
		.type = TYPE_INFER,
		.d_lambda = {
			.args = vars,
			.scope = s1,
			.expr = hir_memdup(match),
		}
	});

	*out_expr = (hir_node_t){
		.kind = NODE_LET_DECL,
		.loc = name_loc,
		.type = TYPE_UNIT,
		.d_let_decl.pattern = {
			.kind = PATTERN_VAR,
			.loc = name_loc,
			.d_var = proc_var,
		},
		.d_let_decl.expr = lamdba,
	};
}

// `previous_exprs` is used when multiple function declarations are in the same scope
// can be null
// IT IS VERY IMPORTANT THAT `previous_exprs` HAS SCOPE `s`
// remember: the `hir_node_t *expr` WILL ALWAYS BE INVALIDATED. ALWAYS.
bool pstmt(hir_node_t *expr, hir_scope_t *s, hir_node_t *previous_exprs) {
	bool set = false;

	switch (p.token.kind) {
		case TOK_IDENT: {
			if (p.peek.kind == TOK_COLON) {
				pproc(expr, s, previous_exprs);
				set = true;
				break;
			}
			// fall through
		}
		default: {
			*expr = pexpr(s, 0, 0, previous_exprs);
			set = true;
			break;
		}
	}

	return set;
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

void pmake_pub(hir_node_t *node) {
	hir_var_t *varp;
	
	/* switch (node->kind) {
		case NODE_PROC_DECL: {
			varp = VAR_PTR(node->d_proc_decl.var);
			break;
		}
		case NODE_VAR_DECL: {
			varp = VAR_PTR(node->d_var);
			break;
		}
		default: { */
			err_with_pos(node->loc, "cannot make this expression public");
		/* }
	} */

	assert(!varp->is_pub);
	varp->is_pub = true;
}

void ptop_stmt(void) {
	if (p.token.kind != TOK_IMPORT) {
		p.has_done_imports = true;
	}

	loc_t oloc = p.token.loc;
	bool is_pub = p.token.kind == TOK_PUB;

	switch (p.token.kind) {
		case TOK_IMPORT: {
			pimport();
			break;
		}
		case TOK_PUB: {
			is_pub = true;
			pnext();
			// fall through
		}
		default: {
			hir_node_t expr;
			bool set = pstmt(&expr, NULL, p.modp->exprs);
			if (set) {
				arrpush(p.modp->exprs, expr);
			}
			if (is_pub && set) {
				pmake_pub(&arrlast(p.modp->exprs));
			} else if (is_pub) {
				err_with_pos(oloc, "cannot make this expression public");
			}
			break;
		}
	}
}

void pentry(rfile_t file) {
	file_t *f = FILE_PTR(file);
	
	p = (pctx_t){
		.pstart = f->data,
		.pc = f->data,
		.pend = f->data + f->len,
		.plast_nl = f->data,
		.file = file,
		.mod = f->mod,
		.modp = MOD_PTR(f->mod),
	};

	pnext(); // tok
	pnext(); // tok peek

	pscope_enter();
	while (p.token.kind != TOK_EOF) {
		ptop_stmt();
	}
	pscope_leave();

	papply_pub();
}

void _hir_dump_var_w_type(hir_rvar_t var) {
	hir_var_t *varp = VAR_PTR(var);
	
	const char *mut_str = varp->is_mut ? "'" : "";
	printf("%s%s.%u", mut_str, sv_from(VAR_PTR(var)->name), var);

	// remove noise on debug output before checker
	if (varp->type != TYPE_INFER) {
		printf(": %s", type_dbg_str(varp->type));
	}
}

void _hir_dump_var(hir_rvar_t var) {
	printf("%s.%u", sv_from(VAR_PTR(var)->name), var);
}

void _hir_dump_pattern(mod_t *modp,hir_scope_t *s, hir_pattern_t pattern) {
	switch (pattern.kind) {
		case PATTERN_UNDERSCORE: {
			printf("_");
			break;
		}
		case PATTERN_INTEGER_LIT: {
			printf("%s", sv_from(pattern.d_integer_lit));
			break;
		}
		case PATTERN_VAR: {
			_hir_dump_var_w_type(pattern.d_var);
			break;
		}
		case PATTERN_TUPLE: {
			printf("(");
			for (u32 i = 0; i < arrlen(pattern.d_tuple.elems); i++) {
				_hir_dump_pattern(modp, s, pattern.d_tuple.elems[i]);
				if (i != arrlen(pattern.d_tuple.elems) - 1) {
					printf(", ");
				}
			}
			printf(")");
			break;
		}
		case PATTERN_TUPLE_UNIT: {
			printf("()");
			break;
		}
		case PATTERN_ARRAY: {
			printf("[");
			if (pattern.d_array.match && pattern.d_array.match_lhs) {
				_hir_dump_pattern(modp, s, *pattern.d_array.match);
				printf("..., ");
			}
			for (u32 i = 0; i < arrlen(pattern.d_array.elems); i++) {
				_hir_dump_pattern(modp, s, pattern.d_array.elems[i]);
				if (i != arrlen(pattern.d_array.elems) - 1) {
					printf(", ");
				}
			}
			if (pattern.d_array.match && !pattern.d_array.match_lhs) {
				printf(", ...");
				_hir_dump_pattern(modp, s, *pattern.d_array.match);
			}
			printf("]");
			break;
		}
		default: {
			assert_not_reached();
		}
	}
}

void _hir_dump_scope(mod_t *modp, hir_scope_t *s) {
	for (int i = 0, c = arrlen(s->locals); i < c; i++) {
		hir_rvar_t var = s->locals[i];
		hir_var_t *varp = VAR_PTR(var);
		printf("  %s :: %s\n", sv_from(varp->name), type_dbg_str(varp->type));
	}
}

static u32 _hir_tabcnt = 0;
void _hir_dump_stmt(mod_t *modp, hir_scope_t *s, hir_node_t node);

void _hir_tabs(void) {
	for (u32 i = 0; i < _hir_tabcnt; i++) {
		printf("  ");
	}
}

#define TAB_PRINTF(...) \
	_hir_tabs(); \
	printf(__VA_ARGS__);

void _hir_dump_expr(mod_t *modp, hir_scope_t *s, hir_node_t node) {
	// printf("[%s]:", type_dbg_str(node.type));

	switch (node.kind) {
		case NODE_VAR: {
			_hir_dump_var(node.d_var);
			break;
		}
		case NODE_GLOBAL_UNRESOLVED: {
			printf("%s*", sv_from(node.d_global_unresolved));
			break;
		}
		case NODE_INTEGER_LIT: {
			if (node.d_integer_lit.negate) {
				printf("-%s", sv_from(node.d_integer_lit.lit));
			} else {
				printf("%s", sv_from(node.d_integer_lit.lit));
			}
			break;
		}
		case NODE_BOOL_LIT: {
			printf("%s", node.d_bool_lit ? "true" : "false");
			break;
		}
		case NODE_POSTFIX: {
			_hir_dump_expr(modp, s, *node.d_postfix.expr);
			printf("%s", node.d_postfix.kind == TOK_INC ? "++" : "--");
			break;
		}
		case NODE_ASSIGN: {
			_hir_dump_expr(modp, s, *node.d_assign.lhs);
			printf(" = ");
			_hir_dump_expr(modp, s, *node.d_assign.rhs);
			break;
		}
		case NODE_INFIX: {
			printf("(%s ", tok_op_str(node.d_infix.kind));
			_hir_dump_expr(modp, s, *node.d_infix.lhs);
			printf(" ");
			_hir_dump_expr(modp, s, *node.d_infix.rhs);
			printf(")");
			break;
		}
		case NODE_PREFIX: {
			printf("%s", tok_op_str(node.d_prefix.kind));
			_hir_dump_expr(modp, s, *node.d_prefix.expr);
			break;
		}
		case NODE_CALL: {
			printf("(");
			_hir_dump_expr(modp, s, *node.d_call.f);
			printf(" ");
			_hir_dump_expr(modp, s, *node.d_call.arg);
			printf(")");
			break;
		}
		case NODE_LOOP: {
			printf(":%u loop ", node.d_loop.blk_id);
			_hir_dump_expr(modp, s, *node.d_loop.expr);
			break;
		}
		case NODE_DO_BLOCK: {
			printf(":%u do\n", node.d_do_block.blk_id);
			_hir_tabcnt++;
			for (int i = 0, c = arrlen(node.d_do_block.exprs); i < c; i++) {
				_hir_dump_stmt(modp, node.d_do_block.scope, node.d_do_block.exprs[i]);
			}
			_hir_tabcnt--;
			break;
		}
		case NODE_TUPLE_UNIT: {
			printf("()");
			break;
		}
		case NODE_TUPLE: {
			printf("(");
			for (int i = 0, c = arrlen(node.d_tuple.elems); i < c; i++) {
				_hir_dump_expr(modp, s, node.d_tuple.elems[i]);
				if (i != c - 1) {
					printf(", ");
				}
			}
			printf(")");
			break;
		}
		case NODE_BREAK_UNIT: {
			printf("brk :%u ()", node.d_break.blk_id);
			break;
		}
		case NODE_BREAK_INFERRED:
		case NODE_BREAK: {
			printf("brk :%u ", node.d_break.blk_id);
			_hir_dump_expr(modp, s, *node.d_break.expr);
			break;
		}
		case NODE_CAST: {
			printf("cast[%s](", type_dbg_str(node.type));
			_hir_dump_expr(modp, s, *node.d_cast);
			printf(")");
			break;
		}
		case NODE_SYM_UNRESOLVED: {
			printf("%s*", fs_module_symbol_str(node.d_sym_unresolved.mod, node.d_sym_unresolved.name));
			break;
		}
		case NODE_SYM: {
			hir_var_t *var = VAR_PTR(node.d_sym.var);
			printf("%s", fs_module_symbol_str(node.d_sym.mod, var->name));
			break;
		}
		case NODE_IF: {
			printf("if (");
			_hir_dump_expr(modp, s, *node.d_if.cond);
			printf(") ");
			_hir_dump_expr(modp, s, *node.d_if.then);
			printf(" else ");
			if (node.d_if.els == NULL) {
				printf("()");
			} else {
				_hir_dump_expr(modp, s, *node.d_if.els);
			}
			break;
		}
		case NODE_UNDEFINED: {
			printf("undefined");
			break;
		}
		case NODE_ARRAY_LIT: {
			printf("[");
			for (int i = 0, c = arrlen(node.d_array_lit.elems); i < c; i++) {
				_hir_dump_expr(modp, s, node.d_array_lit.elems[i]);
				if (i != c - 1) {
					printf(", ");
				}
			}
			printf("]:%s", type_dbg_str(node.type));
			break;
		}
		case NODE_LAMBDA: {
			// \x y z -> x + y + z

			printf("\\");
			for (u32 i = 0, c = arrlenu(node.d_lambda.args); i < c; i++) {
				_hir_dump_var_w_type(node.d_lambda.args[i]);
				if (i != c - 1) {
					printf(" ");
				}
			}

			printf(" -> ");

			_hir_dump_expr(modp, node.d_lambda.scope, *node.d_lambda.expr);
			break;
		}
		case NODE_MATCH: {
			// match expr
			//    (x, y) -> ...

			printf("match ");
			_hir_dump_expr(modp, s, *node.d_match.expr);
			printf("\n");

			_hir_tabcnt++;
			for (u32 i = 0, c = arrlenu(node.d_match.exprs); i < c; i++) {
				_hir_tabs();
				_hir_dump_pattern(modp, &node.d_match.scopes[i], node.d_match.patterns[i]);
				printf(" -> ");
				_hir_dump_expr(modp, &node.d_match.scopes[i], node.d_match.exprs[i]);
				printf("\n");
			}
			_hir_tabcnt--;
			break;
		}
		case NODE_VOIDING: {
			printf("void ");
			_hir_dump_expr(modp, s, *node.d_voiding);
			break;
		}
		case NODE_DEREF: {
			printf("(");
			_hir_dump_expr(modp, s, *node.d_deref);
			printf(").*");
			break;
		}
		case NODE_ADDR_OF: {
			printf("&");
			if (node.d_addr_of.is_mut) {
				printf("'");
			}
			_hir_dump_expr(modp, s, *node.d_addr_of.ref);
			break;
		}
		case NODE_SIZEOF_TYPE: {
			printf("sizeof[%s]", type_dbg_str(node.d_sizeof_type));
			break;
		}
		case NODE_INDEX: {
			_hir_dump_expr(modp, s, *node.d_index.expr);
			printf("[");
			_hir_dump_expr(modp, s, *node.d_index.index);
			printf("]");
			break;
		}
		case NODE_SLICE: {
			_hir_dump_expr(modp, s, *node.d_slice.expr);
			printf("[");
			if (node.d_slice.lo) {
				_hir_dump_expr(modp, s, *node.d_slice.lo);
			}
			printf("..");
			if (node.d_slice.hi) {
				_hir_dump_expr(modp, s, *node.d_slice.hi);
			}
			printf("]");
			break;
		}
		default: {
			printf("\nunknown expr kind %d\n", node.kind);
			print_hint_with_pos(node.loc, "LOC HERE");
			assert_not_reached();
		}
	}
}

void _hir_dump_stmt(mod_t *modp, hir_scope_t *s, hir_node_t node) {
	switch (node.kind) {
		case NODE_LET_DECL: {
			TAB_PRINTF("let ");
			_hir_dump_pattern(modp, s, node.d_let_decl.pattern);
			printf(" = ");
			_hir_dump_expr(modp, s, *node.d_let_decl.expr);
			printf("\n");
			break;
		}
		default: {
			_hir_tabs();
			_hir_dump_expr(modp, s, node);
			printf("\n");
			break;
		}
	}
}

void hir_dump_module(rmod_t mod) {
	void *_ = alloc_scratch(0);

	mod_t *modp = MOD_PTR(mod);
	printf("module %s\n\n", fs_module_symbol_str(mod, ISTR_NONE));
	for (u32 i = 0, c = arrlenu(modp->exprs); i < c; i++) {
		_hir_dump_stmt(modp, NULL, modp->exprs[i]);
	}

	alloc_reset(_);
}
