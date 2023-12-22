#include "all.h"

#include "stb_ds.h"
#include <setjmp.h>

typedef struct pctx_t pctx_t;
typedef struct pimport_t pimport_t;
typedef struct ptypedecl_t ptypedecl_t;

struct pimport_t {
	rmod_t mod;
	istr_t name;
	loc_t loc;
};

struct ptypedecl_t {
	ir_scope_t *scope;
	istr_t name;
	type_t type;
	loc_t loc;
};

struct pctx_t {
	u8 *pstart;
	u8 *pc;
	u8 *pend;
	u8 *plast_nl;
	u32 line_nr;
	token_t token;
	token_t peek;
	pimport_t is[64]; // import stack
	u32 is_len;
	ptypedecl_t tds[128]; // type decl stack
	u32 tds_len;
	rfile_t file;
	rmod_t mod;
	mod_t *modp;
	bool has_done_imports;
};

pctx_t p;

// -1 for not found
int pimport_ident(istr_t name) {
	for (u32 i = 0; i < p.is_len; i++) {
		if (p.is[i].name == name) {
			return i;
		}
	}
	return -1;
}

#define VAR_PTR(id) (&p.modp->vars[id])

static bool ir_name_exists_in(ir_scope_t *scope, istr_t name) {
	if (scope == NULL) {
		scope = &p.modp->toplevel;
	}

	for (u32 i = 0; i < arrlen(scope->locals); i++) {
		ir_rvar_t id = scope->locals[i];
		ir_var_t *var = &p.modp->vars[id];

		if (var->name == name) {
			return true;
		}
	}

	return false;
}

static bool ir_var_exists_in(ir_scope_t *scope, ir_rvar_t var) {
	if (scope == NULL) {
		scope = &p.modp->toplevel;
	}

	for (u32 i = 0; i < arrlen(scope->locals); i++) {
		ir_rvar_t id = scope->locals[i];
		if (id == var) {
			return true;
		}
	}

	return false;
}

static ir_var_t *ir_name_in(ir_scope_t *scope, istr_t name) {
	if (scope == NULL) {
		scope = &p.modp->toplevel;
	}

	for (u32 i = 0; i < arrlen(scope->locals); i++) {
		ir_rvar_t id = scope->locals[i];
		ir_var_t *var = &p.modp->vars[id];

		if (var->name == name) {
			return var;
		}
	}

	return NULL;
}

// NULL meaning toplevel
static ir_rvar_t ir_new_var(ir_scope_t *scope, istr_t name, type_t type, loc_t onerror) {
	int idx;
	if ((idx = pimport_ident(name)) != -1) {
		print_err_with_pos(onerror, "variable `%s` cannot shadow import `%s`", sv_from(name), sv_from(name));
		print_hint_with_pos(p.is[idx].loc, "import declared here");
		err_unwind();
	}
	
	ir_var_t *ex_var;
	if ((ex_var = ir_name_in(scope, name))) {
		print_err_with_pos(onerror, "variable `%s` already exists in scope", sv_from(name));
		print_hint_with_pos(ex_var->loc, "variable `%s` declared here", sv_from(name));
		err_unwind();
	}
	
	ir_rvar_t id = arrlen(p.modp->vars);
	ir_var_t var = {
		.loc = onerror,
		.name = name,
		.type = type,
	};
	arrpush(p.modp->vars, var);

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
static bool ir_var_resolve_name(ir_scope_t *scope, istr_t name, ir_rvar_t *out) {
	bool is_toplevel = false;
	if (scope == NULL) {
		is_toplevel = true;
		scope = &p.modp->toplevel;
	}

	for (u32 i = 0; i < arrlen(scope->locals); i++) {
		ir_rvar_t id = scope->locals[i];
		ir_var_t *var = &p.modp->vars[id];

		if (var->name == name) {
			if (out) {
				*out = id;
			}
			return true;
		}
	}

	// what an opportunity for tail recursion!
	if (!is_toplevel) {
		return ir_var_resolve_name(scope->parent, name, out);
	} else {
		return false;
	}
}

static ir_scope_t *ir_new_scope_ptr(ir_scope_t *parent) {
	ir_scope_t *ptr = malloc(sizeof(ir_scope_t));
	*ptr = (ir_scope_t){
		.parent = parent,
	};
	return ptr;
}

static ir_scope_t ir_new_scope(ir_scope_t *parent) {
	ir_scope_t scope = {
		.parent = parent,
	};
	return scope;
}

// find declaration in exprs, or NULL
// warning: possibly unstable pointer
static ir_node_t *ir_find_proc_decl(ir_node_t *exprs, istr_t name) {
	for (u32 i = 0; i < arrlen(exprs); i++) {
		ir_node_t *expr = &exprs[i];
		if (expr->kind == NODE_PROC_DECL && VAR_PTR(expr->d_proc_decl.var)->name == name) {
			return expr;
		}
	}
	return NULL;
}

// find uses in expressions that are apart of the same scope
// don't recurse into deeper scopes
// WILL ONLY FIND SYMBOLS
// warning: possibly unstable pointer
// WILL IGNORE EXISTING VARS RESOLVED FROM AN OLDER SCOPE, ITS JUST NAMES TO NAMES HERE
static ir_node_t *ir_sym_find_use(ir_node_t *expr, istr_t name) {
	ir_node_t *r;
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
			return ir_sym_find_use(expr->d_postfix.expr, name);
		}
		case NODE_PREFIX: {
			return ir_sym_find_use(expr->d_prefix.expr, name);
		}
		case NODE_INFIX: {
			if ((r = ir_sym_find_use(expr->d_infix.lhs, name))) {
				return r;
			}
			if ((r = ir_sym_find_use(expr->d_infix.rhs, name))) {
				return r;
			}
			return NULL;
		}
		case NODE_CALL: {
			if ((r = ir_sym_find_use(expr->d_call.f, name))) {
				return r;
			}
			if ((r = ir_sym_find_use(expr->d_call.arg, name))) {
				return r;
			}
			return NULL;
		}
		case NODE_TUPLE: {
			for (u32 i = 0, c = arrlen(expr->d_tuple.elems); i < c; i++) {
				ir_node_t *exprp = &expr->d_tuple.elems[i];
				if ((r = ir_sym_find_use(exprp, name))) {
					return r;
				}
			}
			return NULL;
		}
		// even though this doesn't make sense, include for completeness
		case NODE_BREAK: {
			if ((r = ir_sym_find_use(expr->d_break.expr, name))) {
				return r;
			}
			return NULL;
		}
		case NODE_VAR_DECL: {
			if ((r = ir_sym_find_use(expr->d_var_decl.rhs, name))) {
				return r;
			}
			return NULL;
		}
		case NODE_DO_BLOCK:
		case NODE_INTEGER_LIT:
		case NODE_PROC_DECL: {
			return NULL;
		}
		default: {
			// TODO: be sure to be exhaustive
			printf("ir_sym_find_use: unhandled node kind %d\n", expr->kind);
			assert_not_reached();
		}
	}
}

// find uses in expressions that are apart of the same scope
// don't recurse into deeper scopes
// WILL ONLY FIND SYMBOLS
// warning: possibly unstable pointer
static ir_node_t *ir_sym_find_uses(ir_node_t *exprs, istr_t name) {
	for (u32 i = 0, c = arrlenu(exprs); i < c; i++) {
		ir_node_t *expr = &exprs[i];
		if ((expr = ir_sym_find_use(expr, name))) {
			return expr;
		}
	}

	return NULL;
}

static bool is_id_begin(u8 ch) {
    return isalpha(ch) || ch == '_' || ch == '\'';
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
			bool is_tack = *start == '\'';
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

			if (is_tack && p.pc - start == 1) {
				err_with_pos(token.loc, "invalid identifier `\'`");
			}

			if (is_underscore && p.pc - start == 1) {
				token.loc.len = 1;
				token.kind = TOK_UNDERSCORE;
				return token;
			}

			// get length and id pointer
			u32 len = p.pc - start;
			token.loc.len = len;

			// TODO: this should be optimised to a static hash table

			if (is_tack) {
				goto lit;
			}

			if (0);
			#define X(val, lit) \
				else if (sv_cmp_literal(start, len, lit)) token.kind = val;
			TOK_X_KEYWORDS_LIST
			#undef X
			else {
			lit:;
				istr_t istr = sv_intern(start, len);
				if (is_tack) {
					istr = ISTR_SET_T(istr);
				}				
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
	p.token = p.peek;
	p.peek = plex();
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

// will always parse into a type
type_t ptype(void) {
	// loc_t initial_pos = p.token.loc;
	type_t type;
	
	// TODO: support parsing of function types in parentheses

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
				.d_tuple.len = arrlen(elems),
			}, NULL);
			break;
		}
		default: {
			if ((type = ptok_to_type(p.token.kind)) != TYPE_INFER) {
				pnext();
				break;
			}
			punexpected("expected type in definition");
		}
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
	PREC_CAST,    // :
	PREC_PREFIX,  // - * ! &
	PREC_POSTFIX, // ++ --
	PREC_DOT,     // .x
	PREC_MUT,     // mut x
	PREC_INDEX,   // x[]
};

// mut x.thing    -> (mut x).thing
// mut x[y].thing -> (mut x[y]).thing

static u8 ptok_prec(tok_t kind) {
	// PREC_PREFIX is determined elsewhere
	// PREC_MUT : PREC_PREFIX

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
		case TOK_XOR:
			return PREC_XOR;
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

bool pstmt(ir_node_t *expr, ir_scope_t *s, ir_node_t *previous_exprs);
ir_node_t pexpr(ir_scope_t *s, u8 prec, u8 cfg, ir_node_t *previous_exprs);

enum : u8 {
	PEXPR_ET_NONE,
	PEXPR_ET_PAREN,
};

ir_node_t *pindented_block(ir_scope_t *s, loc_t oloc) {
	u32 bcol = p.token.loc.col;
	ir_node_t *exprs = NULL;

	// an indented block has columns greater than the column of the original location
	/* if (oloc.col != bcol) {
		punexpected("expected indented block");
	} */

	while (p.token.kind != TOK_EOF) {
		u32 cln = p.token.loc.line_nr;
		
		ir_node_t expr;
		bool set = pstmt(&expr, s, exprs);
		if (set) {
			arrpush(exprs, expr);
		}

		if (cln != p.token.loc.line_nr && p.token.loc.col != bcol) {
			break;
		}
	}

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

	ir_node_t *last = &arrlast(exprs);
	if (last->type == TYPE_UNIT) {
		ir_node_t node = {
			.kind = NODE_BREAK_UNIT, // unary shorthand
			.loc = node.loc,
			.type = TYPE_BOTTOM,
		};
		
		arrpush(exprs, node);
	} else {
		ir_node_t node = *last;

		*last = (ir_node_t){
			.kind = NODE_BREAK,
			.loc = node.loc,
			.type = TYPE_BOTTOM, // not infer, breaking is a noreturn op
			.d_break.expr = ir_memdup(node),
			.d_break.label = ISTR_NONE,
		};
	}

	return exprs;
}

// in v -> expr
// in v do
//     ...
//     ...
/* ir_node_t pin(ir_scope_t *s) {
	loc_t oloc = p.token.loc;
	pnext();

	pexpr()

	ir_node_t *exprs = NULL;
	ir_scope_t scope = ir_new_scope(s);

	if (p.token.loc.line_nr == oloc.line_nr) {
		// single expression
		ir_node_t expr = pexpr(&scope, 0, 0, NULL);
		arrpush(exprs, expr);

		ir_node_t node = {
			.kind = NODE_IN_BLOCK,
			.loc = oloc,
			.type = TYPE_INFER,
			.d_in_block.scope = scope,
			.d_in_block.head = ir_memdup(head),
			.d_in_block.exprs = exprs,
		};

		return node;
	} else {
		// indented block
		exprs = pindented_block(&scope, oloc);
		if (exprs == NULL) {
			return (ir_node_t){
				.kind = NODE_TUPLE_UNIT,
				.loc = oloc,
				.type = TYPE_UNIT,
			};
		}

		ir_node_t node = {
			.kind = NODE_IN_BLOCK,
			.loc = oloc,
			.type = TYPE_INFER,
			.d_in_block.scope = scope,
			.d_in_block.head = ir_memdup(head),
			.d_in_block.exprs = exprs,
		};

		return node;
	}
} */

// do
//     ...
//     ...
ir_node_t pdo(ir_scope_t *s) {
	loc_t oloc = p.token.loc;
	pnext();
	if (p.token.kind == TOK_EOF) {
		return (ir_node_t){
			.kind = NODE_TUPLE_UNIT,
			.loc = oloc,
			.type = TYPE_UNIT,
		};
	}
	if (p.token.loc.line_nr == oloc.line_nr) {
		err_with_pos(oloc, "expected newline after `do`");
	}

	ir_node_t block = {
		.kind = NODE_DO_BLOCK,
		.loc = oloc,
		.type = TYPE_INFER,
		.d_do_block = {
			.scope = ir_new_scope_ptr(s),
			.exprs = NULL,
			.label = ISTR_NONE,
		},
	};

	ir_node_t *exprs = pindented_block(block.d_do_block.scope, oloc);
	

	if (exprs == NULL) {
		return (ir_node_t){
			.kind = NODE_TUPLE_UNIT,
			.loc = oloc,
			.type = TYPE_UNIT,
		};
	}

	block.d_do_block.exprs = exprs;

	return block;
}

// do complex logic here, so that when typechecking all local variables are resolved
// and it only has to deal with global symbols by resolving them
// -- all variable definitions have been resolved to single assign statements by then
// -- if the checker sees `mut 'v = 20` it's an error, as `mut 'v` means something
ir_node_t passign(ir_scope_t *s, ir_node_t lhs, ir_node_t rhs, loc_t loc, ir_node_t *previous_exprs) {	
	// during parsing, the lhs can be one of three things
	// 1. NODE_GLOBAL_UNRESOLVED (could not resolve from scope)
	// 2. NODE_VAR (could resolve variable)
	// 3. NODE_MUT (could resolve variable, 'var creation)
	// 4. NODE_*   (could be a valid lhs? could also not be? let the checker handle it)

	// v = 20           ; var #1
	// do               ;
	//     v = 25       ; var #2  (create new var)

	// 'v = 20          ; var #1
	// do               ;
	//     'v = 25      ; var #1  (assign var)

	// remember: assigning to an immutable identifier will create a new one
	//           assigning to a mutable identifier will update the existing one
	// remember: mutable identifiers need to be "declared" before they can be used
	//           unlike immutable identifiers (which only have declarations)
	// remember: a variable may possibly be a global one, which NODE_GLOBAL_UNRESOLVED is preferred
	// remember: `mut 'v = ...` means create new variable

	bool assign = false; // unrolled || chain

	// if lhs: mut 'v
	if (lhs.kind == NODE_MUT && lhs.d_mut->kind == NODE_VAR && VAR_PTR_IS_T(VAR_PTR(lhs.d_mut->d_var))) {
		lhs = *lhs.d_mut; // leak
	}
	// if lhs: mut 'v*
	else if (lhs.kind == NODE_MUT && lhs.d_mut->kind == NODE_GLOBAL_UNRESOLVED && ISTR_IS_T(lhs.d_mut->d_global_unresolved)) {
		lhs = *lhs.d_mut; // leak
	}
	// is this not a symbol or var?
	else if (lhs.kind != NODE_GLOBAL_UNRESOLVED && lhs.kind != NODE_VAR) {
		assign = true;
	}
	// if it is a symbol, is it a mutable identifier?
	else if (lhs.kind == NODE_GLOBAL_UNRESOLVED && ISTR_IS_T(lhs.d_global_unresolved)) {
		assign = true;
	}
	// if it is a var, is it a mutable identifier?
	else if (lhs.kind == NODE_VAR && VAR_PTR_IS_T(VAR_PTR(lhs.d_var))) {
		assign = true;
	}

	// if lhs is NODE_GLOBAL_UNRESOLVED and mutable identifier, let it assign
	// if lhs is NODE_VAR and mutable identifier, let it assign
	// if lhs is NODE_*, let it assign
	if (assign) {
		return (ir_node_t){
			.kind = NODE_INFIX,
			.loc = loc,
			.type = TYPE_INFER,
			.d_infix.lhs = ir_memdup(lhs),
			.d_infix.rhs = ir_memdup(rhs),
			.d_infix.kind = TOK_ASSIGN,
		};
	}

	// --- EVERYTHING PAST HERE IS DECL OF NEW VARIABLE
	// lhs is NODE_GLOBAL_UNRESOLVED or NODE_VAR
	//
	// it's okay to define a new variable, it just better not exist already in the scope
	// it's also an error to use before decl in a scope

	if (lhs.kind == NODE_VAR && ir_var_exists_in(s, lhs.d_var)) {
		const char *name = sv_from(VAR_PTR(lhs.d_var)->name);
		loc_t oloc = VAR_PTR(lhs.d_var)->loc;
		print_err_with_pos(lhs.loc, "variable `%s` already exists in scope", name);
		print_hint_with_pos(oloc, "variable `%s` declared here", name);
		err_unwind();
	}
	
	// we know the variable doesn't exist, but before declaring we need to make sure
	// it hasn't been used yet in this scope.

	// decay into name to locate uses in scope
	istr_t name;

	if (lhs.kind == NODE_GLOBAL_UNRESOLVED) {
		name = lhs.d_global_unresolved;
	} else if (lhs.kind == NODE_VAR) {
		name = VAR_PTR(lhs.d_var)->name;
	} else {
		assert_not_reached();
	}

	ir_node_t *use;
	if ((use = ir_sym_find_uses(previous_exprs, name))) {
		print_err_with_pos(use->loc, "use of variable `%s` before declaration in same scope", sv_from(name));
		print_hint_with_pos(lhs.loc, "variable `%s` declared here", sv_from(name));
		err_unwind();
	}

	// TODO: no need for `onerror`, we know var doesn't exist
	//       probably introduce INVALID_LOC macro ??
	ir_rvar_t var = ir_new_var(s, name, TYPE_INFER, lhs.loc);

	// creation of a variable is ()
	// should keep? i don't know
	return (ir_node_t){
		.kind = NODE_VAR_DECL,
		.loc = loc,
		.type = TYPE_UNIT,
		.d_var_decl.lhs = var,
		.d_var_decl.rhs = ir_memdup(rhs),
	};
}

ir_node_t pident(ir_scope_t *s) {
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

		return (ir_node_t){
			.kind = NODE_SYM_UNRESOLVED,
			.type = TYPE_INFER,
			.loc = oloc,
			.d_sym_unresolved = {
				.mod = p.is[id].mod,
				.name = lit,
			},
		};
	}

	ir_node_t node;

	ir_rvar_t var;
	if (ir_var_resolve_name(s, p.token.lit, &var)) {
		node = (ir_node_t){
			.kind = NODE_VAR,
			.type = TYPE_INFER,
			.loc = p.token.loc,
			.d_var = var,
		};
	} else {
		node = (ir_node_t){
			.kind = NODE_GLOBAL_UNRESOLVED,
			.type = TYPE_INFER,
			.loc = p.token.loc,
			.d_global_unresolved = p.token.lit,
		};
	}
	pnext();

	return node;
}

// (                           a b                          )
// ^>    cfg = PEXPR_ET_PAREN  ^^^> cfg = PEXPR_ET_PAREN   >^   break 
ir_node_t pexpr(ir_scope_t *s, u8 prec, u8 cfg, ir_node_t *previous_exprs) {
	token_t token = p.token;
	u32 line_nr = token.loc.line_nr;
	ir_node_t node;
	bool is_single = true;
	bool should_continue = true;

	while (should_continue) {
		ir_node_t onode = node;
		token_t token = p.token;

		switch (token.kind) {
			case TOK_MUT: {
				pnext();
				ir_node_t rhs = pexpr(s, PREC_MUT, cfg, previous_exprs);
				node = (ir_node_t){
					.kind = NODE_MUT,
					.loc = token.loc,
					.type = TYPE_INFER,
					.d_mut = ir_memdup(rhs),
				};
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
					node = (ir_node_t){
						.kind = NODE_TUPLE_UNIT,
						.loc = oloc,
						.type = TYPE_UNIT,
					};
					pnext();
					break;
				}
				bool first = true;
				ir_node_t *elems = NULL;
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
					node = (ir_node_t){
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
			case TOK_INTEGER: {
				node = (ir_node_t){
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
			case TOK_DO: {
				node = pdo(s); // TODO?: no chaining
				should_continue = false;
				break;
			}
			default: {
				if (TOK_IS_PREFIX(token.kind)) {
					pnext();
					if (token.kind == TOK_SUB && p.token.kind == TOK_INTEGER) {
						node = (ir_node_t){
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
						ir_node_t rhs = pexpr(s, PREC_PREFIX, cfg, previous_exprs);
						node = (ir_node_t){
							.kind = NODE_PREFIX,
							.loc = token.loc,
							.type = TYPE_INFER,
							.d_prefix.expr = ir_memdup(rhs),
							.d_prefix.kind = token.kind,
						};
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
			default: {
				assert_not_reached();
			}
		}

		if (is_single) {
			is_single = false;
		} else {
			node = (ir_node_t){
				.kind = NODE_CALL,
				.loc = onode.loc,
				.type = TYPE_INFER,
				.d_call.f = ir_memdup(onode),
				.d_call.arg = ir_memdup(node),
			};
		}

		if (ptok_prec(p.token.kind) == 0 && p.token.loc.line_nr == line_nr) {
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

		if (prec < ptok_prec(p.token.kind)) {
			switch (token.kind) {
				case TOK_INC:
				case TOK_DEC: {
					pnext();
					node = (ir_node_t){
						.kind = NODE_POSTFIX,
						.loc = token.loc,
						.type = TYPE_INFER,
						.d_postfix.expr = ir_memdup(node),
						.d_postfix.kind = token.kind,
					};
					continue;
				}
				default: {
					if (TOK_IS_INFIX(token.kind)) {
						pnext();

						// cast
						if (token.kind == TOK_COLON) {
							type_t type = ptype();
							node = (ir_node_t){
								.kind = NODE_CAST,
								.loc = token.loc,
								.type = type,
								.d_cast = ir_memdup(node),
							};
							continue;
						}

						ir_node_t rhs = pexpr(s, ptok_prec(token.kind), cfg, previous_exprs);

						// assign
						if (token.kind == TOK_ASSIGN) {
							node = passign(s, node, rhs, token.loc, previous_exprs);
							continue;
						}

						// random infix
						node = (ir_node_t){
							.kind = NODE_INFIX,
							.loc = token.loc,
							.type = TYPE_INFER,
							.d_infix.lhs = ir_memdup(node),
							.d_infix.rhs = ir_memdup(rhs),
							.d_infix.kind = token.kind,
						};
						continue;
					}
				}
			}
		}
		break;
	}

	return node;
}

// will create vars it cannot find
ir_pattern_t ppattern(ir_scope_t *s) {
	switch (p.token.kind) {
		case TOK_UNDERSCORE: {
			pnext();
			return (ir_pattern_t){
				.kind = PATTERN_UNDERSCORE,
			};
		}
		case TOK_IDENT: {
			istr_t name = p.token.lit;
			loc_t name_loc = p.token.loc;
			pnext();
			ir_rvar_t var = ir_new_var(s, name, TYPE_INFER, name_loc);
			return (ir_pattern_t){
				.loc = name_loc,
				.kind = PATTERN_VAR,
				.d_var = var,
			};
		}
		case TOK_INTEGER: {
			istr_t val = p.token.lit;
			loc_t loc = p.token.loc;
			pnext();
			return (ir_pattern_t){
				.loc = loc,
				.kind = PATTERN_INTEGER_LIT,
				.d_integer_lit = val,
			};
		}
		default: {
			punexpected("expected pattern");
		}
	}
}

// function declarations:
// - io <name>            = ...
// - <name>:    <pattern> = ...
bool pproc(ir_node_t *out_expr, ir_scope_t *s, ir_node_t *previous_exprs) {
	bool is_io = false;

	if (p.token.kind == TOK_IO) {
		is_io = true;
		pnext();
	}

	pcheck(TOK_IDENT);
	istr_t name = p.token.lit;
	loc_t name_loc = p.token.loc;

	if (ISTR_IS_T(name)) {
		punexpected("expected a non-tack name");
	}

	// io name
	//    ^^^^

	pnext();

	// name: <pattern> =
	//     ^

	// io name =
	//         ^

	ir_scope_t enclosing_scope = ir_new_scope(s);
	ir_pattern_t pattern;
	
	// remember, `io fn` has no patterns
	if (!is_io && p.token.kind == TOK_COLON) {
		pnext();
		ir_pattern_t *patterns = NULL;

		// <pattern>, <pattern>, <pattern> = ...
		// ^^^^^^^^^

		while (true) {
			ir_pattern_t pattern = ppattern(&enclosing_scope);
			arrpush(patterns, pattern);
			if (p.token.kind == TOK_COMMA) {
				pnext();
			} else {
				break;
			}
		}

		pattern = (ir_pattern_t){
			.kind = PATTERN_TUPLE,
			.loc = patterns[0].loc,
			.d_tuple = {
				.elems = patterns,
				.len = arrlen(patterns),
			},
		};
	}

	pexpect(TOK_ASSIGN);

	// `previous_exprs` doesn't share scope with `enclosing_scope`
	ir_node_t expr = pexpr(&enclosing_scope, 0, 0, NULL);

	ir_node_t *other_def;
	if (!is_io && (other_def = ir_find_proc_decl(previous_exprs, name))) {
		if (other_def->kind != NODE_PROC_DECL) {
			err_with_pos(name_loc, "redefinition of `%s`", sv_from(name));
		}

		arrpush(other_def->d_proc_decl.exprs, expr);
		arrpush(other_def->d_proc_decl.patterns, pattern);
		arrpush(other_def->d_proc_decl.scopes, enclosing_scope);
		return false;
	}

	// create new var before evaluation so it can be referenced recursively
	ir_rvar_t var = ir_new_var(s, name, TYPE_INFER, name_loc);

	ir_node_t *exprs = NULL;
	arrpush(exprs, expr);

	ir_pattern_t *patterns = NULL;
	if (!is_io) {
		arrpush(patterns, pattern);
	}

	ir_scope_t *scopes = NULL;
	arrpush(scopes, enclosing_scope);

	*out_expr = (ir_node_t){
		.kind = NODE_PROC_DECL,
		.loc = name_loc,
		.type = TYPE_UNIT,
		.d_proc_decl = {
			.var = var,
			.exprs = exprs,
			.patterns = patterns,
			.scopes = scopes,
		}
	};
	return true;
}

void ptypedecl(ir_scope_t *s) {
	istr_t name = p.token.lit;
	loc_t name_loc = p.token.loc;

	pnext();
	pnext();
	
	// v :: i32 -> i32 -> ...
	//      ^^^

	if (ir_name_exists_in(s, name)) {
		err_with_pos(name_loc, "type decl must come before definition `%s`", sv_from(name));
	}

	// check if duplicate exists already in stack
	for (u32 i = 0; i < p.tds_len; i++) {
		if (p.tds[i].name == name && p.tds[i].scope == s) {
			print_err_with_pos(name_loc, "duplicate type decl `%s`", sv_from(name));
			print_hint_with_pos(p.tds[i].loc, "previous type decl was here");
			err_unwind();
		}
	}

	type_t type = ptype();

	// add to stack
	p.tds[p.tds_len++] = (ptypedecl_t){
		.name = name,
		.scope = s,
		.loc = name_loc,
		.type = type,
	};
}

// `previous_exprs` is used when multiple function declarations are in the same scope
// can be null
// IT IS VERY IMPORTANT THAT `previous_exprs` HAS SCOPE `s`
bool pstmt(ir_node_t *expr, ir_scope_t *s, ir_node_t *previous_exprs) {
	bool set = false;

	switch (p.token.kind) {
		case TOK_IO: {
			set = pproc(expr, s, NULL);
			break;
		}
		case TOK_IDENT: {
			if (p.peek.kind == TOK_COLON) {
				set = pproc(expr, s, previous_exprs);
				break;
			}
			if (p.peek.kind == TOK_DOUBLE_COLON) {
				ptypedecl(s);
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

void pmake_pub(ir_node_t *node) {
	ir_var_t *varp;
	
	switch (node->kind) {
		case NODE_PROC_DECL: {
			varp = VAR_PTR(node->d_proc_decl.var);
			break;
		}
		case NODE_VAR_DECL: {
			varp = VAR_PTR(node->d_var);
			break;
		}
		default: {
			err_with_pos(node->loc, "cannot make this expression public");
		}
	}

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
			ir_node_t expr;
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

void papply_typedecls(void) {
	for (u32 i = 0; i < p.tds_len; i++) {
		ptypedecl_t *td = &p.tds[i];
		ir_var_t *varp = ir_name_in(td->scope, td->name);

		if (!varp) {
			err_with_pos(td->loc, "cannot find variable `%s`", sv_from(td->name));
		}

		assert(varp->type == TYPE_INFER);
		varp->type = td->type;
	}

	// pubs all have types, i don't want to sort modules... global exprs is enough
	for (u32 i = 0, c = arrlenu(p.modp->vars); i < c; i++) {
		ir_var_t *varp = &p.modp->vars[i];
		if (varp->is_pub && varp->type == TYPE_INFER) {
			err_with_pos(varp->loc, "cannot make expression with inferred type public");
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

	while (p.token.kind != TOK_EOF) {
		ptop_stmt();
	}

	papply_typedecls();
}

void _ir_dump_pattern(mod_t *modp,ir_scope_t *s, ir_pattern_t pattern) {
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
			ir_var_t *varp = &modp->vars[pattern.d_var];
			printf("%s.%u", sv_from(varp->name), pattern.d_var);
			break;
		}
		case PATTERN_TUPLE: {
			printf("(");
			for (u32 i = 0; i < pattern.d_tuple.len; i++) {
				_ir_dump_pattern(modp, s, pattern.d_tuple.elems[i]);
				if (i != pattern.d_tuple.len - 1) {
					printf(", ");
				}
			}
			printf(")");
			break;
		}
		default: {
			assert_not_reached();
		}
	}
}

void _ir_dump_scope(mod_t *modp, ir_scope_t *s) {
	for (int i = 0, c = arrlen(s->locals); i < c; i++) {
		ir_rvar_t var = s->locals[i];
		ir_var_t *varp = &modp->vars[var];
		printf("  %s :: %s\n", sv_from(varp->name), type_dbg_str(varp->type));
	}
}

static u32 _ir_tabcnt = 0;
void _ir_dump_stmt(mod_t *modp, ir_scope_t *s, ir_node_t node);

void _ir_tabs(void) {
	for (u32 i = 0; i < _ir_tabcnt; i++) {
		printf("  ");
	}
}

#define TAB_PRINTF(...) \
	_ir_tabs(); \
	printf(__VA_ARGS__);

void _ir_dump_expr(mod_t *modp, ir_scope_t *s, ir_node_t node) {
	// printf("[%s]:", type_dbg_str(node.type));

	switch (node.kind) {
		case NODE_VAR: {
			printf("%s.%u", sv_from(modp->vars[node.d_var].name), node.d_var);
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
		case NODE_POSTFIX: {
			_ir_dump_expr(modp, s, *node.d_postfix.expr);
			printf("%s", node.d_postfix.kind == TOK_INC ? "++" : "--");
			break;
		}
		case NODE_INFIX: {
			printf("(%s ", tok_op_str(node.d_infix.kind));
			_ir_dump_expr(modp, s, *node.d_infix.lhs);
			printf(" ");
			_ir_dump_expr(modp, s, *node.d_infix.rhs);
			printf(")");
			break;
		}
		case NODE_PREFIX: {
			printf("%s", tok_op_str(node.d_prefix.kind));
			_ir_dump_expr(modp, s, *node.d_prefix.expr);
			break;
		}
		case NODE_CALL: {
			printf("(");
			_ir_dump_expr(modp, s, *node.d_call.f);
			printf(" ");
			_ir_dump_expr(modp, s, *node.d_call.arg);
			printf(")");
			break;
		}
		case NODE_DO_BLOCK: {
			printf("do\n");
			_ir_tabcnt++;
			for (int i = 0, c = arrlen(node.d_do_block.exprs); i < c; i++) {
				_ir_dump_stmt(modp, node.d_do_block.scope, node.d_do_block.exprs[i]);
			}
			_ir_tabcnt--;
			break;
		}
		case NODE_TUPLE_UNIT: {
			printf("()");
			break;
		}
		case NODE_TUPLE: {
			printf("(");
			for (int i = 0, c = arrlen(node.d_tuple.elems); i < c; i++) {
				_ir_dump_expr(modp, s, node.d_tuple.elems[i]);
				if (i != c - 1) {
					printf(", ");
				}
			}
			printf(")");
			break;
		}
		case NODE_BREAK: {
			printf("brk ");
			if (node.d_break.label != ISTR_NONE) {
				printf(":%s ", sv_from(node.d_break.label));
			}
			_ir_dump_expr(modp, s, *node.d_break.expr);
			break;
		}
		case NODE_BREAK_UNIT: {
			printf("brk ()");
			break;
		}
		case NODE_MUT: {
			printf("(mut ");
			_ir_dump_expr(modp, s, *node.d_mut);
			printf(")");
			break;
		}
		case NODE_CAST: {
			printf("cast(");
			_ir_dump_expr(modp, s, *node.d_cast);
			printf(")");
			break;
		}
		case NODE_SYM_UNRESOLVED: {
			printf("%s*", fs_module_symbol_str(node.d_sym_unresolved.mod, node.d_sym_unresolved.name));
			break;
		}
		case NODE_SYM: {
			ir_var_t *var = MOD_VAR_PTR(node.d_sym.mod, node.d_sym.var);
			printf("%s", fs_module_symbol_str(node.d_sym.mod, var->name));
			break;
		}
		default: {
			printf("\nunknown expr kind %d\n", node.kind);
			print_hint_with_pos(node.loc, "LOC HERE");
			assert_not_reached();
		}
	}
}

void _ir_dump_stmt(mod_t *modp, ir_scope_t *s, ir_node_t node) {
	switch (node.kind) {
		case NODE_PROC_DECL: {
			ir_var_t var = modp->vars[node.d_proc_decl.var];
			type_t fn_type = var.type;
			istr_t name = var.name;
			
			TAB_PRINTF("%s.%u :: %s\n", sv_from(name), node.d_proc_decl.var, type_dbg_str(fn_type));
			if (node.d_proc_decl.patterns) {
				TAB_PRINTF("%s.%u _ = switch\n", sv_from(name), node.d_proc_decl.var);
				_ir_tabcnt++;
				for (u32 i = 0, c = arrlenu(node.d_proc_decl.exprs); i < c; i++) {
					// _ir_dump_scope(modp, &node.d_proc_decl.scopes[i]);
					_ir_tabs();
					_ir_dump_pattern(modp, &node.d_proc_decl.scopes[i], node.d_proc_decl.patterns[i]);
					printf(" -> ");
					_ir_dump_expr(modp, &node.d_proc_decl.scopes[i], node.d_proc_decl.exprs[i]);
					printf("\n");
				}
				_ir_tabcnt--;
			} else {
				TAB_PRINTF("%s _ = ", sv_from(name));
				_ir_dump_expr(modp, &node.d_proc_decl.scopes[0], node.d_proc_decl.exprs[0]);
				printf("\n");
			}
			break;
		}
		case NODE_VAR_DECL: {
			ir_var_t var = modp->vars[node.d_var_decl.lhs];
			TAB_PRINTF("%s.%u :: %s\n", sv_from(var.name), node.d_var_decl.lhs, type_dbg_str(var.type));
			TAB_PRINTF("%s.%u", sv_from(var.name), node.d_var_decl.lhs);
			printf(" = ");
			_ir_dump_expr(modp, s, *node.d_var_decl.rhs);
			printf("\n");
			break;
		}
		default: {
			_ir_tabs();
			_ir_dump_expr(modp, s, node);
			printf("\n");
			break;
		}
	}
}

void ir_dump_module(rmod_t mod) {
	void *_ = alloc_scratch(0);

	mod_t *modp = MOD_PTR(mod);
	printf("module %s\n\n", fs_module_symbol_str(mod, ISTR_NONE));
	for (u32 i = 0, c = arrlenu(modp->exprs); i < c; i++) {
		_ir_dump_stmt(modp, NULL, modp->exprs[i]);
		if (i != c - 1) {
			printf("\n");
		}
	}

	alloc_reset(_);
}
