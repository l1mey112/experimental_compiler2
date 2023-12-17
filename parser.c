#include "all.h"

#include "stb_ds.h"
#include <setjmp.h>

typedef struct pctx_t pctx_t;
typedef struct pimport_t pimport_t;

struct pimport_t {

};

struct pctx_t {
	u8 *pstart;
	u8 *pc;
	u8 *pend;
	u8 *plast_nl;
	u32 line_nr;
	token_t token;
	token_t peek;
	// pimport_t is[64]; // import stack
	// u32 is_len;
	rfile_t file;
	rmod_t mod;
	mod_t *modp;
	bool has_done_imports;
	//
	struct {
		istr_t name; // -1 for none
		type_t type;
		loc_t loc;
	} next_type;
};

pctx_t p;

#define VAR_PTR(id) (&p.modp->vars[id])

static bool ir_exists_in(ir_scope_t *scope, istr_t name) {
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

// NULL meaning toplevel
static ir_rvar_t ir_new_var(ir_scope_t *scope, istr_t name, type_t type, loc_t onerror) {
	if (ir_exists_in(scope, name)) {
		err_with_pos(onerror, "variable `%s` already exists in scope", sv_from(name));
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
// will search parent
static bool ir_search_var(ir_scope_t *scope, istr_t name, ir_rvar_t *out) {
	if (scope == NULL) {
		scope = &p.modp->toplevel;
	}

	for (u32 i = 0; i < arrlen(scope->locals); i++) {
		ir_rvar_t id = scope->locals[i];
		ir_var_t *var = &p.modp->vars[id];

		if (var->name == name) {
			*out = id;
			return true;
		}
	}

	// what an opportunity for tail recursion!
	if (scope->parent != NULL) {
		return ir_search_var(scope->parent, name, out);
	} else {
		return false;
	}
}

static ir_scope_t *ir_new_scope(ir_scope_t *parent) {
	ir_scope_t *scope = malloc(sizeof(ir_scope_t));
	*scope = (ir_scope_t){.parent = parent};
	return scope;
}

// find declaration in exprs, or NULL
// warning: possibly unstable pointer
static ir_node_t *ir_find_decl(ir_node_t *exprs, istr_t name) {
	for (u32 i = 0; i < arrlen(exprs); i++) {
		ir_node_t *expr = &exprs[i];
		if (expr->kind == NODE_PROC_DECL && expr->d_proc_decl.name == name) {
			return expr;
		}
	}
	return NULL;
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

ir_node_t pexpr(ir_scope_t *s) {
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
	
	// TODO: support parsing of function types in parentheses

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

// will create vars it cannot find
ir_pattern_t ppattern(ir_scope_t *s) {
	assert_not_reached();
}

// function declarations:
// - io <name>            = ...
// - <name>:    <pattern> = ...
bool pproc(ir_node_t *out_expr, ir_scope_t *s, ir_node_t *multi_exprs) {
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
	
	// remember, `io fn` has no patterns
	if (!is_io && p.token.kind == TOK_COLON) {
		pnext();
		// TODO: parse pattern
	}

	pexpect(TOK_ASSIGN);
	
	ir_node_t expr = {.kind = NODE_INTEGER_LIT, .type = TYPE_INFER, .d_integer_lit = sv_move("999")}; // pexpr(s);
	ir_pattern_t pattern = {.kind = PATTERN_UNDERSCORE}; // ppattern(scope);

	type_t type = TYPE_INFER;
	if (p.next_type.name == name) {
		type = p.next_type.type;
		p.next_type.name = (u32)-1;
	}

	if (!is_io && ir_exists_in(s, name)) {
		// this will never be NULL
		ir_node_t *other_def = ir_find_decl(multi_exprs, name);

		if (other_def->kind != NODE_PROC_DECL) {
			err_with_pos(name_loc, "redefinition of `%s`", sv_from(name));
		}

		arrpush(other_def->d_proc_decl.exprs, expr);
		arrpush(other_def->d_proc_decl.patterns, pattern);
		return false;
	}

	// create new var before evaluation so it can be referenced recursively
	(void)ir_new_var(s, name, type, name_loc);
	ir_scope_t *scope = ir_new_scope(s);

	ir_node_t *exprs = NULL;
	arrpush(exprs, expr);

	ir_pattern_t *patterns = NULL;
	arrpush(patterns, pattern);

	*out_expr = (ir_node_t){
		.kind = NODE_PROC_DECL,
		.loc = name_loc,
		.type = TYPE_UNIT,
		.d_proc_decl = {
			.name = name,
			.exprs = exprs,
			.patterns = patterns,
			.scope = scope,
		}
	};
	return true;
}

void ptypedecl(ir_scope_t *s) {
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

	if (ir_exists_in(s, name)) {
		err_with_pos(name_loc, "type decl cannot proceed definition `%s`", sv_from(name));
	}

	type_t type = ptype();

	p.next_type.type = type;
	p.next_type.name = name;
	p.next_type.loc = name_loc;
}

void pcheck_unused_typedecl(void) {
	if (p.next_type.name != (u32)-1) {
		err_with_pos(p.next_type.loc, "unused type decl `%s`", sv_from(p.next_type.name));
	}
}

// `multi_exprs` is used when multiple function declarations are in the same scope
// can be null
bool pstmt(ir_node_t *expr, ir_scope_t *s, ir_node_t *multi_exprs) {
	bool set = false;
	bool has_next_type = p.next_type.name != (u32)-1;
	
	switch (p.token.kind) {
		case TOK_IO: {
			set = pproc(expr, s, NULL);
			break;
		}
		case TOK_IDENT: {
			if (p.peek.kind == TOK_COLON) {
				set = pproc(expr, s, multi_exprs);
				break;
			}
			if (p.peek.kind == TOK_DOUBLE_COLON) {
				ptypedecl(s);
				break;
			}
			// fall through
		}
		default: {
			*expr = pexpr(s);
			set = true;
			break;
		}
	}

	// error if type decl not used
	if (has_next_type) {
		pcheck_unused_typedecl();
	}

	return set;
}

void ptop_stmt(void) {
	switch (p.token.kind) {
		default: {
			ir_node_t expr;
			bool set = pstmt(&expr, NULL, p.modp->exprs);
			if (set) {
				arrpush(p.modp->exprs, expr);
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
		.next_type.name = -1,
	};

	pnext(); // tok
	pnext(); // tok peek

	while (p.token.kind != TOK_EOF) {
		ptop_stmt();
	}
	pcheck_unused_typedecl();
}

void _ir_dump_pattern(ir_pattern_t pattern) {
	switch (pattern.kind) {
		case PATTERN_UNDERSCORE: {
			printf("_");
			break;
		}
		default: {
			assert_not_reached();
		}
	}
}

void _ir_dump_node(mod_t *modp, ir_scope_t *s, ir_node_t node) {
	void *_ = alloc_scratch(0);
	switch (node.kind) {
		case NODE_PROC_DECL: {
			ir_rvar_t var;
			assert(ir_search_var(s, node.d_proc_decl.name, &var));
			type_t fn_type = modp->vars[var].type;
			
			printf("%s :: %s\n", sv_from(node.d_proc_decl.name), type_dbg_str(fn_type));
			printf("proc %s _ =\n", sv_from(node.d_proc_decl.name));
			for (int i = 0, c = arrlen(node.d_proc_decl.exprs); i < c; i++) {
				printf("  | ");
				_ir_dump_pattern(node.d_proc_decl.patterns[i]);
				printf(" = ");
				_ir_dump_node(modp, &node.d_proc_decl.scope, node.d_proc_decl.exprs[i]);
				printf("\n");
			}
			break;
		}
		case NODE_INTEGER_LIT: {
			printf("%s", sv_from(node.d_integer_lit));
			break;
		}
		default: {
			assert_not_reached();
		}
	}
	alloc_reset(_);
}

void ir_dump_module(rmod_t mod) {
	mod_t *modp = MOD_PTR(mod);
	printf("module %s\n", sv_from(modp->on_disk.name));
	for (u32 i = 0; i < arrlen(modp->exprs); i++) {
		printf("\n");
		_ir_dump_node(modp, NULL, modp->exprs[i]);
	}
}
