#include "all.h"

// shared parser defs

typedef struct pctx_t pctx_t;
typedef struct pimport_t pimport_t;
typedef struct pproc_decls_t pproc_decls_t;
typedef struct pscope_entry_t pscope_entry_t;
typedef struct pblk_t pblk_t;

// TODO: since we have no need to reconstruct the block stack in the checker
//       we can just ignore brks to a block that has `always_brk` set to false
//       like in label-less`do` blocks.
struct pblk_t {
	istr_t label;
	loc_t loc; // TODO: i don't even think we'll need this?
	bool always_brk; // if false, a `brk` without a label doesn't resolve to this
	lir_rblock_t brk;
	lir_rblock_t rep;
	bool is_brk; // if broken to (used with loop)
	rlocal_t brk_local; // immutable intermediate local to store `brk`
};

struct pimport_t {
	rmod_t mod;
	istr_t name;
	loc_t loc;
};

// locals, functions, types
struct pscope_entry_t {
	istr_t name;
	loc_t loc;

	bool is_masked; // when masked it doesn't resolve to the entry

	enum {
		PS_LOCAL,
		PS_DEBUG_REFERENCE,
		PS_SYMBOL,
		// PS_TYPE,
	} kind;

	union {
		struct {
			// will probably need to let the parser know if a local resides in a parent fn
			rlocal_t local;
		} d_local;
		struct {
			istr_t qualified_name;
		} d_symbol;
	};
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
	//
	u32 scope[128]; // store integer idxs
	u32 scope_len;
	pscope_entry_t scope_entries[256]; // scope entries
	u32 scope_entries_len;
	pblk_t blks[256]; // block stack, idx encoded in a u8
	u32 blks_len;
	//
	rfile_t file;
	rmod_t mod;
	mod_t *modp;
	bool has_done_imports;
};

extern pctx_t p;
token_t plex(void);
void pnext(void);
const char *tok_op_str(tok_t tok);
const char *tok_dbg_str(token_t tok);
bool pprev_next_to(void);
bool ppeek_next_to(void);
void pcheck(tok_t expected);
void pexpect(tok_t expected);
void NORETURN punexpected(const char *err);
type_t ptype(void);
void pimport(void);
int pimport_ident(istr_t name);
void pscope_register(pscope_entry_t entry);
void ppush_scope(void);
void ppop_scope(void);
void pmask_scope(u32 entries_lo, u32 entries_hi, bool mask);
pattern_t ppattern(lir_proc_t *proc);
u32 pblk_locate(istr_t opt_label, loc_t onerror);
// result of parsing an expression
typedef struct rexpr_t rexpr_t;

// make sure to zero initialise an `rexpr_t` for the best experience

// parsing an expression:
// 1. results in a value or lvalue
// 2. could introduce control flow (block changing)
struct rexpr_t {
	lir_rblock_t block;
	lir_value_t value;
	lir_lvalue_t lvalue;
	bool is_lvalue;
};

// TODO: nicer interface with default vars?
rexpr_t pexpr(lir_proc_t *proc, lir_rblock_t block, u8 prec, u8 cfg);

// create a ! value by creating an unreachable block and returning a ! literal
rexpr_t pnoreturn_value(lir_proc_t *proc, loc_t loc, const char *debug_name);

// shorthand for spilling an expression for reading, will void all mutable lvalue properties
//
// ensure to spill as soon as possible and in the same block before you start evaluating
// other expressions. if you don't you'll most likely end up with invalid state/order
// of evaluation
//
// will also return a local if you want to use that too
// rlocal_t pexpr_spill(lir_proc_t *proc, rexpr_t *expr);

// be careful, as specified above, misuse of these functions could result in incorrect behavior

// force a spill, like a late use
lir_value_t pexpr_spill(lir_proc_t *proc, lir_rblock_t block, lir_value_t value);
rlocal_t pexpr_spill_local(lir_proc_t *proc, lir_rblock_t block, lir_value_t value);

// extract lvalue, raises error if not an lvalue
lir_lvalue_t pexpr_lvalue(lir_proc_t *proc, rexpr_t expr);

// construct an lvalue from an expr
lir_lvalue_t pexpr_lvalue_from(lir_proc_t *proc, rexpr_t expr);

// late use
lir_value_t pexpr_lu(lir_proc_t *proc, rexpr_t expr);
// early use
lir_value_t pexpr_eu(lir_proc_t *proc, rexpr_t expr);
//
// when composing multiple expressions together, all expressions
// except for the last one are "late use" which means they must
// be spilled to respect the order of evaluation with the presence
// of control flow.
//
//    expr   +   expr
//    ^^^^       ^^^^
//  late use   early use
//
//  [expr, expr, expr, expr]
//   ^^^^^^^^^^^^^^^^  ^^^^
//       late use    early use
//
// this is an expanded definition of the lvalue_spill used before
// to be more specific about semantics.
//
// it also matters WHEN you emit a late use, since it void the purpose
// if you did it after some kind of side effecting operation. do it
// as soon as possible.
//
