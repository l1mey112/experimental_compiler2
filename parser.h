#include "all.h"
#include "hir.h"

typedef struct pctx_t pctx_t;
typedef struct pimport_t pimport_t;
typedef struct pproc_decls_t pproc_decls_t;
typedef struct pscope_entry_t pscope_entry_t;
typedef struct pblk_t pblk_t;
typedef struct plex_pair_t plex_pair_t;

struct pblk_t {
	istr_t label;
	loc_t loc; // TODO: i don't even think we'll need this?
	bool always_brk; // if false, a `brk` without a label doesn't resolve to this
	bool forward; // brk
	bool backward; // rep
};

struct pimport_t {
	fs_rmod_t mod;
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
	u32 pline_nr;
	//
	token_t prev;
	token_t token;
	token_t peek;
	//
	lineinfo_t line_prev;
	lineinfo_t line_token;
	lineinfo_t line_peek;
	//
	pimport_t is[64]; // import stack
	u32 is_len;
	//
	u32 scope[128]; // store integer idxs
	u32 scope_len;
	pscope_entry_t scope_entries[256]; // scope entries
	u32 scope_entries_len;
	pblk_t blks[256]; // a stack using wasm like branching semantics
	u32 blks_len;
	//
	fs_rfile_t file;
	fs_file_t *filep;
	fs_rmod_t mod;
	fs_mod_t *modp;
	bool has_done_imports;
	//
	bool inside_fn;
};

struct plex_pair_t {
	token_t token;
	lineinfo_t lineinfo;
};

extern pctx_t p;
plex_pair_t plex(void);
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
void pimport_main(void);
int pimport_ident(istr_t name);
void pscope_register(pscope_entry_t entry);
void ppush_scope(void);
void ppop_scope(void);
void pmask_scope(u32 entries_lo, u32 entries_hi, bool mask);
pattern_t ppattern(ir_desc_t *desc);
u32 pblk_locate_label(istr_t opt_label, loc_t onerror);
u64 pparse_int(token_t token);

hir_expr_t pexpr(ir_desc_t *desc, u8 prec);
