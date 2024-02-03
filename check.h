#pragma once

#include "all.h"

// shared checker defs

typedef struct cctx_t cctx_t;
typedef struct cblk_t cblk_t;

struct cblk_t {
	type_t upvalue;
	type_t brk_type;
	loc_t brk_loc; // if `first_type` not TYPE_INFER
};

struct cctx_t {
	rmod_t mod;
	mod_t *modp;
	cblk_t blocks[64];
	u32 blocks_len;
};

extern cctx_t c;

// return a postorder sorted list of symbols, reverse for proper ordering
rsym_t *creorder_po_and_sanity(void);

enum : u8 {
	BM_RVALUE = 1 << 0, // rvalue is default
	BM_LVALUE = 1 << 1,
	BM_CALL = 1 << 2,
	BM_RECALL = 1 << 3,
};

type_t cexpr(ir_desc_t *desc, type_t upvalue, hir_expr_t *expr, u8 cfg);

type_t ctype_unify_type(type_t lhs_t, type_t rhs_t, loc_t onerror);
type_t ctype_unify(type_t lhs_t, hir_expr_t *rhs);