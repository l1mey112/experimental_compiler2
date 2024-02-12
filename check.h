#pragma once

#include "all.h"

// shared checker defs

typedef struct cctx_t cctx_t;
typedef struct cblk_t cblk_t;

struct cblk_t {
	type_t upvalue;
	type_t brk_type;
	loc_t brk_loc; // if `first_type` not TYPE_INFER
	bool is_rep;
};

struct cctx_t {
	rmod_t mod;
	mod_t *modp;
	cblk_t blocks[128]; // overkill, index by blk_id
};

extern cctx_t c;

enum : u8 {
	BM_RVALUE = 1 << 0, // rvalue is default
	BM_LVALUE = 1 << 1,
	BM_CALL = 1 << 2,
};

type_t cexpr(ir_desc_t *desc, type_t upvalue, hir_expr_t *expr, u8 cfg);

type_t ctype_unify_type(type_t lhs_t, type_t rhs_t, loc_t onerror);
type_t ctype_unify(type_t lhs_t, hir_expr_t *rhs);

// auto insert UFCS calls
void cufcs_autocall(ir_desc_t *desc, hir_expr_t *expr, u8 cfg);
void ccall(ir_desc_t *desc, type_t upvalue, hir_expr_t *expr, u8 cfg);
void cnamed_field(ir_desc_t *desc, type_t upvalue, hir_expr_t *expr, hir_expr_t *call);
