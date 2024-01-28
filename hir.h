#pragma once

typedef struct hir_expr_t hir_expr_t;

#include "all.h"

// TODO: need to represent `let` decls as actual exprs..

struct hir_expr_t {
	enum {
		EXPR_INTEGER_LIT,
		EXPR_BOOL_LIT,
		EXPR_ARRAY,
		EXPR_TUPLE_UNIT,
		EXPR_TUPLE,
		EXPR_LAMBDA,
		EXPR_MATCH,
		EXPR_DO_BLOCK,
		EXPR_LOOP,
		EXPR_IF, // in checker, if type == TYPE_BOOL it is a desugared logic op
		EXPR_ASSIGN,
		EXPR_INFIX,
		EXPR_POSTFIX,
		EXPR_PREFIX,
		EXPR_DEREF,
		EXPR_ADDR_OF,
		EXPR_INDEX,
		EXPR_SLICE,
		EXPR_LOCAL,
		EXPR_SYM,
		EXPR_CAST,
		EXPR_CALL,
		EXPR_BREAK, // always !.
		EXPR_VOIDING, // evaluates expr, possible effects. discards return value, returning ()
		EXPR_SIZEOF_TYPE, // is usize
		EXPR_TUPLE_FIELD,
        EXPR_FIELD,
	} kind;
	
	type_t type;
	loc_t loc;

	union {
		rlocal_t d_local;
        rsym_t d_sym;
		hir_expr_t *d_voiding;
		hir_expr_t *d_deref;
		type_t d_sizeof_type;
		struct {
			hir_expr_t *expr;
			u16 field;
		} d_tuple_field;
		struct {
			hir_expr_t *expr;
			hir_expr_t *index;
		} d_index;
		struct {
			hir_expr_t *expr;
			hir_expr_t *lo; // possible NULLs
			hir_expr_t *hi; // possible NULLs
		} d_slice;
		struct {
			hir_expr_t *lhs;
			hir_expr_t *rhs;
		} d_assign;
		struct {
			hir_expr_t *ref;
			bool is_mut;
		} d_addr_of;
		struct {
			hir_expr_t *expr;
            enum : u8 {
                EXPR_K_INC,
                EXPR_K_DEC,
            } kind;
		} d_postfix;
		struct {
			hir_expr_t *expr;
			tok_t kind; // TODO: create enum kind
		} d_prefix;
		struct {
			hir_expr_t *exprs; // all do blocks have at least one expr, unless they become a EXPR_TUPLE_UNIT
			u8 blk_id;
		} d_do_block;
		struct {
			hir_expr_t *expr;
			u8 blk_id;
		} d_loop;
		struct {
			hir_expr_t *lhs;
			hir_expr_t *rhs;
			tok_t kind; // TODO: create enum kind
		} d_infix;
		hir_expr_t *d_cast;
		struct {
			istr_t lit;
			bool negate;
		} d_integer_lit;
		bool d_bool_lit;
		/* struct {
			hir_rvar_t *args;
			hir_expr_t *expr;
		} d_lambda; */
		struct {
			hir_expr_t *expr;
			pattern_t *patterns;
			hir_expr_t *exprs;
		} d_match;
		/* struct {
			pattern_t pattern;
			hir_expr_t *expr;
		} d_let_decl; */
		struct {
			hir_expr_t *f;
			hir_expr_t *args; // f(...args)
		} d_call;
		struct {
			hir_expr_t *elems;
		} d_tuple;
		struct {
			hir_expr_t *elems;
		} d_array;
		struct {
			hir_expr_t *expr; // in use with EXPR_BREAK*
			u8 blk_id;
		} d_break;
		struct {
			hir_expr_t *cond;
			hir_expr_t *then;
			hir_expr_t *els;
		} d_if;
	};
};

// heapalloc
hir_expr_t *hir_dup(hir_expr_t expr);