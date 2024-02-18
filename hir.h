#pragma once

typedef struct hir_expr_t hir_expr_t;
typedef struct hir_sf_t hir_sf_t;

#include "all.h"

// TODO: need to represent `let` decls as actual exprs..

#define BLK_ID_NONE ((u8)-1)

struct hir_sf_t {
	istr_t field;
	loc_t field_loc;
	hir_expr_t *expr;
	//type_t type;
};

struct hir_expr_t {
	enum {
		EXPR_INTEGER_LIT,
		EXPR_BOOL_LIT,
		EXPR_ARRAY,
		EXPR_TUPLE_UNIT,
		EXPR_TUPLE,
		//EXPR_LAMBDA,
		//EXPR_MATCH,
		EXPR_DO_BLOCK,
		EXPR_LOOP,
		EXPR_IF,
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
		EXPR_CONTINUE, // always !.
		EXPR_RETURN, // always !.
		EXPR_VOIDING, // evaluates expr, possible effects. discards return value, returning ()
		//EXPR_SIZEOF_TYPE, // is usize
        EXPR_FIELD, // named field and tuple field
		EXPR_LET,
		EXPR_UNREACHABLE, // TODO: unreachable
		EXPR_STRUCT_POSITIONAL,
		EXPR_STRUCT, // canonical representation
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
			loc_t type_loc; // type is stored inside `type`
		} d_cast;
		u64 d_integer;
		bool d_bool_lit;
		struct {
			hir_expr_t *expr;
			u16 field_idx; // (default -1) set for tuples, for fields after checking both are set 
			istr_t field;  // set for fields
		} d_field;
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
			hir_expr_t *ref;
			bool is_mut;
		} d_addr_of;
		struct {
			hir_expr_t *expr;
            enum : u8 {
                EXPR_K_INC,
                EXPR_K_DEC,
            } op;
		} d_postfix;
		struct {
			hir_expr_t *expr;
			 enum : u8 {
                EXPR_K_NOT,
                EXPR_K_SUB,
            } op;
		} d_prefix;
		struct {
			hir_expr_t *exprs; // all do blocks have at least one expr (before transformations), unless they become a EXPR_TUPLE_UNIT
			bool forward; // brk, set by parser meaning the last expr isn't a `brk` yet
			bool backward; // rep
			bool no_branch; // doesn't participate as branch target
		} d_do_block;
		struct {
			hir_expr_t *expr;
			bool forward;
			bool backward;
		} d_loop;
		struct {
			hir_expr_t *lhs;
			hir_expr_t *rhs;
			tok_t kind; // TODO: create enum kind
		} d_assign;
		struct {
			hir_expr_t *lhs;
			hir_expr_t *rhs;
			tok_t kind; // TODO: create enum kind
		} d_infix;
		/* struct {
			hir_rvar_t *args;
			hir_expr_t *expr;
		} d_lambda; */
		struct {
			hir_expr_t *expr;
			pattern_t *patterns;
			hir_expr_t *exprs;
		} d_match;
		struct {
			pattern_t pattern;
			hir_expr_t *expr;
		} d_let;
		struct {
			hir_expr_t *f;
			hir_expr_t *args; // f(...args)
		} d_call;
		hir_expr_t *d_tuple;
		hir_expr_t *d_array;
		struct {
			hir_expr_t *expr;
			u32 branch_level;
		} d_break;
		struct {
			u32 branch_level;
		} d_continue;
		struct {
			hir_expr_t *expr;
		} d_return;
		struct {
			hir_expr_t *cond;
			hir_expr_t *then;
			hir_expr_t *els; // can be none
		} d_if;
		struct {
			enum : u8 {
				UNREACHABLE_ASSERTION, // full debuginfo - generates an assertion, deleted on production
				UNREACHABLE_HEURISTIC, // no debuginfo - doesn't generate an assertion at all
				UNREACHABLE_UD2, // minimal debuginfo - unreachable, generates a trap always
				//
				//                         +-------------------------+-------------------------+
				//                         | debug                   | prod                    |
				// +-----------------------+-------------------------+-------------------------+
				// | UNREACHABLE_ASSERTION | assert(0)               | __builtin_unreachable() |
				// | UNREACHABLE_HEURISTIC | __builtin_unreachable() | __builtin_unreachable() |
				// | UNREACHABLE_UD2       | __builtin_trap()        | __builtin_trap()        |
				// +-----------------------+-------------------------+-------------------------+
			} kind;
		} d_unreachable;
		struct {
			hir_sf_t *fields; // type is expr->type
		} d_struct;
		struct {
			type_t struc;
			hir_expr_t *exprs;
		} d_struct_positional;
	};
};

// heapalloc
hir_expr_t *hir_dup(hir_expr_t expr);
