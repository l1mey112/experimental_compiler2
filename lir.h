#pragma once

#include "all.h"

typedef struct lir_lvalue_t lir_lvalue_t;
typedef struct lir_lvalue_proj_t lir_lvalue_proj_t;
typedef struct lir_stmt_t lir_stmt_t;
typedef struct lir_lvalue_t lir_lvalue_t;
typedef struct lir_value_t lir_value_t;
typedef struct lir_proc_t lir_proc_t;

#define PROJ_IDX_INVALID ((u16)-1)

struct lir_lvalue_proj_t {
	enum : u8 {
		PROJ_DEREF,
		PROJ_FIELD,
		PROJ_INDEX,
	} kind;

	loc_t loc;

	union {
		/* struct {
			u16 field_idx; // (default -1) set for tuples, after checking both are set 
			istr_t field; // set for fields
		} d_field;
		struct {
			rlocal_t index;
		} d_index; */
	};
};

// TODO: what you probably want in an lvalue
//
//       - we store the location of each projection (can reconstruct everything)
//       - we don't store the type of each
//       - we don't store the resultant type

struct lir_lvalue_t {
	union {
		rlocal_t local;
		rsym_t symbol;
	};
	loc_t loc;
	bool is_sym;
	lir_lvalue_proj_t *proj;
};

struct lir_value_t {
	enum : u8 {
		VALUE_INTEGER_LIT,
		VALUE_BOOL_LIT,
		VALUE_ARRAY,
		VALUE_TUPLE_UNIT,
		VALUE_TUPLE,
		//
		VALUE_ADD, // infix
		VALUE_SUB, // infix
		VALUE_MUL, // infix
		VALUE_DIV, // infix
		VALUE_MOD, // infix
		VALUE_EQ,  // infix
		VALUE_NE,  // infix
		VALUE_LE,  // infix
		VALUE_LT,  // infix
		VALUE_GE,  // infix
		VALUE_GT,  // infix
		VALUE_AND, // infix
		VALUE_OR,  // infix
		VALUE_NEG, // unary
		VALUE_NOT, // unary
		//
		VALUE_ADDRESS_OF,
		VALUE_SLICE, // slices aren't lvalues
		//
		VALUE_CALL, // f(a, b, c)
		VALUE_CAST,
		VALUE_SIZEOF, // is usize
		VALUE_LVALUE,
	} kind;

	// sizeof will be lowered into a constant
	// after visiting a symbol it's fields is reordered based on attributes
	// and then when visiting sizeof it will be lowered into a constant

	// TODO: where do you put the types?

	type_t type;
	loc_t loc;

	union {
		// u64 d_integer_lit;
		istr_t d_integer_lit; // we need to know types really
		bool d_bool_lit;
		lir_value_t *d_array;
		lir_value_t *d_tuple;
		lir_lvalue_t d_lvalue;
		struct {
			lir_lvalue_t lvalue;
			bool is_mut;
		} d_address_of;
		struct {
			lir_value_t *src;
			lir_value_t *lo; // possible NULLs (LOCAL_NONE)
			lir_value_t *hi; // possible NULLs (LOCAL_NONE)
		} d_slice;
		struct {
			lir_value_t *lhs;
			lir_value_t *rhs;
		} d_infix;
		struct {
			lir_value_t *src;
		} d_unary;
		struct {
			lir_value_t *f;
			lir_value_t *args;
		} d_call;
		struct {
			lir_value_t *src; // `type` filled in as cast type dest
		} d_cast;
	};
};

struct lir_stmt_t {
	// l = f      (lvalue assignment)
	// f          (ignore)
	// ...control flow
	enum : u8 {
		STMT_ASSIGN,  // dest = d_value
		STMT_DISCARD, // dest = d_value
		STMT_RETURN,  // return d_value
		// STMT_IF,
	} kind;
	
	// TODO: flags for brk operations

	union {
		lir_lvalue_t dest;
	};

	union {
		lir_value_t d_value;
	};
};

struct lir_proc_t {
	lir_stmt_t *stmts; // TODO: figure out if you need a `lir_block_t`
};

lir_value_t *lir_dup(lir_value_t value);
void lir_stmt(lir_stmt_t **stmts, lir_stmt_t stmt);
