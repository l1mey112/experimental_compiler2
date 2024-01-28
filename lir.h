#pragma once

#include "all.h"

typedef struct lir_proc_t lir_proc_t;
typedef struct lir_block_t lir_block_t;
typedef struct lir_term_t lir_term_t;
typedef struct lir_lvalue_t lir_lvalue_t;
typedef struct lir_lvalue_proj_t lir_lvalue_proj_t;
typedef u32 lir_rblock_t;

typedef struct lir_value_t lir_value_t;
typedef struct lir_stmt_t lir_stmt_t;

#define BLOCK_NONE ((lir_rblock_t)-1)
#define LOCAL_NONE ((rlocal_t)-1)
#define INST_NONE ((u32)-1)

struct local_t {
	type_t type;
	enum : u8 {
		LOCAL_MUT, // assigned multiple times
		LOCAL_IMM, // assigned once in all flows of control
	} kind;

	// TODO: optimised out or removed flag so debuggers
	//       like GDB can let the user know that

	// TODO: we desperately need a NULL value for locations or
	//       just revamp locations entirely which we should
	loc_t loc;
	istr_t name;
};

#define PROJ_IDX_INVALID ((u16)-1)

struct lir_lvalue_proj_t {
	enum : u8 {
		PROJ_DEREF,
		PROJ_FIELD,
		PROJ_INDEX,
	} kind;

	loc_t loc;

	union {
		struct {
			u16 field_idx; // (default -1) set for tuples, after checking both are set 
			istr_t field; // set for fields
		} d_field;
		struct {
			rlocal_t index;
		} d_index;
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
		//
		VALUE_CTRL_TEMP, // control flow evaluating to a value, spliced away after type check
		VALUE_CTRL_NORETURN, // construction of ! type, deleted before analysis
	} kind;

	// sizeof will be lowered into a constant
	// after visiting a symbol it's fields is reordered based on attributes
	// and then when visiting sizeof it will be lowered into a constant

	type_t type; // value numbering will take type into account
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
		//
		// control flow that can return values it's prepared seperately
		// by the parser, then expanded and spliced into place 
		//
		struct {
			rlocal_t local; // imm local that is the target of assignments
			lir_rblock_t entry; // the exit block is the block we're currently in right now
			// splice: term(DOM(current)) = goto(entry)
		} d_ctrl_temp;
	};
};

struct lir_stmt_t {
	// all statements in LIR are assignments
	// l = f      (lvalue assignment)          -> lir_assign()
	// _ = f      (user/explicit discard)      -> lir_discard()
	// f          (compiler ignore value)      -> lir_ignore()
	enum : u8 {
		STMT_LVALUE,
		STMT_DISCARD,
		STMT_IGNORE,
	} kind;
	
	// TODO: flags for brk operations

	union {
		lir_lvalue_t dest;
	};

	lir_value_t value;
};


struct lir_term_t {
	enum {
		TERM_UNINIT, // shouldn't be this
		TERM_GOTO,
		TERM_RET,
		TERM_GOTO_PATTERN,
	} kind;

	union {
		lir_rblock_t d_goto;
		struct {
			lir_value_t value;
		} d_ret;
		struct {
			lir_value_t value;
			lir_rblock_t *blocks;
			pattern_t *patterns;
		} d_goto_pattern;
	};
};

struct lir_block_t {
	const char *debug_name;
	lir_stmt_t *stmts;
	lir_term_t term;
	bool visited; // easier to store here
};

struct lir_proc_t {
	lir_block_t *blocks; // block 0 is always entry block
};

// l = f      (lvalue assignment)
void lir_assign(lir_proc_t *proc, lir_rblock_t block, lir_lvalue_t lvalue, lir_value_t value);
// _ = f      (user/explicit discard)
void lir_discard(lir_proc_t *proc, lir_rblock_t block, lir_value_t value);
// f          (compiler ignore value)
void lir_ignore(lir_proc_t *proc, lir_rblock_t block, lir_value_t value);

// local
void lir_assign_local(lir_proc_t *proc, lir_rblock_t block, rlocal_t local, lir_value_t value);

// local
lir_value_t lir_local_value(rlocal_t local, loc_t loc);
lir_lvalue_t lir_local_lvalue(rlocal_t local, loc_t loc);

// malloc and copy
lir_value_t *lir_dup(lir_value_t value);

// construct value from lvalue
lir_value_t lir_value_lvalue(lir_lvalue_t lvalue);

// returns inst idx
// u32 lir_inst(lir_proc_t *proc, lir_rblock_t block, lir_inst_t inst);
// create local, local = inst
// rlocal_t lir_ssa_tmp_inst(lir_proc_t *proc, lir_rblock_t block, type_t type, loc_t loc, lir_inst_t inst);

//void lir_inst_insert(lir_proc_t *proc, lir_rblock_t block, u32 inst_idx, lir_inst_t inst);

rlocal_t lir_local_new(lir_proc_t *proc, local_t local);
lir_rblock_t lir_block_new(lir_proc_t *proc, const char *debug_name); // takes ownership of debug_name
void lir_block_term(lir_proc_t *proc, lir_rblock_t block, lir_term_t term);
void lir_print_symbol(sym_t *symbol);
void lir_print_symbols(void);

//void lir_inst_lvalue(lir_proc_t *proc, lir_rblock_t block, lir_lvalue_t dest, rlocal_t src, loc_t loc);

// having named field tuples and options would be pretty nice

// INST_NONE for none
//u32 lir_find_inst_ssa_block(lir_proc_t *proc, lir_rblock_t block, rlocal_t local);

// INFO: in the parser we assign around lvalues willy nilly.
//       there is a possibility for aliasing, but it doesn't
//       seem much of an issue for now.
//
//       multiple instructions may share the same lvalue
//
lir_lvalue_t lir_lvalue(rlocal_t local, loc_t loc);
lir_lvalue_t lir_lvalue_sym(rsym_t sym, loc_t loc);
void lir_lvalue_deref(lir_lvalue_t *lvalue, loc_t loc);
void lir_lvalue_struct_field(lir_lvalue_t *lvalue, loc_t loc, istr_t field);
void lir_lvalue_index_field(lir_lvalue_t *lvalue, loc_t loc, u16 field_idx);
void lir_lvalue_index(lir_lvalue_t *lvalue, loc_t loc, rlocal_t index);

// remove an instruction from a block, returning it
/* lir_inst_t lir_inst_pop(lir_proc_t *proc, lir_rblock_t block, u32 inst);

// INFO: spill for READING only, writing to this would cause weird behaviour
//       spill will return the local if it contains no projections
//       otherwise unwrap projections into a new local and return it
//
//       ~~after a write to the original lvalue, the spilled local is invalidated~~
//
// UPDATE: spills locals with projections
//         spills mutable locals to SSA values
//
//         this this is to guarantee a specific order of evaluation when lvalues
//         are converted into locals for reading
//
//         it is safe to assume that all arguments to instructions are IMMUTABLE
//         and without side effects from their neighbouring operands.
//
rlocal_t lir_lvalue_spill(lir_proc_t *proc, lir_rblock_t block, lir_lvalue_t lvalue); */