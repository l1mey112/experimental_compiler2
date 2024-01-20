#pragma once

#include <ctype.h>
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <assert.h>
#include <setjmp.h>
#include <string.h>
#include <stdlib.h>
#include "stb_ds.h"

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef float f32;
typedef double f64;

// there is no robust workaround, must pass type
#define arr(type, ...) ({                              \
		type *ret = NULL;                              \
		type v[] = {__VA_ARGS__};                      \
		if (sizeof(v)) {                               \
			arraddnptr(ret, sizeof(v) / sizeof(type)); \
			memcpy(ret, v, sizeof(v));                 \
		}                                              \
		ret;                                           \
	})

typedef struct loc_t loc_t;
typedef struct err_diag_t err_diag_t;
typedef struct token_t token_t;
typedef u16 type_t;
typedef u16 rmod_t;
typedef u16 rfile_t;
typedef struct mod_t mod_t;
typedef struct file_t file_t;

// a handle to an interned string
typedef u32 istr_t;

#define RMOD_NONE ((rmod_t)-1)

#define ISTR_NONE ((istr_t)-1)

istr_t sv_intern(u8 *sv, size_t len);
istr_t sv_move(const char *p);
const char *sv_from(istr_t str);
u8 *alloc_scratch(size_t size);
void alloc_reset(u8 *p);

#define eprintf(...) fprintf(stderr, __VA_ARGS__)
#define eputs(v) fputs(v, stderr); fputc('\n', stderr)

#define MAYBE_UNUSED __attribute__((unused))
#define NORETURN __attribute__ ((noreturn))
#define ARRAYLEN(v) ((u32)(sizeof(v) / sizeof(*(v))))

#ifdef __SANITIZE_ADDRESS__

void __sanitizer_print_stack_trace(void);
#undef assert
#define assert(expr) do {                                             \
	if (!(expr)) {                                                    \
		__sanitizer_print_stack_trace();                              \
		__assert_fail (#expr, __FILE__, __LINE__, __ASSERT_FUNCTION); \
	}                                                                 \
} while (0)

#endif

struct err_diag_t {
	jmp_buf unwind;
};

extern err_diag_t err_diag;

void print_diag_with_pos(const char *type, loc_t loc, const char *fmt, ...);
void print_diag_without_pos(const char *type, const char *fmt, ...);

#define err_unwind() longjmp(err_diag.unwind, 1)

#define print_err_with_pos(loc, fmt, ...) do {                 \
		print_diag_with_pos("error", loc, fmt, ##__VA_ARGS__); \
	} while (0)

#define print_err_without_pos(fmt, ...) do {                 \
		print_diag_without_pos("error", fmt, ##__VA_ARGS__); \
	} while (0)

#define print_hint_with_pos(loc, fmt, ...) do {               \
		print_diag_with_pos("hint", loc, fmt, ##__VA_ARGS__); \
	} while (0)

#define print_hint_without_pos(fmt, ...) do {               \
		print_diag_without_pos("hint", fmt, ##__VA_ARGS__); \
	} while (0)

#define err_with_pos(loc, fmt, ...) do {             \
		print_err_with_pos(loc, fmt, ##__VA_ARGS__); \
		err_unwind();                                \
	} while (0)

#define err_without_pos(fmt, ...) do {             \
		print_err_without_pos(fmt, ##__VA_ARGS__); \
		err_unwind();                              \
	} while (0)


#define ptr_cmp_literal(a, alen, b) ptr_cmp(a, alen, (u8 *)b, sizeof(b "") - 1)
static inline bool ptr_cmp(u8 *a, size_t alen, u8 *b, size_t blen) {
	if (alen != blen) {
		return false;
	}
	return memcmp(a, b, alen) == 0;
}

#define sv_cmp_literal(a, b) (strcmp(sv_from(a), b) == 0)

//#ifdef NDEBUG
//	#define assert_not_reached()  __builtin_unreachable()
//#else
	#define assert_not_reached()  assert(0 && "unreachable")
//#endif

static inline u32 ptrcpy(u8 *p, u8 *q, u32 len) {
	memcpy(p, q, len);
	return len;
}

// TYPE_UNKNOWN is something else and non concrete
#define TYPE_INFER ((type_t)-1)

#define TYPE_X_CONCRETE_LITERALS_LIST \
	X(TYPE_I8, "i8") \
	X(TYPE_I16, "i16") \
	X(TYPE_I32, "i32") \
	X(TYPE_I64, "i64") \
	X(TYPE_ISIZE, "isize") \
	X(TYPE_U8, "u8") \
	X(TYPE_U16, "u16") \
	X(TYPE_U32, "u32") \
	X(TYPE_U64, "u64") \
	X(TYPE_USIZE, "usize") \
	X(TYPE_F32, "f32") \
	X(TYPE_F64, "f64") \
	X(TYPE_BOOL, "bool")

#define TYPE_SIGNED_INTEGERS_START TYPE_I8
#define TYPE_SIGNED_INTEGERS_END TYPE_ISIZE
#define TYPE_UNSIGNED_INTEGERS_START TYPE_U8
#define TYPE_UNSIGNED_INTEGERS_END TYPE_USIZE

#define TYPE_X_CONCRETE_LIST \
	TYPE_X_CONCRETE_LITERALS_LIST \
	X(TYPE_UNIT, "()") \
	X(TYPE_BOTTOM, "!") \
	X(TYPE_UNDEFINED, "undefined")

#define TOK_X_KEYWORDS_LIST \
	X(TOK_I8, "i8") \
	X(TOK_I16, "i16") \
	X(TOK_I32, "i32") \
	X(TOK_I64, "i64") \
	X(TOK_ISIZE, "isize") \
	X(TOK_U8, "u8") \
	X(TOK_U16, "u16") \
	X(TOK_U32, "u32") \
	X(TOK_U64, "u64") \
	X(TOK_USIZE, "usize") \
	X(TOK_F32, "f32") \
	X(TOK_F64, "f64") \
	X(TOK_BOOL, "bool") \
	X(TOK_TRUE, "true") \
	X(TOK_FALSE, "false") \
	X(TOK_LET, "let") \
	X(TOK_UNDEFINED, "undefined") \
	X(TOK_DO, "do") \
	X(TOK_LOOP, "loop") \
	X(TOK_IO, "io") \
	X(TOK_IMPORT, "import") \
	X(TOK_PUB, "pub") \
	X(TOK_BREAK, "brk") \
	X(TOK_VOID, "void") \
	X(TOK_IF, "if") \
	X(TOK_ELSE, "else")

//	X(TOK_IN, "in")

// in specific order due to how operators are parsed
#define TOK_X_OPERATOR_LIST \
	X(TOK_TACK, "'") \
	X(TOK_UNDERSCORE, "_") \
	X(TOK_ARROW, "->") \
	X(TOK_INC, "++") \
	X(TOK_ASSIGN_ADD, "+=") \
	X(TOK_ADD, "+") \
	X(TOK_ASSIGN_SUB, "-=") \
	X(TOK_DEC, "--") \
	X(TOK_SUB, "-") \
	X(TOK_ASSIGN_MUL, "*=") \
	X(TOK_MUL, "*") \
	X(TOK_ASSIGN_DIV, "/=") \
	X(TOK_DIV, "/") \
	X(TOK_ASSIGN_MOD, "%=") \
	X(TOK_MOD, "%") \
	X(TOK_EQ, "==") \
	X(TOK_ASSIGN, "=") \
	X(TOK_NE, "!=") \
	X(TOK_NOT, "!") \
	X(TOK_LE, "<=") \
	X(TOK_LT, "<") \
	X(TOK_GE, ">=") \
	X(TOK_GT, ">") \
	X(TOK_AND, "&&") \
	X(TOK_OR, "||") \
	X(TOK_PIPE, "|") \
	X(TOK_TILDE, "~") \
	X(TOK_TRIPLE_DOTS, "...") \
	X(TOK_DOUBLE_DOTS, "..") \
	X(TOK_DOT, ".") \
	X(TOK_COMMA, ",") \
	X(TOK_OPAR, "(") \
	X(TOK_CPAR, ")") \
	X(TOK_OSQ, "[") \
	X(TOK_CSQ, "]") \
	X(TOK_COLON, ":") \
	X(TOK_QUESTION, "?") \
	X(TOK_SINGLE_AND, "&")

// X(TOK_LSHIFT, "<<")
// X(TOK_RSHIFT, ">>")
// X(TOK_RUSHIFT, ">>>")
// X(TOK_XOR, "^")
// X(TOK_BAND, "&")

#define TOK_HAS_LIT(t) \
	((t) == TOK_IDENT || \
	(t) == TOK_INTEGER)

#define TOK_IS_PREFIX(t) \
	((t) == TOK_SUB || \
	(t) == TOK_NOT || \
	(t) == TOK_SINGLE_AND)

// these tokens will always evaluate to bool
#define TOK_IS_COND(t) \
	((t) == TOK_EQ || \
	(t) == TOK_NE || \
	(t) == TOK_LT || \
	(t) == TOK_GT || \
	(t) == TOK_LE || \
	(t) == TOK_GE || \
	(t) == TOK_AND || \
	(t) == TOK_OR)

/* (t) == TOK_TILDE || \
	(t) == TOK_MUL || \
	(t) == TOK_BAND) */

#define TOK_IS_INFIX(t) \
	((t) == TOK_ADD || \
	(t) == TOK_SUB || \
	(t) == TOK_MUL || \
	(t) == TOK_DIV || \
	(t) == TOK_MOD || \
	(t) == TOK_ASSIGN || \
	(t) == TOK_ASSIGN_ADD || \
	(t) == TOK_ASSIGN_SUB || \
	(t) == TOK_ASSIGN_MUL || \
	(t) == TOK_ASSIGN_DIV || \
	(t) == TOK_ASSIGN_MOD || \
	(t) == TOK_EQ || \
	(t) == TOK_NE || \
	(t) == TOK_LT || \
	(t) == TOK_GT || \
	(t) == TOK_LE || \
	(t) == TOK_GE || \
	(t) == TOK_AND || \
	(t) == TOK_OR || \
	(t) == TOK_DOT || \
	(t) == TOK_COLON)

// TODO: impl pipe
// (t) == TOK_BAND ||
// (t) == TOK_BOR ||
// (t) == TOK_XOR ||
// (t) == TOK_LSHIFT ||
// (t) == TOK_RSHIFT ||
// (t) == TOK_RUSHIFT ||
	

#define TOK_X_LIST \
	X(TOK_NIL, "tok_nil") \
	X(TOK_EOF, "EOF") \
	X(TOK_IDENT, "identifier") \
	X(TOK_INTEGER, "integer") \
	TOK_X_KEYWORDS_LIST \
	TOK_X_OPERATOR_LIST

enum tok_t {
    #define X(name, _) name,
    TOK_X_LIST
    #undef X
};

// []i32   -> TYPE_SLICE
// [10]i32 -> TYPE_ARRAY

// types
enum ti_kind {
	#define X(name, _) name,
    TYPE_X_CONCRETE_LIST
    #undef X
	_TYPE_CONCRETE_MAX,
	//
	TYPE_UNKNOWN, // reference to be filled in later
	// TYPE_GENERIC, // generic (we don't make guarantees about polymorphism)
	TYPE_TUPLE,
	TYPE_FUNCTION,
	TYPE_CLOSURE,
	TYPE_CLOSURE_UNION,
	TYPE_PTR,
	TYPE_SLICE,
	TYPE_ARRAY,
	// TYPE_OPTION,
	// TYPE_ARRAY,
	// TYPE_ENUM,
	// TYPE_FUNCTION_PTR,
	// TYPE_STRUCT,
	// TYPE_FIXEDARRAY,
};

typedef enum ti_kind ti_kind;
typedef struct tinfo_t tinfo_t;

struct tinfo_t {
	ti_kind kind;

	bool is_named; // module.lit for named types

	union {
		struct {
			type_t *args;
			type_t ret;
			// bool effects and so on
		} d_fn;
		struct {
			type_t fn;
			// captured vars, etc
		} d_closure;
		struct {
			// type_t *elems; // array of closures
		} d_closure_union;
		struct  {
			type_t elem;
		} d_slice;
		struct {
			size_t length;
			type_t elem;
		} d_array;
		struct {
			type_t *elems;
		} d_tuple;
		struct {
			type_t ref;
			bool is_mut;
		} d_ptr;
		struct {
			rmod_t mod;
			istr_t name;
		} d_named;
	};
};

typedef enum tok_t tok_t;

struct loc_t {
	u32 line_nr;
	u32 col;
	u32 pos;
	u16 len;
	rfile_t file;
};

struct token_t {
	tok_t kind;
	loc_t loc;
	istr_t lit;
};

// TODO: define helper function for invalid loc_t
// TODO: fancy arena allocators? fuck that!
//       we're going for a quick and dirty bootstrap

struct mod_t {
	struct disk_t {
		bool is_stub;
		bool is_files_read;
		const char *path;
		//
		rmod_t parent;
		rmod_t *children;
		u32 children_len;
		istr_t name; // shortname
		u32 files_count;
	} on_disk;

	/* hir_scope_t toplevel;
	hir_node_t *exprs; */
};

struct file_t {
	const char *fp;
	u8 *data;
	size_t len;
	rmod_t mod;
};

extern u32 fs_files_queue_len;
extern file_t fs_files_queue[512];
extern u32 fs_mod_arena_len;
extern mod_t fs_mod_arena[128];

#define MOD_PTR(mod) (&fs_mod_arena[mod])
#define FILE_PTR(file) (&fs_files_queue[file])

void fs_set_entry_argp(const char *argp);
rfile_t fs_set_entry_repl(void); // will register current directory
rmod_t fs_register_root(const char *dp);
rmod_t fs_register_import(rmod_t src, istr_t *path, u32 path_len, loc_t onerror);
//
istr_t fs_module_symbol_sv(rmod_t mod, istr_t symbol);
const char *fs_module_symbol_str(rmod_t mod, istr_t symbol);
void fs_dump_tree(void);

void lir_process_file(rfile_t f);
/* void hir_check_module(rmod_t mod);
void hir_dump_module(rmod_t mod);
void hir_typed_desugar(void); */

type_t type_array_or_slice_to_slice(type_t type);
type_t type_new_inc_mul(type_t type, bool is_mut);
type_t type_new(tinfo_t typeinfo, loc_t *onerror);
tinfo_t *type_get(type_t type);
ti_kind type_kind(type_t type);
const char *type_dbg_str(type_t type);
const char *tok_op_str(tok_t tok);
const char *tok_dbg_str(token_t tok);

#define TI_GUARD(type, kind, lvalue) \
	(type_kind(type) == (kind) && ((lvalue) = type_get(type)))

typedef struct lir_symbol_t lir_symbol_t;
typedef struct lir_proc_t lir_proc_t;
typedef struct lir_block_t lir_block_t;
typedef struct lir_term_t lir_term_t;
typedef struct lir_inst_t lir_inst_t;
typedef struct lir_local_t lir_local_t;
typedef struct lir_term_sc_t lir_term_sc_t;
typedef struct lir_term_block_t lir_term_block_t;
typedef struct lir_term_pat_t lir_term_pat_t; // pattern match abstraction
typedef struct lir_lvalue_t lir_lvalue_t;
typedef struct lir_lvalue_proj_t lir_lvalue_proj_t;
typedef u32 lir_rblock_t;
typedef u32 lir_rlocal_t;

#define BLOCK_NONE ((lir_rblock_t)-1)

struct lir_local_t {
	type_t type;
	enum {
		LOCAL_MUT, // assigned multiple times
		LOCAL_IMM, // assigned once in all flows of control
		LOCAL_SSA, // assigned once (block args can only be SSA locals)
	} kind;

	// semantically, SSA locals are constants
	// they can't have their address taken or assigned to

	// TODO: optimised out or removed flag so debuggers
	//       like GDB can let the user know that

	// TODO: instead of duplicating debuginfo on SSA locals, make it point to the derived original local
	bool is_debuginfo;
	struct {
		loc_t loc;
		istr_t name;
	} d_debuginfo;
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
			lir_rlocal_t index;
		} d_index;
	};
};

// TODO: what you probably want in an lvalue
//
//       - we store the location of each projection (can reconstruct everything)
//       - we don't store the type of each
//       - we don't store the resultant type

struct lir_lvalue_t {
	lir_rlocal_t local; // TODO: global symbols
	lir_lvalue_proj_t *proj;
};

struct lir_inst_t {
	bool is_root; // for checker
	enum : u8 {
		INST_INTEGER_LIT,
		INST_BOOL_LIT,
		INST_ARRAY,
		INST_TUPLE_UNIT,
		INST_TUPLE,
		INST_UNDEFINED,
		//
		INST_ADD, // infix
		INST_SUB, // infix
		INST_MUL, // infix
		INST_DIV, // infix
		INST_MOD, // infix
		INST_EQ,  // infix
		INST_NE,  // infix
		ISNT_LE,  // infix
		ISNT_LT,  // infix
		ISNT_GE,  // infix
		ISNT_GT,  // infix
		INST_AND, // infix
		INST_OR,  // infix
		INST_NEG, // unary
		INST_NOT, // unary
		//
		INST_LVALUE,
		INST_ADDRESS_OF,
		//
		INST_CALL, // f(a, b, c)
		INST_CAST,
		INST_SIZEOF, // is usize
		_INST_MAX,
	} kind;

	lir_lvalue_t dest;

	union {
		istr_t d_integer_lit;
		bool d_bool_lit;
		lir_rlocal_t *d_array;
		lir_rlocal_t *d_tuple;
		lir_lvalue_t d_lvalue;
		struct {
			lir_lvalue_t lvalue;
			bool is_mut;
		} d_address_of;
		struct {
			istr_t qualified_name; // TODO: remove
		} d_symbol;
		struct {
			lir_rlocal_t lhs;
			lir_rlocal_t rhs;
		} d_infix;
		struct {
			lir_rlocal_t src;
		} d_unary;
		struct {
			lir_rlocal_t f;
			lir_rlocal_t *args;
		} d_call;
		struct {
			lir_rlocal_t src;
			type_t type;
		} d_cast;
	};
};

// recurse in the same direction
struct lir_term_pat_t {
	enum pattern_kind_t {
		PATTERN_LOCAL,
		PATTERN_UNDERSCORE,
		PATTERN_ARRAY,
		PATTERN_TUPLE,
		PATTERN_TUPLE_UNIT,
		PATTERN_INTEGER_LIT,
		PATTERN_BOOL_LIT,
	} kind;

	loc_t loc;

	union {
		lir_rlocal_t d_local; // index into local args
		struct {
			lir_term_pat_t *elems;
		} d_tuple;
		struct {
			lir_term_pat_t *elems;
			lir_term_pat_t *match; // single pattern, possible NULL
			bool match_lhs; // else, rhs
		} d_array;
		istr_t d_integer_lit;
		bool d_bool_lit;
	};
};

struct lir_term_block_t {
	lir_rblock_t block;
	lir_rlocal_t *args;
};

/* struct lir_term_sc_t {
	lir_lvalue_t value;
	lir_term_block_t block;
};
 */
struct lir_term_t {
	enum {
		TERM_UNINIT, // shouldn't be this
		TERM_GOTO,
		TERM_IF,
		TERM_SWITCH,
		TERM_RET,
		TERM_GOTO_PATTERN,
	} kind;

	// TODO: merge TERM_IF with TERM_SWITCH?

	union {
		lir_term_block_t d_goto;
		struct {
			lir_rlocal_t cond;
			lir_term_block_t then;
			lir_term_block_t els;
		} d_if;
		/* struct {
			lir_rvalue_t value;
			lir_term_sc_t *cases;
			lir_term_block_t default_block; // BLOCK_NONE for no default
		} d_switch; */
		struct {
			lir_rlocal_t value;
		} d_ret;
		//
		// abstractions/intrinsics
		//
		struct {
			lir_rlocal_t value;
			lir_rblock_t *blocks;
			lir_term_pat_t *patterns;
		} d_goto_pattern;
	};
};

struct lir_block_t {
	lir_rblock_t index; // self
	const char *debug_name;
	lir_inst_t *insts;
	lir_rlocal_t *args;
	lir_term_t term;
};

struct lir_proc_t {
	lir_block_t *blocks; // block 0 is always entry block
	lir_local_t *locals; // stack slots
};

// TODO: symbols into hashmap
struct lir_symbol_t {
	istr_t qualified_name;
	istr_t short_name;
	rmod_t mod;
	loc_t loc;

	enum : u8 {
		SYMBOL_VAR,
		SYMBOL_PROC,
	} kind;

	union {
		lir_proc_t d_proc;
	};
};

// search by qualified name
extern lir_symbol_t *symbols;

void lir_inst(lir_proc_t *proc, lir_rblock_t block, lir_inst_t inst);
// create local, local = inst
lir_rlocal_t lir_ssa_tmp_inst(lir_proc_t *proc, lir_rblock_t block, type_t type, loc_t *loc, lir_inst_t inst);

lir_rlocal_t lir_local_new(lir_proc_t *proc, lir_local_t local);
lir_rlocal_t lir_local_new_named(lir_proc_t *proc, istr_t name, loc_t loc, type_t type, bool is_mut);
lir_rblock_t lir_block_new(lir_proc_t *proc, const char *debug_name); // takes ownership of debug_name
lir_rlocal_t lir_block_new_arg(lir_proc_t *proc, lir_rblock_t block, type_t type, loc_t *loc);
void lir_block_arg_assign(lir_proc_t *proc, lir_rblock_t block, lir_rlocal_t local); // assert local is LOCAL_SSA
void lir_block_term(lir_proc_t *proc, lir_rblock_t block, lir_term_t term);
void lir_print_symbol(lir_symbol_t *symbol);
void lir_print_symbols(void);

// INFO: in the parser we assign around lvalues willy nilly.
//       there is a possibility for aliasing, but it doesn't
//       seem much of an issue for now.
//
//       multiple instructions may share the same lvalue

lir_lvalue_t lir_lvalue(lir_rlocal_t local);
void lir_lvalue_deref(lir_lvalue_t *lvalue, loc_t loc);
void lir_lvalue_struct_field(lir_lvalue_t *lvalue, loc_t loc, istr_t field);
void lir_lvalue_index_field(lir_lvalue_t *lvalue, loc_t loc, u16 field_idx);
void lir_lvalue_index(lir_lvalue_t *lvalue, loc_t loc, lir_rlocal_t index);

// INFO: spill for READING only, writing to this would cause weird behaviour
//       spill will return the local if it contains no projections
//       otherwise unwrap projections into a new local and return it
//
//       after a write to the original lvalue, the spilled local is invalidated
//       returns SSA locals
lir_rlocal_t lir_lvalue_spill(lir_proc_t *proc, lir_rblock_t block, lir_lvalue_t lvalue);
