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
	X(TOK_NEQ, "!=") \
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
	(t) == TOK_NEQ || \
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
	(t) == TOK_NEQ || \
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
	TYPE_VAR, // typevars
	// TYPE_GENERIC, // generic (we don't make guarantees about polymorphism)
	TYPE_TUPLE,
	TYPE_FN,
	TYPE_PTR,
	TYPE_SLICE,
	TYPE_ARRAY,
	// TYPE_OPTION,
	// TYPE_ARRAY,
	// TYPE_ENUM,
	// TYPE_FN_PTR,
	// TYPE_STRUCT,
	// TYPE_FIXEDARRAY,
};

typedef enum ti_kind ti_kind;
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

typedef struct hir_scope_t hir_scope_t;
typedef u32 hir_rvar_t;
typedef struct hir_var_t hir_var_t;
typedef struct hir_node_t hir_node_t;
typedef struct hir_pattern_t hir_pattern_t;

// TODO: define helper function for invalid loc_t
// TODO: fancy arena allocators? fuck that!
//       we're going for a quick and dirty bootstrap

extern hir_var_t *hir_vars;

#define VAR_PTR(id) (&hir_vars[id])

struct hir_var_t {
	istr_t name;
	loc_t loc;
	type_t type;
	bool is_in_decl; // ignore var, used in parser
	bool is_mut; // is mut
	bool is_proc_decl; // is a desugared proc decl
	bool is_arg; // is lambda argument
	bool is_pub; // is exposed to other modules
};

struct hir_scope_t {
	hir_scope_t *parent;
	hir_rvar_t *locals;
};

struct hir_pattern_t {
	enum pattern_kind_t {
		PATTERN_VAR,
		PATTERN_UNDERSCORE,
		PATTERN_ARRAY,
		PATTERN_TUPLE,
		PATTERN_TUPLE_UNIT,
		PATTERN_INTEGER_LIT,
	} kind;

	loc_t loc;

	union {
		hir_rvar_t d_var;
		struct {
			hir_pattern_t *elems;
		} d_tuple;
		struct {
			hir_pattern_t *elems;
			hir_pattern_t *match; // single pattern, possible NULL
			bool match_lhs; // else, rhs
		} d_array;
		istr_t d_integer_lit;
	};
};

struct hir_node_t {
	enum node_kind_t {
		NODE_LET_DECL,
		NODE_LAMBDA,
		NODE_MATCH,
		NODE_DO_BLOCK,
		NODE_LOOP,
		NODE_IF, // in checker, if type == TYPE_BOOL it is a desugared logic op
		NODE_ASSIGN,
		NODE_INFIX,
		NODE_POSTFIX, // TODO: why do we even have these stupid little ops, why can't we just desugar??
		NODE_PREFIX,
		NODE_DEREF,
		NODE_ADDR_OF,
		NODE_INDEX,
		NODE_SLICE,
		NODE_INTEGER_LIT,
		NODE_BOOL_LIT,
		NODE_VAR,
		NODE_GLOBAL_UNRESOLVED,
		NODE_SYM,
		NODE_SYM_UNRESOLVED,
		NODE_CAST,
		NODE_CALL,
		NODE_TUPLE_UNIT,
		NODE_TUPLE,
		NODE_ARRAY_LIT,
		NODE_BREAK_UNIT, // always !. ignore expr
		NODE_BREAK_INFERRED, // always !. inserted by the parser
		NODE_BREAK, // always !. expr type never (), otherwise it would be NODE_BREAK_UNIT
		NODE_UNDEFINED,
		NODE_VOIDING, // evaluates expr, possible effects. discards return value, returning ()
		NODE_SIZEOF_TYPE, // is usize
		NODE_TUPLE_FIELD,
	} kind;
	
	type_t type;
	loc_t loc;

	// #define MATCH_DO_BLOCK(expr, v, t) \
	// 	case NODE_DO_BLOCK: {typeof((expr).do_block) *v = &(expr).do_block; t break;}

	union {
		hir_rvar_t d_var;
		istr_t d_global_unresolved;
		hir_node_t *d_voiding;
		hir_node_t *d_deref;
		type_t d_sizeof_type;
		struct {
			hir_node_t *expr;
			size_t field;
		} d_tuple_field;
		struct {
			hir_node_t *expr;
			hir_node_t *index;
		} d_index;
		struct {
			hir_node_t *expr;
			hir_node_t *lo; // possible NULLs
			hir_node_t *hi; // possible NULLs
		} d_slice;
		struct {
			hir_node_t *lhs;
			hir_node_t *rhs;
		} d_assign;
		struct {
			hir_node_t *ref;
			bool is_mut;
		} d_addr_of;
		struct {
			rmod_t mod;
			istr_t name;
		} d_sym_unresolved;
		struct {
			rmod_t mod;
			hir_rvar_t var;
		} d_sym;
		struct {
			hir_node_t *expr;
			tok_t kind;
		} d_postfix;
		struct {
			hir_node_t *expr;
			tok_t kind;
		} d_prefix;
		struct {
			hir_scope_t *scope; // scopes are pointers here for a reason
			hir_node_t *exprs; // all do blocks have at least one expr, unless they become a NODE_TUPLE_UNIT
			u8 blk_id;
		} d_do_block;
		struct {
			hir_node_t *expr;
			u8 blk_id;
		} d_loop;
		struct {
			hir_node_t *lhs;
			hir_node_t *rhs;
			tok_t kind;
		} d_infix;
		hir_node_t *d_cast;
		struct {
			istr_t lit;
			bool negate;
		} d_integer_lit;
		bool d_bool_lit;
		struct {
			hir_rvar_t *args;
			hir_scope_t *scope;
			hir_node_t *expr;
		} d_lambda;
		struct {
			hir_node_t *expr;
			hir_scope_t *scopes;
			hir_pattern_t *patterns;
			hir_node_t *exprs;
		} d_match;
		struct {
			hir_pattern_t pattern;
			hir_node_t *expr;
		} d_let_decl;
		struct {
			hir_node_t *f;
			hir_node_t *arg;
		} d_call;
		struct {
			hir_node_t *elems;
		} d_tuple;
		struct {
			hir_node_t *elems;
		} d_array_lit;
		struct {
			hir_node_t *expr; // in use with NODE_BREAK*
			u8 blk_id;
		} d_break;
		struct {
			hir_node_t *cond;
			hir_node_t *then;
			hir_node_t *els;
		} d_if;
	};
};

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

	hir_scope_t toplevel;
	hir_node_t *exprs;
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

void hir_process_file(rfile_t f);
void hir_check_module(rmod_t mod);
void hir_dump_module(rmod_t mod);
void hir_typed_desugar(void);

hir_node_t *hir_memdup(hir_node_t node);

typedef struct tinfo_t tinfo_t;

struct tinfo_t {
	ti_kind kind;

	bool is_named; // module.lit for named types

	union {
		type_t d_typevar_type;
		struct {
			// curried representation
			type_t arg;
			type_t ret;
			// executing this function will cause an `io` effect
			// bool is_effect;
		} d_fn;
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

type_t type_array_or_slice_to_slice(type_t type);
type_t type_new_inc_mul(type_t type, bool is_mut);
type_t typevar_new(void);
void typevar_replace(type_t typevar, type_t type);
type_t type_new(tinfo_t typeinfo, loc_t *onerror);
tinfo_t *type_get_raw(type_t type);
tinfo_t *type_get(type_t type);
ti_kind type_kind(type_t type);
ti_kind type_kind_raw(type_t type);
type_t type_underlying(type_t a);
bool type_eq(type_t a, type_t b);
void types_dump(void);
const char *type_dbg_str(type_t type);
const char *tok_op_str(tok_t tok);
const char *tok_dbg_str(token_t tok);

typedef struct lir_proc_t lir_proc_t;
typedef struct lir_block_t lir_block_t;
typedef struct lir_term_t lir_term_t;
typedef struct lir_inst_t lir_inst_t;
typedef struct lir_value_t lir_value_t;
typedef u32 lir_rvalue_t;
typedef u32 lir_rblock_t;
typedef u32 lir_rinst_t;

#define LIR_VALUE_NONE ((lir_rvalue_t)-1)

// v0 = 10
struct lir_value_t {
	lir_rvalue_t index; // self
	type_t type;
	loc_t loc; // more debuginfo
};

struct lir_inst_t {
	lir_rvalue_t target; // LIR_VALUE_NONE for none
	// no need for self index

	enum {
		INST_INTEGER_LITERAL,
		INST_ADD,
		INST_SUB,
		INST_MUL,
		INST_DIV,
	} kind;

	union {
		struct {
			istr_t lit;
		} d_integer_literal;
		struct {
			lir_rvalue_t lhs;
			lir_rvalue_t rhs;
		} d_infix;
	};
};

struct lir_term_t {
	enum {
		TERM_GOTO,
		TERM_RET,
	} kind;

	union {
		struct {
			lir_rblock_t target;
		} d_goto;
		struct {
			lir_rvalue_t value;
		} d_ret;
	};
};

struct lir_block_t {
	lir_rblock_t index; // self
	lir_inst_t *insts;
	lir_rvalue_t *args;
	lir_term_t term;
};

struct lir_proc_t {
	lir_block_t *blocks; // block 0 is always entry block
	lir_value_t *values;
};

lir_rvalue_t lir_value_new(lir_proc_t *proc, lir_value_t value);
lir_rblock_t lir_block_new(lir_proc_t *proc, lir_block_t block);
lir_rvalue_t lir_block_arg(lir_proc_t *proc, lir_rblock_t block, lir_value_t value);
void lir_block_term(lir_proc_t *proc, lir_rblock_t block, lir_term_t term);
lir_rinst_t lir_inst_new(lir_proc_t *proc, lir_rblock_t block, lir_inst_t inst);
lir_rvalue_t lir_inst_value(lir_proc_t *proc, lir_rblock_t block, type_t type, loc_t loc, lir_inst_t inst);