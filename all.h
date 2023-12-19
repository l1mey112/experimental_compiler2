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

#define ISTR_NONE ((istr_t)-1)
#define ISTR_T_MASK 0x7fffffff
#define ISTR_SET_T(v) ((v) | 0x80000000)
#define ISTR_IS_T(v) ((v) & 0x80000000)

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


#define sv_cmp_literal(a, alen, b) sv_cmp(a, alen, (u8 *)b, sizeof(b "") - 1)
static inline bool sv_cmp(u8 *a, size_t alen, u8 *b, size_t blen) {
	if (alen != blen) {
		return false;
	}
	return memcmp(a, b, alen) == 0;
}

//#ifdef NDEBUG
//	#define assert_not_reached()  __builtin_unreachable()
//#else
	#define assert_not_reached()  assert(0 && "unreachable")
//#endif

static inline u32 ptrcpy(u8 *p, u8 *q, u32 len) {
	memcpy(p, q, len);
	return len;
}

#define TOK_X_KEYWORDS_LIST \
	X(TOK_DO, "do") \
	X(TOK_LOOP, "loop") \
	X(TOK_IO, "io") \
	X(TOK_MUT, "mut")

// in specific order due to how operators are parsed
#define TOK_X_OPERATOR_LIST \
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
	X(TOK_LSHIFT, "<<") \
	X(TOK_RSHIFT, ">>") \
	X(TOK_RUSHIFT, ">>>") \
	X(TOK_LE, "<=") \
	X(TOK_LT, "<") \
	X(TOK_GE, ">=") \
	X(TOK_GT, ">") \
	X(TOK_AND, "&&") \
	X(TOK_OR, "||") \
	X(TOK_PIPE, "|") \
	X(TOK_XOR, "^") \
	X(TOK_TILDE, "~") \
	X(TOK_DOT, ".") \
	X(TOK_COMMA, ",") \
	X(TOK_OPAR, "(") \
	X(TOK_CPAR, ")") \
	X(TOK_OSQ, "[") \
	X(TOK_CSQ, "]") \
	X(TOK_DOUBLE_COLON, "::") \
	X(TOK_COLON, ":") \
	X(TOK_QUESTION, "?")

//	X(TOK_BAND, "&") \

#define TOK_HAS_LIT(t) \
	((t) == TOK_IDENT || \
	(t) == TOK_INTEGER)

#define TOK_IS_PREFIX(t) \
	((t) == TOK_SUB || \
	(t) == TOK_NOT)

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
	(t) == TOK_XOR || \
	(t) == TOK_LSHIFT || \
	(t) == TOK_RSHIFT || \
	(t) == TOK_RUSHIFT || \
	(t) == TOK_DOT)

// TODO: impl pipe
// (t) == TOK_BAND || \
// (t) == TOK_BOR || \
	

#define TOK_X_LIST \
	X(TOK_UNDEFINED, "tok_undefined") \
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

typedef struct ir_scope_t ir_scope_t;
typedef u32 ir_rvar_t;
typedef struct ir_var_t ir_var_t;
typedef struct ir_node_t ir_node_t;
typedef struct ir_pattern_t ir_pattern_t;

// TODO: define helper function for invalid loc_t
// TODO: fancy arena allocators? fuck that!
//       we're going for a quick and dirty bootstrap

#define VAR_IS_T(v) ISTR_IS_T((v).name)
#define VAR_PTR_IS_T(v) ISTR_IS_T((v)->name)

struct ir_var_t {
	istr_t name;
	loc_t loc;
	type_t type;
};

struct ir_scope_t {
	ir_scope_t *parent;
	ir_rvar_t *locals;
};

// TODO: match on tuples and arrays
struct ir_pattern_t {
	enum pattern_kind_t {
		PATTERN_VAR,
		PATTERN_UNDERSCORE,
		PATTERN_TUPLE,
		PATTERN_INTEGER_LIT,
	} kind;

	loc_t loc;

	union {
		ir_rvar_t d_var;
		struct {
			ir_pattern_t *elems;
			u32 len;
		} d_tuple;
		istr_t d_integer_lit;
	};
};

struct ir_node_t {
	enum node_kind_t {
		NODE_PROC_DECL,
		NODE_VAR_DECL,
		NODE_DO_BLOCK,
		NODE_INFIX,
		NODE_POSTFIX,
		NODE_PREFIX,
		NODE_INTEGER_LIT,
		NODE_VAR,
		NODE_SYM,
		NODE_CAST,
		NODE_CALL,
		NODE_TUPLE_UNIT,
		NODE_TUPLE,
		NODE_BREAK_UNIT,
		NODE_BREAK_INFERRED,
		NODE_MUT,
	} kind;
	
	type_t type;
	loc_t loc;

	// #define MATCH_DO_BLOCK(expr, v, t) \
	// 	case NODE_DO_BLOCK: {typeof((expr).do_block) *v = &(expr).do_block; t break;}

	union {
		ir_rvar_t d_var;
		istr_t d_sym;
		ir_node_t *d_mut;
		struct {
			ir_node_t *expr;
			tok_t kind;
		} d_postfix;
		struct {
			ir_node_t *expr;
			tok_t kind;
		} d_prefix;
		struct {
			ir_scope_t scope;
			istr_t label; // -1 for none
			ir_node_t *exprs;
		} d_do_block;
		struct {
			ir_node_t *lhs;
			ir_node_t *rhs;
			tok_t kind;
		} d_infix;
		ir_node_t *d_cast;
		struct {
			istr_t lit;
			bool negate;
		} d_integer_lit;
		struct {
			ir_rvar_t var;
			ir_scope_t *scopes;
			//
			ir_pattern_t *patterns; // NULL meaning single expr single scope
			ir_node_t *exprs;
		} d_proc_decl;
		struct {
			ir_rvar_t lhs;
			ir_node_t *rhs;
		} d_var_decl;
		struct {
			ir_node_t *f;
			ir_node_t *arg;
		} d_call;
		struct {
			ir_node_t *elems;
		} d_tuple;
		struct {
			ir_node_t *expr;
		} d_break;
	};
};

struct mod_t {
	struct disk_t {
		bool is_stub;
		const char *path;
		//
		rmod_t parent;
		rmod_t *children;
		u32 children_len;
		istr_t name; // shortname
		u32 files_count;
	} on_disk;

	ir_var_t *vars;
	ir_scope_t toplevel;
	ir_node_t *exprs;
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
rmod_t fs_register_import(rmod_t src, const char *fp, loc_t onerror_loc);
//
istr_t fs_module_symbol_sv(rmod_t mod, istr_t symbol);
const char *fs_module_symbol_str(rmod_t mod, istr_t symbol);

void pentry(rfile_t f);
void ir_dump_module(rmod_t mod);

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

#define TYPE_X_CONCRETE_LIST \
	TYPE_X_CONCRETE_LITERALS_LIST \
	X(TYPE_UNIT, "()") \
	X(TYPE_BOTTOM, "!")

// types
enum ti_kind {
	#define X(name, _) name,
    TYPE_X_CONCRETE_LIST
    #undef X
	_TYPE_CONCRETE_MAX,
	//
	TYPE_UNKNOWN, // reference to be filled in later
	TYPE_TUPLE,
	TYPE_FN,
	// TYPE_PTR,
	// TYPE_OPTION,
	// TYPE_ARRAY,
	// TYPE_ENUM,
	// TYPE_FN_PTR,
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
			// curried representation
			type_t arg;
			type_t ret;
		} d_fn;
		struct {
			type_t *elems;
			u8 len;
		} d_tuple;
		struct {
			rmod_t mod;
			istr_t name;
		} d_named;

		// type_t type_ref;
	};
};

type_t type_new(tinfo_t typeinfo, loc_t *loc);
void types_dump(void);
const char *type_dbg_str(type_t type);