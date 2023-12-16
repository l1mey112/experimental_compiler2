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
typedef struct fs_file_t fs_file_t;
typedef struct fs_node_t fs_node_t;
typedef struct token_t token_t;
typedef u32 fs_rnode_t;
typedef u32 fs_rfile_t;
typedef u16 type_t;
typedef u32 istr_t;

typedef u32 ir_rblk_t;

istr_t sv_intern(u8 *sv, size_t len);
istr_t sv_move(const char *p);
const char *sv_from(istr_t str);
ptrdiff_t sv_index(const char *p);
u8 *alloc_scratch(size_t size);
void alloc_reset(u8 *p);

#define eprintf(...) fprintf(stderr, __VA_ARGS__)
#define eputs(v) fputs(v, stderr); fputc('\n', stderr)

#define MAYBE_UNUSED __attribute__((unused))
#define NORETURN __attribute__ ((noreturn))
#define ARRAYLEN(v) ((u32)(sizeof(v) / sizeof(*(v))))

void err_with_pos(loc_t loc, const char *fmt, ...)
	__attribute__((format(printf, 2, 3))) NORETURN;

void err_without_pos(const char *fmt, ...)
	__attribute__((format(printf, 1, 2))) NORETURN;

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
	#define assert_not_reached()  assert(0 && "assert_not_reached")
//#endif

static inline u32 ptrcpy(u8 *p, u8 *q, u32 len) {
	memcpy(p, q, len);
	return len;
}

#define TOK_X_KEYWORDS_LIST \
	X(TOK_DO, "do") \
	X(TOK_LOOP, "loop") \
	X(TOK_IO, "io")

// in specific order due to how operators are parsed
#define TOK_X_OPERATOR_LIST \
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
	X(TOK_BAND, "&") \
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

#define TOK_HAS_LIT(t) \
	((t) == TOK_IDENT || \
	(t) == TOK_INTEGER)

#define TOK_IS_PREFIX(t) \
	((t) == TOK_SUB || \
	(t) == TOK_NOT || \
	(t) == TOK_TILDE || \
	(t) == TOK_MUL || \
	(t) == TOK_BAND)

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
	(t) == TOK_BAND || \
	(t) == TOK_BOR || \
	(t) == TOK_XOR || \
	(t) == TOK_LSHIFT || \
	(t) == TOK_RSHIFT || \
	(t) == TOK_RUSHIFT || \
	(t) == TOK_DOT || \
	(t) == TOK_AS)

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
	fs_rfile_t file;
};

struct token_t {
	tok_t kind;
	loc_t loc;
	istr_t lit;
};

struct fs_node_t {
	const char *path;
	fs_rnode_t parent;
	fs_rnode_t *children;
	u32 children_len;
	u32 our_files;
	istr_t name; // shortname
	bool is_src_scanned; // importing a module where == true and our_files == 0 is an error
	bool is_main;
};

struct fs_file_t {
	const char *fp;
	u8 *data;
	size_t len;
	fs_rnode_t module;
};

struct err_diag_t {
	jmp_buf unwind;
	char err_string[256];
	// TODO: more err information
	// loc_t err_loc;
};

extern u32 fs_files_queue_len;
extern fs_file_t fs_files_queue[512];

fs_rfile_t fs_register_repl(void);
fs_file_t *fs_filep(fs_rfile_t ref);
fs_node_t *fs_nodep(fs_rnode_t ref);
const char *fs_module_symbol_sv(fs_rnode_t module, istr_t symbol);

void pentry(fs_rfile_t f);

#define SCOPE_ROOT 0

typedef u32 ir_rinst_t;
typedef u32 ir_rscope_t;
typedef struct ir_scope_t ir_scope_t;
typedef struct ir_inst_t ir_inst_t;

enum ir_ikind_t {
	INST_NOP,
	INST_PROC_DECL,
	INST_INTEGER_LITERAL,
	INST_CALL,
};

typedef enum ir_ikind_t ir_ikind_t;

// lexical basic blocks?
struct ir_scope_t {
	ir_rscope_t id;
	ir_rinst_t label_self; // continue here
	ir_rinst_t label_pred; // break here
	ir_rinst_t first;
	ir_rinst_t last;
	u32 len;
};

// TODO: define helper function for invalid loc_t

struct ir_inst_t {
	ir_ikind_t kind;
	ir_rinst_t id;
	ir_rinst_t next;
	ir_rinst_t prev;
	loc_t loc;
	type_t type;

	union {
		
	};
};

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
			fs_rnode_t module;
			istr_t name;
		} d_named;
		// type_t type_ref;
	};

	/* union {
		struct {
			bool atomic : 1;
			bool nullable : 1;
		};
		u8 flags;
	}; */
};

type_t type_new(tinfo_t typeinfo, loc_t *loc);
void types_dump(void);
const char *type_dbg_str(type_t type);