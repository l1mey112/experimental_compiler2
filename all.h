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

#define LOC_NONE ((loc_t){})

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

#define TYPE_INFER ((type_t)-1)

#include "def.h"

enum tok_t {
    #define X(name, _) name,
    TOK_X_LIST
    #undef X
};

typedef enum tok_t tok_t;

// TODO(zakazaka): u32 start u32 end is better (compute later)
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

// []i32   -> TYPE_SLICE
// [10]i32 -> TYPE_ARRAY

// types
enum ti_kind {
	#define X(name, _) name,
    TYPE_X_CONCRETE_LIST
    #undef X
	_TYPE_CONCRETE_MAX,
	//
	// TODO: change TYPE_UNKNOWN to type symbol referencing a symbol
	//       there is no "filling in" with the type table, only symbol table
	// TYPE_UNKNOWN, // reference to be filled in later
	// TYPE_GENERIC, // generic (we don't make guarantees about polymorphism)
	TYPE_TUPLE,
	TYPE_FUNCTION,
	TYPE_CLOSURE,
	TYPE_CLOSURE_UNION,
	TYPE_PTR,
	TYPE_SLICE,
	TYPE_ARRAY,
	TYPE_SYMBOL, // or ALIAS ??
	// TYPE_OPTION,
	// TYPE_ARRAY,
	// TYPE_ENUM,
	// TYPE_FUNCTION_PTR,
	// TYPE_STRUCT,
	// TYPE_FIXEDARRAY,
};

typedef struct sym_t sym_t;
typedef u32 rsym_t;

enum tsymbol_kind {
	TYPESYMBOL_STRUCT,
};

typedef enum ti_kind ti_kind;
typedef struct tinfo_t tinfo_t;
typedef struct tsymbol_sf_t tsymbol_sf_t;

typedef struct tsymbol_t tsymbol_t;
typedef enum tsymbol_kind tsymbol_kind;

// named types are typesymbols.
// named struct types aren't interned, they're just a list of fields

struct tsymbol_sf_t {
	istr_t field;
	type_t type;
	loc_t field_loc;
	loc_t type_loc;
};

struct tsymbol_t {
	tsymbol_kind kind;

	loc_t name_loc;

	union {
		struct {
			tsymbol_sf_t *fields;
		} d_struct;
	};
};

struct tinfo_t {
	ti_kind kind;

	union {
		rsym_t d_symbol;
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
			size_t length; // TODO: length or constant symbol?
			type_t elem;
		} d_array;
		type_t *d_tuple;
		struct {
			type_t ref;
			bool is_mut;
		} d_ptr;
	};
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
extern tinfo_t types[1024];
extern u32 type_len;

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

void compiler_process_file(rfile_t f);
void compiler_pass_all(void);

/* void hir_check_module(rmod_t mod);
void hir_dump_module(rmod_t mod);
void hir_typed_desugar(void); */

type_t type_array_or_slice_to_slice(type_t type);
type_t type_array_or_slice_elem(type_t type);
type_t type_new_inc_mul(type_t type, bool is_mut);
type_t type_new(tinfo_t typeinfo);
tinfo_t *type_get(type_t type);
ti_kind type_kind(type_t type);
const char *type_dbg_str(type_t type);
const char *tok_op_str(tok_t tok);
const char *tok_dbg_str(token_t tok);

// only checks for literals
bool type_is_literal_number(type_t type);
// checks for literals
bool type_is_number(type_t type);
// does not check for literals
bool type_is_integer(type_t type);
// does not check for literals
bool type_is_float(type_t type);
bool type_is_signed(type_t type);
bool type_is_unsigned(type_t type);

#define TI_GUARD(type, kind, lvalue) \
	(type_kind(type) == (kind) && ((lvalue) = type_get(type)))

typedef struct local_t local_t;
typedef u32 rlocal_t;

typedef struct pattern_t pattern_t;

// recurse in the same direction
struct pattern_t {
	enum {
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
		rlocal_t d_local; // index into local args
		pattern_t *d_tuple;
		struct {
			pattern_t *elems;
			pattern_t *match; // single pattern, possible NULL
			bool match_lhs; // else, rhs
		} d_array;
		istr_t d_integer_lit;
		bool d_bool_lit;
	};
};

#include "hir.h"
#include "lir.h"

typedef struct proc_t proc_t;
typedef struct global_t global_t;
typedef struct ir_desc_t ir_desc_t;

rlocal_t ir_local_new(ir_desc_t *desc, local_t local);

struct ir_desc_t {
	local_t *locals; // variables
	hir_expr_t hir;
	lir_proc_t lir;
};

struct proc_t {
	ir_desc_t desc;
	type_t type; // type available/constructed after checking
	type_t ret_type; // annotation
	loc_t ret_type_loc; // used in sym analysis
	u16 arguments;   // first [0..arguments] are locals with types inserted
};

struct global_t {
	ir_desc_t desc;
	type_t type;
	loc_t type_loc; // used in sym analysis
	bool is_mut;
	// TODO: consteval etc
};

struct local_t {
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

	type_t type;
	loc_t type_loc; // used in sym analysis
};

struct sym_t {
	istr_t key; // key into hashmap, fully qualified name
	//
	rmod_t mod;
	istr_t short_name;
	loc_t loc;

	// if true, this is a placeholder symbol
	// nothing else is stored here, it's just an entry with a key
	//
	// TODO: remove this and put inside `kind` ??
	bool is_placeholder;
	
	// TODO: also mark dead symbols that are apart of different roots
	
	enum : u8 {
		SYM_SORT_WHITE,
		SYM_SORT_GREY,
		SYM_SORT_BLACK,
	} sort_colour;

	enum : u8 {
		SYMBOL_PROC,
		SYMBOL_GLOBAL,
		SYMBOL_TYPE,
	} kind;

	union {
		proc_t d_proc;
		global_t d_global;
		tsymbol_t d_type;
	};
};

// search by qualified name
extern sym_t *symbols;

rsym_t table_resolve(rmod_t mod, istr_t short_name);
rsym_t table_register(sym_t desc);
void table_dump(sym_t *sym);
void table_dump_all(void);
