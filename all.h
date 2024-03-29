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
#define arr(type, ...) ({                                  \
		type *__ret = NULL;                                \
		type __v[] = {__VA_ARGS__};                        \
		if (sizeof(__v)) {                                 \
			arraddnptr(__ret, sizeof(__v) / sizeof(type)); \
			memcpy(__ret, __v, sizeof(__v));               \
		}                                                  \
		__ret;                                             \
	})

typedef struct err_diag_t err_diag_t;
typedef struct token_t token_t;
typedef u16 type_t;
typedef u16 fs_rmod_t;
typedef u16 fs_rfile_t;
typedef struct fs_mod_t fs_mod_t;
typedef struct fs_file_t fs_file_t;
typedef struct fs_platform_t fs_platform_t;

// a handle to an interned string
typedef u32 istr_t;

#define RMOD_NONE ((fs_rmod_t)-1)
#define RFILE_NONE ((fs_rfile_t)-1)
#define RSYM_NONE ((rsym_t)-1)
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

typedef struct loc_t loc_t;
typedef struct lineinfo_t lineinfo_t;

struct lineinfo_t {
	u32 line_nr;
	u32 col;
	u32 pos;
	u16 len;
	fs_rfile_t file;
};

// len == 0 is invalid
struct loc_t {
	u64 pos : 48;
	u16 len;
};

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


#define ptr_cmp_literal(a, alen, b) ptr_cmp(a, alen, (u8*)b, sizeof(b "") - 1)
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
	TYPE_SUM,
	TYPE_STRUCT,
	// TYPE_OPTION,
	// TYPE_ARRAY,
	// TYPE_ENUM,
	// TYPE_FUNCTION_PTR,
	// TYPE_STRUCT,
	// TYPE_FIXEDARRAY,
};

typedef struct sym_t sym_t;
typedef u32 rsym_t;

typedef enum ti_kind ti_kind;
typedef struct tinfo_t tinfo_t;
typedef struct tinfo_sf_t tinfo_sf_t;

typedef struct typesymbol_debug_t typesymbol_debug_t;
typedef struct typesymbol_debug_sf_t typesymbol_debug_sf_t;
typedef struct typesymbol_t typesymbol_t;

// named types are typesymbols.
// named struct types aren't interned, they're just a list of fields

struct tinfo_sf_t {
	istr_t field;
	type_t type;
	
};

struct typesymbol_debug_sf_t {
	loc_t field_loc;
	loc_t type_loc;
};

// TODO: possibly switch to an AST representation for types just after the parsing stage
//       since we'll eventually need to represent types with debuginfo and in match arms
struct typesymbol_debug_t {
	enum : u8 {
		TYPESYMBOL_STRUCT,
		TYPESYMBOL_ALIAS,
	} kind;
	
	union {
		typesymbol_debug_sf_t *d_struct;
		loc_t d_alias;
	};
};

struct typesymbol_t {
	type_t type;
	typesymbol_debug_t debug;
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
			bool is_symbol; // after checking will be desugared
			union {
				rsym_t d_symbol;
				size_t d_length; // TODO: length or constant symbol?
			};
			type_t elem;
		} d_array;
		type_t *d_tuple;
		struct {
			type_t ref;
			bool is_mut;
		} d_ptr;
		struct {
			type_t *elems;
		} d_sum;
		struct {
			tinfo_sf_t *fields;
		} d_struct;
	};
};

// TODO: define helper function for invalid loc_t
// TODO: fancy arena allocators? fuck that!
//       we're going for a quick and dirty bootstrap

struct fs_mod_t {
	enum : u8 {
		MOD_STUB,       // head of the tree
		MOD_LAZY_CHILD, // path traversed, files are lazy
		MOD_CHILD,      // child module, fully inspected dirpath and files
		MOD_INCLUDE,    // doesn't include files, include path
		MOD_RT,         // runtime module, we only get ONE of these
		MOD_MAIN,       // main module, we only get ONE of these
	} kind;

	istr_t key;  // fully qualified
	istr_t short_name; // short

	const char *path;

	fs_rmod_t *children;
	fs_rmod_t parent;
	fs_rmod_t root;
	
	u32 files_count;
};

struct fs_file_t {
	const char *fp;
	u8 *data;
	size_t len;
	fs_rmod_t mod;
	u64 bs_offset_pos;
};

// registered runtime which can be rematerialised into the MOD_RT
struct fs_platform_t {
	const char *name;
	const char *rt_path;
};

extern u32 fs_files_queue_len;
extern fs_file_t fs_files_queue[512];
extern u32 fs_mod_arena_len;
extern fs_mod_t fs_mod_arena[128];
extern u32 fs_platforms_len;
extern fs_platform_t fs_platforms[32];

extern rsym_t __main_init;
extern fs_rmod_t main_module;
extern fs_rmod_t rt_module;
extern char *build_token;

extern tinfo_t types[1024];
extern u32 type_len;

const char *fs_make_relative(const char *path);
lineinfo_t fs_reconstruct_lineinfo(loc_t loc);
fs_rmod_t fs_register_root(const char *dp);
fs_rmod_t fs_register_import(fs_rmod_t src, istr_t *path, u32 path_len, loc_t onerror);
istr_t fs_module_symbol(fs_rmod_t mod, istr_t symbol);
// return qualified_name:selector
istr_t fs_module_symbol_selector(istr_t qualified_name, istr_t selector);
void fs_dump_tree(void);
void fs_entrypoint(const char *argv);
void fs_target(const char *arch, const char *platform, bool debug_symbols);

void compiler_process_file(fs_rfile_t f);
void compiler_pass_all(void);

type_t type_array_or_slice_to_slice(type_t type);
type_t type_array_or_slice_elem(type_t type);
type_t type_new_inc_mul(type_t type, bool is_mut);
type_t type_new(tinfo_t typeinfo);
tinfo_t *type_get(type_t type);
ti_kind type_kind(type_t type);

// unalias a type
type_t type_underlying(type_t type);
// ***i32 -> i32
type_t type_strip_muls(type_t type);
// ***i32 -> 3
u32 type_nr_muls(type_t type);

static const tok_t tok_assign_op_to_op[] = {
	[TOK_ASSIGN_ADD] = TOK_ADD,
	[TOK_ASSIGN_SUB] = TOK_SUB,
	[TOK_ASSIGN_MUL] = TOK_MUL,
	[TOK_ASSIGN_DIV] = TOK_DIV,
	[TOK_ASSIGN_MOD] = TOK_MOD,
};

const char *type_dbg_str(type_t type);
const char *tok_op_str(tok_t tok);
const char *tok_dbg_str(token_t tok);

typedef struct arch_t arch_t;
typedef struct platform_t platform_t;
typedef struct target_t target_t;

#define X_ARCHS \
	X(ARCH_STDC_64, "stdc_64")

#define X_PLATFORMS \
	X(PLATFORM_LIBC, "libc")

struct arch_t {
	enum : u8 {
		#define X(arch, name) arch,
			X_ARCHS
		#undef X
	} kind;

	bool debug_symbols;
	u8 ptr_size;
};

extern arch_t abi;

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

// for ZSTs, regardless of ABI it returns the same 0

bool type_is_diverging(type_t type);

u32 type_sizeof(arch_t *abi, type_t type);

// alignment throws on types where `TYPE_SIZE_DIVERGING` would be returned
// also throws for ZSTs
u32 type_alignof(arch_t *abi, type_t type);

bool type_abi_integer_fits_in(arch_t *arch, u64 lit, type_t type);
u64 type_abi_truncate(arch_t *arch, type_t type, u64 lit);
i64 type_abi_sign_extend(arch_t *arch, type_t type, u64 lit);
i64 type_abi_signed_int_min(arch_t *arch, type_t type);
i64 type_abi_signed_int_max(arch_t *arch, type_t type);
u64 type_abi_unsigned_int_max(arch_t *arch, type_t type);

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

typedef struct proc_t proc_t;
typedef struct global_t global_t;
typedef struct ir_desc_t ir_desc_t;
typedef struct imas_t imas_t;
typedef struct imas_entry_t imas_entry_t;

rlocal_t ir_local_new(ir_desc_t *desc, local_t local);

struct ir_desc_t {
	local_t *locals; // variables
	hir_expr_t *hir; // possible NULL
};

struct proc_t {
	ir_desc_t desc;
	type_t type; // type available/constructed after checking
	type_t ret_type; // annotation
	loc_t ret_type_loc; // used in sym analysis
	rlocal_t *arguments; // locals with types inserted
};

struct global_t {
	ir_desc_t desc;
	type_t type;
	loc_t type_loc; // used in sym analysis
	bool is_mut;
	//
	hir_expr_t *constant; // possible NULL
};

struct imas_entry_t {
	rsym_t symbol;
	loc_t symbol_loc;
	type_t type;
	loc_t type_loc;
};

struct imas_t {
	imas_entry_t *entries;
};

struct local_t {
	enum : u8 {
		LOCAL_MUT, // assigned multiple times
		LOCAL_IMM, // assigned once in all flows of control
		_LOCAL_ZST_DELETED,
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
	fs_rmod_t mod;
	istr_t short_name;
	loc_t loc;

	// if true, this is a placeholder symbol
	// nothing else is stored here, it's just an entry with a key
	//
	// TODO: remove this and put inside `kind` ??
	// bool is_placeholder;
	
	// TODO: also mark dead symbols that are apart of different roots

	// error if `mod != current mod`
	bool is_pub;
	bool is_extern; // exposed to the outside world. for globals and procs
	istr_t extern_symbol; // always set. either supplied by user or `key`

	enum : u8 {
		SYM_SORT_WHITE,
		SYM_SORT_GREY,
		SYM_SORT_BLACK,
	} sort_colour;

	enum : u8 {
		_SYMBOL_PLACEHOLDER, // zero init
		SYMBOL_PROC,
		SYMBOL_GLOBAL,
		SYMBOL_TYPE,
		SYMBOL_IMPORT_ASSERTION, // relies on deps
	} kind;

	union {
		proc_t d_proc;
		global_t d_global;
		typesymbol_t d_type;
		imas_t d_imas;
	};
};

// search by qualified name
extern sym_t *symbols;
extern rsym_t *symbols_po;

istr_t table_anon_symbol(void);
rsym_t table_resolve(fs_rmod_t mod, istr_t short_name, loc_t onerror);
// returns RSYM_NONE if not found
rsym_t table_resolve_qualified_opt(istr_t qualified_name);
// returns RSYM_NONE if not found
rsym_t table_resolve_method(type_t bare_type, istr_t method);
istr_t table_type_qualified_name(type_t type);
rsym_t table_register(sym_t desc);
void table_dump(sym_t *sym);
void table_dump_po(void);
