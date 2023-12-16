#include "all.h"
#include <assert.h>

static tinfo_t types[1024];
static u32 type_len;

static bool cmp_typeinfo(tinfo_t *a, tinfo_t *b) {
	ti_kind a_type = a->kind, b_type = b->kind;
	
	if (a->is_named && b->is_named) {
		// may be TYPE_UNKNOWN
		return a->d_named.module == b->d_named.module && a->d_named.name == b->d_named.name;
	}

	if (a_type != b_type) {
		return false;
	}

	switch (a_type) {
		/* case TYPE_PTR:
			return a->type_ref == b->type_ref; */
		case TYPE_FN:
			if (a->d_fn.arg != b->d_fn.arg) {
				return false;
			}
			if (a->d_fn.ret != b->d_fn.ret) {
				return false;
			}
			return true;
		case TYPE_TUPLE:
			if (a->d_tuple.len != b->d_tuple.len) {
				return false;
			}
			for (u32 i = 0; i < a->d_tuple.len; i++) {
				if (a->d_tuple.elems[i] != b->d_tuple.elems[i]) {
					return false;
				}
			}
			return true;
		default:
			assert_not_reached();
	}
}

// reference T -> TYPE_UNKNOWN 
// define T -> TYPE_!UNKNOWN probably TYPE_STRUCT
type_t type_new(tinfo_t typeinfo, loc_t *loc) {
	assert(typeinfo.kind >= _TYPE_CONCRETE_MAX);

	// intern types, there aren't that many in an average program
	// the same types are often used in the same context
	// so iterate backwards instead
	//
	// will also define types, but not redefine them
	for (u32 i = type_len; i > 0;) {
		i--;
		tinfo_t *nb = &types[i];
		if (cmp_typeinfo(nb, &typeinfo)) {
			// for is_named:
			//     a->module == b->module && a->name == b->name
			// but may have different typei_kind_t, meaning referencing same name
			if (typeinfo.is_named && nb->is_named && typeinfo.kind != nb->kind) {
				if (typeinfo.kind != TYPE_UNKNOWN) {
					// therefore nb->kind != TYPE_UNKNOWN, and is already defined
					err_with_pos(*loc, "type '%s' already defined", fs_module_symbol_sv(typeinfo.d_named.module, typeinfo.d_named.name));
				}
			}

			return i + _TYPE_CONCRETE_MAX;
		}
	}

	// assert(size <= _TYPE_CONCRETE_MAX || size - _TYPE_CONCRETE_MAX < UINT16_MAX);
	assert(type_len < ARRAYLEN(types));

	type_t type = type_len + _TYPE_CONCRETE_MAX;
	types[type_len++] = typeinfo;
	return type;
}

tinfo_t *type_get(type_t type) {
	assert(type >= _TYPE_CONCRETE_MAX);
	u32 idx = type - _TYPE_CONCRETE_MAX;
	assert(idx < type_len);
	return &types[idx];
}

static u8 *p;

static const char *ctinfo_str[] = {
	#define X(_, lit) lit,
		TYPE_X_CONCRETE_LIST
	#undef X
};

static void _type_dbg_str(type_t type) {
	#define COMMIT(expr) \
		do { \
			p += (expr); \
		} while (0)
		
	if (type == TYPE_INFER) {
		COMMIT(sprintf((char *)p, "<infer:-1>"));
		return;
	}

	if (type < _TYPE_CONCRETE_MAX) {
		COMMIT(sprintf((char *)p, "%s", ctinfo_str[type]));			
		return;
	}

	tinfo_t *typeinfo = type_get(type);

	switch (typeinfo->kind) {
		case TYPE_UNKNOWN:
			assert(typeinfo->is_named);
			COMMIT(sprintf((char *)p, "%s", fs_module_symbol_sv(typeinfo->d_named.module, typeinfo->d_named.name)));
			return;
		case TYPE_TUPLE:
			COMMIT(sprintf((char *)p, "("));
			for (u32 i = 0; i < typeinfo->d_tuple.len; i++) {
				type_t elem = typeinfo->d_tuple.elems[i];
				_type_dbg_str(elem);
				if (i + 1 < typeinfo->d_tuple.len) {
					COMMIT(sprintf((char *)p, ", "));
				}
			}
			COMMIT(sprintf((char *)p, ")"));
			return;
		case TYPE_FN:
			_type_dbg_str(typeinfo->d_fn.arg);
			COMMIT(sprintf((char *)p, " -> "));
			_type_dbg_str(typeinfo->d_fn.ret);
			return;
		default:
			assert_not_reached();
	}
}

// allocates using alloc_* functions
const char *type_dbg_str(type_t type) {
	// u8 *p = alloc_scratch(0);
	//u32 len = _type_dbg_str(p, type);

	
	// ARGHHHHH
	p = alloc_scratch(1024);
	u8 *oldp = p;

	_type_dbg_str(type);

	u32 nwritten = p - oldp;

	p[nwritten] = '\0';

	return (const char *)oldp;
}

void types_dump(void) {
	for (type_t i = 0; i < type_len; i++) {
		printf("%u: %s\n", i + _TYPE_CONCRETE_MAX, type_dbg_str(i + _TYPE_CONCRETE_MAX));
	}
}