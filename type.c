#include "all.h"

static tinfo_t types[1024];
static u32 type_len;

static bool cmp_typeinfo(tinfo_t *a, tinfo_t *b) {
	ti_kind a_type = a->kind, b_type = b->kind;
	
	if (a->is_named && b->is_named) {
		// may be TYPE_UNKNOWN
		return a->d_named.mod == b->d_named.mod && a->d_named.name == b->d_named.name;
	}

	if (a_type != b_type) {
		return false;
	}

	switch (a_type) {
		case TYPE_FUNCTION: {
			u32 a_args_len = arrlenu(a->d_fn.args);
			u32 b_args_len = arrlenu(b->d_fn.args);
			if (a_args_len != b_args_len) {
				return false;
			}
			if (memcmp(a->d_fn.args, b->d_fn.args, a_args_len * sizeof(type_t)) != 0) {
				return false;
			}
			if (a->d_fn.ret != b->d_fn.ret) {
				return false;
			}
			return true;
		}
		case TYPE_TUPLE: {
			if (arrlenu(a->d_tuple.elems) != arrlenu(b->d_tuple.elems)) {
				return false;
			}
			for (u32 i = 0, c = arrlenu(a->d_tuple.elems); i < c; i++) {
				if (a->d_tuple.elems[i] != b->d_tuple.elems[i]) {
					return false;
				}
			}
			return true;
		}
		case TYPE_PTR: {
			return a->d_ptr.ref == b->d_ptr.ref && a->d_ptr.is_mut == b->d_ptr.is_mut;
		}
		case TYPE_SLICE: {
			return a->d_slice.elem == b->d_slice.elem;
		}
		case TYPE_ARRAY: {
			return a->d_array.elem == b->d_array.elem && a->d_array.length == b->d_array.length;
		}
		default: {
			assert_not_reached();
		}
	}
}

// reference T -> TYPE_UNKNOWN 
// define T -> TYPE_!UNKNOWN probably TYPE_STRUCT
type_t type_new(tinfo_t typeinfo, loc_t *onerror) {
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
					err_with_pos(*onerror, "type `%s` already defined", fs_module_symbol_str(typeinfo.d_named.mod, typeinfo.d_named.name));
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

type_t type_new_inc_mul(type_t type, bool is_mut) {
	tinfo_t typeinfo = {
		.kind = TYPE_PTR,
		.d_ptr.ref = type,
		.d_ptr.is_mut = is_mut,
	};

	return type_new(typeinfo, NULL);
}

// safe for comparisions
ti_kind type_kind(type_t type) {
	assert(type != TYPE_INFER);
	if (type < _TYPE_CONCRETE_MAX) {
		return type;
	}

	tinfo_t *typeinfo = type_get(type);
	return typeinfo->kind;
}

// don't unwrap vars
tinfo_t *type_get(type_t type) {
	assert(type != TYPE_INFER);
	assert(type >= _TYPE_CONCRETE_MAX);
	u32 idx = type - _TYPE_CONCRETE_MAX;
	
	tinfo_t *typeinfo = &types[idx];
	return typeinfo;
}

type_t type_array_or_slice_to_slice(type_t type) {
	ti_kind kind = type_kind(type);
	
	assert(kind == TYPE_SLICE || kind == TYPE_ARRAY);
	
	if (kind == TYPE_SLICE) {
		return type;
	}

	tinfo_t typeinfo = {
		.kind = TYPE_SLICE,
		.d_slice.elem = type_get(type)->d_array.elem,
	};

	return type_new(typeinfo, NULL);
}

static u8 *p;

static const char *ctinfo_str[] = {
	#define X(_, lit) lit,
		TYPE_X_CONCRETE_LIST
	#undef X
};

static void _type_dbg_str(type_t type, bool inner) {
	#define COMMIT(expr) \
		do { \
			p += (expr); \
		} while (0)
		
	if (type == TYPE_INFER) {
		COMMIT(sprintf((char *)p, "<infer>"));
		return;
	}

	if (type < _TYPE_CONCRETE_MAX) {
		COMMIT(sprintf((char *)p, "%s", ctinfo_str[type]));			
		return;
	}

	tinfo_t *typeinfo = type_get(type);

	switch (typeinfo->kind) {
		case TYPE_UNKNOWN: {
			assert(typeinfo->is_named);
			COMMIT(sprintf((char *)p, "%s", fs_module_symbol_str(typeinfo->d_named.mod, typeinfo->d_named.name)));
			break;
		}
		case TYPE_TUPLE: {
			COMMIT(sprintf((char *)p, "("));
			for (u32 i = 0, c = arrlenu(typeinfo->d_tuple.elems); i < c; i++) {
				type_t elem = typeinfo->d_tuple.elems[i];
				_type_dbg_str(elem, false);
				if (i + 1 < c) {
					COMMIT(sprintf((char *)p, ", "));
				}
			}
			COMMIT(sprintf((char *)p, ")"));
			break;
		}
		case TYPE_FUNCTION: {
			if (!inner) {
				COMMIT(sprintf((char *)p, "("));
			}
			for (u32 i = 0, c = arrlenu(typeinfo->d_fn.args); i < c; i++) {
				type_t arg = typeinfo->d_fn.args[i];
				_type_dbg_str(arg, false);
				if (i + 1 < c) {
					COMMIT(sprintf((char *)p, ", "));
				}
			}
			COMMIT(sprintf((char *)p, " -> "));
			_type_dbg_str(typeinfo->d_fn.ret, false);
			if (!inner) {
				COMMIT(sprintf((char *)p, ")"));
			}
			break;
		}
		case TYPE_PTR: {
			COMMIT(sprintf((char *)p, "*"));
			if (typeinfo->d_ptr.is_mut) {
				COMMIT(sprintf((char *)p, "'"));
			}
			_type_dbg_str(typeinfo->d_ptr.ref, false);
			break;
		}
		case TYPE_ARRAY: {
			COMMIT(sprintf((char *)p, "[%zu]", typeinfo->d_array.length));
			_type_dbg_str(typeinfo->d_array.elem, false);
			break;
		}
		case TYPE_SLICE: {
			COMMIT(sprintf((char *)p, "[]"));
			_type_dbg_str(typeinfo->d_slice.elem, false);
			break;
		}
		default:
			assert_not_reached();
	}
}

// allocates using alloc_* functions
const char *type_dbg_str(type_t type) {
	// u8 *p = alloc_scratch(0);
	//u32 len = _type_dbg_str(p, type);

	
	// ARGHHHHH
	p = alloc_scratch(256);
	u8 *oldp = p;

	_type_dbg_str(type, true);

	u32 nwritten = p - oldp;

	assert(nwritten < 256);

	p[nwritten] = '\0';

	return (const char *)oldp;
}
