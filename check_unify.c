#include "all.h"
#include "check.h"

type_t ctype_convert_numbers(type_t to, type_t from) {
	assert(type_is_number(to));
	assert(type_is_number(from));
	
	type_t ret;

	if (to == from) {
		return to;
	}

	bool t_si = type_is_integer(to) && type_is_signed(to);
	bool f_si = type_is_integer(from) && type_is_signed(from);
	bool t_ui = type_is_integer(to) && type_is_unsigned(to);
	bool f_ui = type_is_integer(from) && type_is_unsigned(from);

	bool t_lit = type_is_literal_number(to);
	bool f_lit = type_is_literal_number(from);

    // {integer literals} <- {integers}
    // {float literals} <- {floats}

	if (t_lit) {
        if (f_lit && to == from) {
            ret = to;
            goto end;
        }

        if (to == TYPE_INT_LITERAL && type_is_integer(from)) {
            ret = to;
            goto end;
        }

        if (to == TYPE_FLOAT_LITERAL && type_is_float(from)) {
            ret = to;
            goto end;
        }

		return TYPE_INFER;
	}

	// signed type to signed type
	if (t_si && f_si) {
		if (to > from) {
			ret = to;
			goto end;
		}
	}

	// unsigned type to unsigned type
	if (t_ui && f_ui) {
		if (to > from) {
			ret = to;
			goto end;
		}
	}

	// from:   u8 → u16 → u32 → u64
	//            ↘     ↘     ↘
	// to:     i8 → i16 → int → i64

	// unsigned to signed, if possible
	if ((f_ui && from < TYPE_U64) && (t_si && to > TYPE_I8)) {
		ret = to;
		goto end;
	} 

	// from: f32 → to: f64

	// floats to floats
	if (from == TYPE_F32 && to == TYPE_F64) {
		ret = to;
		goto end;
	}

	// from:   i8 → i16 → int → i64
	//                        ↘     ↘
	//                          f32 → f64
	//                        ↗     ↗
	// to:     u8 → u16 → u32 → u64

    // no float conversions? sure.

	// a u32 can go into an f32
	/* if (to == TYPE_F32 && (f_ui && from <= TYPE_U32)) {
		ret = to;
		goto end;
	}

	// a i32 can go into an f32
	if (to == TYPE_F32 && (f_si && from <= TYPE_I32)) {
		ret = to;
		goto end;
	}

	// a u64 can go into an f64
	if (to == TYPE_F64 && (f_ui && from <= TYPE_USIZE)) {
		ret = to;
		goto end;
	}

	// a i64 can go into an f64
	if (to == TYPE_F64 && (f_si && from <= TYPE_ISIZE)) {
		ret = to;
		goto end;
	} */
	
	return TYPE_INFER;
end:
	return ret;
}

type_t ctype_unify_innards(type_t lhs_t, type_t rhs_t) {
	ti_kind lhs_kind = type_kind(lhs_t);
	ti_kind rhs_kind = type_kind(rhs_t);

	if (lhs_t == rhs_t) {
		return lhs_t;
	}

	// ! coerces to everything
	if (rhs_kind == TYPE_BOTTOM) {
		return lhs_t;
	}

	// ! coerces to everything
	if (lhs_kind == TYPE_BOTTOM) {
		return rhs_t;
	}

	// before undefined coercing, make sure to cuse() to ensure this is a safe unification

	// TODO: no undefined yet

	// undefined coerces to everything
	/* if (rhs_kind == TYPE_UNDEFINED) {
		return lhs_t;
	}

	// undefined coerces to everything
	if (lhs_kind == TYPE_UNDEFINED) {
		return rhs_t;
	} */

	// functions
	if (lhs_kind == TYPE_FUNCTION && rhs_kind == TYPE_FUNCTION) {
		assert_not_reached();
	}

	return TYPE_INFER;
}

// lhs_t <- rhs_t
// ~~doesn't apply implicit casts like cunify does~~
// INFO: don't insert casts yet really, figure it out
type_t ctype_unify(type_t lhs_t, type_t rhs_t, loc_t onerror) {
	type_t t;
	if ((t = ctype_unify_innards(lhs_t, rhs_t)) != TYPE_INFER) {
		return t;
	}

	err_with_pos(onerror, "type mismatch: expected `%s`, got `%s`", type_dbg_str(lhs_t), type_dbg_str(rhs_t));
}
