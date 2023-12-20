#include "all.h"

#include "stb_ds.h"

typedef struct cctx_t cctx_t;

struct cctx_t {
	rmod_t mod;
	mod_t *modp;
};

cctx_t c;

#define VAR_PTR(id) (&c.modp->vars[id])

void ctype_assert(type_t src, type_t of, loc_t onerror) {
	if (src != of) {
		err_with_pos(onerror, "type mismatch: expected `%s`, got `%s`", type_dbg_str(of), type_dbg_str(src));
	}
}

void cir_unchecked_cast(type_t to, ir_node_t *node, loc_t loc) {
	ir_node_t *dup = ir_memdup(*node);

	*node = (ir_node_t) {
		.kind = NODE_CAST,
		.loc = loc,
		.type = to,
		.d_cast = dup,
	};
}

// TYPE_INFER on fail to convert
type_t cconvert_implicit(type_t to, type_t from) {
	type_t ret;

	if (to == from) {
		return to;
	}

	bool t_si = to >= TYPE_SIGNED_INTEGERS_START && to <= TYPE_SIGNED_INTEGERS_END;
	bool f_si = from >= TYPE_SIGNED_INTEGERS_START && from <= TYPE_SIGNED_INTEGERS_END;
	bool t_ui = to >= TYPE_UNSIGNED_INTEGERS_START && to <= TYPE_UNSIGNED_INTEGERS_END;
	bool f_ui = from >= TYPE_UNSIGNED_INTEGERS_START && from <= TYPE_UNSIGNED_INTEGERS_END;

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

	// a u32 can go into an f32
	if (to == TYPE_F32 && (f_ui && from <= TYPE_U32)) {
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
	}

	return TYPE_INFER;
end:
	return ret;
}

type_t cinfix_promotion(ir_node_t *lhs, ir_node_t *rhs, loc_t onerror) {
	type_t ret;
	type_t rt = cconvert_implicit(lhs->type, rhs->type);
	type_t lt = cconvert_implicit(rhs->type, lhs->type);

	if (lt == TYPE_INFER && rt == TYPE_INFER) {
		err_with_pos(onerror, "type mismatch: cannot implicitly promote `%s` and `%s`", type_dbg_str(lt), type_dbg_str(rt));
	}

	if (lt == TYPE_INFER) {
		ret = rt;
	} else if (rt == TYPE_INFER) {
		ret = lt;
	}

	// insert explicit casts

	if (lhs->type != ret) {
		cir_unchecked_cast(ret, lhs, lhs->loc);
	} else if (rhs->type != ret) {
		cir_unchecked_cast(ret, rhs, rhs->loc);
	} else {
		assert_not_reached();
	}

	return ret;
}

type_t cexpr(ir_scope_t *s, type_t upvalue, ir_node_t *expr) {
	switch (expr->kind) {
		case NODE_VAR_DECL: {
			ir_var_t *var = VAR_PTR(expr->d_var_decl.lhs);
			if (var->type == TYPE_INFER) {
				var->type = cexpr(s, upvalue, expr->d_var_decl.rhs);
			} else {
				type_t ret = cexpr(s, var->type, expr->d_var_decl.rhs);

				// need to convert expression to variable type
				if (ret != var->type) {
					// implicitly convert
					ret = cconvert_implicit(var->type, ret);
					if (ret == TYPE_INFER) {
						err_with_pos(expr->loc, "type mismatch: expected `%s`, got `%s`", type_dbg_str(var->type), type_dbg_str(ret));
					}
					// ret == var->type
					cir_unchecked_cast(var->type, expr->d_var_decl.rhs, expr->loc);
				}
			}
			return TYPE_UNIT;
		}
		case NODE_INFIX: {
			// integer promotion rules for lhs and rhs
			// TODO: don't check right expression without left expression
			type_t lhs_type = cexpr(s, upvalue, expr->d_infix.lhs);
			type_t rhs_type = cexpr(s, lhs_type, expr->d_infix.rhs);
			// _ = x + y

			if (lhs_type == rhs_type) {
				expr->type = lhs_type;
			} else {
				expr->type = cinfix_promotion(expr->d_infix.lhs, expr->d_infix.rhs, expr->loc);
			}
			return expr->type;
		}
		case NODE_VAR: {
			// we know this already
			ir_var_t *var = VAR_PTR(expr->d_var_decl.lhs);
			expr->type = var->type;
			return expr->type;
		}
		case NODE_INTEGER_LIT: {
			// default integer type is i32
			// TODO: bounds checking (obviously)
			// TODO: check that upvalue is a valid integer type
			//       however we can ignore upvalue and the caller MUST handle it
			//       -- that is more elegant than having to check for it here
			if (upvalue == TYPE_INFER) {
				expr->type = TYPE_I32;
			} else {
				expr->type = upvalue;
			}
			return expr->type;
		}
		case NODE_CAST: {
			type_t t = cexpr(s, expr->type, expr->d_cast);
			// TODO: check if this cast is even possible
			// if (cconvert_implicit(expr->type, t) == TYPE_INFER)
			(void)t;
			return expr->type;
		}
		default: {
			printf("unhandled expression kind: %d\n", expr->kind); 
			assert_not_reached();
		}
	}
}

void ctoplevel_exprs(ir_scope_t *s, ir_node_t *exprs) {
	for (u32 i = 0, c = arrlenu(exprs); i < c; i++) {
		cexpr(s, TYPE_INFER, &exprs[i]);
	}
}

void cmodule(rmod_t mod) {
	c = (cctx_t){
		.mod = mod,
		.modp = MOD_PTR(mod)
	};

	ctoplevel_exprs(&c.modp->toplevel, c.modp->exprs);
}