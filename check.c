#include "all.h"

#include "stb_ds.h"

typedef struct cctx_t cctx_t;

struct cctx_t {
	rmod_t mod;
	mod_t *modp;
};

cctx_t c;

#define VAR_PTR(id) (&c.modp->vars[id])

void ctype_assert(type_t src, type_t of, loc_t loc) {
	if (src != of) {
		err_with_pos(loc, "type mismatch: expected %s, got %s", type_dbg_str(of), type_dbg_str(src));
	}
}
type_t cinfix_promotion(ir_node_t *lhs, ir_node_t *rhs) {
	// -- what V does: (use as basic reference)
	//   i8 → i16 → int → i64
	//                  ↘     ↘
	//                    f32 → f64
	//                  ↗     ↗
	//   u8 → u16 → u32 → u64 ⬎
	//      ↘     ↘     ↘      ptr
	//   i8 → i16 → int → i64 ⬏

	type_t ret;
	type_t lt = lhs->type;
	type_t rt = rhs->type;

	// promote based on integer order

	bool l_si = lt >= TYPE_SIGNED_INTEGERS_START && lt <= TYPE_SIGNED_INTEGERS_END;
	bool r_si = rt >= TYPE_SIGNED_INTEGERS_START && rt <= TYPE_SIGNED_INTEGERS_END;
	bool l_ui = lt >= TYPE_UNSIGNED_INTEGERS_START && lt <= TYPE_UNSIGNED_INTEGERS_END;
	bool r_ui = rt >= TYPE_UNSIGNED_INTEGERS_START && rt <= TYPE_UNSIGNED_INTEGERS_END;

	// signed type to signed type
	if (l_si && r_si) {
		if (lt > rt) {
			ret = lt;
			goto end;
		} else {
			ret = rt;
			goto end;
		}
	}

	// unsigned type to unsigned type
	if (l_ui && r_ui) {
		if (lt > rt) {	
			ret = lt;
			goto end;
		} else {
			ret = rt;
			goto end;
		}
	}

	//   u8 → u16 → u32 → u64
	//      ↘     ↘     ↘
	//   i8 → i16 → int → i64

	// signed to unsigned and unsigned to signed
	if ((l_ui && lt < TYPE_U64) && r_si) {
		ret = rt;
		goto end;
	}

	if ((r_ui && rt < TYPE_U64) && l_si) {
		ret = lt;
		goto end;
	}

	// floats to floats
	if ((lt == TYPE_F32 && rt == TYPE_F64) || (lt == TYPE_F64 && rt == TYPE_F32)) {
		ret = TYPE_F64;
		goto end;
	}

	// integers to floats
	
	// a u32 can go into an f32
	if ((lt == TYPE_F32 && (r_si && rt <= TYPE_U32)) || (rt == TYPE_F32 && (l_si && lt <= TYPE_U32))) {
		ret = TYPE_F32;
		goto end;
	}

	// a u64 can go into an f64
	if ((lt == TYPE_F64 && (r_si && rt <= TYPE_U64)) || (rt == TYPE_F64 && (l_si && lt <= TYPE_U64))) {
		ret = TYPE_F64;
		goto end;
	}

	// a i32 can go into an f32
	if ((lt == TYPE_F32 && (r_si && rt <= TYPE_I32)) || (rt == TYPE_F32 && (l_si && lt <= TYPE_I32))) {
		ret = TYPE_F32;
		goto end;
	}

	// a i64 can go into an f64
	if ((lt == TYPE_F64 && (r_si && rt <= TYPE_I64)) || (rt == TYPE_F64 && (l_si && lt <= TYPE_I64))) {
		ret = TYPE_F64;
		goto end;
	}

	assert_not_reached();
end:

	// insert explicit casts

	if (lt != ret) {
		ir_node_t *lhs_dup = ir_memdup(*lhs);
		
		*lhs = (ir_node_t) {
			.kind = NODE_CAST,
			.loc = lhs->loc,
			.type = ret,
			.d_cast = lhs_dup,
		};
		return ret;
	} else if (rt != ret) {
		ir_node_t *rhs_dup = ir_memdup(*rhs);
		
		*rhs = (ir_node_t) {
			.kind = NODE_CAST,
			.loc = rhs->loc,
			.type = ret,
			.d_cast = rhs_dup,
		};
		return ret;
	} else {
		assert_not_reached();
	}
}

type_t cexpr(ir_scope_t *s, type_t upvalue, ir_node_t *expr) {
	switch (expr->kind) {
		case NODE_VAR_DECL: {
			ir_var_t *var = VAR_PTR(expr->d_var_decl.lhs);
			if (var->type == TYPE_INFER) {
				var->type = cexpr(s, upvalue, expr->d_var_decl.rhs);
			} else {
				type_t rhs_type = cexpr(s, var->type, expr->d_var_decl.rhs);
				ctype_assert(rhs_type, var->type, expr->loc);
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
				expr->type = cinfix_promotion(expr->d_infix.lhs, expr->d_infix.rhs);
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
		default: {
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