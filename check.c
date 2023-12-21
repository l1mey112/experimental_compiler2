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

bool ctype_is_numeric(type_t type) {
	// TODO: aliases etc
	return (type >= TYPE_SIGNED_INTEGERS_START && type <= TYPE_SIGNED_INTEGERS_END) ||
		(type >= TYPE_UNSIGNED_INTEGERS_START && type <= TYPE_UNSIGNED_INTEGERS_END) ||
		(type == TYPE_F32 || type == TYPE_F64);
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

// convert a big curry into a tuple of args
type_t cfn_args_to_tuple(type_t fn) {
	tinfo_t tuple = {
		.kind = TYPE_TUPLE,
	};

	tinfo_t *tinfo = type_get(fn);
	assert(tinfo->kind == TYPE_FN);

	do {
		type_t arg = tinfo->d_fn.arg;
		arrpush(tuple.d_tuple.elems, arg);
		if (tinfo->d_fn.ret < _TYPE_CONCRETE_MAX) {
			break;
		}
		tinfo = type_get(tinfo->d_fn.ret);
	} while (tinfo->kind == TYPE_FN);

	tuple.d_tuple.len = arrlenu(tuple.d_tuple.elems);

	return type_new(tuple, NULL);
}

// get return type
type_t cfn_type_full_return(type_t fn) {
	assert(type_kind(fn) == TYPE_FN);

	while (type_kind(fn) == TYPE_FN) {
		tinfo_t *tinfo = type_get(fn);
		fn = tinfo->d_fn.ret;
	}

	return fn;
}

void cpattern(ir_scope_t *s, ir_pattern_t *pattern, type_t type) {
	switch (pattern->kind) {
		case PATTERN_TUPLE: {
			tinfo_t *tinfo = type_get(type);
			if (tinfo->kind != TYPE_TUPLE) {
				err_with_pos(pattern->loc, "cannot match tuple pattern against non-tuple type `%s`", type_dbg_str(type));
			}
			if (tinfo->d_tuple.len != arrlenu(pattern->d_tuple.elems)) {
				err_with_pos(pattern->loc, "cannot match tuple pattern against tuple type `%s` of different length", type_dbg_str(type));
			}
			for (size_t i = 0; i < arrlenu(pattern->d_tuple.elems); i++) {
				cpattern(s, &pattern->d_tuple.elems[i], tinfo->d_tuple.elems[i]);
			}
			break;
		}
		case PATTERN_VAR: {
			ir_var_t *var = VAR_PTR(pattern->d_var);
			if (var->type == TYPE_INFER) {
				var->type = type;
			} else {
				// var patterns don't have explicit types
				assert_not_reached();
			}
			break;
		}
		case PATTERN_INTEGER_LIT: {
			// TODO: check integer is inbounds for type
			if (!ctype_is_numeric(type)) {
				err_with_pos(pattern->loc, "cannot match integer literal against non-numeric type `%s`", type_dbg_str(type));
			}
			break;
		}
		case PATTERN_UNDERSCORE: {
			break;
		}
	}
}

// TODO: eventually?
enum : u8 {
	CEXPR_NONE,
	CEXPR_LVALUE,
};

type_t cexpr(ir_scope_t *s, type_t upvalue, ir_node_t *expr) {
	switch (expr->kind) {
		case NODE_GLOBAL_UNRESOLVED: {
			for (u32 i = 0; i < arrlenu(c.modp->toplevel.locals); i++) {
				ir_rvar_t var = c.modp->toplevel.locals[i];
				ir_var_t *varp = &c.modp->vars[var];
				if (varp->name == expr->d_global_unresolved) {
					assert(varp->type != TYPE_INFER && "global variable type not inferred");
					expr->kind = NODE_VAR;
					expr->type = varp->type;
					expr->d_var = var;
					return expr->type;
				}
			}
			err_with_pos(expr->loc, "unknown ident `%s`", sv_from(expr->d_global_unresolved));
		}
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
		case NODE_PROC_DECL: {
			// 1. check if return type is infer, IT SHOULD NOT BE
			//    err_with_pos("complex inference and generics aren't implemented yet")
			// 2. cpattern_to_type(...) for each pattern
			ir_var_t *varp = VAR_PTR(expr->d_proc_decl.var);
			if (varp->type == TYPE_INFER) {
				// TODO: when generics come around, sure
				err_with_pos(expr->loc, "function type inference is not implemented yet");
			}
			if (type_kind(varp->type) != TYPE_FN) {
				err_with_pos(expr->loc, "expected function type, got `%s`", type_dbg_str(varp->type));
			}
			// you know, a function being a bunch of patterns matching
			// on a tuple of args is a pretty good abstraction
			type_t args_tuple = cfn_args_to_tuple(varp->type);
			type_t return_type = cfn_type_full_return(varp->type);
			for (u32 i = 0, c = arrlenu(expr->d_proc_decl.scopes); i < c; i++) {
				cpattern(&expr->d_proc_decl.scopes[i], &expr->d_proc_decl.patterns[i], args_tuple);
			}
			for (u32 i = 0, c = arrlenu(expr->d_proc_decl.exprs); i < c; i++) {
				type_t type = cexpr(&expr->d_proc_decl.scopes[i], return_type, &expr->d_proc_decl.exprs[i]);
				ctype_assert(type, return_type, expr->d_proc_decl.exprs[i].loc);
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
			if (upvalue != TYPE_INFER && ctype_is_numeric(upvalue)) {
				expr->type = upvalue;
			} else {
				expr->type = TYPE_I32;
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
		case NODE_SYM_UNRESOLVED: {
			rmod_t sym_mod = expr->d_sym_unresolved.mod;
			mod_t *sym_modp = MOD_PTR(sym_mod);
			for (u32 i = 0, c = arrlenu(sym_modp->vars); i < c; i++) {
				ir_var_t *var = &sym_modp->vars[i];
				if (var->name == expr->d_sym_unresolved.name) {
					*expr = (ir_node_t){
						.kind = NODE_SYM,
						.loc = expr->loc,
						.type = var->type,
						.d_sym = {
							.mod = sym_mod,
							.var = i,
						}
					};
					
					return expr->type;
				}
			}
			err_with_pos(expr->loc, "unresolved symbol `%s`", expr->d_sym_unresolved.name);
		}
		case NODE_CALL: {
			type_t f_type = cexpr(s, TYPE_INFER, expr->d_call.f);
			if (type_kind(f_type) != TYPE_FN) {
				err_with_pos(expr->loc, "expected function type, got `%s`", type_dbg_str(f_type));
			}
			tinfo_t *fn = type_get(f_type);
			type_t arg_type = cexpr(s, fn->d_fn.arg, expr->d_call.arg);
			if (fn->d_fn.arg != arg_type) {
				err_with_pos(expr->loc, "expected argument type `%s`, got `%s`", type_dbg_str(fn->d_fn.arg), type_dbg_str(arg_type));
			}
			expr->type = fn->d_fn.ret;
			return expr->type;
		}
		case NODE_TUPLE_UNIT: {
			return TYPE_UNIT;
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