#include "all.h"

typedef struct cctx_t cctx_t;
typedef struct cblk_t cblk_t;
typedef struct cscope_t cscope_t;
typedef struct cinfer_vars_t cinfer_vars_t;

struct cblk_t {
	type_t upvalue;
	type_t brk_type;
	loc_t brk_loc; // if `first_type` not TYPE_INFER
};

struct cscope_t {
	u32 ifvars_lvl;
};

struct cinfer_vars_t {
	type_t type;
	loc_t onerror;
	ir_node_t *def;
};

struct cctx_t {
	rmod_t mod;
	mod_t *modp;
	cblk_t blocks[64];
	u32 blocks_len;
	cscope_t scopes[64];
	u32 scopes_len;
	cinfer_vars_t ifvars[256];
	u32 ifvars_len;
};

cctx_t c;

void cscope_enter(void) {
	c.scopes[c.scopes_len++] = (cscope_t){
		.ifvars_lvl = c.ifvars_len,
	};
}

void cscope_leave(void) {
	u32 idx = --c.scopes_len;
	u32 old_lvl = c.scopes[idx].ifvars_lvl;

	// make sure all unknown vars are substituted
	for (u32 i = old_lvl; i < c.ifvars_len; i++) {
		if (type_kind(c.ifvars[i].type) == TYPE_VAR) {
			err_with_pos(c.ifvars[i].onerror, "type annotations needed");
		}
	}
	
	c.ifvars_len = old_lvl;
}

cinfer_vars_t *cinfer_get(type_t type) {
	for (u32 i = 0; i < c.ifvars_len; i++) {
		if (c.ifvars[i].type == type) {
			return &c.ifvars[i];
		}
	}
	return NULL;
}

void cinfer_register(type_t type, loc_t onerror, ir_node_t *def) {
	assert(type_kind(type) == TYPE_VAR);
	assert(cinfer_get(type) == NULL);

	c.ifvars[c.ifvars_len++] = (cinfer_vars_t){
		.type = type,
		.onerror = onerror,
		.def = def,
	};
}

void ctype_assert(type_t src, type_t of, loc_t onerror) {
	src = type_underlying(src);
	of = type_underlying(of);
	
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

bool ctype_is_integer(type_t type) {
	ti_kind kind = type_kind(type);
	
	// TODO: aliases etc
	return (kind >= TYPE_SIGNED_INTEGERS_START && kind <= TYPE_SIGNED_INTEGERS_END) ||
		(kind >= TYPE_UNSIGNED_INTEGERS_START && kind <= TYPE_UNSIGNED_INTEGERS_END);
}

bool ctype_is_numeric(type_t type) {
	ti_kind kind = type_kind(type);
	
	// TODO: aliases etc
	return (kind >= TYPE_SIGNED_INTEGERS_START && kind <= TYPE_SIGNED_INTEGERS_END) ||
		(kind >= TYPE_UNSIGNED_INTEGERS_START && kind <= TYPE_UNSIGNED_INTEGERS_END) ||
		(kind == TYPE_F32 || kind == TYPE_F64);
}

// TODO: maybe never implement?
//       cconvert_implicit handles only implicit casts
enum _safe_cast_kind : u8 {
	SC_N, // impossible
	SC_I, // implicit
	SC_E, // explicit
};

// TYPE_INFER on fail to convert
type_t cconvert_integers(type_t to, type_t from) {
	assert(ctype_is_numeric(to));
	assert(ctype_is_numeric(from));
	
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
	
	// ! goes into everything
	if (from == TYPE_BOTTOM) {
		ret = to;
		goto end;
	}

	return TYPE_INFER;
end:
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

// useful for determining a functions type, but not now
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
				if (type == TYPE_UNDEFINED) {
					type = typevar_new();
					cinfer_register(type, var->loc, NULL);
				}
				
				var->type = type;
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

type_t cexpr(ir_scope_t *s, type_t upvalue, ir_node_t *expr);

type_t cdo(ir_node_t *node, type_t upvalue) {
	u8 blk_id = c.blocks_len++;
	cblk_t *blk = &c.blocks[blk_id];

	*blk = (cblk_t){
		.brk_type = TYPE_INFER,
		.upvalue = upvalue,
	};
	cscope_enter();
	for (u32 i = 0, c = arrlenu(node->d_do_block.exprs); i < c; i++) {
		ir_node_t *expr = &node->d_do_block.exprs[i];
		// nonsensical to pass upvalue
		type_t type = cexpr(node->d_do_block.scope, TYPE_INFER, expr);
		if (type == TYPE_BOTTOM && i + 1 < c) {
			err_with_pos(node->d_do_block.exprs[i + 1].loc, "unreachable code");
		}
	}
	cscope_leave();
	c.blocks_len--;

	// no breaks pointing to this do, but somehow some ! code ran
	if (blk->brk_type == TYPE_INFER) {
		assert(arrlast(node->d_do_block.exprs).type == TYPE_BOTTOM);
		blk->brk_type = TYPE_BOTTOM;
	}

	ir_node_t last;
	if (blk->brk_type == TYPE_UNIT && arrlast(node->d_do_block.exprs).kind == NODE_BREAK_INFERRED) {
		last = arrlast(node->d_do_block.exprs);

		arrlast(node->d_do_block.exprs) = (ir_node_t){
			.kind = NODE_BREAK_UNIT,
			.loc = last.loc,
			.type = TYPE_BOTTOM,
			.d_break.blk_id = blk_id,
			.d_break.expr = ir_memdup(last),
		};
	}

	return blk->brk_type;
}

type_t cloop(ir_scope_t *s, ir_node_t *node, type_t upvalue) {
	cblk_t *blk = &c.blocks[c.blocks_len++];

	*blk = (cblk_t){
		.brk_type = TYPE_INFER,
		.upvalue = upvalue,
	};
	// TODO: see with `if`, we don't ignore the type and actually check it against `()`
	//       maybe we should go back to `if` and remove that behavior
	(void)cexpr(s, TYPE_INFER, node->d_loop.expr);
	c.blocks_len--;

	// if a loop block has no breaks, it loops forever
	if (blk->brk_type == TYPE_INFER) {
		blk->brk_type = TYPE_BOTTOM;
	}

	return blk->brk_type;
}

// TYPE_INFER on error
type_t cinfix_type_resolution(ir_node_t *lhs, ir_node_t *rhs, loc_t onerror) {
	ti_kind tkind_lhs = type_kind(lhs->type);
	ti_kind tkind_rhs = type_kind(rhs->type);

	bool lhs_numeric = ctype_is_numeric(lhs->type);
	bool rhs_numeric = ctype_is_numeric(rhs->type);

	// no explicit cast because pointer arithmetic relies on underlying type
	if (tkind_lhs == TYPE_PTR && ctype_is_integer(rhs->type)) {
		return lhs->type;
	}

	if (!lhs_numeric || !rhs_numeric) {
		return TYPE_INFER;
	}
	
	type_t ret;
	type_t rt = cconvert_integers(lhs->type, rhs->type);
	type_t lt = cconvert_integers(rhs->type, lhs->type);

	if (lt == TYPE_INFER && rt == TYPE_INFER) {
		return TYPE_INFER;
	}

	if (lt == TYPE_INFER) {
		ret = rt;
	} else if (rt == TYPE_INFER) {
		ret = lt;
	} else {
		ret = lt;
	}

	// insert explicit casts

	if (lhs->type != ret && lhs->type != TYPE_BOTTOM) {
		cir_unchecked_cast(ret, lhs, lhs->loc);
	} else if (rhs->type != ret && rhs->type != TYPE_BOTTOM) {
		cir_unchecked_cast(ret, rhs, rhs->loc);
	}

	return ret;
}

// TYPE_INFER on error
type_t cimplcit_cast(ir_node_t *expr, type_t to) {
	ti_kind tkind_to = type_kind(to);
	ti_kind tkind_from = type_kind(expr->type);

	bool to_numeric = ctype_is_numeric(to);
	bool from_numeric = ctype_is_numeric(expr->type);

	if (!to_numeric || !from_numeric) {
		return TYPE_INFER;
	}

	type_t rt = cconvert_integers(to, expr->type);

	if (rt == TYPE_INFER) {
		return TYPE_INFER;
	}

	if (rt != expr->type) {
		cir_unchecked_cast(rt, expr, expr->loc);
	}

	return rt;
}

// ? and i32 -> ? = i32
type_t cunify(type_t lhs, type_t rhs, loc_t onerror) {
	ti_kind lhs_kind = type_kind(lhs);
	ti_kind rhs_kind = type_kind(rhs);

	if (lhs == rhs) {
		return lhs;
	}

	// ? = lhs
	if (lhs_kind == TYPE_VAR) {
		typevar_replace(lhs, rhs);
		return rhs;
	}

	// ? = rhs
	if (rhs_kind == TYPE_VAR) {
		typevar_replace(rhs, lhs);
		return lhs;
	}

	// ! coerces to everything
	if (lhs_kind == TYPE_BOTTOM) {
		return rhs;
	}

	// ! coerces to everything
	if (rhs_kind == TYPE_BOTTOM) {
		return lhs;
	}
	
	err_with_pos(onerror, "type mismatch: expected `%s`, got `%s`", type_dbg_str(lhs), type_dbg_str(rhs));
}

void cfn(ir_node_t *node) {
	// 1. check if return type is infer, IT SHOULD NOT BE
	//    err_with_pos("complex inference and generics aren't implemented yet")
	// 2. cpattern_to_type(...) for each pattern
	ir_var_t *varp = VAR_PTR(node->d_proc_decl.var);
	
	// create the typevars
	if (varp->type == TYPE_INFER) {
		// make sure they have the same argument length
		int plen = -1;
		for (u32 i = 0, c = arrlenu(node->d_proc_decl.patterns); i < c; i++) {
			if (plen == (u32)-1) {
				plen = arrlenu(node->d_proc_decl.patterns[i].d_tuple.elems);
			} else {
				if (plen != arrlenu(node->d_proc_decl.patterns[i].d_tuple.elems)) {
					err_with_pos(node->d_proc_decl.patterns[i].loc, "function argument length mismatch on pattern");
				}
			}
		}

		assert(plen > 0); // all functions take a single argument

		// plen: 2
		//     ? -> ? -> ?
		//     ^    ^

		// TODO: ?18 -> ?16 -> ?15
		// TODO: do not create a typevar for the return value
		type_t last = typevar_new();
		type_t fn_type = last;
		for (u32 i = 0; i < plen; i++) {
			tinfo_t typeinfo = {
				.kind = TYPE_FN,
				.d_fn = {
					.arg = typevar_new(),
					.ret = fn_type,
				}
			};
			
			fn_type = type_new(typeinfo, NULL);
		}
		// add to infer vars
		c.ifvars[c.ifvars_len++] = (cinfer_vars_t){
			.type = last,
			.def = node,
			.onerror = node->loc,
		};
		varp->type = fn_type;
	} else {
		if (type_kind(varp->type) != TYPE_FN) {
			err_with_pos(node->loc, "type mismatch: expected function type, got `%s`", type_dbg_str(varp->type));
		}
		// you know, a function being a bunch of patterns matching
		// on a tuple of args is a pretty good abstraction
		type_t args_tuple = cfn_args_to_tuple(varp->type);
		type_t return_type = cfn_type_full_return(varp->type);
		for (u32 i = 0, c = arrlenu(node->d_proc_decl.scopes); i < c; i++) {
			cpattern(&node->d_proc_decl.scopes[i], &node->d_proc_decl.patterns[i], args_tuple);
		}
		for (u32 i = 0, c = arrlenu(node->d_proc_decl.exprs); i < c; i++) {
			cscope_enter();
			type_t type = cexpr(&node->d_proc_decl.scopes[i], return_type, &node->d_proc_decl.exprs[i]);
			cscope_leave();
			ctype_assert(type, return_type, node->d_proc_decl.exprs[i].loc);
		}
	}
}

void cfn_calculate_return(ir_node_t *node) {
	ir_var_t *varp = VAR_PTR(node->d_proc_decl.var);

	type_t args_tuple = cfn_args_to_tuple(varp->type);
	type_t return_var = cfn_type_full_return(varp->type);
	assert(type_kind_raw(return_var) == TYPE_VAR);
	type_t return_type = TYPE_INFER;

	for (u32 i = 0, c = arrlenu(node->d_proc_decl.scopes); i < c; i++) {
		cpattern(&node->d_proc_decl.scopes[i], &node->d_proc_decl.patterns[i], args_tuple);
	}
	for (u32 i = 0, c = arrlenu(node->d_proc_decl.exprs); i < c; i++) {
		cscope_enter();
		type_t type = cexpr(&node->d_proc_decl.scopes[i], return_type, &node->d_proc_decl.exprs[i]);
		cscope_leave();
		if (return_type == TYPE_INFER) {
			typevar_replace(return_var, type);
			return_type = return_var;
		}
		ctype_assert(type, return_type, node->d_proc_decl.exprs[i].loc);
	}
}

// TODO: eventually?
enum : u8 {
	CEXPR_NONE,
	CEXPR_LVALUE,
};

// upvalue only makes sense for number literals, which may have a lot of types
type_t cexpr(ir_scope_t *s, type_t upvalue, ir_node_t *expr) {
	switch (expr->kind) {
		case NODE_GLOBAL_UNRESOLVED: {
			for (u32 i = 0; i < arrlenu(c.modp->toplevel.locals); i++) {
				ir_rvar_t var = c.modp->toplevel.locals[i];
				ir_var_t *varp = VAR_PTR(var);
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
		/* case NODE_VAR_DECL: {
			ir_var_t *var = VAR_PTR(expr->d_var_decl.lhs);
			if (var->type == TYPE_INFER) {
				var->type = cexpr(s, upvalue, expr->d_var_decl.rhs);
			} else {
				type_t expr_type = cexpr(s, var->type, expr->d_var_decl.rhs);

				// need to convert expression to variable type
				if (expr_type != var->type) {
					// implicitly convert
					type_t ret = cimplcit_cast(expr->d_var_decl.rhs, var->type);
					if (ret == TYPE_INFER) {
						err_with_pos(expr->loc, "type mismatch: expected `%s`, got `%s`", type_dbg_str(var->type), type_dbg_str(expr_type));
					}
				}
			}
			return TYPE_UNIT;
		} */
		case NODE_LET_DECL: {
			type_t expr_type = cexpr(s, TYPE_INFER, expr->d_let_decl.expr);
			cpattern(s, &expr->d_let_decl.pattern, expr_type);
			return TYPE_UNIT;
		}
		case NODE_PROC_DECL: {
			cfn(expr);
			return TYPE_UNIT;
		}
		case NODE_PREFIX: {
			switch (expr->d_prefix.kind) {
				case TOK_SUB: {
					type_t type = cexpr(s, upvalue, expr->d_prefix.expr);
					if (!ctype_is_numeric(type)) {
						err_with_pos(expr->loc, "type mismatch: expected numeric type, got `%s`", type_dbg_str(type));
					}
					expr->type = type;
					break;
				}
				case TOK_NOT: {
					type_t type = cexpr(s, TYPE_INFER, expr->d_prefix.expr);
					if (type != TYPE_BOOL) {
						err_with_pos(expr->loc, "type mismatch: expected `bool`, got `%s`", type_dbg_str(type));
					}
					expr->type = TYPE_BOOL;
					break;
				}
				default: {
					assert_not_reached();
				}
			}
			return expr->type;
		}
		case NODE_POSTFIX: {
			type_t type = cexpr(s, upvalue, expr->d_postfix.expr);
			if (!ctype_is_numeric(type)) {
				err_with_pos(expr->loc, "type mismatch: expected numeric type, got `%s`", type_dbg_str(type));
			}
			expr->type = type;
			return expr->type;
		}
		case NODE_INFIX: {
			ir_node_t *lhs = expr->d_infix.lhs;
			ir_node_t *rhs = expr->d_infix.rhs;

			// short circuting || and &&
			bool both_be_bool = expr->d_infix.kind == TOK_AND || expr->d_infix.kind == TOK_OR;

			if (!both_be_bool) {
				// integer promotion rules for lhs and rhs
				// TODO: don't check right expression without left expression
				type_t lhs_type = cexpr(s, upvalue, lhs);
				type_t rhs_type = cexpr(s, lhs_type, rhs);
				// _ = x + y

				if (expr->d_infix.kind == TOK_ASSIGN) {
					expr->type = cunify(lhs_type, rhs_type, expr->d_infix.rhs->loc);
				} else if ((expr->type = cinfix_type_resolution(lhs, rhs, expr->loc)) == TYPE_INFER) {
					err_with_pos(expr->loc, "type mismatch: cannot perform `%s` %s `%s`", type_dbg_str(lhs->type), tok_op_str(expr->d_infix.kind), type_dbg_str(rhs->type));
				}
			} else {
				type_t lhs_type = cexpr(s, TYPE_INFER, lhs);
				type_t rhs_type = cexpr(s, TYPE_INFER, rhs);

				if (lhs_type != TYPE_BOTTOM && lhs_type != TYPE_BOOL) {
					err_with_pos(lhs->loc, "type mismatch: expected `bool`, got `%s`", type_dbg_str(lhs_type));
				}
				if (rhs_type != TYPE_BOTTOM && rhs_type != TYPE_BOOL) {
					err_with_pos(rhs->loc, "type mismatch: expected `bool`, got `%s`", type_dbg_str(rhs_type));
				}
			}

			if (TOK_IS_COND(expr->d_infix.kind)) {
				expr->type = TYPE_BOOL;
			}

			return expr->type;
		}
		case NODE_VAR: {
			// we know this already
			ir_var_t *var = VAR_PTR(expr->d_var);
			// only variables may contain typevars
			expr->type = type_underlying(var->type);
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
		case NODE_BOOL_LIT: {
			return TYPE_BOOL;
		}
		case NODE_CAST: {
			type_t t = cexpr(s, expr->type, expr->d_cast);

			if (expr->d_cast->kind == NODE_INTEGER_LIT) {
				if (sv_cmp_literal(expr->d_cast->d_integer_lit.lit, "0")) {
					err_with_pos(expr->loc, "cannot cast `0` to `%s`", type_dbg_str(expr->type));
				}
			}
			// ????????????
			
			// TODO: check if this cast is even possible
			// if (cconvert_implicit(expr->type, t) == TYPE_INFER)
			(void)t;
			return expr->type;
		}
		case NODE_SYM_UNRESOLVED: {
			rmod_t sym_mod = expr->d_sym_unresolved.mod;
			mod_t *sym_modp = MOD_PTR(sym_mod);
			for (u32 i = 0, c = arrlenu(sym_modp->toplevel.locals); i < c; i++) {
				ir_var_t *var = VAR_PTR(sym_modp->toplevel.locals[i]);
				if (var->is_pub && var->name == expr->d_sym_unresolved.name) {
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
				err_with_pos(expr->loc, "type mismatch: expected function type, got `%s`", type_dbg_str(f_type));
			}
			tinfo_t *fn = type_get(f_type);
			type_t arg_type = cexpr(s, fn->d_fn.arg, expr->d_call.arg);
			assert(cunify(fn->d_fn.arg, arg_type, expr->loc) != TYPE_INFER); // TODO: use

			// infer the return value, no more functions left
			if (type_kind(fn->d_fn.ret) == TYPE_VAR) {
				// ?1 -> ?2 -> ?3
				cinfer_vars_t *ifvars;
				assert((ifvars = cinfer_get(fn->d_fn.ret)) != NULL);
				cfn_calculate_return(ifvars->def);
			}
			expr->type = fn->d_fn.ret;
			return expr->type;
		}
		case NODE_DO_BLOCK: {
			return cdo(expr, upvalue);
		}
		case NODE_LOOP: {
			return cloop(s, expr, upvalue);
		}
		case NODE_TUPLE_UNIT: {
			return TYPE_UNIT;
		}
		case NODE_BREAK_INFERRED:
		case NODE_BREAK_UNIT:
		case NODE_BREAK: {
			cblk_t *blk = &c.blocks[expr->d_break.blk_id]; // 1:1 correspondence
			
			type_t brk_type;
			switch (expr->kind) {
				case NODE_BREAK_UNIT: {
					if (expr->d_break.expr) {
						(void)cexpr(s, blk->upvalue, expr->d_break.expr);
					}
					// ignore type from expr
					brk_type = TYPE_UNIT;
					break;
				}
				case NODE_BREAK_INFERRED:
				case NODE_BREAK: {
					brk_type = cexpr(s, blk->upvalue, expr->d_break.expr);
					break;
				}
				default: {
					assert_not_reached();
				}
			}

			// last expr in a `do` block, break inserted by compiler
			// the distinction is made so we can do such:
			//
			// t :: () -> ()
			// t: _ = do
			//     expr          ; this expr returns non ()
			//
			if (expr->kind == NODE_BREAK_INFERRED && blk->upvalue == TYPE_UNIT) {
				brk_type = TYPE_UNIT;
			}

			// ! encompasses all types
			if (blk->brk_type == TYPE_INFER || blk->brk_type == TYPE_BOTTOM) {
				blk->brk_type = brk_type;
				blk->brk_loc = expr->loc;
			} else if (brk_type != TYPE_BOTTOM && blk->brk_type != brk_type) {
				print_err_with_pos(expr->loc, "type mismatch: expected `%s`, got `%s`", type_dbg_str(blk->brk_type), type_dbg_str(brk_type));
				print_hint_with_pos(blk->brk_loc, "type `%s` deduced here", type_dbg_str(blk->brk_type));
				err_unwind();
			}
			return TYPE_BOTTOM;
		}
		case NODE_IF: {
			type_t cond_type = cexpr(s, TYPE_BOOL, expr->d_if.cond);
			if (cond_type != TYPE_BOOL) {
				err_with_pos(expr->d_if.cond->loc, "type mismatch: expected `bool`, got `%s`", type_dbg_str(cond_type));
			}
			if (expr->d_if.els == NULL) {
				upvalue = TYPE_INFER; // should be (), but does that even matter really?
			}
			type_t then_type = cexpr(s, upvalue, expr->d_if.then);

			if (expr->d_if.els == NULL) {
				// TODO: possibly ignore?
				if (then_type != TYPE_UNIT) {
					err_with_pos(expr->d_if.then->loc, "type mismatch: expected `()`, got `%s`", type_dbg_str(then_type));
				}
				expr->type = TYPE_UNIT;
			} else {
				type_t else_type = cexpr(s, upvalue, expr->d_if.els);
				if ((expr->type = cinfix_type_resolution(expr->d_if.then, expr->d_if.els, expr->loc)) == TYPE_INFER) {
					err_with_pos(expr->d_if.els->loc, "type mismatch: expected `%s`, got `%s`", type_dbg_str(then_type), type_dbg_str(else_type));
				}
			}
			return expr->type;
		}
		case NODE_UNDEFINED: {
			return TYPE_UNDEFINED;
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

	cscope_enter();
	ctoplevel_exprs(&c.modp->toplevel, c.modp->exprs);
	cscope_leave();
}