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

void cir_cast(type_t to, ir_node_t *node, loc_t loc) {
	ir_node_t *dup = ir_memdup(*node);

	*node = (ir_node_t) {
		.kind = NODE_CAST,
		.loc = loc,
		.type = to,
		.d_cast = dup,
	};
}

bool ctype_is_numeric(type_t type) {
	ti_kind kind = type_kind(type);
	
	// TODO: aliases etc
	return (kind >= TYPE_SIGNED_INTEGERS_START && kind <= TYPE_SIGNED_INTEGERS_END) ||
		(kind >= TYPE_UNSIGNED_INTEGERS_START && kind <= TYPE_UNSIGNED_INTEGERS_END) ||
		(kind == TYPE_F32 || kind == TYPE_F64);
}

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
		if (type_kind(tinfo->d_fn.ret) < _TYPE_CONCRETE_MAX) {
			break;
		}
		tinfo = type_get(tinfo->d_fn.ret);
	} while (tinfo->kind == TYPE_FN);

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

bool ckind_is_numeric(ti_kind kind) {
	// TODO: aliases etc
	return (kind >= TYPE_SIGNED_INTEGERS_START && kind <= TYPE_SIGNED_INTEGERS_END) ||
		(kind >= TYPE_UNSIGNED_INTEGERS_START && kind <= TYPE_UNSIGNED_INTEGERS_END) ||
		(kind == TYPE_F32 || kind == TYPE_F64);
}

// signal that node is used, raise errors if needed
// cuse() should be called when you're:
//
// 1. mutating an expr
// 2. need to know the type of something to perform an action
//
// + reading a variable isn't a use
//
// so then, the rhs of an assignment is not a use, brk with a value
// isn't a use, and so on. adding values and calling are definitely uses though.
//
// what isn't a use but we need to know it's type, is casting.
// call cuse_type() on that. it will then allow:
//
// let 'v = undefined:i32
//
// all variables that aren't used or are simply () are removed entirely,
// which allows this to work.
//
// TODO: a simple flag on a var to indicate if it's used or not should be good
//       enough to weed out undefined and etc. although you should check types.
//
void cuse_type(ir_node_t *node);
//
void cuse(ir_node_t *node) {
	assert(node->type != TYPE_INFER);

	if (type_kind(node->type) == TYPE_UNDEFINED) {
		err_with_pos(node->loc, "use will cause undefined behaviour");
	}

	cuse_type(node);
}

void cuse_type(ir_node_t *node) {
	if (type_kind(node->type) == TYPE_VAR) {
		// TODO: search for typevar in infer vars, then get `onerror` loc
		//       will probably have to walk down the function types to find it
		//       or just simply walk the `let decls`
		printf("TODO: cuse() TYPE_VAR\n");
	}
}

void cmutable_lvalue(ir_node_t *node) {
	if (node->kind != NODE_VAR) {
		err_with_pos(node->loc, "cannot assign to non-variable");
	}
}

void clambda_body(ir_node_t *lambda);

// TYPE_INFER on error
// don't call this
type_t cunify_innards(type_t lhs_t, type_t rhs_t) {
	ti_kind lhs_kind = type_kind(lhs_t);
	ti_kind rhs_kind = type_kind(rhs_t);

	if (lhs_t == rhs_t) {
		return lhs_t;
	}

	// ? = lhs
	if (lhs_kind == TYPE_VAR) {
		typevar_replace(lhs_t, rhs_t);
		return rhs_t;
	}

	// ? = rhs
	if (rhs_kind == TYPE_VAR) {
		typevar_replace(rhs_t, lhs_t);
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

	// undefined coerces to everything
	if (rhs_kind == TYPE_UNDEFINED) {
		return lhs_t;
	}

	// undefined coerces to everything
	if (lhs_kind == TYPE_UNDEFINED) {
		return rhs_t;
	}

	// functions
	if (lhs_kind == TYPE_FN && rhs_kind == TYPE_FN) {
		tinfo_t *lhs_tinfo = type_get(lhs_t);
		tinfo_t *rhs_tinfo = type_get(rhs_t);

		bool sub_lhs = type_kind_raw(lhs_tinfo->d_fn.ret) == TYPE_VAR;
		bool sub_rhs = type_kind_raw(rhs_tinfo->d_fn.ret) == TYPE_VAR;

		assert(!(sub_lhs && sub_rhs));

		type_t arg = cunify_innards(lhs_tinfo->d_fn.arg, rhs_tinfo->d_fn.arg);
		type_t ret = cunify_innards(lhs_tinfo->d_fn.ret, rhs_tinfo->d_fn.ret);

		if (arg == TYPE_INFER || ret == TYPE_INFER) {
			return TYPE_INFER;
		}

		if (sub_lhs) {
			cinfer_vars_t *ifvars;
			assert((ifvars = cinfer_get(lhs_tinfo->d_fn.ret)) != NULL);
			clambda_body(ifvars->def);
		}

		if (sub_rhs) {
			cinfer_vars_t *ifvars;
			assert((ifvars = cinfer_get(rhs_tinfo->d_fn.ret)) != NULL);
			clambda_body(ifvars->def);
		}

		tinfo_t fn = {
			.kind = TYPE_FN,
			.d_fn = {
				.arg = arg,
				.ret = ret,
			}
		};

		return type_new(fn, NULL);
	}

	return TYPE_INFER;
}

// lhs_t <- rhs_t
// doesn't apply implicit casts like cunify does
type_t cunify_type(type_t lhs_t, type_t rhs_t, loc_t onerror) {
	type_t t;
	if ((t = cunify_innards(lhs_t, rhs_t)) != TYPE_INFER) {
		return t;
	}

	err_with_pos(onerror, "type mismatch: expected `%s`, got `%s`", type_dbg_str(lhs_t), type_dbg_str(rhs_t));
}

// lhs <- rhs
type_t cunify(type_t lhs_t, ir_node_t *rhs) {
	type_t rhs_t = type_underlying(rhs->type);
	ti_kind lhs_kind = type_kind(lhs_t);
	ti_kind rhs_kind = type_kind(rhs_t);

	type_t t;
	if ((t = cunify_innards(lhs_t, rhs_t)) != TYPE_INFER) {
		return t;
	}

	if (ckind_is_numeric(lhs_kind) && ckind_is_numeric(rhs_kind)) {
		type_t lhs_c = cconvert_integers(lhs_t, rhs_t);

		if (lhs_c != TYPE_INFER) {
			if (lhs_c != rhs_t) {
				cir_cast(lhs_c, rhs, rhs->loc);
			}

			return lhs_c;
		}
	}

	err_with_pos(rhs->loc, "type mismatch: expected `%s`, got `%s`", type_dbg_str(lhs_t), type_dbg_str(rhs_t));
}

// useful for determining a functions type, but not now
void cpattern(ir_pattern_t *pattern, type_t type) {
	switch (pattern->kind) {
		case PATTERN_TUPLE: {
			tinfo_t *tinfo = type_get(type);
			if (tinfo->kind != TYPE_TUPLE) {
				err_with_pos(pattern->loc, "cannot match tuple pattern against non-tuple type `%s`", type_dbg_str(type));
			}
			if (arrlenu(tinfo->d_tuple.elems) != arrlenu(pattern->d_tuple.elems)) {
				err_with_pos(pattern->loc, "cannot match tuple pattern against tuple type `%s` of different length", type_dbg_str(type));
			}
			for (size_t i = 0; i < arrlenu(pattern->d_tuple.elems); i++) {
				cpattern(&pattern->d_tuple.elems[i], tinfo->d_tuple.elems[i]);
			}
			break;
		}
		case PATTERN_TUPLE_UNIT: {
			(void)cunify_type(TYPE_UNIT, type, pattern->loc);
			break;
		}
		case PATTERN_VAR: {
			ir_var_t *var = VAR_PTR(pattern->d_var);
			if (var->type == TYPE_INFER) {
				var->type = type;
			} else {
				(void)cunify_type(var->type, type, pattern->loc);
			}
			break;
		}
		case PATTERN_INTEGER_LIT: {
			// TODO: check integer is inbounds for type
			if (!ctype_is_numeric(type)) {
				err_with_pos(pattern->loc, "type mismatch: expected integer, got `%s`", type_dbg_str(type));
			}
			break;
		}
		case PATTERN_UNDERSCORE: {
			break;
		}
		default: {
			assert_not_reached();
		}
	}
}

type_t cpattern_upvalue(ir_pattern_t *pattern) {
	switch (pattern->kind) {
		case PATTERN_TUPLE: {
			type_t *types = NULL;

			for (size_t i = 0; i < arrlenu(pattern->d_tuple.elems); i++) {
				type_t type = cpattern_upvalue(&pattern->d_tuple.elems[i]);
				arrpush(types, type);
			}

			tinfo_t tuple = {
				.kind = TYPE_TUPLE,
				.d_tuple = {
					.elems = types,
				}
			};

			return type_new(tuple, NULL);
		}
		case PATTERN_VAR: {
			ir_var_t *var = VAR_PTR(pattern->d_var);
			return var->type;
		}
		case PATTERN_INTEGER_LIT: {
			return TYPE_INFER; // or TYPE_I32? i don't think it matters.
		}
		case PATTERN_UNDERSCORE: {
			return TYPE_INFER;
		}
		case PATTERN_TUPLE_UNIT: {
			return TYPE_UNIT;
		}
		default: {
			assert_not_reached();
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

type_t cinfix(ir_scope_t *s, type_t upvalue, ir_node_t *node) {
	ir_node_t *lhs = node->d_infix.lhs;
	ir_node_t *rhs = node->d_infix.rhs;
	tok_t kind = node->d_infix.kind;

	bool is_bool_op = kind == TOK_AND || kind == TOK_OR;

	type_t lhs_t;
	type_t rhs_t;

	// ignore `upvalue`, the only places where a var doesn't have an underlying type is when it's `undefined`
	// undefined means it's an error to read, so upvalues don't really matter anyway.
	//
	// no need for a cuse() on the assignments, we're only reading here
	// further uses of possible unknown copies will be caught by cuse()
	if (kind == TOK_ASSIGN) {
		// TODO: do lvalue checks here
		// TODO: extract place, but it would probably just be a var anyway

		// unification of TYPE_UNDEFINED if posed with such
		if (lhs->kind != NODE_VAR) {
			err_with_pos(lhs->loc, "cannot assign to non-variable, TODO: lvalue checks");
		}

		ir_var_t *var = VAR_PTR(lhs->d_var);
		if (var->type == TYPE_UNDEFINED) {
			var->type = rhs_t;
		}

		lhs_t = var->type;
		rhs_t = cexpr(s, lhs_t, rhs);
	} else if (!is_bool_op) {
		lhs_t = cexpr(s, upvalue, lhs);
		rhs_t = cexpr(s, lhs_t, rhs);
		cuse(lhs);
		cuse(rhs);
	} else {
		lhs_t = cexpr(s, TYPE_BOOL, lhs);
		rhs_t = cexpr(s, TYPE_BOOL, rhs);
		cuse(lhs);
		cuse(rhs);
	}
	// small sanity checks
	assert(type_underlying(lhs_t) == lhs_t);
	assert(type_underlying(rhs_t) == rhs_t);
	//
	ti_kind lhs_kind = type_kind(lhs_t);
	ti_kind rhs_kind = type_kind(rhs_t);

	if (is_bool_op) {
		(void)cunify(TYPE_BOOL, lhs);
		(void)cunify(TYPE_BOOL, rhs);
		node->type = TYPE_BOOL;
		return node->type;
	}
	
	// v :: *i32
	// v + 20      becomes:      v + (20:usize * 4):*i32
	// pointer arithmetic relies on underlying type
	if (lhs_kind == TYPE_PTR) {
		// disallow / and % on pointers? maybe not.
		// subtracting two pointers should result in an isize? possibly.
		assert_not_reached();
	}

	node->type = cunify(lhs_t, rhs);

	// comparison operators
	// ==  !=  <  >  <=  >= 
	if (kind == TOK_EQ || kind == TOK_NEQ || kind == TOK_LT || kind == TOK_GT || kind == TOK_LE || kind == TOK_GE) {
		node->type = TYPE_BOOL;
	}

	return node->type;
}

type_t cmatch(ir_scope_t *s, ir_node_t *node, type_t upvalue) {
	type_t expr_type = cexpr(s, TYPE_INFER, node->d_match.expr);

	for (u32 i = 0, c = arrlenu(node->d_match.patterns); i < c; i++) {
		cpattern(&node->d_match.patterns[i], expr_type);
	}

	cscope_enter();
	type_t ret_type = cexpr(&node->d_match.scopes[0], upvalue, &node->d_match.exprs[0]);
	cscope_leave();

	for (u32 i = 1, c = arrlenu(node->d_match.exprs); i < c; i++) {
		ir_node_t *arm = &node->d_match.exprs[i];

		cscope_enter();
		(void)cexpr(&node->d_match.scopes[i], upvalue, arm);
		cscope_leave();

		(void)cunify(ret_type, arm);
	}

	return node->type = ret_type;
}

// 0 on error
u32 cfn_length(type_t fn) {
	u32 len = 0;
	while (type_kind(fn) == TYPE_FN) {
		len++;
		tinfo_t *tinfo = type_get(fn);
		fn = tinfo->d_fn.ret;
	}
	return len;
}

// apply types to args and return ret type
type_t clambda_apply_type(ir_node_t *lambda) {
	u32 plen = arrlenu(lambda->d_lambda.args);

	type_t type = lambda->type;
	for (u32 i = 0; i < plen; i++) {
		assert(type_kind(type) == TYPE_FN);
		type_t arg = type_get(type)->d_fn.arg;
		VAR_PTR(lambda->d_lambda.args[i])->type = arg;
		type = type_get(type)->d_fn.ret;
	}

	return type;
}

void clambda_body(ir_node_t *lambda) {
	type_t ret = clambda_apply_type(lambda);
	type_t upvalue = TYPE_INFER;

	if (type_kind(ret) != TYPE_VAR) {
		upvalue = ret;
	}

	// type_t type = cexpr(lambda->d_lambda.scope, upvalue, lambda->d_lambda.expr);
	assert(lambda->d_lambda.expr->kind == NODE_MATCH);
	type_t type = cmatch(lambda->d_lambda.scope, lambda->d_lambda.expr, upvalue);

	if (type_kind(ret) == TYPE_VAR) {
		typevar_replace(ret, type);
	} else {
		// the type of a match is determined by the type of the first expression in the arm
		// if there is a type mismatch, we need to raise an error with the loc of that expr

		// what a mouthful...
		loc_t onerror = lambda->d_lambda.expr->d_match.exprs[0].loc;
		(void)cunify_type(ret, type, onerror);
	}
}

// if `opt_decl` is nonnull, it's a proc decl
type_t clambda(ir_node_t *lambda, type_t upvalue, ir_var_t *opt_decl) {
	loc_t onerror = lambda->loc;

	if (opt_decl) {
		onerror = opt_decl->loc;
	}

	// TODO: normal lambdas will always have a match expression just like decls
	//       build up the most generic type from a match expression

	u32 plen = arrlenu(lambda->d_lambda.args);

	// plen: 2
	//     ? -> ? -> ?
	//     ^    ^

	// `>= plen` because we can return functions
	if (upvalue != TYPE_INFER && cfn_length(upvalue) >= plen) {
		// we have type
		lambda->type = upvalue;
		clambda_body(lambda);
	} else {
		// no type, infer please

		// TODO: do not create a typevar for the return value
		//       simple as `last = TYPE_INFER`
		//       however you'll have to match on the arg typevar
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
			.def = lambda,
			.onerror = onerror,
		};

		lambda->type = fn_type;
	}

	return lambda->type;
}

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
		case NODE_LET_DECL: {
			type_t expr_type;
			
			ir_var_t *varp;
			if (expr->d_let_decl.pattern.kind == PATTERN_VAR && (varp = VAR_PTR(expr->d_let_decl.pattern.d_var))->is_proc_decl) {
				assert(expr->d_let_decl.expr->kind == NODE_LAMBDA);
				expr_type = clambda(expr->d_let_decl.expr, varp->type, varp);
			} else {
				type_t upvalue = cpattern_upvalue(&expr->d_let_decl.pattern);
				expr_type = cexpr(s, upvalue, expr->d_let_decl.expr);			
			}
			
			cpattern(&expr->d_let_decl.pattern, expr_type);
			return TYPE_UNIT;
		}
		case NODE_MATCH: {
			return cmatch(s, expr, upvalue);
		}
		/* case NODE_PROC_DECL: {
			cfn(expr);
			return TYPE_UNIT;
		} */
		case NODE_PREFIX: {
			switch (expr->d_prefix.kind) {
				case TOK_SUB: {
					type_t type = cexpr(s, upvalue, expr->d_prefix.expr);
					cuse(expr->d_prefix.expr);
					if (!ctype_is_numeric(type)) {
						err_with_pos(expr->loc, "type mismatch: expected numeric type, got `%s`", type_dbg_str(type));
					}
					expr->type = type;
					break;
				}
				case TOK_NOT: {
					type_t type = cexpr(s, TYPE_INFER, expr->d_prefix.expr);
					cuse(expr->d_prefix.expr);
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
			cuse(expr->d_postfix.expr);
			if (!ctype_is_numeric(type)) {
				err_with_pos(expr->loc, "type mismatch: expected numeric type, got `%s`", type_dbg_str(type));
			}
			expr->type = type;
			return expr->type;
		}
		case NODE_INFIX: {
			return cinfix(s, upvalue, expr);
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
			type_t type = cexpr(s, expr->type, expr->d_cast);
			cuse_type(expr->d_cast);

			if (type == expr->type) {
				*expr = *expr->d_cast;
			}

			// ????????????
			/* if (expr->d_cast->kind == NODE_INTEGER_LIT) {
				if (sv_cmp_literal(expr->d_cast->d_integer_lit.lit, "0")) {
					err_with_pos(expr->loc, "cannot cast `0` to `%s`", type_dbg_str(expr->type));
				}
			} */
			
			// TODO: check if this cast is even possible
			// if (cconvert_implicit(expr->type, t) == TYPE_INFER)
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
			cuse(expr->d_call.f);
			if (type_kind(f_type) != TYPE_FN) {
				err_with_pos(expr->loc, "type mismatch: expected function type, got `%s`", type_dbg_str(f_type));
			}

			// f :: ?1 -> ?2
			// a :: i32
			// - f a
			// * ?1 = i32

			tinfo_t *fn = type_get(f_type);
			type_t arg_type = cexpr(s, fn->d_fn.arg, expr->d_call.arg);
			cuse(expr->d_call.arg);
			(void)cunify(fn->d_fn.arg, expr->d_call.arg);

			// infer the return value, no more functions left
			if (type_kind(fn->d_fn.ret) == TYPE_VAR) {
				// ?1 -> ?2 -> ?3
				cinfer_vars_t *ifvars;
				assert((ifvars = cinfer_get(fn->d_fn.ret)) != NULL);
				clambda_body(ifvars->def);
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
				expr->type = cunify(TYPE_UNIT, expr->d_if.then);
			} else {
				type_t else_type = cexpr(s, upvalue, expr->d_if.els);
				expr->type = cunify(then_type, expr->d_if.els);
			}
			return expr->type;
		}
		case NODE_UNDEFINED: {
			return TYPE_UNDEFINED;
		}
		case NODE_TUPLE: {
			// unpack upvalue if applicable
			type_t *upvales = NULL;
			if (upvalue != TYPE_INFER && type_kind(upvalue) == TYPE_TUPLE) {
				upvales = type_get(upvalue)->d_tuple.elems;
				if (arrlenu(upvales) != arrlenu(expr->d_tuple.elems)) {
					upvales = NULL;
				}
			}

			type_t *types = NULL;

			for (u32 i = 0, c = arrlenu(expr->d_tuple.elems); i < c; i++) {
				type_t upvalue = TYPE_INFER;
				if (upvales) {
					upvalue = upvales[i];
				}
				ir_node_t *inner = &expr->d_tuple.elems[i];
				type_t type = cexpr(s, upvalue, inner);
				cuse(inner);
				arrpush(types, type);
			}

			expr->type = type_new((tinfo_t){
				.kind = TYPE_TUPLE,
				.d_tuple = {
					.elems = types,
				}
			}, NULL);

			return expr->type;
		}
		case NODE_ARRAY_LIT: {
			assert(expr->d_array_lit.elems != NULL); // TODO: handle []

			// never implicit, always demote to slice when upvalue only 
			//
			type_t elem_upvale = TYPE_INFER;
			bool is_slice = false;
			if (upvalue != TYPE_INFER) {
				switch (type_kind(upvalue)) {
					case TYPE_ARRAY: {
						elem_upvale = type_get(upvalue)->d_array.elem;
						break;
					}
					case TYPE_SLICE: {
						elem_upvale = type_get(upvalue)->d_slice.elem;
						is_slice = true;
						break;
					}
					default: {
						break;
					}
				}
			}

			type_t elem_type = cexpr(s, elem_upvale, &expr->d_array_lit.elems[0]);
			// []elem_type

			u32 length = arrlenu(expr->d_array_lit.elems);

			for (u32 i = 1; i < length; i++) {
				ir_node_t *inner = &expr->d_array_lit.elems[i];
				
				type_t type = cexpr(s, elem_type, inner);
				cuse(inner);
				(void)cunify(elem_type, inner);
			}

			tinfo_t info;
			if (is_slice) {
				info = (tinfo_t){
					.kind = TYPE_SLICE,
					.d_slice = {
						.elem = elem_type,
					}
				};
			} else {
				info = (tinfo_t){
					.kind = TYPE_ARRAY,
					.d_array = {
						.elem = elem_type,
						.length = length,
					}
				};
			}

			expr->type = type_new(info, NULL);
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
		(void)cexpr(s, TYPE_INFER, &exprs[i]);
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