#include "all.h"
#include "check.h"
#include "hir.h"

void ccall_args(ir_desc_t *desc, type_t upvalue, hir_expr_t *expr);

// TODO: remove `cufcs_autocall` and make specialised functions
//       for expressions that take a `*call` parameter

// auto insert UFCS
void cufcs_autocall(ir_desc_t *desc, hir_expr_t *expr, u8 cfg) {
	// f x
	// f = x
	// do nothing, a call isn't applicable here
	if (cfg & BM_LVALUE || cfg & BM_CALL) {
		return;
	}

	// k = f     (call f)

	// convert to call and recheck
	if (type_kind(expr->type) == TYPE_FUNCTION) {
		hir_expr_t call = (hir_expr_t){
			.kind = EXPR_CALL,
			.loc = expr->loc,
			.type = TYPE_INFER,
			.d_call = {
				.f = hir_dup(*expr),
				.args = NULL,
			},
		};
		*expr = call;
		ccall_args(desc, TYPE_INFER, expr);
	}
}

// sets expr->type, doesn't check `f`
void ccall_args(ir_desc_t *desc, type_t upvalue, hir_expr_t *expr) {
	// iterate one by one over the args
	// we don't use a curried form internally to not waste analysis checking
	// for partial application. most function calls are complete calls anyway.
	//
	// `f x g` will compile into a merged call, you'll need to separate them.
	//
	//     f(x: T): T -> ()          (TODO: update fn type syntax, functions can be 0 arity now)
	//
	//     f(x, g) -> f(x)(g)
	//

	// TODO: remove partial application from the language

	// for every arg, if it exhausts the current arguments of a function type
	// set that return value to the list of argument types, and so on.

	tinfo_t *fn_info = type_get(expr->d_call.f->type);

	type_t *args = fn_info->d_fn.args;
	u32 cursor = 0;
	for (u32 i = 0, c = arrlenu(args); i < c; i++, cursor++) {
		if (cursor >= arrlenu(expr->d_call.args)) {
			// TODO: possible papp
			// TODO: extract the return value, using the type as a function itself
			err_with_pos(expr->loc, "not enough arguments, expected %u", c);
		}

		hir_expr_t *arg_expr = &expr->d_call.args[i];
		type_t arg_upvalue = args[cursor];

		(void)cexpr(desc, arg_upvalue, arg_expr, BM_RVALUE);
		(void)ctype_unify(arg_upvalue, arg_expr);
	}

	u32 papp = arrlenu(expr->d_call.args) - cursor;
	assert(papp == 0); // TODO: partial apply

	expr->type = fn_info->d_fn.ret;
}

void ccall(ir_desc_t *desc, type_t upvalue, hir_expr_t *expr) {
	u8 cfg = BM_RVALUE | BM_CALL;
	
	if (expr->d_call.f->kind == EXPR_FIELD && expr->d_call.f->d_field.field != ISTR_NONE) {
		cnamed_field(desc, TYPE_INFER, expr->d_call.f, expr, cfg);
	} else {
		type_t f_type = cexpr(desc, TYPE_INFER, expr->d_call.f, cfg);
		if (type_kind(f_type) != TYPE_FUNCTION) {
			err_with_pos(expr->loc, "type mismatch: expected function type, got `%s`", type_dbg_str(f_type));
		}
	}

	ccall_args(desc, TYPE_INFER, expr);
}

// TODO: extract cnamed_field into UFCS autocall
// TODO: field access properly into call

// named field access, unwrap to method/access. perform auto deref
// cfg only used on field access or when call == NULL
void cnamed_field(ir_desc_t *desc, type_t upvalue, hir_expr_t *expr, hir_expr_t *call, u8 cfg) {

	// TODO: hmm
	//
	//       type K = f: (): ()
	//       K.method() = ...
	//
	//       k: K; k.method
	//
	//       do we call the method, or call the function then access method?
	//       probably the former, TODO: disallow methods on function types.
	//       passing `BM_RVALUE` exhibits the former.
	//

	// i32.method(k: **i32)

	// k.method
	istr_t field = expr->d_field.field;
	hir_expr_t *field_expr = expr->d_field.expr;

	type_t type = cexpr(desc, upvalue, field_expr, BM_RVALUE);

	// i32
	type_t bare = type_strip_muls(type);
	rsym_t method;

	if ((method = table_resolve_method(bare, field)) == RSYM_NONE) {
		goto field_access;
	}

	sym_t *symbol = &symbols[method];

	// the only way to construct a sort of "gated/selector/namespaced" method is for
	// it to actually be a function. just assume that the symbol is one.
	assert(symbol->kind == SYMBOL_PROC); // assert to be sure

	type_t *fn_args = type_get(symbol->d_proc.type)->d_fn.args;
	type_t type0 = fn_args[0];

	// dref =
	//  | *T -> T
	//  | T -> T
	//
	// if dref(type) != dref(method.0) then this is a sort of
	// static method and we need to skip it outright.
	if (arrlenu(fn_args) == 0 || type_strip_muls(type0) != bare) {
		goto field_access;
	}

	// auto deref/ref `type` to match `method.0`
	// need to pay attention to mutable and immutable pointer types
	// rules:
	// 1. unbounded amount of derefs to match `method.0`
	// 2. singular ref to match `method.0`

	// no work
	if (type == type0) {
		goto skip_expr;
	}

	// try single ref
	if (type_new_inc_mul(type, false) == type0) {
		// insert ref
		*field_expr = (hir_expr_t){
			.kind = EXPR_ADDR_OF,
			.loc = field_expr->loc,
			.type = type0,
			.d_addr_of = {
				.ref = hir_dup(*field_expr),
				.is_mut = false,
			},
		};
		goto skip_expr;
	}

	if (type_new_inc_mul(type, true) == type0) {
		// insert 'ref
		*field_expr = (hir_expr_t){
			.kind = EXPR_ADDR_OF,
			.loc = field_expr->loc,
			.type = type0,
			.d_addr_of = {
				.ref = hir_dup(*field_expr),
				.is_mut = true,
			},
		};
		goto skip_expr;
	}

	// try derefs
	if (type_nr_muls(type) > type_nr_muls(type0)) {
		// insert derefs

		type_t ctype = type;

		while (ctype != type0) {
			ctype = type_get(ctype)->d_ptr.ref;
			*field_expr = (hir_expr_t){
				.kind = EXPR_DEREF,
				.loc = field_expr->loc,
				.type = ctype,
				.d_deref = hir_dup(*field_expr),
			};
		}
		goto skip_expr;
	}

	// failure
skip_expr:;
	// expr has been transformed. splice in a call that may fail to unify

	// expr = field_expr.field
	// call = expr a b c

	hir_expr_t method_sym = {
		.kind = EXPR_SYM,
		.type = symbol->d_proc.type,
		.loc = field_expr->loc,
		.d_sym = method,
	};

	// convert into:
	//   call = T:method field_expr a b c
	if (call) {
		*call->d_call.f = method_sym;
		arrins(call->d_call.args, 0, *field_expr);
	} else {
		// call = T:method field_expr

		*expr = (hir_expr_t){
			.kind = EXPR_CALL,
			.loc = expr->loc,
			.type = TYPE_INFER,
			.d_call = {
				.f = hir_dup(method_sym),
				.args = arr(hir_expr_t, *field_expr),
			}
		};

		ccall_args(desc, TYPE_INFER, expr);
	}

	return;
field_access:;

	// field accesses
	type_t type_use = type_underlying(type);

	if (type_kind(type_use) != TYPE_STRUCT) {
		goto err;
	}

	tinfo_t *struc_info = type_get(type_use);

	for (u32 i = 0, c = arrlenu(struc_info->d_struct.fields); i < c; i++) {
		tinfo_sf_t *field = &struc_info->d_struct.fields[i];
		if (field->field == expr->d_field.field) {
			expr->d_field.field_idx = i;
			expr->type = field->type;
		}
	}

	if (expr->d_field.field_idx == (u16)-1) {
		goto err;
	}

	cufcs_autocall(desc,expr, cfg);
	return;
err:
	err_with_pos(expr->loc, "not a struct field or method");
}
