#include "all.h"
#include "hir.h"

// ensure ran after normalisation

typedef struct edesc_t edesc_t;
typedef struct eresult_t eresult_t;

// if aggregate type, then index to arena allocated memory

// 64 bits
// all signed types are sign extended from "creation" to use
struct eresult_t {
	union {
		u8 d_8;
		u16 d_16;
		u32 d_32;
		u64 d_64;
		// size_t d_size; (ABI)
		f32 d_f32;
		f64 d_f64;
		bool d_bool;
		// non concrete
		// void *d_large;
	};
};

struct edesc_t {
	ir_desc_t *desc;

	char *stack;
	u32 *local_offsets;
	bool *local_tags;

	jmp_buf ret;
	eresult_t result;
	bool has_result; // false if non constant expression
};

static void _estate(edesc_t *desc) {
	u32 locals = arrlenu(desc->local_offsets);
	
	for (u32 i = 0; i < locals; i++) {
		local_t *local = &desc->desc->locals[i];

		if (local->name == ISTR_NONE) {
			eprintf("_%u", i);
		} else {
			eprintf("%s", sv_from(local->name));
		}

		eprintf(": %u (%s) init: %u\n", desc->local_offsets[i], type_dbg_str(local->type), desc->local_tags[i]);
	}
}

static eresult_t ehir_infix_eval(eresult_t lhs, eresult_t rhs, type_t type, tok_t tok, loc_t onerror) {
	assert(type_is_integer(type)); // TODO: floats

	bool is_signed = type_is_signed(type);
	
	u64 a = lhs.d_64;
	u64 b = rhs.d_64;
	u64 r;

	if (is_signed) {
		a = type_abi_sign_extend(&abi, type, a);
		b = type_abi_sign_extend(&abi, type, b);

		// overflow
		if (tok == TOK_DIV || tok == TOK_MOD) {
			if ((i64)a == type_abi_signed_int_min(&abi, type) && (i64)b == -1) {
				goto ov;
			}
			if (b == 0) {
				err_with_pos(onerror, "division by zero");
			}
		}
	}

	// TODO: only op that doesn't have same type for both operands is `>>` and `<<`

	// TODO: in lang provide a `(+)` as a function that decays down to typed infix operators
	//       so you can perform something similar like in rust
	// TODO: explicit overflowing operators like in zig and so on, so this can be implemented easily.
	//       unwrap those operators into the builtins below inline

	#define BT(op, ovf) case op: if (is_signed) { if (ovf((i64)a, (i64)b, (i64*)&r)) goto ov; } \
	                             else           { if (ovf(a, b, &r)) goto ov;                 } break;

	#define PT(op, cmp) case op: if (is_signed) { r = (i64)a cmp (i64)b; } \
	                             else           { r = a cmp b;           } break;

	switch (tok) {
		BT(TOK_ADD, __builtin_add_overflow)
		BT(TOK_SUB, __builtin_sub_overflow)
		BT(TOK_MUL, __builtin_mul_overflow)
		PT(TOK_DIV, /);
		PT(TOK_MOD, %);
		default: {
			assert_not_reached();
		}
	}

	#undef BT
	#undef PT

	if (!type_abi_integer_fits_in(&abi, r, type)) {
		goto ov;
	}

	eresult_t res = { .d_64 = r };
	return res;
ov:
	err_with_pos(onerror, "integer overflow/underflow");
}


// implement duck typed best effort constants for now
// if we can't consteval an expression we branch out and return
#define BAIL() do { longjmp(desc->ret, 1); } while(0)

static eresult_t ehir_expr_eval(edesc_t *desc, hir_expr_t *expr) {
	eresult_t r;

	switch (expr->kind) {
		case EXPR_INTEGER_LIT: {
			assert(type_is_integer(expr->type));

			// negative numbers can't be represented
			// they're always positive with attached unary minus
			// no need to sign extend

			r.d_64 = expr->d_integer;
			return r;
		}
		case EXPR_BOOL_LIT: {
			r.d_bool = expr->d_bool_lit;
			return r;
		}
		case EXPR_INFIX: {
			eresult_t lhs = ehir_expr_eval(desc, expr->d_infix.lhs);
			eresult_t rhs = ehir_expr_eval(desc, expr->d_infix.rhs);
			return ehir_infix_eval(lhs, rhs, expr->type, expr->d_infix.kind, expr->loc);
		}
		case EXPR_PREFIX: {
			r = ehir_expr_eval(desc, expr->d_prefix.expr);
			
			switch (expr->d_prefix.op) {
				case EXPR_K_NOT: {
					r.d_bool = !r.d_bool;
					break;
				}
				case EXPR_K_SUB: {
					if (type_is_integer(expr->type)) {
						r.d_64 = -r.d_64;
					} else {
						BAIL();
					}
					break;
				}
			}

			return r;
		}
		case EXPR_LOCAL: {
			rlocal_t local = expr->d_local;

			if (!desc->local_tags[local]) {
				err_with_pos(expr->loc, "use of uninitialised local");
			}

			// definitely init
			u32 offset = desc->local_offsets[local];

			// read
			memcpy(&r, desc->stack + offset, type_sizeof(&abi, expr->type));
			return r;
		}
		case EXPR_SYM: {
			sym_t *sym = &symbols[expr->d_sym];

			if (sym->kind == SYMBOL_PROC) {
				BAIL();
			}

			assert(sym->kind == SYMBOL_GLOBAL);
			if (sym->d_global.constant == NULL) {
				BAIL();
			}

			return ehir_expr_eval(desc, sym->d_global.constant);
		}
		default: {
			BAIL();
		}
	}

	assert_not_reached();
}

static void ehir_stmts_eval(edesc_t *desc, hir_expr_t *exprs) {
	for (u32 i = 0, c = arrlenu(exprs); i < c; i++) {
		hir_expr_t *stmt = &exprs[i];
		
		switch (stmt->kind) {
			case EXPR_BREAK: {
				return;
			}
			case EXPR_ASSIGN: {
				eresult_t res = ehir_expr_eval(desc, stmt->d_assign.rhs);

				assert(stmt->d_assign.lhs->kind == EXPR_LOCAL);
				rlocal_t local = stmt->d_assign.lhs->d_local;

				// definitely init
				desc->local_tags[local] = true;
				u32 offset = desc->local_offsets[local];

				// write
				memcpy(desc->stack + offset, &res, type_sizeof(&abi, stmt->d_assign.lhs->type));			
				break;
			}
			case EXPR_RETURN: {
				desc->result = ehir_expr_eval(desc, stmt->d_return.expr);
				desc->has_result = true;
				return;
			}
			default: {
				ehir_expr_eval(desc, stmt);
				break;
			}
		}
	}
}

static hir_expr_t ehir_construct_expr(eresult_t res, type_t type, loc_t loc) {
	if (type == TYPE_BOOL) {
		return (hir_expr_t){
			.kind = EXPR_BOOL_LIT,
			.type = type,
			.loc = loc,
			.d_bool_lit = res.d_bool,
		};
	}
	
	if (type_is_integer(type)) {
		bool neg = false;
		
		if (type_is_signed(type) && (i64)(res.d_64) < 0) {
			res.d_64 = -res.d_64;
			neg = true;
		}
		
		hir_expr_t expr = {
			.kind = EXPR_INTEGER_LIT,
			.type = type,
			.loc = loc,
			.d_integer = res.d_64,
		};

		if (neg) {
			expr = (hir_expr_t){
				.kind = EXPR_PREFIX,
				.type = type,
				.loc = loc,
				.d_prefix = {
					.op = EXPR_K_SUB,
					.expr = hir_dup(expr),
				}
			};
		}
		return expr;
	}

	// TODO: we should be able to construct everything
	assert_not_reached();
}

void hir_eval_global(sym_t *sym, global_t *global) {
	edesc_t desc = {
		.desc = &global->desc,
		.has_result = false,
	};

	u32 locals = arrlenu(desc.desc->locals);

	u32 total_size = 0;
	for (u32 i = 0; i < locals; i++) {
		local_t *local = &desc.desc->locals[i];

		if (local->kind == _LOCAL_ZST_DELETED) {
			arrpush(desc.local_offsets, (u32)-1);
			continue;
		}

		u32 size = 0;
		if (!type_is_diverging(local->type)) {
			size = type_sizeof(&abi, local->type);
		}
		u32 align = type_alignof(&abi, local->type);

		u32 padding = (align - total_size % align) % align;
		arrpush(desc.local_offsets, total_size + padding);
		total_size += size + padding;
	}

	desc.stack = malloc(total_size);
	// memset(desc.stack, 0xCC, total_size);

	// one bool for each local
	desc.local_tags = malloc(locals);
	memset(desc.local_tags, 0, locals);

	// TODO: blk ids
	assert(desc.desc->hir->kind == EXPR_DO_BLOCK);

	_estate(&desc);
	if (setjmp(desc.ret) == 0) {
		ehir_stmts_eval(&desc, desc.desc->hir->d_do_block.exprs);
	}
	_estate(&desc);

	if (desc.has_result) {
		hir_expr_t expr = ehir_construct_expr(desc.result, global->type, sym->loc);
		global->constant = hir_dup(expr);
	}
}
