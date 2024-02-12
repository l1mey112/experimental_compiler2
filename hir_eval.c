#include "all.h"
#include "hir.h"

// ensure ran after normalisation

typedef struct edesc_t edesc_t;
typedef struct eresult_t eresult_t;

struct eresult_t {
	union {
		i8 d_i8;
		i16 d_i16;
		i32 d_i32;
		i64 d_i64;
		ptrdiff_t d_isize;
		u8 d_u8;
		u16 d_u16;
		u32 d_u32;
		u64 d_u64;
		size_t d_usize;
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

};

static void _estate(edesc_t *desc) {
	u32 locals = arrlenu(desc->local_offsets);
	
	for (u32 i = 0; i < locals; i++) {
		local_t *local = &desc->desc->locals[i];

		if (local->name == ISTR_NONE) {
			printf("_%u", i);
		} else {
			printf("%s", sv_from(local->name));
		}

		printf(": %u (%s) init: %u\n", desc->local_offsets[i], type_dbg_str(local->type), desc->local_tags[i]);
	}
}

static void ehir_write_value(void *loc, eresult_t r, type_t type) {
	u32 size = type_sizeof(type);
	memcpy(loc, &r, size);
}

static void ehir_read_value(void *loc, eresult_t *r, type_t type) {
	u32 size = type_sizeof(type);
	memcpy(r, loc, size);
}

static void ehir_print_value(eresult_t r, type_t type) {
	switch (type) {
		case TYPE_I8: printf("%d", r.d_i8); break;
		case TYPE_I16: printf("%d", r.d_i16); break;
		case TYPE_I32: printf("%d", r.d_i32); break;
		case TYPE_I64: printf("%ld", r.d_i64); break;
		case TYPE_ISIZE: printf("%ld", r.d_isize); break;
		case TYPE_U8: printf("%u", r.d_u8); break;
		case TYPE_U16: printf("%u", r.d_u16); break;
		case TYPE_U32: printf("%u", r.d_u32); break;
		case TYPE_U64: printf("%lu", r.d_u64); break;
		case TYPE_USIZE: printf("%lu", r.d_usize); break;
		case TYPE_F32: printf("%f", r.d_f32); break;
		case TYPE_F64: printf("%f", r.d_f64); break;
		case TYPE_BOOL: printf("%s", r.d_bool ? "true" : "false"); break;
		default: {
			assert_not_reached();
		}
	}
}

static eresult_t ehir_expr_eval(edesc_t *desc, hir_expr_t *expr) {
	eresult_t r;

	switch (expr->kind) {
		case EXPR_INTEGER_LIT: {
			assert(type_is_integer(expr->type));

			u64 v = strtoull(sv_from(expr->d_integer_lit), NULL, 10);

			switch (expr->type) {
				case TYPE_I8: r.d_i8 = v; break;
				case TYPE_I16: r.d_i16 = v; break;
				case TYPE_I32: r.d_i32 = v; break;
				case TYPE_I64: r.d_i64 = v; break;
				case TYPE_ISIZE: r.d_isize = v; break;
				case TYPE_U8: r.d_u8 = v; break;
				case TYPE_U16: r.d_u16 = v; break;
				case TYPE_U32: r.d_u32 = v; break;
				case TYPE_U64: r.d_u64 = v; break;
				case TYPE_USIZE: r.d_usize = v; break;
				default: {
					assert_not_reached();
				}
			}

			break;
		}
		case EXPR_INFIX: {
			eresult_t lhs = ehir_expr_eval(desc, expr->d_infix.lhs);
			eresult_t rhs = ehir_expr_eval(desc, expr->d_infix.rhs);
			
			switch (expr->d_infix.kind) {
				case TOK_ADD: {
					switch (expr->type) {
						case TYPE_I8: r.d_i8 = lhs.d_i8 + rhs.d_i8; break;
						case TYPE_I16: r.d_i16 = lhs.d_i16 + rhs.d_i16; break;
						case TYPE_I32: r.d_i32 = lhs.d_i32 + rhs.d_i32; break;
						case TYPE_I64: r.d_i64 = lhs.d_i64 + rhs.d_i64; break;
						case TYPE_ISIZE: r.d_isize = lhs.d_isize + rhs.d_isize; break;
						case TYPE_U8: r.d_u8 = lhs.d_u8 + rhs.d_u8; break;
						case TYPE_U16: r.d_u16 = lhs.d_u16 + rhs.d_u16; break;
						case TYPE_U32: r.d_u32 = lhs.d_u32 + rhs.d_u32; break;
						case TYPE_U64: r.d_u64 = lhs.d_u64 + rhs.d_u64; break;
						case TYPE_USIZE: r.d_usize = lhs.d_usize + rhs.d_usize; break;
						default: {
							assert_not_reached();
						}
					}
					break;
				}
				default: {
					assert_not_reached();
				}
			}

			break;
		}
		case EXPR_LOCAL: {
			rlocal_t local = expr->d_local;

			// definitely init

			if (!desc->local_tags[local]) {
				err_with_pos(expr->loc, "use of uninitialised local");
			}

			u32 offset = desc->local_offsets[local];

			ehir_read_value(desc->stack + offset, &r, expr->type);
			break;
		}
		default: {
			printf("kind: %d\n", expr->kind);
			assert_not_reached();
		}
	}

	return r;
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

				printf("READ: %p <> %d\n", desc->stack + offset, offset);
				ehir_write_value(desc->stack + offset, res, stmt->d_assign.lhs->type);				
				break;
			}
			case EXPR_RETURN: {
				desc->result = ehir_expr_eval(desc, stmt->d_return.expr);
				return;
			}
			default: {
				ehir_expr_eval(desc, stmt);
				break;
			}
		}
	}
}

void hir_eval_global(global_t *global) {
	edesc_t desc = {
		.desc = &global->desc,
	};

	u32 locals = arrlenu(desc.desc->locals);

	u32 total_offset = 0;
	u32 total_size = 0;
	for (u32 i = 0; i < locals; i++) {
		local_t *local = &desc.desc->locals[i];

		if (local->kind == _LOCAL_ZST_DELETED) {
			arrpush(desc.local_offsets, (u32)-1);
			continue;
		}

		// TODO: host ABI, not target ABI

		u32 size = type_sizeof(local->type);
		u32 align = type_alignof(local->type);
		assert(size != TYPE_SIZE_DIVERGING);

		u32 padding = (align - total_size % align) % align;
		arrpush(desc.local_offsets, total_size + padding);
		total_size += size + padding;
	}

	desc.stack = malloc(total_size);
	// memset(desc.stack, 0xCC, total_size);

	printf("total size: %d\n", total_size);

	// one bool for each local
	desc.local_tags = malloc(locals);
	memset(desc.local_tags, 0, locals);

	// TODO: blk ids
	assert(desc.desc->hir.kind == EXPR_DO_BLOCK);

	_estate(&desc);
	if (setjmp(desc.ret) == 0) {
		ehir_stmts_eval(&desc, desc.desc->hir.d_do_block.exprs);
	}
	_estate(&desc);

	printf("result: ");
	ehir_print_value(desc.result, global->type);
	printf("\n");
}
