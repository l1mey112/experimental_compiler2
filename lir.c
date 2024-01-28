#include "all.h"

lir_rlocal_t lir_local_new(lir_proc_t *proc, lir_local_t local) {
	u32 idx = arrlenu(proc->locals);
	arrpush(proc->locals, local);
	return idx;
}

lir_rblock_t lir_block_new(lir_proc_t *proc, const char *debug_name) {
	lir_block_t block = {
		.debug_name = debug_name,
	};
	u32 idx = arrlenu(proc->blocks);
	arrpush(proc->blocks, block);
	return idx;
}

void lir_block_term(lir_proc_t *proc, lir_rblock_t block, lir_term_t term) {
	lir_block_t *b = &proc->blocks[block];
	b->term = term;
}

lir_value_t *lir_dup(lir_value_t value) {
	lir_value_t *r = malloc(sizeof(lir_value_t));
	*r = value;
	return r;
}

u32 lir_stmt(lir_proc_t *proc, lir_rblock_t block, lir_stmt_t stmt) {
	lir_block_t *b = &proc->blocks[block];
	u32 idx = arrlenu(b->stmts);
	arrpush(b->stmts, stmt);
	return idx;
}

void lir_stmt_at(lir_proc_t *proc, lir_rblock_t block, u32 idx, lir_stmt_t stmt) {
	lir_block_t *b = &proc->blocks[block];
	arrins(b->stmts, idx, stmt);
}

void lir_assign_local(lir_proc_t *proc, lir_rblock_t block, lir_rlocal_t local, lir_value_t value) {
	lir_assign(proc, block, lir_local_lvalue(local, value.loc), value);
}

void lir_assign(lir_proc_t *proc, lir_rblock_t block, lir_lvalue_t lvalue, lir_value_t value) {
	lir_stmt(proc, block, (lir_stmt_t){
		.kind = STMT_LVALUE,
		.dest = lvalue,
		.value = value,
	});
}

void lir_discard(lir_proc_t *proc, lir_rblock_t block, lir_value_t value) {
	lir_stmt(proc, block, (lir_stmt_t){
		.kind = STMT_DISCARD,
		.value = value,
	});
}

void lir_ignore(lir_proc_t *proc, lir_rblock_t block, lir_value_t value) {
	lir_stmt(proc, block, (lir_stmt_t){
		.kind = STMT_IGNORE,
		.value = value,
	});
}

lir_value_t lir_value_lvalue(lir_lvalue_t lvalue) {
	return (lir_value_t){
		.kind = VALUE_LVALUE,
		.type = TYPE_INFER,
		.loc = lvalue.loc,
		.d_lvalue = lvalue,
	};
}

lir_lvalue_t lir_local_lvalue(lir_rlocal_t local, loc_t loc) {
	return (lir_lvalue_t){
		.local = local,
		.proj = NULL,
		.loc = loc,
	};
}

lir_value_t lir_local_value(lir_rlocal_t local, loc_t loc) {
	return (lir_value_t){
		.kind = VALUE_LVALUE,
		.type = TYPE_INFER,
		.loc = loc,
		.d_lvalue = lir_local_lvalue(local, loc),
	};
}

/* u32 lir_inst(lir_proc_t *proc, lir_rblock_t block, lir_inst_t inst) {
	lir_block_t *b = &proc->blocks[block];
	u32 idx = arrlenu(b->insts);
	arrpush(b->insts, inst);
	return idx;
}

void lir_inst_insert(lir_proc_t *proc, lir_rblock_t block, u32 inst_idx, lir_inst_t inst) {
	lir_block_t *b = &proc->blocks[block];
	arrins(b->insts, inst_idx, inst);
} */

// spill projections and mut values into an immutable value
/* lir_rlocal_t lir_lvalue_spill(lir_proc_t *proc, lir_rblock_t block, lir_lvalue_t lvalue) {
	if (!lvalue.is_sym && arrlenu(lvalue.proj) == 0 && proc->locals[lvalue.local].kind != LOCAL_MUT) {
		return lvalue.local;
	}

	return lir_ssa_tmp_inst(proc, block, TYPE_INFER, lvalue.loc, (lir_inst_t){
		.kind = INST_LVALUE,
		.d_lvalue = lvalue,
	});
} */

// ensure no assignments to locals are made through non-SSA locals
// requirement of tree LIR
// spill anything but an SSA local
//
// TODO: can be removed really, it doesn't matter anymore
//       try relaxing it and see what it does
/* void lir_inst_lvalue(lir_proc_t *proc, lir_rblock_t block, lir_lvalue_t dest, lir_rlocal_t src, loc_t loc) {
	if (proc->locals[src].kind != LOCAL_SSA) {
		src = lir_ssa_tmp_inst(proc, block, TYPE_INFER, loc, (lir_inst_t){
			.kind = INST_LVALUE,
			.d_lvalue = lir_lvalue(src, loc),
		});
	}
	
	lir_inst(proc, block, (lir_inst_t){
		.dest.lvalue = dest,
		.kind = INST_LVALUE,
		.lvalue_dest = true,
		.d_lvalue = lir_lvalue(src, loc),
	});
} */

lir_lvalue_t lir_lvalue(lir_rlocal_t local, loc_t loc) {
	return (lir_lvalue_t){
		.local = local,
		.proj = NULL,
		.loc = loc,
	};
}

lir_lvalue_t lir_lvalue_sym(lir_rsym_t sym, loc_t loc) {
	return (lir_lvalue_t){
		.is_sym = true,
		.symbol = sym,
		.proj = NULL,
		.loc = loc,
	};
}

void lir_lvalue_deref(lir_lvalue_t *lvalue, loc_t loc) {
	lir_lvalue_proj_t desc = {
		.kind = PROJ_DEREF,
		.loc = loc,
	};

	arrpush(lvalue->proj, desc);
}

void lir_lvalue_struct_field(lir_lvalue_t *lvalue, loc_t loc, istr_t field) {
	lir_lvalue_proj_t desc = {
		.kind = PROJ_FIELD,
		.loc = loc,
		.d_field = {
			.field = field,
			.field_idx = PROJ_IDX_INVALID,
		},
	};

	arrpush(lvalue->proj, desc);
}

void lir_lvalue_index_field(lir_lvalue_t *lvalue, loc_t loc, u16 field_idx) {
	lir_lvalue_proj_t desc = {
		.kind = PROJ_FIELD,
		.loc = loc,
		.d_field = {
			.field = ISTR_NONE,
			.field_idx = field_idx,
		},
	};

	arrpush(lvalue->proj, desc);
}

void lir_lvalue_index(lir_lvalue_t *lvalue, loc_t loc, lir_rlocal_t index) {
	lir_lvalue_proj_t desc = {
		.kind = PROJ_INDEX,
		.loc = loc,
		.d_index = {
			.index = index,
		},
	};

	arrpush(lvalue->proj, desc);
}

/* u32 lir_find_inst_ssa_block(lir_proc_t *proc, lir_rblock_t block, lir_rlocal_t local) {
	lir_block_t *b = &proc->blocks[block];
	for (u32 i = 0, c = arrlenu(b->insts); i < c; i++) {
		lir_inst_t *inst = &b->insts[i];
		if (!inst->lvalue_dest && inst->dest.ssa == local) {
			return i;
		}
	}
	return INST_NONE;
}

lir_inst_t lir_inst_pop(lir_proc_t *proc, lir_rblock_t block, u32 inst) {
	lir_block_t *b = &proc->blocks[block];
	assert(inst < arrlenu(b->insts));
	lir_inst_t r = b->insts[inst];
	arrdel(b->insts, inst);
	return r;
} */

static void _print_local(lir_proc_t *proc, lir_rlocal_t local) {
	lir_local_t *localp = &proc->locals[local];

	if (localp->name != ISTR_NONE) {
		printf("%s.", sv_from(localp->name));
	} else {
		printf("_");
	}
	printf("%u", local);
}

// : type
static void _print_local_type(lir_proc_t *proc, lir_rlocal_t local) {
	lir_local_t *localp = &proc->locals[local];
	printf(": %s", type_dbg_str(localp->type));
}

static void _print_term_pattern(lir_proc_t *proc, lir_pattern_t *pattern) {
	switch (pattern->kind) {
		case PATTERN_TUPLE: {
			printf("(");
			for (u32 i = 0, c = arrlenu(pattern->d_tuple.elems); i < c; i++) {
				lir_pattern_t *elem = &pattern->d_tuple.elems[i];
				_print_term_pattern(proc, elem);
				if (i + 1 < c) {
					printf(", ");
				}
			}
			printf(")");
			break;
		}
		case PATTERN_ARRAY: {
			printf("[");
			if (pattern->d_array.match && pattern->d_array.match_lhs) {
				_print_term_pattern(proc, pattern->d_array.match);
				printf("..., ");
			}
			for (u32 i = 0, c = arrlenu(pattern->d_array.elems); i < c; i++) {
				lir_pattern_t *elem = &pattern->d_array.elems[i];
				_print_term_pattern(proc, elem);
				if (i + 1 < c) {
					printf(", ");
				}
			}
			if (pattern->d_array.match && !pattern->d_array.match_lhs) {
				printf(", ...");
				_print_term_pattern(proc, pattern->d_array.match);
			}
			printf("]");
			break;
		}
		case PATTERN_LOCAL: {
			_print_local(proc, pattern->d_local);
			break;
		}
		case PATTERN_TUPLE_UNIT: {
			printf("()");
			break;
		}
		case PATTERN_INTEGER_LIT: {
			printf("%s", sv_from(pattern->d_integer_lit));
			break;
		}
		case PATTERN_BOOL_LIT: {
			printf("%s", pattern->d_bool_lit ? "true" : "false");
			break;
		}
		case PATTERN_UNDERSCORE: {
			printf("_");
			break;
		}
		default: {
			assert_not_reached();
		}
	}
}

static void _print_blockref(lir_proc_t *proc, lir_rblock_t block) {
	lir_block_t *blockp = &proc->blocks[block];
	if (blockp->debug_name) {
		printf("%s.", blockp->debug_name);
	} else {
		printf(".L");
	}

	printf("%u", block);
}

static void _print_lvalue(lir_proc_t *proc, lir_lvalue_t lvalue) {
	if (lvalue.is_sym) {
		lir_sym_t *symbol = &symbols[lvalue.symbol];
		printf("{%s}", sv_from(symbol->key));
	} else {
		_print_local(proc, lvalue.local);
	}
	for (u32 i = 0, c = arrlenu(lvalue.proj); i < c; i++) {
		lir_lvalue_proj_t *proj = &lvalue.proj[i];
		switch (proj->kind) {
			case PROJ_DEREF: {
				printf(".*");
				break;
			}
			case PROJ_FIELD: {
				if (proj->d_field.field != ISTR_NONE) {
					printf(".%s", sv_from(proj->d_field.field));
				} else {
					printf(".%u", proj->d_field.field_idx);
				}
				break;
			}
			case PROJ_INDEX: {
				printf("[");
				_print_local(proc, proj->d_index.index);
				printf("]");
				break;
			}
			default: {
				assert_not_reached();
			}
		}
	}
}

static void _print_value(lir_proc_t *proc, lir_value_t *value) {
	switch (value->kind) {
		case VALUE_INTEGER_LIT: {
			printf("%s", sv_from(value->d_integer_lit));
			break;
		}
		case VALUE_BOOL_LIT: {
			printf("%s", value->d_bool_lit ? "true" : "false");
			break;
		}
		case VALUE_ARRAY: {
			printf("[");
			for (u32 i = 0, c = arrlenu(value->d_array); i < c; i++) {
				_print_value(proc, &value->d_array[i]);
				if (i + 1 < c) {
					printf(", ");
				}
			}
			printf("]");
			break;
		}
		/* case VALUE_UNDEFINED: {
			printf("undefined");
			break;
		} */
		case VALUE_TUPLE_UNIT: {
			printf("()");
			break;
		}
		case VALUE_TUPLE: {
			printf("(");
			for (u32 i = 0, c = arrlenu(value->d_tuple); i < c; i++) {
				_print_value(proc, &value->d_tuple[i]);
				if (i + 1 < c) {
					printf(", ");
				}
			}
			printf(")");
			break;
		}
		case VALUE_ADD:
		case VALUE_SUB:
		case VALUE_MUL:
		case VALUE_DIV:
		case VALUE_MOD:
		case VALUE_EQ:
		case VALUE_NE:
		case VALUE_LE:
		case VALUE_LT:
		case VALUE_GE:
		case VALUE_GT:
		case VALUE_AND:
		case VALUE_OR: {
			const char *lit_tbl[] = {
				[VALUE_ADD] = "+",
				[VALUE_SUB] = "-",
				[VALUE_MUL] = "*",
				[VALUE_DIV] = "/",
				[VALUE_MOD] = "%",
				[VALUE_EQ] = "==",
				[VALUE_NE] = "!=",
				[VALUE_LE] = "<=",
				[VALUE_LT] = "<",
				[VALUE_GE] = ">=",
				[VALUE_GT] = ">",
				[VALUE_AND] = "&",
				[VALUE_OR] = "|",
			};

			_print_value(proc, value->d_infix.lhs);
			printf(" %s ", lit_tbl[value->kind]);
			_print_value(proc, value->d_infix.rhs);
			break;
		}
		case VALUE_NEG:
		case VALUE_NOT: {
			const char *lit_tbl[] = {
				[VALUE_NEG] = "-",
				[VALUE_NOT] = "!",
			};

			printf("%s", lit_tbl[value->kind]);
			_print_value(proc, value->d_unary.src);
			break;
		}
		case VALUE_CALL: {
			// value(a, b, c)
			_print_value(proc, value->d_call.f);
			printf("(");
			for (u32 i = 0, c = arrlenu(value->d_call.args); i < c; i++) {
				_print_value(proc, &value->d_call.args[i]);
				if (i + 1 < c) {
					printf(", ");
				}
			}
			printf(")");
			break;
		}
		case VALUE_CAST: {
			// value as type
			_print_value(proc, value->d_cast.src);
			printf(": %s", type_dbg_str(value->type));
			break;
		}
		case VALUE_LVALUE: {
			_print_lvalue(proc, value->d_lvalue);
			break;
		}
		case VALUE_ADDRESS_OF: {
			printf("&");
			if (value->d_address_of.is_mut) {
				printf("'");
			}
			_print_lvalue(proc, value->d_address_of.lvalue);
			break;
		}
		case VALUE_SLICE: {
			// x[?lo..?hi]
			_print_value(proc, value->d_slice.src);
			printf("[");
			if (value->d_slice.lo) {
				_print_value(proc, value->d_slice.lo);
			}
			printf("..");
			if (value->d_slice.hi) {
				_print_value(proc, value->d_slice.hi);
			}
			printf("]");
			break;
		}
		// case VALUE_SIZEOF:
		case VALUE_CTRL_TEMP: {
			printf("ctrl {entry: ");
			_print_blockref(proc, value->d_ctrl_temp.entry);
			printf(", local: ");
			_print_local(proc, value->d_ctrl_temp.local);
			printf("}");
			break;
		}
		case VALUE_CTRL_NORETURN: {
			printf("ctrl !");
			break;
		}
		default: {
			printf("\n\nkind: %u\n\n", value->kind);
			assert_not_reached();
		}
	}
}

static void _print_stmt(lir_proc_t *proc, lir_stmt_t *stmt) {
	printf("  ");
	switch (stmt->kind) {
		case STMT_LVALUE: {
			_print_lvalue(proc, stmt->dest);
			printf(" = ");
			break;
		}
		case STMT_DISCARD: {
			printf("_ = ");
			
		}
		case STMT_IGNORE: break;
		default: {
			assert_not_reached();
		}
	}

	_print_value(proc, &stmt->value);

	printf("\n");
}

void lir_print_proc(lir_sym_t *symbol) {
	assert(symbol->kind == SYMBOL_PROC);
	lir_proc_t *proc = &symbol->d_proc;
	printf("function %s(", sv_from(symbol->key));

	// print args
	for (u32 i = 0, c = proc->arguments; i < c; i++) {
		lir_local_t *localp = &proc->locals[i];

		if (localp->kind == LOCAL_MUT) {
			printf("'");
		}
		_print_local(proc, i);
		_print_local_type(proc, i);

		if (i + 1 < c) {
			printf(", ");
		}
	}

	printf(") -> %s lir {\n", type_dbg_str(type_get(symbol->type)->d_fn.ret));

	// print locals
	for (u32 i = proc->arguments, c = arrlenu(proc->locals); i < c; i++) {
		lir_local_t *localp = &proc->locals[i];

		printf("  ");

		if (localp->kind == LOCAL_MUT) {
			printf("'");
		}
		_print_local(proc, i);
		_print_local_type(proc, i);
		printf("\n");
	}

	printf("\n");

	// print blocks
	for (u32 i = 0, c = arrlenu(proc->blocks); i < c; i++) {
		lir_block_t *block = &proc->blocks[i];
		_print_blockref(proc, i);
		printf(":\n");

		// print stmts
		for (u32 i = 0, c = arrlenu(block->stmts); i < c; i++) {
			lir_stmt_t *stmt = &block->stmts[i];
			_print_stmt(proc, stmt);
		}

		// print terminator
		switch (block->term.kind) {
			case TERM_GOTO: {
				printf("  goto ");
				_print_blockref(proc, block->term.d_goto);
				printf("\n");
				break;
			}
			case TERM_RET: {
				printf("  ret ");
				_print_value(proc, &block->term.d_ret.value);
				printf("\n");
				break;
			}
			case TERM_UNINIT: {
				printf("  <none>\n");
				break;
			}
			case TERM_GOTO_PATTERN: {
				// goto_pattern v0
				//     (a, b) -> block_two
				//     (0, b) -> block_two

				printf("  goto_pattern ");
				_print_value(proc, &block->term.d_goto_pattern.value);
				printf("\n");

				for (u32 i = 0, c = arrlenu(block->term.d_goto_pattern.patterns); i < c; i++) {
					lir_pattern_t *patternp = &block->term.d_goto_pattern.patterns[i];
					printf("    ");
					_print_term_pattern(proc, patternp);
					printf(" -> ");
					_print_blockref(proc, block->term.d_goto_pattern.blocks[i]);
					printf("\n");
				}
				break;
			}
			default: {
				assert_not_reached();
			}
		}
		if (i + 1 < c) {
			printf("\n");
		}
	}
	printf("}\n");
}

void lir_print_symbol(lir_sym_t *symbol) {
	switch (symbol->kind) {
		case SYMBOL_PROC: {
			lir_print_proc(symbol);
			break;
		}
		default: {
			assert_not_reached();
		}
	}
}

void lir_print_symbols(void) {
	for (u32 i = 0, c = hmlenu(symbols); i < c; i++) {
		if (symbols[i].is_placeholder) {
			continue;
		}

		lir_print_symbol(&symbols[i]);
		if (i + 1 < c) {
			printf("\n");
		}
	}
}