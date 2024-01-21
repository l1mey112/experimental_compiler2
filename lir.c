#include "all.h"

lir_symbol_t *symbols;

#include "stb_ds.h"

lir_rlocal_t lir_local_new(lir_proc_t *proc, lir_local_t local) {
	u32 idx = arrlenu(proc->locals);
	arrpush(proc->locals, local);
	return idx;
}

lir_rlocal_t lir_local_new_named(lir_proc_t *proc, istr_t name, loc_t loc, type_t type, bool is_mut) {
	lir_local_t local = {
		.kind = is_mut ? LOCAL_MUT : LOCAL_IMM,
		.type = type,
		.is_debuginfo = true,
		.d_debuginfo = {
			.name = name,
			.loc = loc,
		}
	};
	u32 idx = arrlenu(proc->locals);
	arrpush(proc->locals, local);
	return idx;
}

lir_rblock_t lir_block_new(lir_proc_t *proc, const char *debug_name) {
	lir_block_t block = {};
	
	u32 idx = arrlenu(proc->blocks);
	block.index = idx;
	block.debug_name = debug_name;
	arrpush(proc->blocks, block);
	return idx;
}

lir_rlocal_t lir_block_new_arg(lir_proc_t *proc, lir_rblock_t block, type_t type, loc_t *loc) {
	lir_local_t desc = (lir_local_t){
		.kind = LOCAL_SSA,
		.type = type,
	};

	if (loc) {
		desc.is_debuginfo = true;
		desc.d_debuginfo.loc = *loc;
	}
	
	lir_rlocal_t local = lir_local_new(proc, desc);
	lir_block_arg_assign(proc, block, local);
	return local;
}

void lir_block_arg_assign(lir_proc_t *proc, lir_rblock_t block, lir_rlocal_t local) {
	lir_block_t *b = &proc->blocks[block];
	lir_local_t *localp = &proc->locals[local];
	assert(localp->kind == LOCAL_SSA);
	arrpush(b->args, local);
}

void lir_block_term(lir_proc_t *proc, lir_rblock_t block, lir_term_t term) {
	lir_block_t *b = &proc->blocks[block];
	b->term = term;
}

void lir_inst(lir_proc_t *proc, lir_rblock_t block, lir_inst_t inst) {
	lir_block_t *b = &proc->blocks[block];
	arrpush(b->insts, inst);
}

// create local, local = inst
lir_rlocal_t lir_ssa_tmp_inst(lir_proc_t *proc, lir_rblock_t block, type_t type, loc_t *loc, lir_inst_t inst) {
	lir_local_t desc = (lir_local_t){
		.kind = LOCAL_SSA,
		.type = type,
	};

	if (loc) {
		desc.is_debuginfo = true;
		desc.d_debuginfo.loc = *loc;
	}
	
	lir_rlocal_t local = lir_local_new(proc, desc);
	inst.dest = lir_lvalue(local);
	lir_inst(proc, block, inst);
	return local;
}

lir_lvalue_t lir_lvalue(lir_rlocal_t local) {
	return (lir_lvalue_t){
		.local = local,
		.proj = NULL,
	};
}

lir_lvalue_t lir_lvalue_sym(lir_rsymbol_t sym) {
	return (lir_lvalue_t){
		.is_sym = true,
		.symbol = sym,
		.proj = NULL,
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

lir_rlocal_t lir_lvalue_spill(lir_proc_t *proc, lir_rblock_t block, lir_lvalue_t lvalue) {
	if (!lvalue.is_sym && arrlenu(lvalue.proj) == 0) {
		return lvalue.local;
	}

	return lir_ssa_tmp_inst(proc, block, lvalue.local, &lvalue.proj[0].loc, (lir_inst_t){
		.kind = INST_LVALUE,
		.d_lvalue = lvalue,
	});
}

static void _print_local(lir_proc_t *proc, lir_rlocal_t local) {
	lir_local_t *localp = &proc->locals[local];

	switch (localp->kind) {
		case LOCAL_MUT:
		case LOCAL_IMM: {
			if (localp->is_debuginfo) {
				printf("%s", sv_from(localp->d_debuginfo.name));
			} else {
				printf("_");
			}
			printf(".%u", local);
			break;
		}
		case LOCAL_SSA: {
			printf("%%");
			printf("%u", local);
			break;
		}
	}

}

// : type
static void _print_local_type(lir_proc_t *proc, lir_rlocal_t local) {
	lir_local_t *localp = &proc->locals[local];
	printf(": %s", type_dbg_str(localp->type));
}

static void _print_term_pattern(lir_proc_t *proc, lir_term_pat_t *pattern) {
	switch (pattern->kind) {
		case PATTERN_TUPLE: {
			printf("(");
			for (u32 i = 0, c = arrlenu(pattern->d_tuple.elems); i < c; i++) {
				lir_term_pat_t *elem = &pattern->d_tuple.elems[i];
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
				lir_term_pat_t *elem = &pattern->d_array.elems[i];
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

static void _print_blockterm(lir_proc_t *proc, lir_term_block_t block) {
	_print_blockref(proc, block.block);
	// block(v0, v1)
	u32 c = arrlenu(block.args);
	if (c > 0) {
		printf("(");
		for (u32 i = 0; i < c; i++) {
			lir_rlocal_t arg = block.args[i];
			_print_local(proc, arg);
			if (i + 1 < c) {
				printf(", ");
			}
		}
		printf(")");
	}
}

static void _print_lvalue(lir_proc_t *proc, lir_lvalue_t lvalue) {
	_print_local(proc, lvalue.local);
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

static void _print_inst(lir_proc_t *proc, lir_inst_t *inst) {
	// assert(inst->kind < _INST_MAX);	

	printf("  ");

	_print_lvalue(proc, inst->dest);

	printf(" = ");

	switch (inst->kind) {
		case INST_INTEGER_LIT: {
			printf("%s", sv_from(inst->d_integer_lit));
			break;
		}
		case INST_BOOL_LIT: {
			printf("%s", inst->d_bool_lit ? "true" : "false");
			break;
		}
		case INST_ARRAY: {
			printf("[");
			for (u32 i = 0, c = arrlenu(inst->d_array); i < c; i++) {
				lir_rlocal_t elem = inst->d_array[i];
				_print_local(proc, elem);
				if (i + 1 < c) {
					printf(", ");
				}
			}
			printf("]");
		}
		case INST_UNDEFINED: {
			printf("undefined");
			break;
		}
		case INST_TUPLE_UNIT: {
			printf("()");
			break;
		}
		case INST_TUPLE: {
			printf("(");
			for (u32 i = 0, c = arrlenu(inst->d_tuple); i < c; i++) {
				lir_rlocal_t elem = inst->d_tuple[i];
				_print_local(proc, elem);
				if (i + 1 < c) {
					printf(", ");
				}
			}
			printf(")");
			break;
		}
		case INST_ADD:
		case INST_SUB:
		case INST_MUL:
		case INST_DIV:
		case INST_MOD:
		case INST_EQ:
		case INST_NE:
		case ISNT_LE:
		case ISNT_LT:
		case ISNT_GE:
		case ISNT_GT:
		case INST_AND:
		case INST_OR: {
			const char *lit_tbl[] = {
				[INST_ADD] = "+",
				[INST_SUB] = "-",
				[INST_MUL] = "*",
				[INST_DIV] = "/",
				[INST_MOD] = "%",
				[INST_EQ] = "==",
				[INST_NE] = "!=",
				[ISNT_LE] = "<=",
				[ISNT_LT] = "<",
				[ISNT_GE] = ">=",
				[ISNT_GT] = ">",
				[INST_AND] = "&",
				[INST_OR] = "|",
			};

			_print_local(proc, inst->d_infix.lhs);
			printf(" %s ", lit_tbl[inst->kind]);
			_print_local(proc, inst->d_infix.rhs);
			break;
		}
		case INST_NEG:
		case INST_NOT: {
			const char *lit_tbl[] = {
				[INST_NEG] = "-",
				[INST_NOT] = "!",
			};

			printf("%s", lit_tbl[inst->kind]);
			_print_local(proc, inst->d_unary.src);
			break;
		}
		case INST_CALL: {
			// value(a, b, c)
			_print_local(proc, inst->d_call.f);
			printf("(");
			for (u32 i = 0, c = arrlenu(inst->d_call.args); i < c; i++) {
				lir_rlocal_t arg = inst->d_call.args[i];
				_print_local(proc, arg);
				if (i + 1 < c) {
					printf(", ");
				}
			}
			printf(")");
			break;
		}
		case INST_CAST: {
			// value as type
			_print_local(proc, inst->d_cast.src);
			printf(" as %s", type_dbg_str(inst->d_cast.type));
			break;
		}
		case INST_LVALUE: {
			_print_lvalue(proc, inst->d_lvalue);
			break;
		}
		case INST_ADDRESS_OF: {
			printf("&");
			if (inst->d_address_of.is_mut) {
				printf("'");
			}
			_print_lvalue(proc, inst->d_address_of.lvalue);
			break;
		}
		// case INST_SIZEOF:
		default: {
			printf("\n\nkind: %u\n\n", inst->kind);
			assert_not_reached();
		}
	}

	printf("\n");
}

void lir_print_proc(lir_symbol_t *symbol) {
	assert(symbol->kind == SYMBOL_PROC);
	lir_proc_t *proc = &symbol->d_proc;
	printf("function %s\n", sv_from(symbol->qualified_name));

	// print locals
	for (u32 i = 0, c = arrlenu(proc->locals); i < c; i++) {
		lir_local_t *localp = &proc->locals[i];

		if (localp->kind == LOCAL_SSA) {
			continue;
		}

		printf("  ");

		if (localp->kind == LOCAL_MUT) {
			printf("'");
		}
		_print_local(proc, i);
		_print_local_type(proc, i);
		printf("\n");
	}

	// print blocks
	for (u32 i = 0, c = arrlenu(proc->blocks); i < c; i++) {
		lir_block_t *block = &proc->blocks[i];
		_print_blockref(proc, i);

		// entry(v0: i32, v1: i32)
		//
		u32 c = arrlenu(block->args);
		if (c > 0) {
			printf("(");
			for (u32 i = 0; i < c; i++) {
				lir_rlocal_t arg = block->args[i];
				_print_local(proc, arg);
				_print_local_type(proc, arg);
				if (i + 1 < c) {
					printf(", ");
				}
			}
			printf(")");
		}

		printf(":\n");

		// print insts
		for (u32 i = 0, c = arrlenu(block->insts); i < c; i++) {
			lir_inst_t *inst = &block->insts[i];
			_print_inst(proc, inst);
		}

		// print terminator
		switch (block->term.kind) {
			case TERM_GOTO: {
				printf("  goto ");
				_print_blockterm(proc, block->term.d_goto);
				printf("\n");
				break;
			}
			case TERM_RET: {
				printf("  ret ");
				_print_local(proc, block->term.d_ret.value);
				printf("\n");
				break;
			}
			case TERM_IF: {
				printf("  if ");
				_print_local(proc, block->term.d_if.cond);
				printf(" goto ");
				_print_blockterm(proc, block->term.d_if.then);
				printf(" else goto ");
				_print_blockterm(proc, block->term.d_if.els);
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
				_print_local(proc, block->term.d_goto_pattern.value);
				printf("\n");

				for (u32 i = 0, c = arrlenu(block->term.d_goto_pattern.patterns); i < c; i++) {
					lir_term_pat_t *patternp = &block->term.d_goto_pattern.patterns[i];
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
	}
}

void lir_print_symbol(lir_symbol_t *symbol) {
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
	for (u32 i = 0, c = arrlenu(symbols); i < c; i++) {
		lir_print_symbol(&symbols[i]);
		if (i + 1 < c) {
			printf("\n");
		}
	}
}