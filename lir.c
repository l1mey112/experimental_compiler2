#include "all.h"

lir_symbol_t *symbols;

#include "stb_ds.h"

lir_rvalue_t lir_value_new(lir_proc_t *proc, lir_value_t value) {
	u32 idx = arrlenu(proc->values);
	value.index = idx;
	arrpush(proc->values, value);
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

lir_rvalue_t lir_block_arg(lir_proc_t *proc, lir_rblock_t block, lir_value_t value) {
	lir_block_t *b = &proc->blocks[block];
	lir_rvalue_t rvalue = lir_value_new(proc, value);
	arrpush(b->args, rvalue);
	return rvalue;
}

void lir_block_term(lir_proc_t *proc, lir_rblock_t block, lir_term_t term) {
	lir_block_t *b = &proc->blocks[block];
	b->term = term;
}

lir_rinst_t lir_inst_new(lir_proc_t *proc, lir_rblock_t block, lir_inst_t inst) {
	lir_block_t *b = &proc->blocks[block];
	u32 idx = arrlenu(b->insts);
	arrpush(b->insts, inst);
	return idx;
}

// inst builder
// v2 = v0 + v1
lir_rvalue_t lir_inst_value(lir_proc_t *proc, lir_rblock_t block, type_t type, loc_t loc, lir_inst_t inst) {
	lir_rvalue_t rvalue = lir_value_new(proc, (lir_value_t){.type = type, .loc = loc});
	inst.target = rvalue;
	(void)lir_inst_new(proc, block, inst);
	return rvalue;
}

lir_rvalue_t lir_mut(lir_proc_t *proc, lir_rblock_t block, lir_rvalue_t value) {
	lir_value_t *valuep = &proc->values[value];
	return lir_inst_value(proc, block, valuep->type, valuep->loc, (lir_inst_t){
		.kind = INST_MUT,
		.d_mut = value,
	});
}

lir_rvalue_t lir_local_addr(lir_proc_t *proc, lir_rblock_t block, lir_rlocal_t local) {
	// to avoid duplications of &local, search for refs in the current block
	lir_block_t *blockp = &proc->blocks[block];
	for (u32 i = 0, c = arrlenu(blockp->insts); i < c; i++) {
		lir_inst_t *inst = &blockp->insts[i];
		if (inst->kind == INST_LOCAL && inst->d_local.local == local) {
			return inst->target;
		}
	}
	
	lir_block_t *b = &proc->blocks[block];
	lir_local_t *localp = &proc->locals[local];
	lir_rvalue_t val = lir_inst_value(proc, block, localp->type, localp->loc, (lir_inst_t){
		.kind = INST_LOCAL,
		.d_local.local = local,
	});
	return val;
}

lir_rlocal_t lir_local_new(lir_proc_t *proc, istr_t name, loc_t loc, type_t type, bool is_mut) {
	lir_local_t local = {
		.name = name,
		.loc = loc,
		.type = type,
		.is_mut = is_mut,
	};
	u32 idx = arrlenu(proc->locals);
	arrpush(proc->locals, local);
	return idx;
}

void lir_local_store(lir_proc_t *proc, lir_rblock_t block, lir_rlocal_t local, lir_rvalue_t value) {
	lir_block_t *b = &proc->blocks[block];
	lir_rvalue_t val = lir_local_addr(proc, block, local);
	lir_rvalue_t mut = lir_mut(proc, block, val);
	lir_inst_new(proc, block, (lir_inst_t){
		.target = LIR_VALUE_NONE,
		.kind = INST_STORE,
		.d_store = {
			.dest = mut,
			.src = value,
		},
	});
}

lir_rvalue_t lir_local_load(lir_proc_t *proc, lir_rblock_t block, lir_rlocal_t local) {
	lir_block_t *b = &proc->blocks[block];
	lir_rvalue_t val = lir_local_addr(proc, block, local);
	lir_rvalue_t rvalue = lir_value_new(proc, (lir_value_t){.type = proc->locals[local].type, .loc = proc->locals[local].loc});
	lir_inst_new(proc, block, (lir_inst_t){
		.target = rvalue,
		.kind = INST_LOAD,
		.d_load = {
			.src = val,
		}
	});
	return rvalue;
}

static void _print_local(lir_proc_t *proc, lir_rlocal_t local) {
	lir_local_t *localp = &proc->locals[local];
	if (localp->name) {
		printf("%s", sv_from(localp->name));
	}
	printf(".%u", local);
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
		printf("%s", blockp->debug_name);
	} else {
		printf(".L%d", block);
	}
}

static void _print_value(lir_proc_t *proc, lir_rvalue_t value) {
	lir_value_t *valuep = &proc->values[value];
	printf("v%u", value);
}

static void _print_blockterm(lir_proc_t *proc, lir_term_block_t *block) {
	_print_blockref(proc, block->block);
	// block(v0, v1)
	u32 c = arrlenu(block->args);
	if (c > 0) {
		printf("(");
		for (u32 i = 0; i < c; i++) {
			lir_rvalue_t arg = block->args[i];
			lir_value_t *argp = &proc->values[arg];
			_print_value(proc, arg);
			if (i + 1 < c) {
				printf(", ");
			}
		}
		printf(")");
	}
}

static void _print_inst(lir_proc_t *proc, lir_inst_t *inst) {
	// assert(inst->kind < _INST_MAX);	

	printf("  ");

	if (inst->target != LIR_VALUE_NONE) {
		_print_value(proc, inst->target);
		printf(" = ");
	}

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
				lir_rvalue_t elem = inst->d_array[i];
				_print_value(proc, elem);
				if (i + 1 < c) {
					printf(", ");
				}
			}
			printf("]");
		}
		case INST_TUPLE_UNIT: {
			printf("()");
			break;
		}
		case INST_TUPLE: {
			printf("(");
			for (u32 i = 0, c = arrlenu(inst->d_tuple); i < c; i++) {
				lir_rvalue_t elem = inst->d_tuple[i];
				_print_value(proc, elem);
				if (i + 1 < c) {
					printf(", ");
				}
			}
			printf(")");
			break;
		}
		case INST_LOCAL: {
			printf("&");
			_print_local(proc, inst->d_local.local);
			break;
		}
		case INST_SYMBOL: {
			assert_not_reached();
		}
		case INST_MUT: {
			printf("mut ");
			_print_value(proc, inst->d_mut);
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

			_print_value(proc, inst->d_infix.lhs);
			printf(" %s ", lit_tbl[inst->kind]);
			_print_value(proc, inst->d_infix.rhs);
			break;
		}
		case INST_NEG:
		case INST_NOT: {
			const char *lit_tbl[] = {
				[INST_NEG] = "-",
				[INST_NOT] = "!",
			};

			printf("%s", lit_tbl[inst->kind]);
			_print_value(proc, inst->d_unary.src);
			break;
		}
		case INST_LOAD: {
			printf("load ");
			_print_value(proc, inst->d_load.src);
			break;
		}
		case INST_STORE: {
			printf("store ");
			_print_value(proc, inst->d_store.dest);
			printf(", ");
			_print_value(proc, inst->d_store.src);
			break;
		}
		case INST_LEA: {
			// &value[index]
			printf("&");
			_print_value(proc, inst->d_lea.src);
			printf("[");
			_print_value(proc, inst->d_lea.index);
			printf("]");
			break;
		}
		case INST_SLICE: {
			// value[start..end]
			_print_value(proc, inst->d_slice.src);
			printf("[");
			if (inst->d_slice.lo != LIR_VALUE_NONE) {
				_print_value(proc, inst->d_slice.lo);
			}
			printf("..");
			if (inst->d_slice.hi != LIR_VALUE_NONE) {
				_print_value(proc, inst->d_slice.hi);
			}
		}
		case INST_CALL: {
			// value(a, b, c)
			_print_value(proc, inst->d_call.f);
			printf("(");
			for (u32 i = 0, c = arrlenu(inst->d_call.args); i < c; i++) {
				lir_rvalue_t arg = inst->d_call.args[i];
				_print_value(proc, arg);
				if (i + 1 < c) {
					printf(", ");
				}
			}
			printf(")");
			break;
		}
		case INST_CAST: {
			// value as type
			_print_value(proc, inst->d_cast.src);
			printf(" as %s", type_dbg_str(inst->d_cast.type));
			break;
		}
		case INST_FIELD_OFFSET: {
			// value.field
			_print_value(proc, inst->d_field_offset.src);
			printf(".field %s", sv_from(inst->d_field_offset.field));
			break;
		}
		case INST_TUPLE_OFFSET: {
			// value.0
			_print_value(proc, inst->d_tuple_offset.src);
			printf(".tuple %zu", inst->d_tuple_offset.index);
			break;
		}
		/* case INST_SIZEOF: {

		} */
		default: {
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
		printf("  ");
		if (localp->is_mut) {
			printf("'");
		}
		_print_local(proc, i);
		printf(": %s", type_dbg_str(localp->type));
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
				lir_rvalue_t arg = block->args[i];
				lir_value_t *argp = &proc->values[arg];
				_print_value(proc, arg);
				printf(": %s", type_dbg_str(argp->type));
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
				_print_blockterm(proc, &block->term.d_goto);
				printf("\n");
				break;
			}
			case TERM_RET: {
				printf("  ret ");
				_print_value(proc, block->term.d_ret.value);
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
				_print_value(proc, block->term.d_goto_pattern.value);
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
			/* case TERM_GOTO_PATTERN: {
				printf("  goto_pattern ");
				_print_value(proc, block->term.d_goto_pattern.value);
				printf(" (");
				for (u32 i = 0, c = arrlenu(block->term.d_goto_pattern.blocks); i < c; i++) {
					lir_rblock_t block = block->term.d_goto_pattern.blocks[i];
					_print_blockref(proc, block);
					if (i + 1 < c) {
						printf(", ");
					}
				}
				printf(") (");
				for (u32 i = 0, c = arrlenu(block->term.d_goto_pattern.patterns); i < c; i++) {
					lir_rpattern_t pattern = block->term.d_goto_pattern.patterns[i];
					lir_pattern_t *patternp = &proc->patterns[pattern];
					printf("%s", sv_from(patternp->name));
					if (i + 1 < c) {
						printf(", ");
					}
				}
				printf(")\n");
				break;
			} */
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