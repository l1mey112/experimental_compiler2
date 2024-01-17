#include "all.h"

#include "stb_ds.h"

lir_rvalue_t lir_value_new(lir_proc_t *proc, lir_value_t value) {
    u32 idx = arrlenu(proc->values);
    value.index = idx;
    arrpush(proc->values, value);
    return idx;
}

lir_rblock_t lir_block_new(lir_proc_t *proc, lir_block_t block) {
    u32 idx = arrlenu(proc->blocks);
    block.index = idx;
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