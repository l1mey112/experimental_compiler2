#include "all.h"

#include "stb_ds.h"

// ROOT IS ALWAYS ZERO
u32 ir_inst_arena_len = 1;
u32 ir_scope_arena_len;
ir_inst_t ir_inst_arena[2048];
ir_scope_t ir_scope_arena[2048];

ir_scope_t *ir_scope(void) {
	ir_rscope_t id = ir_scope_arena_len;
	ir_scope_t *s = &ir_scope_arena[ir_scope_arena_len++];
	s->id = id;
	s->first = (ir_rinst_t)-1;
	s->last = (ir_rinst_t)-1;
	s->len = 0;
	return s;
}

ir_rinst_t ir_append(ir_rscope_t scope_ref, ir_rinst_t at, ir_inst_t inst) {
	ir_rinst_t id = ir_inst_arena_len;
	inst.id = id;
	inst.next = (ir_rinst_t)-1;
	inst.prev = (ir_rinst_t)-1;
	ir_inst_arena[ir_inst_arena_len++] = inst;

	ir_scope_t *s = &ir_scope_arena[scope_ref];
	s->len++;

	if (s->first == (ir_rinst_t)-1) {
		s->first = id;
		s->last = id;
	} else {
		ir_inst_t *at_inst = &ir_inst_arena[at];

		if (at == s->last) {
			s->last = id;
		}

		if (at_inst->next != (ir_rinst_t)-1) {
			ir_inst_t *next_inst = &ir_inst_arena[at_inst->next];
			next_inst->prev = id;
		}

		at_inst->next = id;
		
		inst.prev = at;
		inst.next = at_inst->next;
	}

	return id;
}

ir_rinst_t ir_prepend(ir_rscope_t scope_ref, ir_rinst_t at, ir_inst_t inst) {
	ir_rinst_t id = ir_inst_arena_len;
	inst.id = id;
	inst.next = (ir_rinst_t)-1;
	inst.prev = (ir_rinst_t)-1;
	ir_inst_arena[ir_inst_arena_len++] = inst;

	ir_scope_t *s = &ir_scope_arena[scope_ref];
	s->len++;

	if (s->first == (ir_rinst_t)-1) {
		s->first = id;
		s->last = id;
	} else {
		ir_inst_t *at_inst = &ir_inst_arena[at];

		if (at == s->first) {
			s->first = id;
		}

		if (at_inst->next != (ir_rinst_t)-1) {
			ir_inst_t *prev_inst = &ir_inst_arena[at_inst->prev];
			prev_inst->next = id;
		}

		at_inst->prev = id;

		inst.prev = at_inst->prev;
		inst.next = at;
	}

	return id;
}
