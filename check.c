#include "check.h"
#include "all.h"

// typecheck a list of topologically ordered symbols

// #define FOR_USERS(u, n) for (User *u = n->users; u; u = u->next)

#define FOR_BLOCKS(proc, block) for (lir_rblock_t block = 0, c = arrlenu((proc)->blocks); block < c; block++)

void cinst(lir_proc_t *proc, lir_rblock_t block) {
	
}

// ensure `value` is defined here
void cblock_term_inst(lir_proc_t *proc, lir_rblock_t block, lir_rlocal_t local) {
	// no type can be acertained for user variables, they have their own defs
	if (proc->locals[local].kind != LOCAL_SSA) {
		return;
	}

	// 1. flood fill with upvalues
	// 2. recurse downwards?

	//     %11 = 20
	//     %2 = 0
	//     f = 0
	//     goto if.exit(%2: u8)
	// if.exit.4(%12: u8):
	//     %0 = f
	//     %1 = (%0, %11, %12)         (i32, u8)
	//     ret %1

	// 1. assign upvalues to propagate upwards
	// 2. recurse upwards
	// 3. typecheck the current block
}

void cblock_term(lir_proc_t *proc, lir_rblock_t block, lir_term_block_t term) {
	for (u32 i = 0, c = arrlenu(term.args); i < c; i++) {
		lir_rlocal_t arg = term.args[i];
		cblock_term_inst(proc, block, arg);
	}
}

void cblock(lir_proc_t *proc, lir_rblock_t block) {
	lir_block_t *b = &proc->blocks[block];

	// 0. start at all `ret` instructions
	//
	// 1. all arguments to a terminator are roots
	// 2. walk upwards in a block, for all instructions follow the chain down and
	//    mark each instruction or assign a type
	// 3. all assignments to non SSA locals are roots
	// 4. when reaching a parameter to a block, recurse into (with upvalues) all
	//    blocks with edges that share the current one. then perform unification
	//    (and possibly explicit casting) on all of the values coming in

	switch (b->term.kind) {
		case TERM_GOTO: {
			cblock_term(proc, block, b->term.d_goto);
			break;
		}
		case TERM_IF: {
			// if usually has ZERO arguments flowing in
			
			cblock_term_inst(proc, block, b->term.d_if.cond);
			cblock_term(proc, block, b->term.d_if.then);
			cblock_term(proc, block, b->term.d_if.els);
			break;
		}
		case TERM_RET: {
			cblock_term_inst(proc, block, b->term.d_ret.value);
			break;
		}
		case TERM_GOTO_PATTERN: {
			cblock_term_inst(proc, block, b->term.d_goto_pattern.value);
			break;
		}
		default: {
			assert_not_reached();
		}
	}
}

void cproc(lir_rsym_t rsym) {
	lir_sym_t *sym = &symbols[rsym];
	assert(sym->kind == SYMBOL_PROC);

	// NOTE: this ptr is unstable, don't create more symbols
	//       (we don't really need to anyway)
	lir_proc_t *proc = &sym->d_proc;

	FOR_BLOCKS(proc, i) {
		lir_block_t *b = &proc->blocks[i];

		if (b->term.kind == TERM_RET) {
			// root block
			cblock(proc, i);
		}
	}
}

void lir_check(void) {
    creorder();

	for (lir_rsym_t i = 0; i < arrlenu(creorder_sorted); i++) {
		lir_sym_t *sym = &symbols[creorder_sorted[i]];

		switch (sym->kind) {
			case SYMBOL_PROC: {
				cproc(i);
				break;
			}
			case SYMBOL_GLOBAL:
			case SYMBOL_TYPE: {
				assert_not_reached();
			}
		}
	}
}