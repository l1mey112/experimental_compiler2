#include "all.h"
#include "check.h"

// TODO: fix algorithm to a coloured one, then reverse the list
//       this one is quite broken
//       https://eli.thegreenplace.net/2015/directed-graph-traversal-orderings-and-applications-to-data-flow-analysis/#color-dfs-and-edge-classification

/* lir_rsym_t *creorder_sorted;

static void visit(lir_rsym_t rsym);

static void visit_dependent(lir_rsym_t parent, lir_rsym_t rsym) {
	lir_sym_t *sym = &symbols[rsym];
	
	if (sym->is_placeholder) {
		// error user
		assert_not_reached();
		return;
	}

	// it's impossible to self recurse into incomplete functions	
	if (sym->kind == SYMBOL_PROC && parent == rsym) {
		return;
	}
	
	if (sym->is_visited) {
		// cycle error
		assert_not_reached();
		return;
	}

	visit(rsym);
}

static void visit(lir_rsym_t rsym) {
	lir_sym_t *sym = &symbols[rsym];

	// contains
	for (u32 i = 0; i < arrlenu(creorder_sorted); i++) {
		if (creorder_sorted[i] == rsym) {
			return;
		}
	}

	// these can be removed really
	assert(sym->is_placeholder == false);
	assert(sym->is_visited == false);
	sym->is_visited = true;

	// it would be nice if we had closures so we could run a function
	// on each dependent

	// type, function, global
	//
	// types
	// 1. dependent on other types
	//
	// functions
	// 1. dependent on types in the function signature (annotated type)
	// 2. dependent on globals
	//
	// globals
	// 1. dependent on their annotated type
	// 2. dependent on other globals
	//
	//
	// function dependencies
	//
	// in short, functions can't depend on other functions under any
	// circumstance. it just can't happen.
	//
	// let x
    // let y = \t -> x t
    // let x = \t -> y t
	//
	// above is a nice attempt at a cyclic dependency but it's not allowed
	// since `x` is being used before it is defined inside the closure
	// instanciation of `y`. this is a compile time error.
	//
	// we could try this at the top level, but closures are not allowed
	// in constant expressions and functions must be completely typed.
	//
	// functions can't depend on eachother
	//

	switch (sym->kind) {
		case SYMBOL_PROC: {
			// we don't have user defined types yet, just search lvalues

			// blocks[] -> insts[] -> { dest: lvalue } & { d_lvalue: lvalue }

			lir_proc_t *proc = &sym->d_proc;

			for (lir_rblock_t block = 0, c = arrlenu(proc->blocks); block < c; block++) {
				lir_block_t *b = &proc->blocks[block];

				for (u32 inst = 0, c = arrlenu(b->insts); inst < c; inst++) {
					lir_inst_t *i = &b->insts[inst];

					if (i->kind == INST_LVALUE && i->d_lvalue.is_sym) {
						visit_dependent(rsym, i->d_lvalue.symbol);
					}

					if (i->lvalue_dest && i->dest.lvalue.is_sym) {
						visit_dependent(rsym, i->dest.lvalue.symbol);
					}
				}
			}
			
			break;
		}
		case SYMBOL_TYPE:
		case SYMBOL_GLOBAL:
		default: {
			printf("kind: %u %u - %zu\n", sym->kind, rsym, hmlenu(symbols));
			assert_not_reached();
		}
	}

	sym->is_visited = false;
	arrpush(creorder_sorted, rsym);
} */

// 1. sanity checks on the table (placeholders)
// 2. identifies cyclic dependencies
// 3. returns a list of symbols in the order they should be processed
void creorder_and_type(void) {
	// raising an error on referencing a placeholder symbol (doesn't exist)
	// it must be raised on the actual lvalue. so when visiting definitions,
	// ignore placeholders. they'll be errors soon when we visit the dependents

	// we store the visited status on the actual `lir_sym_t`, it's easier

	/* for (lir_rsym_t i = 0; i < hmlenu(symbols); i++) {
		lir_sym_t *sym = &symbols[i];

		if (sym->is_placeholder) {
			continue;
		}

		if (sym->is_visited) {
			continue;
		}

		visit(i);
	}

	for (lir_rsym_t i = 0; i < arrlenu(creorder_sorted); i++) {
		lir_sym_t *sym = &symbols[creorder_sorted[i]];

		switch (sym->kind) {
			case SYMBOL_PROC: {
				ctype_proc(sym, &sym->d_proc);
				break;
			}
			case SYMBOL_GLOBAL:
			case SYMBOL_TYPE: {
				assert_not_reached();
			}
		}
	} */
}
