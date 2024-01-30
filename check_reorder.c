#include "all.h"
#include "check.h"
#include "hir.h"

// TODO: fix algorithm to a coloured one, then reverse the list
//       this one is quite broken
//       https://eli.thegreenplace.net/2015/directed-graph-traversal-orderings-and-applications-to-data-flow-analysis/#color-dfs-and-edge-classification

/* rsym_t *creorder_sorted;

static void visit(rsym_t rsym);

static void visit_dependent(rsym_t parent, rsym_t rsym) {
	sym_t *sym = &symbols[rsym];
	
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

static void visit(rsym_t rsym) {
	sym_t *sym = &symbols[rsym];

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

static void visit_po(rsym_t **po, rsym_t rsym);

// all sym that have referents will go through here
static void visit_definite_successor(rsym_t **po, rsym_t rsym, rsym_t rsucc, loc_t onerror) {
	sym_t *sym = &symbols[rsym];
	sym_t *succ = &symbols[rsucc];

	// all placeholders are referents
	if (succ->is_placeholder) {
		err_with_pos(onerror, "unknown ident `%s`", sv_from(succ->short_name));
	}

	if (succ->sort_colour == SYM_SORT_GREY) {
		// cycle self -> self
		if (rsucc == rsym) {
			err_with_pos(onerror, "self cyclic dependency `%s`", sv_from(succ->short_name));
		}

		// cycle self -> ... -> self
		err_with_pos(onerror, "cyclic dependency in `%s` on `%s`", sv_from(sym->key), sv_from(succ->key));
	}

	// dfs walk
	if (succ->sort_colour == SYM_SORT_WHITE) {
		visit_po(po, rsucc);
		return;
	}
}

static void visit_successors_hir(rsym_t **po, rsym_t rsym, hir_expr_t *expr) {
	switch (expr->kind) {
		case EXPR_SYM: {
			visit_definite_successor(po, rsym, expr->d_sym, expr->loc);
			break;
		}
		//
		case EXPR_ARRAY: {
			for (u32 i = 0, c = arrlenu(expr->d_array); i < c; i++) {
				visit_successors_hir(po, rsym, &expr->d_array[i]);
			}
			break;
		}
		case EXPR_TUPLE: {
			for (u32 i = 0, c = arrlenu(expr->d_tuple); i < c; i++) {
				visit_successors_hir(po, rsym, &expr->d_tuple[i]);
			}
			break;
		}
		case EXPR_DO_BLOCK: {
			for (u32 i = 0, c = arrlenu(expr->d_do_block.exprs); i < c; i++) {
				visit_successors_hir(po, rsym, &expr->d_do_block.exprs[i]);
			}
			break;
		}
		case EXPR_LOOP: {
			visit_successors_hir(po, rsym, expr->d_loop.expr);
			break;
		}
		case EXPR_IF: {
			visit_successors_hir(po, rsym, expr->d_if.cond);
			visit_successors_hir(po, rsym, expr->d_if.then);
			visit_successors_hir(po, rsym, expr->d_if.els);
			break;
		}
		case EXPR_ASSIGN: {
			visit_successors_hir(po, rsym, expr->d_assign.lhs);
			visit_successors_hir(po, rsym, expr->d_assign.rhs);
			break;
		}
		case EXPR_INFIX: {
			visit_successors_hir(po, rsym, expr->d_infix.lhs);
			visit_successors_hir(po, rsym, expr->d_infix.rhs);
			break;
		}
		case EXPR_POSTFIX: {
			visit_successors_hir(po, rsym, expr->d_postfix.expr);
			break;
		}
		case EXPR_PREFIX: {
			visit_successors_hir(po, rsym, expr->d_prefix.expr);
			break;
		}
		case EXPR_DEREF: {
			visit_successors_hir(po, rsym, expr->d_deref->d_deref);
			break;
		}
		case EXPR_INDEX: {
			visit_successors_hir(po, rsym, expr->d_index.expr);
			visit_successors_hir(po, rsym, expr->d_index.index);
			break;
		}
		case EXPR_SLICE: {
			visit_successors_hir(po, rsym, expr->d_slice.expr);
			if (expr->d_slice.lo) {
				visit_successors_hir(po, rsym, expr->d_slice.lo);
			}
			if (expr->d_slice.hi) {
				visit_successors_hir(po, rsym, expr->d_slice.hi);
			}
			break;
		}
		case EXPR_CAST: {
			visit_successors_hir(po, rsym, expr->d_cast);
			break;
		}
		case EXPR_CALL: {
			visit_successors_hir(po, rsym, expr->d_call.f);
			for (u32 i = 0, c = arrlenu(expr->d_call.args); i < c; i++) {
				visit_successors_hir(po, rsym, &expr->d_call.args[i]);
			}
			break;
		}
		case EXPR_BREAK: {
			visit_successors_hir(po, rsym, expr->d_break.expr);
			break;
		}
		case EXPR_RETURN: {
			visit_successors_hir(po, rsym, expr->d_return);
			break;
		}
		case EXPR_VOIDING: {
			visit_successors_hir(po, rsym, expr->d_voiding);
			break;
		}
		case EXPR_LET: {
			visit_successors_hir(po, rsym, expr->d_let.expr);
			break;
		}
		//
		case EXPR_ADDR_OF: {
			// TODO: needs more analysis to convert into proper symbol lvalue
			assert_not_reached();
			break;
		}
		//
		case EXPR_LOCAL:
		case EXPR_CONTINUE:
		case EXPR_FIELD:
		case EXPR_INTEGER_LIT:
		case EXPR_BOOL_LIT:
		case EXPR_TUPLE_UNIT: {
			break; // empty
		}
		default: {
			assert_not_reached();
		}
	}
}

static void visit_successors_type(rsym_t **po, rsym_t rsym, type_t type) {
	if (type < _TYPE_CONCRETE_MAX) {
		return;
	}

	tinfo_t *info = type_get(type);

	// TODO: needs loc

	/* switch (info->kind) {
		case TYPE_TUPLE: {
			for (u32 i = 0, c = arrlenu(info->d_tuple); i < c; i++) {
				visit_successors_type(po, rsym, info->d_tuple[i]);
			}
			break;
		}
		case TYPE_ARRAY:
		case TYPE_STRUCT:
		
		case TYPE_SYMBOL: {
			visit_definite_successor(po, rsym, info->d_symbol, info->loc);
			break;
		}

		case TYPE_PTR: break;
		case TYPE_CLOSURE: break;
		case TYPE_CLOSURE_UNION: break;
		case TYPE_SLICE: break;
		case TYPE_FUNCTION: break;
		default: {
			assert_not_reached();
		}
	} */
}

// dfs walk
static void visit_po(rsym_t **po, rsym_t rsym) {
	sym_t *sym = &symbols[rsym];

	sym->sort_colour = SYM_SORT_GREY;

	// walk the HIR (tree) for dependencies/successors
	switch (sym->kind) {
		case SYMBOL_PROC: {
			visit_successors_hir(po, rsym, &sym->d_proc.desc.hir);
			break;
		}
		case SYMBOL_GLOBAL: {
			visit_successors_hir(po, rsym, &sym->d_global.desc.hir);
			break;
		}
		case SYMBOL_TYPE: {
			eprintf("TODO: doing nothing on SYMBOL_TYPE\n");
			break;
		}
		default: {
			assert_not_reached();
		}
	}

	arrpush(*po, rsym);
	sym->sort_colour = SYM_SORT_BLACK; // full sorted
}

// 1. sanity checks on the table (placeholders)
// 2. identifies cyclic dependencies
// 3. returns a list of symbols in the order they should be processed
void creorder_and_type(void) {
	// raising an error on referencing a placeholder symbol (doesn't exist)
	// it must be raised on the actual symbol. so when visiting definitions,
	// ignore placeholders. they'll be errors soon when we visit the dependents

	// we store the visited status on the actual `sym_t`, it's easier

	rsym_t *po = NULL;

	// main.Function
	//      ^^^^^^^^
	// TODO: raise on use

	// iterate through entire type table
	/* for (u32 i = 0; i < type_len; i++) {
		tinfo_t *info = &types[i];

		if (info->kind == TYPE_SYMBOL) {
			sym_t *sym = &symbols[info->d_symbol];

			// will raise placeholder errors inside the topological sort
			// because we need to identify cyclic recursive types
			if (sym->kind != SYMBOL_TYPE) {
				err_with_pos(sym->loc, "expected type symbol");
			}
		}
	} */

	for (rsym_t rsym = 0; rsym < hmlenu(symbols); rsym++) {
		sym_t *sym = &symbols[rsym];

		// will be an error soon
		if (sym->is_placeholder) {
			continue;
		}

		// will access all roots
		if (sym->sort_colour == SYM_SORT_WHITE) {
			visit_po(&po, rsym);
		}
	}

	// RPO
	for (u32 i = arrlenu(po); i-- > 0;) {
		rsym_t rsym = po[i];
		sym_t *sym = &symbols[rsym];
		printf("%u: %s\n", i, sv_from(sym->key));
	}

	/* for (rsym_t i = 0; i < arrlenu(creorder_sorted); i++) {
		sym_t *sym = &symbols[creorder_sorted[i]];

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
