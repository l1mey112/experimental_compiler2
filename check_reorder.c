#include "all.h"
#include "check.h"
#include "hir.h"

static void visit_po(rsym_t **po, rsym_t rsym);
static void visit_sanity_type(rsym_t **po, type_t type, loc_t onerror);

// all sym that have referents will go through here
static void visit_definite_successor(rsym_t **po, rsym_t rsym, rsym_t rsucc, loc_t onerror, bool indirect) {
	sym_t *sym = &symbols[rsym];
	sym_t *succ = &symbols[rsucc];

	if (succ->sort_colour == SYM_SORT_GREY) {
		// global = &global      (not allowed, need annotation)
		// func() = func         (not allowed, need annotation)

		// assumes sym.kind == succ.kind
		switch (succ->kind) {
			case SYMBOL_PROC: {
				if (type_get(succ->d_proc.type)->d_fn.ret == TYPE_INFER) {
					if (rsucc == rsym) {
						// cycle self -> self
						err_with_pos(succ->loc, "self recursive function `%s` needs return type annotation", sv_from(succ->short_name));
					} else {
						// cycle self -> ... -> self
						err_with_pos(onerror, "mutually recursive function `%s` needs return type annotation", sv_from(succ->short_name));
					}
				}
				break;
			}
			case SYMBOL_GLOBAL: {
				if (indirect) {
					if (rsucc == rsym && succ->d_global.type == TYPE_INFER) {
						err_with_pos(onerror, "ref to `%s` needs type annotation", sv_from(succ->short_name));
					} else {
						break;
					}
				}
				// fallthrough
			}
			default: {
				// TODO: better errors		
				if (rsucc == rsym) {
					// cycle self -> self
					err_with_pos(onerror, "self cyclic dependency `%s`", sv_from(succ->short_name));
				} else {
					// cycle self -> ... -> self
					err_with_pos(onerror, "cyclic dependency in `%s` on `%s`", sv_from(sym->key), sv_from(succ->key));
				}
			}
		}
	}

	// dfs walk
	if (succ->sort_colour == SYM_SORT_WHITE) {
		visit_po(po, rsucc);
		return;
	}
}

// run first before `visit_definite_successor()`
static void visit_sanity_value_sym(rsym_t **po, rsym_t rsym, hir_expr_t *expr) {
	// sanity, can't be type
	sym_t *sym = &symbols[expr->d_sym];

	if (sym->is_placeholder) {
		err_with_pos(expr->loc, "unknown ident `%s`", sv_from(sym->short_name));
	}

	if (sym->kind == SYMBOL_TYPE) {
		err_with_pos(expr->loc, "expected value, got type `%s`", sv_from(sym->short_name));
	}

	// check `test = &test` without `: T`
}

// &expr -> direct = true
static void visit_successors_hir_impl(rsym_t **po, rsym_t rsym, hir_expr_t *expr, bool direct) {
	switch (expr->kind) {
		case EXPR_SYM: {
			visit_sanity_value_sym(po, rsym, expr);
			visit_definite_successor(po, rsym, expr->d_sym, expr->loc, !direct);
			break;
		}
		//
		case EXPR_ARRAY: {
			for (u32 i = 0, c = arrlenu(expr->d_array); i < c; i++) {
				visit_successors_hir_impl(po, rsym, &expr->d_array[i], true);
			}
			break;
		}
		case EXPR_TUPLE: {
			for (u32 i = 0, c = arrlenu(expr->d_tuple); i < c; i++) {
				visit_successors_hir_impl(po, rsym, &expr->d_tuple[i], true);
			}
			break;
		}
		case EXPR_DO_BLOCK: {
			for (u32 i = 0, c = arrlenu(expr->d_do_block.exprs); i < c; i++) {
				visit_successors_hir_impl(po, rsym, &expr->d_do_block.exprs[i], true);
			}
			break;
		}
		case EXPR_LOOP: {
			visit_successors_hir_impl(po, rsym, expr->d_loop.expr, true);
			break;
		}
		case EXPR_IF: {
			visit_successors_hir_impl(po, rsym, expr->d_if.cond, true);
			visit_successors_hir_impl(po, rsym, expr->d_if.then, true);
			visit_successors_hir_impl(po, rsym, expr->d_if.els, true);
			break;
		}
		case EXPR_ASSIGN: {
			visit_successors_hir_impl(po, rsym, expr->d_assign.lhs, true);
			visit_successors_hir_impl(po, rsym, expr->d_assign.rhs, true);
			break;
		}
		case EXPR_INFIX: {
			visit_successors_hir_impl(po, rsym, expr->d_infix.lhs, true);
			visit_successors_hir_impl(po, rsym, expr->d_infix.rhs, true);
			break;
		}
		case EXPR_POSTFIX: {
			visit_successors_hir_impl(po, rsym, expr->d_postfix.expr, true);
			break;
		}
		case EXPR_PREFIX: {
			visit_successors_hir_impl(po, rsym, expr->d_prefix.expr, true);
			break;
		}
		case EXPR_DEREF: {
			visit_successors_hir_impl(po, rsym, expr->d_deref->d_deref, true);
			break;
		}
		case EXPR_CAST: {
			visit_successors_hir_impl(po, rsym, expr->d_cast.expr, false);
			visit_sanity_type(po, expr->type, expr->d_cast.type_loc);
			break;
		}
		case EXPR_CALL: {
			visit_successors_hir_impl(po, rsym, expr->d_call.f, true);
			for (u32 i = 0, c = arrlenu(expr->d_call.args); i < c; i++) {
				visit_successors_hir_impl(po, rsym, &expr->d_call.args[i], true);
			}
			break;
		}
		case EXPR_BREAK: {
			visit_successors_hir_impl(po, rsym, expr->d_break.expr, true);
			break;
		}
		case EXPR_RETURN: {
			visit_successors_hir_impl(po, rsym, expr->d_return, true);
			break;
		}
		case EXPR_VOIDING: {
			visit_successors_hir_impl(po, rsym, expr->d_voiding, true);
			break;
		}
		case EXPR_LET: {
			visit_successors_hir_impl(po, rsym, expr->d_let.expr, true);
			break;
		}
		//
		case EXPR_INDEX: {
			// &v[0]
			visit_successors_hir_impl(po, rsym, expr->d_index.expr, direct);
			visit_successors_hir_impl(po, rsym, expr->d_index.index, true);
			break;
		}
		case EXPR_SLICE: {
			// &v[0..1]
			visit_successors_hir_impl(po, rsym, expr->d_slice.expr, direct);
			if (expr->d_slice.lo) {
				visit_successors_hir_impl(po, rsym, expr->d_slice.lo, true);
			}
			if (expr->d_slice.hi) {
				visit_successors_hir_impl(po, rsym, expr->d_slice.hi, true);
			}
			break;
		}
		case EXPR_FIELD: {
			visit_successors_hir_impl(po, rsym, expr->d_field.expr, direct);
			break;
		}
		case EXPR_ADDR_OF: {
			// test = test         (not allowed)
			// test = &test        (allowed)
			// test = &test[test]  (not allowed)
			visit_successors_hir_impl(po, rsym, expr->d_addr_of.ref, false);
			break;
		}
		//
		case EXPR_LOCAL:
		case EXPR_CONTINUE:
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

static void visit_successors_hir(rsym_t **po, rsym_t rsym, hir_expr_t *expr) {
	visit_successors_hir_impl(po, rsym, expr, true);
}

static void visit_successors_type(rsym_t **po, rsym_t rsym, type_t type, loc_t onerror) {
	if (type < _TYPE_CONCRETE_MAX) {
		return;
	}

	tinfo_t *info = type_get(type);

	switch (info->kind) {
		case TYPE_TUPLE: {
			for (u32 i = 0, c = arrlenu(info->d_tuple); i < c; i++) {
				visit_successors_type(po, rsym, info->d_tuple[i], onerror);
			}
			break;
		}
		case TYPE_ARRAY: {
			visit_successors_type(po, rsym, info->d_array.elem, onerror);
			break;
		}
		case TYPE_SYMBOL: {
			visit_definite_successor(po, rsym, info->d_symbol, onerror, false);
			break;
		}
		case TYPE_PTR: break;
		case TYPE_SLICE: break;
		case TYPE_FUNCTION: break;
		default: {
			assert_not_reached();
		}
	}
}

static void visit_sanity_type(rsym_t **po, type_t type, loc_t onerror) {
	if (type < _TYPE_CONCRETE_MAX) {
		return;
	}

	tinfo_t *info = type_get(type);

	switch (info->kind) {
		case TYPE_TUPLE: {
			for (u32 i = 0, c = arrlenu(info->d_tuple); i < c; i++) {
				visit_sanity_type(po,info->d_tuple[i], onerror);
			}
			break;
		}
		case TYPE_ARRAY: {
			visit_sanity_type(po, info->d_array.elem, onerror);
			break;
		}
		case TYPE_PTR: {
			visit_sanity_type(po, info->d_ptr.ref, onerror);
			break;
		}
		case TYPE_SLICE: {
			visit_sanity_type(po, info->d_slice.elem, onerror);
			break;
		}
		case TYPE_FUNCTION: {
			visit_sanity_type(po, info->d_fn.ret, onerror);
			for (u32 i = 0, c = arrlenu(info->d_fn.args); i < c; i++) {
				visit_sanity_type(po, info->d_fn.args[i], onerror);
			}
			break;
		}
		case TYPE_SYMBOL: {
			sym_t *sym = &symbols[info->d_symbol];
			if (sym->is_placeholder) {
				err_with_pos(onerror, "unknown type `%s`", sv_from(sym->short_name));
			}
			if (sym->kind != SYMBOL_TYPE) {
				err_with_pos(onerror, "expected type, got `%s`", sv_from(sym->short_name));
			}
			break;
		}
		default: {
			assert_not_reached();
		}
	}
}

static void visit_successors_typeinfo(rsym_t **po, rsym_t rsym, tsymbol_t *typeinfo) {
	switch (typeinfo->kind) {
		case TYPESYMBOL_STRUCT: {
			for (u32 i = 0, c = arrlenu(typeinfo->d_struct.fields); i < c; i++) {
				type_t type = typeinfo->d_struct.fields[i].type;
				loc_t onerror = typeinfo->d_struct.fields[i].type_loc;
				visit_successors_type(po, rsym, type, onerror);
				visit_sanity_type(po, type, onerror);
			}
			break;
		}
		default: {
			assert_not_reached();
		}
	}
}

static void visit_desc_locals(rsym_t **po, local_t *locals) {
	for (u32 i = 0, c = arrlenu(locals); i < c; i++) {
		local_t *local = &locals[i];
		if (local->type != TYPE_INFER) {
			visit_sanity_type(po, local->type, local->type_loc);
		}
	}
}

// dfs walk
static void visit_po(rsym_t **po, rsym_t rsym) {
	sym_t *sym = &symbols[rsym];

	sym->sort_colour = SYM_SORT_GREY;

	// typesymbols are self referencial types, we only need to check for
	// cycles in the symbol table

	// walk the HIR (tree) for dependencies/successors
	switch (sym->kind) {
		case SYMBOL_PROC: {
			proc_t *proc = &sym->d_proc;
			
			visit_successors_hir(po, rsym, &proc->desc.hir);
			tinfo_t *typeinfo = type_get(proc->type);
			assert(typeinfo->kind == TYPE_FUNCTION);
			
			if (typeinfo->d_fn.ret != TYPE_INFER) {
				visit_sanity_type(po, typeinfo->d_fn.ret, proc->ret_type_loc);
			}
			visit_desc_locals(po, proc->desc.locals);
			break;
		}
		case SYMBOL_GLOBAL: {
			global_t *global = &sym->d_global;
			
			visit_successors_hir(po, rsym, &sym->d_global.desc.hir);
			visit_desc_locals(po, global->desc.locals);
			if (global->type != TYPE_INFER) {
				visit_sanity_type(po, global->type, global->type_loc);
			}
			break;
		}
		case SYMBOL_TYPE: {
			visit_successors_typeinfo(po, rsym, &sym->d_type);
			break;
		}
		default: {
			assert_not_reached();
		}
	}

	arrpush(*po, rsym);
	sym->sort_colour = SYM_SORT_BLACK; // full sorted
}

// reorder pass.
//
// the reorder pass orders symbols topologically so that further passes
// can iterate over symbols dependency free. this is a requirement for
// type checking and most other passes.
//
// currently, symbols are three different things:
//  1. functions
//  2. globals
//  3. user defined types (typesymbols)
//
// NOTE: typesymbols are self referencial types. they are also unique,
//       bound to a name, and don't make sense to be interned.
//
// the reorder pass does a lot more than just reorder. it also identifies
// cycles and self dependencies. it also checks that all symbols are valid.
//
// sanity checks:
//  1. does this symbol exist? (non placeholder)
//  2. is this type cyclic without indirection?
//  3. is this global cyclic/self dependent without &indirection?
//  4. is this function self recursive without return type annotation?
//  5. is this symbol used in the right context? (e.g. typesymbol in a type)
//
rsym_t *creorder_po_and_sanity(void) {
	// use the algorithm under `3-color DFS and edge classification` from Eli's website:
	// https://eli.thegreenplace.net/2015/directed-graph-traversal-orderings-and-applications-to-data-flow-analysis
	//
	// we store the tri-colour on the symbol itself, makes things easier that way.
	// create a postorder ordering, then reversed (RPO) is our final ordering.
	//
	// when visiting definitions, ignore placeholders. they'll be errors soon
	// on reaching actual uses of the symbol. (dependednts)
	//
	rsym_t *po = NULL;

	// 5. symbol being used in the right context.
	//
	//    there are multiple ways to solve this, currently we don't use this
	//    method but im putting it here for reference.
	//
	//    when you `table_resolve()` pass the current context it is being used
	//    in and it's location. resolving more symbols that don't match this
	//    current context is an intant error, and registering a symbol that
	//    doesn't match this current context is an error.
	//
	//    since it's quite rare that you'll have multiple unresolved references
	//    to a placeholder symbol, it should be fine really.
	//
	//    currently, we just try to search for every single use of a symbol
	//    recursively like there is no way around it.

	// iterate over all symbols, white symbols are roots
	for (rsym_t rsym = 0; rsym < hmlenu(symbols); rsym++) {
		sym_t *sym = &symbols[rsym];

		// ignore
		if (sym->is_placeholder) {
			continue;
		}

		// root
		if (sym->sort_colour == SYM_SORT_WHITE) {
			visit_po(&po, rsym);
		}
	}

	return po;
}
