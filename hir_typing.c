#include "all.h"
#include "check.h"

cctx_t c;

void hir_type_proc(sym_t *sym, proc_t *proc) {
	c = (cctx_t){};

	c.proc_upvalue = proc->ret_type;
	c.proc_ret_type = TYPE_INFER;

	bool ret_annotated = proc->ret_type != TYPE_INFER;

	// we'll need this regardless
	type_t *args_types = NULL;

	// mutually recursive or just recursive procs without a type annotation
	// aren't allowed at all. functions without a type annotation don't
	// have `type` set and must be inferred

	if (!ret_annotated) {
		for (u32 i = 0, c = arrlenu(proc->arguments); i < c; i++) {
			local_t *arg = &proc->desc.locals[proc->arguments[i]];
			arrpush(args_types, arg->type);
		}
	}

	// it's possible for these to have no body
	// no body is okay only if it's extern

	if (sym->is_extern && proc->desc.hir == NULL && !ret_annotated) {
		err_with_pos(sym->loc, "extern proc with no body must have a type annotation");
	} else if (proc->desc.hir) {
		type_t expr_type = cexpr(&proc->desc, proc->ret_type, proc->desc.hir, BM_RVALUE);

		// unify the types of all `ret` and the resulting expression
		// if the proc is annotated, the `brk_type` will be the exact
		// same as the return type.

		if (ret_annotated) {
			(void)ctype_unify(proc->ret_type, proc->desc.hir);
		} else {
			if (c.proc_ret_type == TYPE_INFER) {
				proc->ret_type = expr_type;
			} else {
				(void)ctype_unify(c.proc_ret_type, proc->desc.hir);
				proc->ret_type = c.proc_ret_type;
			}
		}

		if (!ret_annotated) {
			proc->type = type_new((tinfo_t){
				.kind = TYPE_FUNCTION,
				.d_fn = {
					.args = args_types,
					.ret = proc->ret_type,
				},
			});
		}
	}
}

void hir_type_global(sym_t *sym, global_t *global) {
	c = (cctx_t){};

	// it's possible for these to have no body
	// no body is okay only if it's extern

	bool ret_annotated = global->type != TYPE_INFER;

	if (sym->is_extern && global->desc.hir == NULL && !ret_annotated) {
		err_with_pos(sym->loc, "extern global with no initialiser must have a type annotation");
	} else if (global->desc.hir) {
		type_t ret = cexpr(&global->desc, global->type, global->desc.hir, BM_RVALUE);

		if (global->type == TYPE_INFER) {
			global->type = ret;
		} else {
			ctype_unify(global->type, global->desc.hir);
		}
	}
}

void hir_type_import(sym_t *sym, imas_t *imas) {
	// import assertions always reside at the top of the file, so they're
	// always type checked first before everything else in the file

	// ensure that every `main: (): i32` has the right type

	for (u32 i = 0, c = arrlenu(imas->entries); i < c; i++) {
		imas_entry_t *entry = &imas->entries[i];

		// this symbol shouldn't a typesymbol
		sym_t *target = &symbols[entry->symbol];

		type_t type;

		// TODO: should probably go back to storing the type in the symbol
		switch (target->kind) {
			case SYMBOL_PROC: {
				type = target->d_proc.type;
				break;
			}
			case SYMBOL_GLOBAL: {
				type = target->d_global.type;
				break;
			}
			default: {
				assert_not_reached();
			}
		}

		// perform unification so below works out fine

		// import_main {
		//     main: (): ()
		// }
		//
		// main(): ! = loop ()

		// the above will pass, but in runtime code it may raise unreachable code errors
		// since we're asserting the type through unification, they aren't actually the same

		if (ctype_unify_innards(entry->type, type) == TYPE_INFER) {
			print_err_with_pos(target->loc, "symbol `%s` has wrong type", sv_from(target->key));
			print_err_with_pos(entry->symbol_loc, "import assertion expected `%s`", type_dbg_str(entry->type));
			err_unwind();
		}

		// hack, but so what?
		// it all checks out
		switch (target->kind) {
			case SYMBOL_PROC: {
				target->d_proc.type = entry->type;
				target->d_proc.ret_type = type_get(entry->type)->d_fn.ret;
				break;
			}
			case SYMBOL_GLOBAL: {
				target->d_global.type = entry->type;
				break;
			}
			default: {
				assert_not_reached();
			}
		}
	}
}
