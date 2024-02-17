#include "all.h"
#include "check.h"

cctx_t c;

void hir_type_proc(sym_t *sym, proc_t *proc) {
	c = (cctx_t){};

	cblk_t *blk = &c.blocks[0]; // we saved space for this in parser.c

	*blk = (cblk_t){
		.upvalue = proc->ret_type,
		.brk_type = TYPE_INFER,
	};

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
			if (blk->brk_type == TYPE_INFER) {
				proc->ret_type = expr_type;
			} else {
				(void)ctype_unify(blk->brk_type, proc->desc.hir);
				proc->ret_type = blk->brk_type;
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
