#include "all.h"
#include "check.h"
#include "pass.h"

cctx_t c;

void cproc(proc_t *proc) {
	c = (cctx_t){};

	cblk_t *blk = &c.blocks[c.blocks_len++];

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
		for (u32 i = 0; i < proc->arguments; i++) {
			local_t *arg = &proc->desc.locals[i];
			arrpush(args_types, arg->type);
		}
	}

	type_t expr_type = cexpr(&proc->desc, proc->ret_type, &proc->desc.hir, BM_RVALUE);

	c.blocks_len--;

	type_t ret_type;

	// unify the types of all `ret` and the resulting expression
	// if the proc is annotated, the `brk_type` will be the exact
	// same as the return type.

	if (ret_annotated) {
		(void)ctype_unify(proc->ret_type, &proc->desc.hir);
		ret_type = proc->ret_type;
	} else {
		if (blk->brk_type == TYPE_INFER) {
			ret_type = expr_type;
		} else {
			(void)ctype_unify(blk->brk_type, &proc->desc.hir);
			ret_type = blk->brk_type;
		}
	}

	if (!ret_annotated) {
		proc->type = type_new((tinfo_t){
			.kind = TYPE_FUNCTION,
			.d_fn = {
				.args = args_types,
				.ret = ret_type,
			},
		});
	}

	proc->ret_type = ret_type;

	printf("type %s\n", type_dbg_str(proc->type));
}

void cglobal(global_t *global) {
	c = (cctx_t){};

	type_t ret = cexpr(&global->desc, global->type, &global->desc.hir, BM_RVALUE);

	if (global->type == TYPE_INFER) {
		global->type = ret;
	} else {
		ctype_unify(global->type, &global->desc.hir);
	}

	printf("type %s\n", type_dbg_str(ret));
}

void pass_check_all(rsym_t *po) {
	for (u32 i = 0, c = arrlenu(po); i < c; i++) {
		rsym_t rsym = po[i];
		sym_t *sym = &symbols[rsym];

		printf("check: %s\n", sv_from(sym->key));

		switch (sym->kind) {
			case SYMBOL_PROC: {
				cproc(&sym->d_proc);
				break;
			}
			case SYMBOL_GLOBAL: {
				cglobal(&sym->d_global);
				break;
			} 
			case SYMBOL_TYPE: {
				break;
			}
			default: {
				assert_not_reached();
			}
		}
	}
}
