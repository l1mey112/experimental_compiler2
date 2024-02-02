#include "check.h"
#include "all.h"

cctx_t c;

void cproc(proc_t *proc) {
	c = (cctx_t){};

	cblk_t *blk = &c.blocks[c.blocks_len++];

	*blk = (cblk_t){
		.upvalue = proc->ret_type,
		.brk_type = TYPE_INFER,
	};

	// we'll need this regardless
	type_t *args_types = NULL;

	for (u32 i = 0; i < proc->arguments; i++) {
		local_t *arg = &proc->desc.locals[i];
		arrpush(args_types, arg->type);
	}

	bool ret_annotated = proc->ret_type != TYPE_INFER;

	// a proc doesn't contain it's `type` filled in yet, only argument locals
	// and possibly a seperate return type annotation. you'll need to create
	// and assign it here.
	//
	// mutually recursive or just recursive procs without a type annotation
	// aren't allowed at all, so this guards against that completely.

	// for mutually recursive or self calls
	if (ret_annotated) {
		proc->type = type_new((tinfo_t){
			.kind = TYPE_FUNCTION,
			.d_fn = {
				.args = args_types,
				.ret = proc->ret_type,
			},
		});
	}

	type_t expr_type = cexpr(&proc->desc, proc->ret_type, &proc->desc.hir);

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

	type_t ret = cexpr(&global->desc, global->type, &global->desc.hir);

	printf("type %s\n", type_dbg_str(ret));
	assert_not_reached();
	// cdesc(&proc->desc);
	// assert_not_reached();	
}

void ccheck_all_symbols(rsym_t *po) {
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

void compiler_check(void) {
	rsym_t *po = creorder_po_and_sanity();
	ccheck_all_symbols(po);
}