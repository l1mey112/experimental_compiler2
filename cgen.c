#include "all.h"
#include "hir.h"
#include "pass.h"
#include <assert.h>
#include <stdio.h>

#define gprintf(...) fprintf(f, __VA_ARGS__)
static u32 _tabs = 0;

void gtabs(FILE *f) {
	for (u32 i = 0; i < _tabs; i++) {
		gprintf("\t");
	}
}

#define gstmt(...) gtabs(f), gprintf(__VA_ARGS__)

void gtype(FILE *f, type_t type) {
	gprintf("i32");
}

void gmangle_local(FILE *f, ir_desc_t *desc, rlocal_t local) {
	istr_t name = desc->locals[local].name;
	
	if (name != ISTR_NONE) {
		gprintf("%s_%u", sv_from(name), local);
	} else {
		gprintf("_%u", local);
	}
}

void gmangle(FILE *f, istr_t name) {
	const char *s = sv_from(name);
	size_t len = strlen(s);

	for (size_t i = 0; i < len; i++) {
		if (s[i] == '.') {
			gprintf("$");
		} else {
			gprintf("%c", s[i]);
		}
	}
}

void gproc_def(FILE *f, sym_t *sym, proc_t *proc) {
	tinfo_t *typeinfo = type_get(proc->type);

	gtype(f, proc->ret_type);
	gprintf(" ");
	gmangle(f, sym->key);
	gprintf("(");
	for (u32 i = 0, c = proc->arguments; i < c; i++) {
		if (i > 0) {
			gprintf(", ");
		}
		gtype(f, proc->desc.locals[i].type);
		gprintf(" ");
		gmangle_local(f, &proc->desc, i);
	}
	gprintf(")");
}

void ghir_expr(FILE *f, ir_desc_t *desc, hir_expr_t *expr) {
	switch (expr->kind) {
		case EXPR_INTEGER_LIT: {
			gprintf("%s", sv_from(expr->d_integer_lit));
			break;
		}
		case EXPR_LOCAL: {
			gmangle_local(f, desc, expr->d_local);
			break;
		}
		case EXPR_SYM: {
			gmangle(f, symbols[expr->d_sym].key);
			break;
		}
		case EXPR_INFIX: {
			printf("(");
			ghir_expr(f, desc, expr->d_infix.lhs);
			gprintf(" %s ", tok_op_str(expr->d_infix.kind));
			ghir_expr(f, desc, expr->d_infix.rhs);
			printf(")");
			break;
		}
		case EXPR_CALL: {
			ghir_expr(f, desc, expr->d_call.f);
			gprintf("(");
			for (u32 i = 0, c = arrlenu(expr->d_call.args); i < c; i++) {
				if (i > 0) {
					gprintf(", ");
				}
				ghir_expr(f, desc, &expr->d_call.args[i]);
			}
			gprintf(")");
			break;
		}
		case EXPR_PREFIX: {
			gprintf("%s", expr->d_prefix.op == EXPR_K_NOT ? "!" : "-");
			ghir_expr(f, desc, expr->d_prefix.expr);
			break;
		}
		default: {
			printf("\n\nkind: %u\n\n", expr->kind);
			assert_not_reached();
		}
	}
}

void gstmts(FILE *f, ir_desc_t *desc, hir_expr_t *stmts) {

	for (u32 i = 0, c = arrlenu(stmts); i < c; i++) {
		hir_expr_t *expr = &stmts[i];

		gtabs(f);

		switch (expr->kind) {
			case EXPR_RETURN: {
				// TODO: () void usage
				gprintf("return ");
				ghir_expr(f, desc, expr->d_return.expr);
				gprintf(";\n");
				break;
			}
			case EXPR_ASSIGN: {
				ghir_expr(f, desc, expr->d_assign.lhs);
				gprintf(" = ");
				ghir_expr(f, desc, expr->d_assign.rhs);
				gprintf(";\n");
				break;
			}
			case EXPR_IF: {
				gprintf("if (");
				ghir_expr(f, desc, expr->d_if.cond);
				assert(expr->d_if.then->kind == EXPR_DO_BLOCK);
				assert(expr->d_if.els->kind == EXPR_DO_BLOCK);
				gprintf(") {\n");
				_tabs++;
				gstmts(f, desc, expr->d_if.then->d_do_block.exprs);
				_tabs--;
				gstmt("} else {\n");
				_tabs++;
				gstmts(f, desc, expr->d_if.els->d_do_block.exprs);
				_tabs--;
				gstmt("}\n");
				break;
			}
			default: {
				ghir_expr(f, desc, expr);
				gprintf(";\n");
			}
		}
	}
}

void gproc(FILE *f, sym_t *sym, proc_t *proc) {
	hir_expr_t body = proc->desc.hir;
	assert(body.kind == EXPR_DO_BLOCK);
	gproc_def(stderr, sym, &sym->d_proc);
	gprintf(" {\n");
	_tabs++;
	for (u32 i = proc->arguments, c = arrlenu(proc->desc.locals); i < c; i++) {
		gstmt("i32 ");
		gmangle_local(f, &proc->desc, i);
		gprintf(";\n");
	}
	gstmts(f, &proc->desc, body.d_do_block.exprs);
	_tabs--;
	gprintf("}\n");
}

// assumed normalised HIR
void pass_debug_cgen(rsym_t *po) {
	for (u32 i = 0, c = arrlenu(po); i < c; i++) {
		rsym_t rsym = po[i];
		sym_t *sym = &symbols[rsym];

		if (sym->kind == SYMBOL_PROC) {
			gproc_def(stderr, sym, &sym->d_proc);
			fprintf(stderr, ";\n");  
		}
	}
	
	for (u32 i = 0, c = arrlenu(po); i < c; i++) {
		rsym_t rsym = po[i];
		sym_t *sym = &symbols[rsym];

		printf("cgen: %s\n", sv_from(sym->key));

		switch (sym->kind) {
			case SYMBOL_PROC: {
				gproc(stderr, sym, &sym->d_proc);
				break;
			}
			case SYMBOL_GLOBAL: {
				assert_not_reached();
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
