#include "all.h"

// walk
void dexpr(mod_t *modp, hir_scope_t *s, hir_node_t node) {
	assert_not_reached(); // unimplemented
}

void dexprs(mod_t *modp, hir_scope_t *s, hir_node_t *node) {
	for (u32 i = 0, c = arrlenu(node); i < c; i++) {
		dexpr(modp, s, node[i]);
	}
}

// || and && -> ternaries
// x[]       -> ptr arithmetic (checked)
// ptr + 1   -> ptr arithmetic
// x[..]     -> ptr arithmetic (checked)
// 
void hir_typed_desugar(void) {
	for (rmod_t i = 0; i < fs_mod_arena_len; i++) {
		mod_t *mod = MOD_PTR(i);
		dexprs(mod, NULL, mod->exprs);
	}
}

// ---------------------------------------

void _hir_dump_var_w_type(hir_rvar_t var) {
	hir_var_t *varp = VAR_PTR(var);
	
	const char *mut_str = varp->is_mut ? "'" : "";
	printf("%s%s.%u", mut_str, sv_from(VAR_PTR(var)->name), var);

	// remove noise on debug output before checker
	if (varp->type != TYPE_INFER) {
		printf(": %s", type_dbg_str(varp->type));
	}
}

void _hir_dump_var(hir_rvar_t var) {
	printf("%s.%u", sv_from(VAR_PTR(var)->name), var);
}

void _hir_dump_pattern(mod_t *modp,hir_scope_t *s, hir_pattern_t pattern) {
	switch (pattern.kind) {
		case PATTERN_UNDERSCORE: {
			printf("_");
			break;
		}
		case PATTERN_INTEGER_LIT: {
			printf("%s", sv_from(pattern.d_integer_lit));
			break;
		}
		case PATTERN_VAR: {
			_hir_dump_var_w_type(pattern.d_var);
			break;
		}
		case PATTERN_TUPLE: {
			printf("(");
			for (u32 i = 0; i < arrlen(pattern.d_tuple.elems); i++) {
				_hir_dump_pattern(modp, s, pattern.d_tuple.elems[i]);
				if (i != arrlen(pattern.d_tuple.elems) - 1) {
					printf(", ");
				}
			}
			printf(")");
			break;
		}
		case PATTERN_TUPLE_UNIT: {
			printf("()");
			break;
		}
		case PATTERN_ARRAY: {
			printf("[");
			if (pattern.d_array.match && pattern.d_array.match_lhs) {
				_hir_dump_pattern(modp, s, *pattern.d_array.match);
				printf("..., ");
			}
			for (u32 i = 0; i < arrlen(pattern.d_array.elems); i++) {
				_hir_dump_pattern(modp, s, pattern.d_array.elems[i]);
				if (i != arrlen(pattern.d_array.elems) - 1) {
					printf(", ");
				}
			}
			if (pattern.d_array.match && !pattern.d_array.match_lhs) {
				printf(", ...");
				_hir_dump_pattern(modp, s, *pattern.d_array.match);
			}
			printf("]");
			break;
		}
		default: {
			assert_not_reached();
		}
	}
}

void _hir_dump_scope(mod_t *modp, hir_scope_t *s) {
	for (int i = 0, c = arrlen(s->locals); i < c; i++) {
		hir_rvar_t var = s->locals[i];
		hir_var_t *varp = VAR_PTR(var);
		printf("  %s :: %s\n", sv_from(varp->name), type_dbg_str(varp->type));
	}
}

static u32 _hir_tabcnt = 0;
void _hir_dump_stmt(mod_t *modp, hir_scope_t *s, hir_node_t node);

void _hir_tabs(void) {
	for (u32 i = 0; i < _hir_tabcnt; i++) {
		printf("  ");
	}
}

#define TAB_PRINTF(...) \
	_hir_tabs(); \
	printf(__VA_ARGS__);

void _hir_dump_expr(mod_t *modp, hir_scope_t *s, hir_node_t node) {
	// printf("[%s]:", type_dbg_str(node.type));

	switch (node.kind) {
		case NODE_VAR: {
			_hir_dump_var(node.d_var);
			break;
		}
		case NODE_GLOBAL_UNRESOLVED: {
			printf("%s*", sv_from(node.d_global_unresolved));
			break;
		}
		case NODE_INTEGER_LIT: {
			if (node.d_integer_lit.negate) {
				printf("-%s", sv_from(node.d_integer_lit.lit));
			} else {
				printf("%s", sv_from(node.d_integer_lit.lit));
			}
			break;
		}
		case NODE_BOOL_LIT: {
			printf("%s", node.d_bool_lit ? "true" : "false");
			break;
		}
		case NODE_POSTFIX: {
			_hir_dump_expr(modp, s, *node.d_postfix.expr);
			printf("%s", node.d_postfix.kind == TOK_INC ? "++" : "--");
			break;
		}
		case NODE_ASSIGN: {
			_hir_dump_expr(modp, s, *node.d_assign.lhs);
			printf(" = ");
			_hir_dump_expr(modp, s, *node.d_assign.rhs);
			break;
		}
		case NODE_INFIX: {
			printf("(%s ", tok_op_str(node.d_infix.kind));
			_hir_dump_expr(modp, s, *node.d_infix.lhs);
			printf(" ");
			_hir_dump_expr(modp, s, *node.d_infix.rhs);
			printf(")");
			break;
		}
		case NODE_PREFIX: {
			printf("%s", tok_op_str(node.d_prefix.kind));
			_hir_dump_expr(modp, s, *node.d_prefix.expr);
			break;
		}
		case NODE_CALL: {
			printf("(");
			_hir_dump_expr(modp, s, *node.d_call.f);
			printf(" ");
			_hir_dump_expr(modp, s, *node.d_call.arg);
			printf(")");
			break;
		}
		case NODE_LOOP: {
			printf(":%u loop ", node.d_loop.blk_id);
			_hir_dump_expr(modp, s, *node.d_loop.expr);
			break;
		}
		case NODE_DO_BLOCK: {
			printf(":%u do\n", node.d_do_block.blk_id);
			_hir_tabcnt++;
			for (int i = 0, c = arrlen(node.d_do_block.exprs); i < c; i++) {
				_hir_dump_stmt(modp, node.d_do_block.scope, node.d_do_block.exprs[i]);
			}
			_hir_tabcnt--;
			break;
		}
		case NODE_TUPLE_UNIT: {
			printf("()");
			break;
		}
		case NODE_TUPLE: {
			printf("(");
			for (int i = 0, c = arrlen(node.d_tuple.elems); i < c; i++) {
				_hir_dump_expr(modp, s, node.d_tuple.elems[i]);
				if (i != c - 1) {
					printf(", ");
				}
			}
			printf(")");
			break;
		}
		case NODE_BREAK_UNIT: {
			printf("brk :%u ()", node.d_break.blk_id);
			break;
		}
		case NODE_BREAK_INFERRED:
		case NODE_BREAK: {
			printf("brk :%u ", node.d_break.blk_id);
			_hir_dump_expr(modp, s, *node.d_break.expr);
			break;
		}
		case NODE_CAST: {
			printf("cast[%s](", type_dbg_str(node.type));
			_hir_dump_expr(modp, s, *node.d_cast);
			printf(")");
			break;
		}
		case NODE_SYM_UNRESOLVED: {
			printf("%s*", fs_module_symbol_str(node.d_sym_unresolved.mod, node.d_sym_unresolved.name));
			break;
		}
		case NODE_SYM: {
			hir_var_t *var = VAR_PTR(node.d_sym.var);
			printf("%s", fs_module_symbol_str(node.d_sym.mod, var->name));
			break;
		}
		case NODE_IF: {
			printf("if (");
			_hir_dump_expr(modp, s, *node.d_if.cond);
			printf(") ");
			_hir_dump_expr(modp, s, *node.d_if.then);
			printf(" else ");
			if (node.d_if.els == NULL) {
				printf("()");
			} else {
				_hir_dump_expr(modp, s, *node.d_if.els);
			}
			break;
		}
		case NODE_UNDEFINED: {
			printf("undefined");
			break;
		}
		case NODE_ARRAY_LIT: {
			printf("[");
			for (int i = 0, c = arrlen(node.d_array_lit.elems); i < c; i++) {
				_hir_dump_expr(modp, s, node.d_array_lit.elems[i]);
				if (i != c - 1) {
					printf(", ");
				}
			}
			printf("]:%s", type_dbg_str(node.type));
			break;
		}
		case NODE_LAMBDA: {
			// \x y z -> x + y + z

			printf("\\");
			for (u32 i = 0, c = arrlenu(node.d_lambda.args); i < c; i++) {
				_hir_dump_var_w_type(node.d_lambda.args[i]);
				if (i != c - 1) {
					printf(" ");
				}
			}

			printf(" -> ");

			_hir_dump_expr(modp, node.d_lambda.scope, *node.d_lambda.expr);
			break;
		}
		case NODE_MATCH: {
			// match expr
			//    (x, y) -> ...

			printf("match ");
			_hir_dump_expr(modp, s, *node.d_match.expr);
			printf("\n");

			_hir_tabcnt++;
			for (u32 i = 0, c = arrlenu(node.d_match.exprs); i < c; i++) {
				_hir_tabs();
				_hir_dump_pattern(modp, &node.d_match.scopes[i], node.d_match.patterns[i]);
				printf(" -> ");
				_hir_dump_expr(modp, &node.d_match.scopes[i], node.d_match.exprs[i]);
				printf("\n");
			}
			_hir_tabcnt--;
			break;
		}
		case NODE_VOIDING: {
			printf("void ");
			_hir_dump_expr(modp, s, *node.d_voiding);
			break;
		}
		case NODE_DEREF: {
			printf("(");
			_hir_dump_expr(modp, s, *node.d_deref);
			printf(").*");
			break;
		}
		case NODE_ADDR_OF: {
			printf("&");
			if (node.d_addr_of.is_mut) {
				printf("'");
			}
			_hir_dump_expr(modp, s, *node.d_addr_of.ref);
			break;
		}
		case NODE_SIZEOF_TYPE: {
			printf("sizeof[%s]", type_dbg_str(node.d_sizeof_type));
			break;
		}
		case NODE_INDEX: {
			_hir_dump_expr(modp, s, *node.d_index.expr);
			printf("[");
			_hir_dump_expr(modp, s, *node.d_index.index);
			printf("]");
			break;
		}
		case NODE_SLICE: {
			_hir_dump_expr(modp, s, *node.d_slice.expr);
			printf("[");
			if (node.d_slice.lo) {
				_hir_dump_expr(modp, s, *node.d_slice.lo);
			}
			printf("..");
			if (node.d_slice.hi) {
				_hir_dump_expr(modp, s, *node.d_slice.hi);
			}
			printf("]");
			break;
		}
		default: {
			printf("\nunknown expr kind %d\n", node.kind);
			print_hint_with_pos(node.loc, "LOC HERE");
			assert_not_reached();
		}
	}
}

void _hir_dump_stmt(mod_t *modp, hir_scope_t *s, hir_node_t node) {
	switch (node.kind) {
		case NODE_LET_DECL: {
			TAB_PRINTF("let ");
			_hir_dump_pattern(modp, s, node.d_let_decl.pattern);
			printf(" = ");
			_hir_dump_expr(modp, s, *node.d_let_decl.expr);
			printf("\n");
			break;
		}
		default: {
			_hir_tabs();
			_hir_dump_expr(modp, s, node);
			printf("\n");
			break;
		}
	}
}

void hir_dump_module(rmod_t mod) {
	void *_ = alloc_scratch(0);

	mod_t *modp = MOD_PTR(mod);
	printf("module %s\n\n", fs_module_symbol_str(mod, ISTR_NONE));
	for (u32 i = 0, c = arrlenu(modp->exprs); i < c; i++) {
		_hir_dump_stmt(modp, NULL, modp->exprs[i]);
	}

	alloc_reset(_);
}
