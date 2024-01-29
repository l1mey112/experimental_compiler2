#include "all.h"
#include "hir.h"
#include "parser.h"

pctx_t p;

void pfn(void) {
	//assert(parent == NULL);

	pcheck(TOK_IDENT);
	istr_t name = p.token.lit;
	loc_t name_loc = p.token.loc;

	/* if (parent != NULL) {
		// reference to var, insert into scope/vars
	} */

	pnext();
	pexpect(TOK_COLON);

	loc_t type_loc = p.token.loc;
	type_t proc_type = ptype();
	if (type_kind(proc_type) != TYPE_FUNCTION) {
		err_with_pos(type_loc, "type mismatch: expected function type, got `%s`", type_dbg_str(proc_type));
	}
	tinfo_t *proc_typeinfo = type_get(proc_type);

	istr_t qualified_name = fs_module_symbol_sv(p.mod, name);

	u32 proc_len = arrlenu(proc_typeinfo->d_fn.args);

	// could keep? could not? it'll get resolved anyway this is just faster.
	(void)pscope_register((pscope_entry_t){
		.kind = PS_SYMBOL,
		.loc = name_loc,
		.name = name,
		.d_symbol = {
			.qualified_name = qualified_name,
		},
	});

	proc_t proc = {};
	proc.arguments = proc_len; // arg length

	// add: i32 -> i32 -> i32
	// add: a b = a + b
	// add: 0 0 = 0
	//
	// desugar:
	//
	// add: i32 -> i32 -> i32
	// add: _0 _1 = match (_0, _1)
	//     a b -> a + b
	//     0 0 -> 0
	//
	// proper desugar into match arms

	hir_expr_t *args = NULL;
	for (u32 i = 0; i < proc_len; i++) {
		rlocal_t v = proc_local_new(&proc, (local_t){
			.kind = LOCAL_IMM,
			.type = proc_typeinfo->d_fn.args[i],
			.name = ISTR_NONE,
			.loc = LOC_NONE,
		});
		hir_expr_t expr = {
			.kind = EXPR_LOCAL,
			.d_local = v,
			.type = TYPE_INFER,
			.loc = LOC_NONE,
		};
		arrpush(args, expr);
	}

	// construct a tuple so it can unpack

	hir_expr_t tuple = {
		.kind = VALUE_TUPLE,
		.type = TYPE_INFER,
		.d_tuple = args,
	};

	hir_expr_t match = {
		.kind = EXPR_MATCH,
		.type = TYPE_INFER,
		.loc = LOC_NONE,
		.d_match = {
			.expr = hir_dup(tuple),
			.patterns = NULL,
			.exprs = NULL,
		},
	};

	// start parsing

	if (!(p.token.kind == TOK_IDENT && p.token.lit == name)) {
		err_with_pos(name_loc, "expected function `%s` implementation after type", sv_from(name));
	}

	u32 pat = 0;
	while (p.token.kind == TOK_IDENT && p.token.lit == name) {
		pnext();
		pexpect(TOK_COLON);

		// add: ...
		//      ^^^ (patterns)

		ppush_scope();
		pattern_t *patterns = NULL;

		while (true) {
			u32 locals_olen = arrlenu(proc.locals);
			pattern_t pattern = ppattern(&proc);
			u32 locals_rlen = arrlenu(proc.locals);

			arrpush(patterns, pattern);
			if (p.token.kind == TOK_ASSIGN) {
				break;
			}
		}

		u32 plen = arrlenu(patterns);
		if (proc_len == 0) {
			proc_len = plen;
		} else if (plen != proc_len) {
			err_with_pos(name_loc, "function pattern matching with different number of arguments", sv_from(name));
		}

		pattern_t pattern = {
			.kind = PATTERN_TUPLE,
			.loc = patterns[0].loc,
			.d_tuple = {
				.elems = patterns,
			},
		};

		// add: ... =
		//          ^
		pnext();

		// with pattern, construct a basic block for it

		ppush_scope();
		hir_expr_t expr = pexpr(&proc, 0);
		ppop_scope();
		ppop_scope();

		pat++;

		arrpush(match.d_match.patterns, pattern);
		arrpush(match.d_match.exprs, expr);
	}

	proc.hir = match;

	table_register((sym_t){
		.key = qualified_name,
		.mod = p.mod,
		.short_name = name,
		.loc = name_loc,
		.type = proc_type,
		.kind = SYMBOL_PROC,
		.d_proc = proc,
	});
}

// functions, constants, globals, types, attributes, etc.
void ptop_stmt(void) {
	if (p.token.kind != TOK_IMPORT) {
		p.has_done_imports = true;
	}

	loc_t oloc = p.token.loc;
	bool is_pub = false;

	retry: switch (p.token.kind) {
		case TOK_IMPORT: {
			pimport();
			break;
		}
		case TOK_PUB: {
			is_pub = true;
			pnext();
			goto retry;
		}
		case TOK_TACK: {
			assert_not_reached();
		}
		case TOK_IDENT: {
			if (p.peek.kind == TOK_COLON) {
				// fn
				pfn();
				break;
			} else if (p.peek.kind == TOK_ASSIGN) {
				// v = 20
				assert_not_reached();
			}
			// fallthrough
		}
		default: {
			punexpected("expected top-level statement");
		}
	}
}

void compiler_process_file(rfile_t file) {
	file_t *f = FILE_PTR(file);
	
	p = (pctx_t){
		.pstart = f->data,
		.pc = f->data,
		.pend = f->data + f->len,
		.plast_nl = f->data,
		.file = file,
		.mod = f->mod,
		.modp = MOD_PTR(f->mod),
	};

	pnext(); // tok
	pnext(); // tok peek

	while (p.token.kind != TOK_EOF) {
		ptop_stmt();
	}
	// papply_pub();
}
