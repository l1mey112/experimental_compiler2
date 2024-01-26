#include "parser.h"
#include "all.h"

pctx_t p;

// valid transformation:
//
// add: i32 -> i32 -> i32
// add: a b = a + b
// add: 0 0 = 0
//
// function main.add
// entry(v0: i32, v1: i32):
//     v10 = (v0, v1)
//     goto_pattern v10, add.0, add.1       | - the goto_pattern abstraction, unwrapped by the checker
// add.0(v2: i32, v3: i32):                 | - the parser inserts each block with params
//     v4 = local a                         | - the parser inserts these stores and assigns
//     v5 = local b                         |   these locals into the scope to be referenced
//     store v4, v2                         |   later on
//     store v5, v3                         |
//     v6 = load v4
//     v7 = load v5
//     v8 = v6 + v7
//     goto ret(v8)
// add.1:                                   | - this has no params, because it doesn't match any variables
//     v9 = 0
//     goto ret(v9)
// ret(v9: i32):                            | - all blocks end up here, the return block
//     ret v9
//
void pfn(lir_proc_t *parent) {
	assert(parent == NULL);

	pcheck(TOK_IDENT);
	istr_t name = p.token.lit;
	loc_t name_loc = p.token.loc;

	if (parent != NULL) {
		// reference to var, insert into scope/vars
	}

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

	(void)pscope_register((pscope_entry_t){
		.kind = PS_SYMBOL,
		.loc = name_loc,
		.name = name,
		.d_symbol = {
			.qualified_name = qualified_name,
		},
	});

	lir_proc_t proc = {};

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
	// create entry block with `len` args
	// entry block must be the first block
	// proper desugar into match arms
	lir_rblock_t entry = lir_block_new(&proc, "entry");
	lir_rblock_t exit = lir_block_new(&proc, "exit");
	proc.blocks[entry].check.next_sequence = exit;

	// TODO: we don't really store the locs of args???
	// INFO: when DCEing locals or merging, take the loc of the one that actually has one

	lir_rlocal_t *args = NULL;
	for (u32 i = 0; i < proc_len; i++) {
		lir_rlocal_t v = lir_local_new(&proc, (lir_local_t){
			.kind = LOCAL_SSA,
			.type = proc_typeinfo->d_fn.args[i],
			.is_debuginfo = true,
			.d_debuginfo = {
				.loc = name_loc,
			},
		});
		lir_block_arg_assign(&proc, entry, v);
		arrpush(args, v);
	}

	// construct a tuple so it can unpack
	// v3 = (v0, v1)

	lir_rlocal_t tuple = lir_ssa_tmp_inst(&proc, entry, proc_typeinfo->d_fn.ret, name_loc, (lir_inst_t){
		.kind = INST_TUPLE,
		.d_tuple = args,
	});

	// use the tuple to construct a goto_pattern after we're done with all the patterns
	lir_block_term(&proc, entry, (lir_term_t){
		.kind = TERM_GOTO_PATTERN,
		.d_goto_pattern = {
			.value = tuple,
			.blocks = NULL,
			.patterns = NULL,
		},
	});

	// ret match (v0, v1)
	//     ...
	lir_rlocal_t exit_arg = lir_block_new_arg(&proc, exit, TYPE_INFER, name_loc);
	lir_block_term(&proc, exit, (lir_term_t){
		.kind = TERM_RET,
		.d_ret = exit_arg,
	});

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
		lir_term_pat_t *patterns = NULL;

		char *debug_name;
		asprintf(&debug_name, "pattern%u", pat);
		lir_rblock_t block = lir_block_new(&proc, debug_name);

		while (true) {
			u32 locals_olen = arrlenu(proc.locals);
			lir_term_pat_t pattern = ppattern(&proc);
			u32 locals_rlen = arrlenu(proc.locals);

			// insert args
			pblock_args_to_vars(&proc, block, locals_olen, locals_rlen);

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

		lir_term_pat_t pattern = {
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
		rexpr_t expr = pexpr(&proc, block, 0, 0);
		ppop_scope();
		ppop_scope();

		lir_block_term(&proc, expr.block, (lir_term_t){
			.kind = TERM_GOTO,
			.d_goto = {
				.block = exit,
				.args = arr(lir_rlocal_t, lir_lvalue_spill(&proc, expr.block, expr.value)),
			},
		});
		pat++;

		arrpush(proc.blocks[entry].term.d_goto_pattern.patterns, pattern);
		arrpush(proc.blocks[entry].term.d_goto_pattern.blocks, block);
	}

	table_register((lir_sym_t){
		.key = qualified_name,
		.mod = p.mod,
		.short_name = name,
		.loc = name_loc,
		.type = proc_type,
		.kind = SYMBOL_PROC,
		.d_proc = proc,
	});
}

// v = 20
// 'v = 30
// fn: ...
// fn: ...
// fn: ...
//
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
				pfn(NULL);
				break;
			} else if (p.peek.kind == TOK_ASSIGN) {
				// v = 20
				assert_not_reached();
			}
			// fallthrough
		}
		default: {
			punexpected("expected top-level statement");
			/* hir_node_t expr;
			bool set = pstmt(&expr, NULL, p.modp->exprs);
			if (set) {
				arrpush(p.modp->exprs, expr);
			}
			if (is_pub && set) {
				pmake_pub(&arrlast(p.modp->exprs));
			} else if (is_pub) {
				err_with_pos(oloc, "cannot make this expression public");
			}
			break; */
		}
	}
}

void lir_process_file(rfile_t file) {
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
