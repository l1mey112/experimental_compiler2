#include "parser.h"
#include "all.h"

pctx_t p;

// valid transformation:
//
// add: i32 -> i32 -> i32
// add: a b = a + b
// add: 0 0 = 0
//
// TODO: the "valid transformation" is in constant flux, ive gone through 5 different LIR iterations
//       fix later.
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

	// could keep? could not? it'll get resolved anyway this is just faster.
	(void)pscope_register((pscope_entry_t){
		.kind = PS_SYMBOL,
		.loc = name_loc,
		.name = name,
		.d_symbol = {
			.qualified_name = qualified_name,
		},
	});

	lir_proc_t proc = {};
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
	// create entry block with `len` args
	// entry block must be the first block
	// proper desugar into match arms
	lir_rblock_t entry = lir_block_new(&proc, "entry");

	// TODO: we don't really store the locs of args???
	// INFO: when DCEing locals or merging, take the loc of the one that actually has one

	lir_value_t *args = NULL;
	for (u32 i = 0; i < proc_len; i++) {
		rlocal_t v = lir_local_new(&proc, (local_t){
			.kind = LOCAL_IMM,
			.type = proc_typeinfo->d_fn.args[i],
			.name = ISTR_NONE,
		});
		lir_value_t nv = {
			.kind = VALUE_LVALUE,
			.type = proc_typeinfo->d_fn.args[i],
			.d_lvalue = {
				.local = v,
			},
		};
		arrpush(args, nv);
	}

	// construct a tuple so it can unpack

	lir_value_t tuple = {
		.kind = VALUE_TUPLE,
		.type = proc_typeinfo->d_fn.ret,
		.d_tuple = args,
	};

	// use the tuple to construct a goto_pattern after we're done with all the patterns
	lir_block_term(&proc, entry, (lir_term_t){
		.kind = TERM_GOTO_PATTERN,
		.d_goto_pattern = {
			.value = tuple,
			.blocks = NULL,
			.patterns = NULL,
		},
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
		pattern_t *patterns = NULL;

		char *debug_name;
		asprintf(&debug_name, "pattern%u", pat);
		lir_rblock_t block = lir_block_new(&proc, debug_name);

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
		rexpr_t expr = pexpr(&proc, block, 0, 0);
		ppop_scope();
		ppop_scope();

		lir_block_term(&proc, expr.block, (lir_term_t){
			.kind = TERM_RET,
			.d_ret = {
				.value = pexpr_eu(&proc, expr),
			},
		});
		pat++;

		arrpush(proc.blocks[entry].term.d_goto_pattern.patterns, pattern);
		arrpush(proc.blocks[entry].term.d_goto_pattern.blocks, block);
	}

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
