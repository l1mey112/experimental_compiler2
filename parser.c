#include "all.h"
#include "parser.h"

pctx_t p;

type_t pfn_arg(ir_desc_t *desc) {
	bool is_mut = false;
	
	pcheck(TOK_IDENT);
	istr_t name = p.token.lit;
	loc_t name_loc = p.token.loc;

	// TODO: possibly insert error if not `f(`, error on `f (`
	
	pnext();
	// (v': i32)
	if (p.token.kind == TOK_TACK) {
		is_mut = true;
		pnext();
		// fallthrough
	}

	pexpect(TOK_COLON);
	type_t arg_type = ptype();

	rlocal_t local = ir_local_new(desc, (local_t){
		.kind = is_mut ? LOCAL_MUT : LOCAL_IMM,
		.type = arg_type,
		.loc = name_loc,
		.name = name,
	});

	(void)pscope_register((pscope_entry_t){
		.name = name,
		.loc = name_loc,
		.kind = PS_LOCAL,
		.d_local = local,
	});

	return arg_type;
}

void pfn_shared(proc_t *proc) {
	pexpect(TOK_OPAR);

	// c(...) = 0
	//   ^^^

	type_t *arg_types = NULL;

	rlocal_t args = 0;

	ppush_scope();
	while (p.token.kind != TOK_CPAR) {
		type_t type = pfn_arg(&proc->desc);
		arrpush(arg_types, type);

		if (p.token.kind == TOK_COMMA) {
			pnext();
		} else if (p.token.kind != TOK_CPAR) {
			punexpected("expected `,` or `)`");
		}

		// push rlocal_t
		arrpush(proc->arguments, (rlocal_t)args);
		args++;
	}
	pnext();

	// ) -> ...
	//   ^^

	type_t ret_type = TYPE_INFER;
	loc_t ret_type_loc = {};
	if (p.token.kind == TOK_COLON) {
		pnext();
		ret_type_loc = p.token.loc;
		ret_type = ptype();
	}

	proc->desc.next_blk_id++; // save pos for later
	p.blks[p.blks_len++] = (pblk_t){
		.kind = BLK_FN,
	};

	// construct type late

	// add(a: i32, b: i32) = a + b
	//                     ^

	pexpect(TOK_ASSIGN);

	ppush_scope();
	hir_expr_t expr = pexpr(&proc->desc, 0);
	ppop_scope();
	ppop_scope();
	p.blks_len--;

	// create function type if return is annotated
	type_t type = TYPE_INFER;
	if (ret_type != TYPE_INFER) {
		type = type_new((tinfo_t){
			.kind = TYPE_FUNCTION,
			.d_fn = {
				.args = arg_types,
				.ret = ret_type,
			},
		});
	}

	proc->desc.hir = expr;
	proc->type = type;
	proc->ret_type = ret_type;
	proc->ret_type_loc = ret_type_loc;
}

// TODO: in pident_wstruc and ( after it, make that illegal
//       can't define a function inside an expression

// c() = 0
void pfn(void) {
	pcheck(TOK_IDENT);
	istr_t name = p.token.lit;
	loc_t name_loc = p.token.loc;

	pnext();
	pcheck(TOK_OPAR);

	proc_t proc = {};
	pfn_shared(&proc);

	istr_t qualified_name = fs_module_symbol_sv(p.mod, name);

	table_register((sym_t){
		.key = qualified_name,
		.mod = p.mod,
		.short_name = name,
		.loc = name_loc,
		.kind = SYMBOL_PROC,
		.d_proc = proc,
	});
}

// TODO: inside hir reorder sanity passes, check that the struct
//       doesn't have any conflicting methods as fields
// TODO: allow i32.function()
void pmethod(void) {
	pcheck(TOK_IDENT);
	
	istr_t sym = p.token.lit;
	loc_t sym_loc = p.token.loc;

	// Foo.function() = ...

	pnext();
	pexpect(TOK_DOT);

	pcheck(TOK_IDENT);
	istr_t name = p.token.lit;
	loc_t name_loc = p.token.loc;

	pnext();
	pcheck(TOK_OPAR);

	proc_t proc = {};
	pfn_shared(&proc);

	// main.Foo
	istr_t qualified_name = fs_module_symbol_sv(p.mod, sym);

	// main.Foo:function
	istr_t selector = fs_module_symbol_selector(qualified_name, name);

	table_register((sym_t){
		.key = selector,
		.mod = p.mod,
		.short_name = name,
		.loc = name_loc,
		.kind = SYMBOL_PROC,
		.d_proc = proc,
	});
}

void ptop_global(void) {
	pcheck(TOK_IDENT);
	istr_t name = p.token.lit;
	loc_t name_loc = p.token.loc;

	istr_t qualified_name = fs_module_symbol_sv(p.mod, name);

	bool is_mut = false;

	// v': T =
	// ^

	pnext();
	if (p.token.kind == TOK_TACK) {
		is_mut = true;
		pnext();
	}

	// v: T =
	//  ^

	type_t type = TYPE_INFER;
	loc_t type_loc = {};

	if (p.token.kind == TOK_COLON) {
		pnext();
		type_loc = p.token.loc;
		type = ptype();
	}

	// v = 
	//   ^

	pexpect(TOK_ASSIGN);

	ir_desc_t desc = {};
	desc.hir = pexpr(&desc, 0);

	table_register((sym_t){
		.key = qualified_name,
		.mod = p.mod,
		.short_name = name,
		.loc = name_loc,
		.kind = SYMBOL_GLOBAL,
		.d_global = {
			.desc = desc,
			.type = type,
			.is_mut = is_mut,
			.type_loc = type_loc,
		},
	});
}

void pstruct(void) {
	pnext();
	pcheck(TOK_IDENT);
	istr_t name = p.token.lit;
	loc_t name_loc = p.token.loc;

	// struct Foo {
	//        ^^^

	pnext();
	pexpect(TOK_OCBR);
	
	// TODO: unexpected `...`, expected `{` to start struct body
	//                                      ^^^^^^^^^^^^^^^^^^^^ `to` clause in unexpected

	// TODO: error out on empty struct, tell them to define ZST alias instead

	tinfo_sf_t *fields = NULL;
	typesymbol_debug_sf_t *loc_fields = NULL;

	while (p.token.kind != TOK_CCBR) {
		istr_t field = p.token.lit;
		loc_t field_loc = p.token.loc;
		
		// a: i32
		// ^
		
		pnext();
		pexpect(TOK_COLON);
		loc_t type_loc = p.token.loc;
		type_t type = ptype();

		tinfo_sf_t sf = {
			.field = field,
			.type = type,
		};

		typesymbol_debug_sf_t loc_sf = {
			.field_loc = field_loc,
			.type_loc = type_loc,
		};

		arrpush(fields, sf);
		arrpush(loc_fields, loc_sf);
	}
	// }
	// ^
	pnext();

	tinfo_t struc = {
		.kind = TYPE_STRUCT,
		.d_struct = {
			.fields = fields,
		},
	};

	typesymbol_debug_t struc_loc = {
		.kind = TYPESYMBOL_STRUCT,
		.d_struct = loc_fields,
	};

	istr_t qualified_name = fs_module_symbol_sv(p.mod, name);

	table_register((sym_t){
		.key = qualified_name,
		.mod = p.mod,
		.short_name = name,
		.loc = name_loc,
		.kind = SYMBOL_TYPE,
		.d_type = {
			.type = type_new(struc),
			.debug = struc_loc,
		},
	});
}

// semantically these are "newtype"
void palias() {
	pnext();
	pcheck(TOK_IDENT);
	istr_t name = p.token.lit;
	loc_t name_loc = p.token.loc;

	// type Foo = i32
	//      ^^^

	pnext();

	// TODO: can omit `=` for a ZST

	pexpect(TOK_ASSIGN);
	loc_t type_loc = p.token.loc;
	type_t type = ptype();

	istr_t qualified_name = fs_module_symbol_sv(p.mod, name);

	typesymbol_debug_t alias_loc = {
		.kind = TYPESYMBOL_ALIAS,
		.d_alias = type_loc,
	};

	table_register((sym_t){
		.key = qualified_name,
		.mod = p.mod,
		.short_name = name,
		.loc = name_loc,
		.kind = SYMBOL_TYPE,
		.d_type = {
			.type = type,
			.debug = alias_loc,
		},
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
			// TODO: `pub pub pub pub` works...
			is_pub = true;
			pnext();
			goto retry;
		}
		case TOK_IDENT: {
			if (p.peek.kind == TOK_OPAR) {
				pfn();
				break;
			}

			if (p.peek.kind == TOK_DOT) {
				pmethod();
				break;
			}

			ptop_global();
			break;	
		}
		// todo make this just a normal decl statement and not restricted to toplevel
		case TOK_STRUCT: {
			pstruct();
			break;
		}
		case TOK_TYPE: {
			palias();
			break;
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
