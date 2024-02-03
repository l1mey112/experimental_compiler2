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

// TODO: in pident and ( after it, make that illegal
//       can't define a function inside an expression

// c() = 0
void pfn2(void) {
	pcheck(TOK_IDENT);
	istr_t name = p.token.lit;
	loc_t name_loc = p.token.loc;

	pnext();
	pexpect(TOK_OPAR);

	// c(...) = 0
	//   ^^^
	
	proc_t proc = {};

	// TODO: creating a function type that is incomplete, will possibly cause incorrect interning
	//       create the type afterwards and fill in later from args
	//
	//       after setting return value, perform type_renew() or type_reintern()

	type_t *arg_types = NULL;

	ppush_scope();
	while (p.token.kind != TOK_CPAR) {
		type_t type = pfn_arg(&proc.desc);
		arrpush(arg_types, type);

		if (p.token.kind == TOK_COMMA) {
			pnext();
		} else if (p.token.kind != TOK_CPAR) {
			punexpected("expected `,` or `)`");
		}

		proc.arguments++;
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

	istr_t qualified_name = fs_module_symbol_sv(p.mod, name);
	// construct type late

	// add(a: i32, b: i32) = a + b
	//                     ^

	pexpect(TOK_ASSIGN);

	p.blks[p.blks_len++] = (pblk_t){
		.kind = BLK_FN,
		.loc = name_loc,
	};

	ppush_scope();
	hir_expr_t expr = pexpr(&proc.desc, 0);
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

	proc.desc.hir = expr;
	proc.type = type;
	proc.ret_type = ret_type;
	proc.ret_type_loc = ret_type_loc;

	table_register((sym_t){
		.key = qualified_name,
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

	tsymbol_sf_t *fields = NULL;

	while (p.token.kind != TOK_CCBR) {
		istr_t field = p.token.lit;
		loc_t field_loc = p.token.loc;
		
		// a: i32
		// ^
		
		pnext();
		pexpect(TOK_COLON);
		loc_t type_loc = p.token.loc;
		type_t type = ptype();

		tsymbol_sf_t sf = {
			.field = field,
			.type = type,
			.field_loc = field_loc,
			.type_loc = type_loc,
		};

		arrpush(fields, sf);
	}
	// }
	// ^
	pnext();

	tsymbol_t typeinfo = {
		.kind = TYPESYMBOL_STRUCT,
		.d_struct = {
			.fields = fields,
		},
	};

	istr_t qualified_name = fs_module_symbol_sv(p.mod, name);

	table_register((sym_t){
		.key = qualified_name,
		.mod = p.mod,
		.short_name = name,
		.loc = name_loc,
		.kind = SYMBOL_TYPE,
		.d_type = typeinfo,
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
				// fn
				pfn2();
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
