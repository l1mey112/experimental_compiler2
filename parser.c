#include "all.h"
#include "hir.h"
#include <stdbool.h>
#include "parser.h"

pctx_t p;

type_t pfn_arg(ir_desc_t *desc) {
	bool is_mut = false;
	
	if (p.token.kind == TOK_TACK) {
		is_mut = true;
		pnext();
		// fallthrough
	}

	pcheck(TOK_IDENT);
	istr_t name = p.token.lit;
	loc_t name_loc = p.token.loc;

	// TODO: possibly insert error if not `f(`, error on `f (`
	
	pnext();
	// (v: i32)
	//   ^
	// force type for args
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
	if (p.token.kind == TOK_ARROW) {
		pnext();
		ret_type = ptype();
	}

	istr_t qualified_name = fs_module_symbol_sv(p.mod, name);
	tinfo_t typeinfo = {
		.kind = TYPE_FUNCTION,
		.d_fn = {
			.args = arg_types,
			.ret = ret_type,
		},
	};

	// add(a: i32, b: i32) = a + b
	//                     ^

	pnext();

	ppush_scope();
	hir_expr_t expr = pexpr(&proc.desc, 0);
	ppop_scope();
	ppop_scope();

	proc.desc.hir = expr;

	table_register((sym_t){
		.key = qualified_name,
		.mod = p.mod,
		.short_name = name,
		.loc = name_loc,
		.type = type_new(typeinfo),
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

	if (p.token.kind == TOK_COLON) {
		pnext();
		type = ptype();
	}

	// v = 
	//   ^

	pexpect(TOK_ASSIGN);

	ir_desc_t desc;
	desc.hir = pexpr(&desc, 0);

	table_register((sym_t){
		.key = qualified_name,
		.mod = p.mod,
		.short_name = name,
		.loc = name_loc,
		.type = type,
		.kind = SYMBOL_GLOBAL,
		.d_global = {
			.is_mut = is_mut,
			.desc = desc,
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

	tinfo_sf_t *fields = NULL;

	while (p.token.kind != TOK_CCBR) {
		istr_t field = p.token.lit;
		loc_t field_loc = p.token.loc;
		
		// a: i32
		// ^
		
		pnext();
		pexpect(TOK_COLON);
		type_t field_type = ptype();

		tinfo_sf_t sf = {
			.field = field,
			.type = field_type,
		};

		arrpush(fields, sf);
	}
	// }
	// ^
	pnext();

	tinfo_t typeinfo = {
		.kind = TYPE_STRUCT,
		.d_struct = {
			.fields = fields,
		},
	};

	type_t type = type_new(typeinfo);
	istr_t qualified_name = fs_module_symbol_sv(p.mod, name);

	table_register((sym_t){
		.key = qualified_name,
		.mod = p.mod,
		.short_name = name,
		.loc = name_loc,
		.type = type,
		.kind = SYMBOL_TYPE,
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
