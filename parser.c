#include "all.h"
#include "parser.h"

pctx_t p;

typedef struct pattrib_t pattrib_t;

struct pattrib_t {
	bool is_pub;
	bool is_extern;
	istr_t export_symbol;
};

#define POP_ATTRIB(attrib, field) ({ \
		bool field = attrib->field;  \
		attrib->field = false;       \
		field;                       \
	})

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
		.d_local = { local },
	});

	return arg_type;
}

void pfn_shared(proc_t *proc) {
	p.inside_fn = true;
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

	// ) {
	//   ^

	type_t ret_type = TYPE_INFER;
	loc_t ret_type_loc = {};
	hir_expr_t *expr = NULL;

	ppush_scope();
	if (p.token.kind == TOK_OCBR || p.token.kind == TOK_DO) {
		// func() {} -> func() = void do ...
		ret_type = TYPE_UNIT;

		// will jmp to `{}` or `do`
		expr = hir_dup(pexpr(&proc->desc, 0));
		expr = hir_dup((hir_expr_t){
			.kind = EXPR_VOIDING,
			.loc = expr->loc,
			.type = TYPE_UNIT,
			.d_voiding = expr,
		});
	} else {
		// ): ...
		//  ^
		if (p.token.kind == TOK_COLON) {
			pnext();
			ret_type_loc = p.token.loc;
			ret_type = ptype();
		}

		// add(a: i32, b: i32) = a + b
		//                     ^

		if (p.token.kind == TOK_ASSIGN) {
			pnext();
			expr = hir_dup(pexpr(&proc->desc, 0));
		}
	}
	ppop_scope();
	ppop_scope();

	// construct type late
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
	p.inside_fn = false;
}

// TODO: in pident_wstruc and ( after it, make that illegal
//       can't define a function inside an expression

// c() = 0
void pfn(pattrib_t *attrib) {
	pcheck(TOK_IDENT);
	istr_t name = p.token.lit;
	loc_t name_loc = p.token.loc;

	pnext();
	pcheck(TOK_OPAR);

	proc_t proc = {};
	pfn_shared(&proc);

	istr_t qualified_name = fs_module_symbol(p.mod, name);

	istr_t extern_symbol = name;

	if (attrib->is_extern && attrib->export_symbol != ISTR_NONE) {
		extern_symbol = attrib->export_symbol;
	}

	table_register((sym_t){
		.key = qualified_name,
		.mod = p.mod,
		.short_name = name,
		.loc = name_loc,
		.kind = SYMBOL_PROC,
		.d_proc = proc,
		.is_pub = POP_ATTRIB(attrib, is_pub),
		.is_extern = POP_ATTRIB(attrib, is_extern),
		.extern_symbol = extern_symbol,
	});
}

// TODO: inside hir reorder sanity passes, check that the struct
//       doesn't have any conflicting methods as fields
// TODO: allow i32.function()
void pmethod(pattrib_t *attrib) {
	pcheck(TOK_IDENT);
	
	istr_t sym = p.token.lit;

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
	istr_t qualified_name = fs_module_symbol(p.mod, sym);

	// main.Foo:function
	istr_t selector = fs_module_symbol_selector(qualified_name, name);

	// TODO: should probably deny methods being extern??
	istr_t extern_symbol = name;

	if (attrib->is_extern && attrib->export_symbol != ISTR_NONE) {
		extern_symbol = attrib->export_symbol;
	}

	table_register((sym_t){
		.key = selector,
		.mod = p.mod,
		.short_name = name,
		.loc = name_loc,
		.kind = SYMBOL_PROC,
		.d_proc = proc,
		.is_pub = POP_ATTRIB(attrib, is_pub),
		.is_extern = POP_ATTRIB(attrib, is_extern),
		.extern_symbol = extern_symbol,
	});
}

void ptop_global(pattrib_t *attrib) {
	pcheck(TOK_IDENT);
	istr_t name = p.token.lit;
	loc_t name_loc = p.token.loc;

	istr_t qualified_name = fs_module_symbol(p.mod, name);

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

	ir_desc_t desc = {};
	// parser will check this

	if (p.token.kind == TOK_ASSIGN) {
		pnext();
		desc.hir = hir_dup(pexpr(&desc, 0));
	}

	istr_t extern_symbol = name;

	if (attrib->is_extern && attrib->export_symbol != ISTR_NONE) {
		extern_symbol = attrib->export_symbol;
	}

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
		.is_pub = POP_ATTRIB(attrib, is_pub),
		.is_extern = POP_ATTRIB(attrib, is_extern),
		.extern_symbol = extern_symbol,
	});
}

void pstruct(pattrib_t *attrib) {
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

	istr_t qualified_name = fs_module_symbol(p.mod, name);

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
		.is_pub = POP_ATTRIB(attrib, is_pub),
		.extern_symbol = ISTR_NONE,
	});
}

// semantically these are "newtype"
void palias(pattrib_t *attrib) {
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

	istr_t qualified_name = fs_module_symbol(p.mod, name);

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
		.is_pub = POP_ATTRIB(attrib, is_pub),
		.extern_symbol = ISTR_NONE,
	});
}

// functions, constants, globals, types, attributes, etc.
void ptop_stmt(void) {
	if (p.token.kind != TOK_IMPORT && p.token.kind != TOK_IMPORT_MAIN) {
		p.has_done_imports = true;
	}

	loc_t oloc = p.token.loc;

	pattrib_t attrib = {};

	// extern extern  (no duplicates)
	// pub pub        (no duplicates)
	// pub extern     (wrong order)
	// extern pub ... (just right)

	retry: switch (p.token.kind) {
		case TOK_IMPORT: {
			pimport();
			break;
		}
		case TOK_IMPORT_MAIN: {
			pimport_main();
			break;
		}
		case TOK_PUB: {
			if (attrib.is_pub) {
				err_with_pos(p.token.loc, "duplicate `pub`");
			}
			attrib.is_pub = true;
			pnext();
			goto retry;
		}
		case TOK_EXTERN: {
			if (attrib.is_extern) {
				err_with_pos(p.token.loc, "duplicate `extern`");
			}
			attrib.is_extern = true;
			if (attrib.is_pub) {
				err_with_pos(p.token.loc, "wrong order, `extern` should be applied to `pub`");
			}
			pnext();
			attrib.export_symbol = ISTR_NONE;
			if (p.token.kind == TOK_STRING) {
				attrib.export_symbol = p.token.lit;
				pnext();
			}
			goto retry;
		}
		case TOK_IDENT: {
			if (p.peek.kind == TOK_OPAR) {
				pfn(&attrib);
				break;
			}

			if (p.peek.kind == TOK_DOT) {
				pmethod(&attrib);
				break;
			}

			ptop_global(&attrib);
			break;	
		}
		// todo make this just a normal decl statement and not restricted to toplevel
		case TOK_STRUCT: {
			pstruct(&attrib);
			break;
		}
		case TOK_TYPE: {
			palias(&attrib);
			break;
		}
		default: {
			punexpected("expected top-level statement");
		}
	}

	if (attrib.is_pub || attrib.is_extern) {
		err_with_pos(oloc, "attribute unused");
	}
}

void compiler_process_file(fs_rfile_t file) {
	fs_file_t *f = &fs_files_queue[file];
	
	p = (pctx_t){
		.pstart = f->data,
		.pc = f->data,
		.pend = f->data + f->len,
		.plast_nl = f->data,
		.file = file,
		.filep = f,
		.mod = f->mod,
		.modp = &fs_mod_arena[f->mod],
	};

	pnext(); // tok
	pnext(); // tok peek

	while (p.token.kind != TOK_EOF) {
		ptop_stmt();
	}
	// papply_pub();
}
