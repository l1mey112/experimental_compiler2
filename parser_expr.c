#include "all.h"
#include "hir.h"
#include "parser.h"
#include <errno.h>

enum : u8 {
	PREC_UNKNOWN, // default
	PREC_ASSIGN,  // = += -= *= /= %=
	PREC_CMP,     // && || (TODO: needs parens)
	PREC_EQ,      // == != < > <= >=
	//PREC_BOR,     // |
	PREC_XOR,     // ^
	//PREC_BAND,    // &
	PREC_ADD,     // + -
	PREC_MUL,     // * / %
	PREC_CALL,    // a b       (determined elsewhere)
	PREC_CAST,    // :
	PREC_PREFIX,  // - * ! &
	PREC_POSTFIX, // ++ --
	PREC_DOT,     // .x
	PREC_INDEX,   // x[]
};

u8 ptok_prec(tok_t kind) {
	// PREC_PREFIX is determined elsewhere

	switch (kind) {
		case TOK_OSQ:
			return PREC_INDEX;
		case TOK_DOT:
			return PREC_DOT;
		case TOK_INC:
		case TOK_DEC:
			return PREC_POSTFIX;
		case TOK_COLON:
			return PREC_CAST;
		case TOK_MUL:
		case TOK_DIV:
		case TOK_MOD:
			return PREC_MUL;
		case TOK_ADD:
		case TOK_SUB:
			return PREC_ADD;
		/* case TOK_BAND:
			return PREC_BAND; */
		/* case TOK_XOR:
			return PREC_XOR; */
		/* case TOK_BOR:
			return PREC_BOR; */
		case TOK_EQ:
		case TOK_NE:
		case TOK_LT:
		case TOK_GT:
		case TOK_GE:
		case TOK_LE:
			return PREC_EQ;
		case TOK_AND:
		case TOK_OR:
			return PREC_CMP;
		case TOK_ASSIGN:
		case TOK_ASSIGN_ADD:
		case TOK_ASSIGN_SUB:
		case TOK_ASSIGN_MUL:
		case TOK_ASSIGN_DIV:
		case TOK_ASSIGN_MOD:
			return PREC_ASSIGN;
		default:
			return PREC_UNKNOWN;
	}
}

// solve syntax ambiguities by looking at whitespace
// 1. f [1, 2, 3] -> parsed as a function call, not index
// 2. f -20       -> parsed as a function call with a negative integer literal, not a subtraction
u8 ptok_prec_ambiguities(void) {
	if (!pprev_next_to()) {
		switch (p.token.kind) {
			case TOK_OSQ: {
				// f [1, 2, 3]
				return 0;
			}
			default: {
				break;
			}
		}
	}

	if (ppeek_next_to()) {
		switch (p.token.kind) {
			case TOK_SUB: {
				// f -20
				return 0;
			}
			default: {
				break;
			}
		}
	}
	
	return ptok_prec(p.token.kind);
}

// load ident ptr, set lvalue
// check for struct{}
hir_expr_t pident_wstruc(ir_desc_t *desc) {
	pcheck(TOK_IDENT);

	// used on struct decl
	rsym_t symbol;

	istr_t lit = p.token.lit;
	loc_t loc = p.token.loc;

	int id;
	if ((id = pimport_ident(lit)) != -1) {
		pnext();
		// TODO: duplicated node inside ptype()
		if (p.token.kind != TOK_DOT) {
			print_err_with_pos(p.token.loc, "expected `.` after import name `%s`", sv_from(p.is[id].name));
			print_hint_with_pos(loc, "import name `%s` used here", sv_from(p.is[id].name));
			err_unwind();
		}
		pnext();
		pcheck(TOK_IDENT);
		istr_t lit = p.token.lit;
		pnext();

		symbol = table_resolve(p.is[id].mod, lit, loc);

		// is struct decl
		if (p.token.kind == TOK_OCBR && pprev_next_to()) {
			goto struc;
		}

		return (hir_expr_t){
			.kind = EXPR_SYM,
			.type = TYPE_INFER,
			.loc = loc,
			.d_sym = symbol,
		};
	}

	pnext();

	if (p.token.kind == TOK_OCBR && pprev_next_to()) {
		symbol = table_resolve(p.mod, lit, loc);
		goto struc;
	}
	
	// 1. for each scope entry, search for the local `lit`

	for (u32 s = p.scope_len; s-- > 0;) {
		u32 lo = p.scope[s];
		u32 hi;
		if (s == p.scope_len - 1) {
			hi = p.scope_entries_len;
		} else {
			hi = p.scope[s + 1];
		}

		for (u32 i = lo; i < hi; i++) {
			pscope_entry_t *entry = &p.scope_entries[i];

			if (entry->is_masked) {
				continue;
			}
			
			if (entry->kind == PS_LOCAL && entry->name == lit) {

				return (hir_expr_t){
					.kind = EXPR_LOCAL,
					.type = TYPE_INFER,
					.loc = loc,
					.d_local = entry->d_local.local,
				};
			}
		}
	}

	// 2. not found, return a symbol
	//    insert a `PS_DEBUG_REFERENCE` to let further references know that
	//    this variable is unresolved

	// used to solve below:
	//
	// let g = x      (error: use before declaration in same scope)
	// let x = 0      (hint: declaration here)
	//
	pscope_register((pscope_entry_t){
		.kind = PS_DEBUG_REFERENCE,
		.loc = loc,
		.name = lit,
	});

	symbol = table_resolve(p.mod, lit, loc);

	return (hir_expr_t){
		.kind = EXPR_SYM,
		.type = TYPE_INFER,
		.loc = loc,
		.d_sym = symbol,
	};
struc:
	// Ident{...}
	//      ^
	pnext();

	// TODO: later, allow: &struct{}
	// TODO: duplicate field init, check for this

	// compile to two different kinds of exprs, later on perform fully qualified kind
	//   T{1, 2}
	//   T{a: 1, b: 2, ...xs}

	type_t type = type_new((tinfo_t){
		.kind = TYPE_SYMBOL,
		.d_symbol = symbol,
	});

	// Ident{} -> EXPR_STRUCT
	// not positional
	// assume all EXPR_STRUCT_POSITIONAL has > 1 expr

	// designated initialisers
	if (p.peek.kind == TOK_COLON || p.token.kind == TOK_CCBR) {
		// T{a: 1, b: 1}
		//   ^

		hir_sf_t *sfs = NULL;

			
		while (p.token.kind != TOK_CCBR) {
			istr_t field = p.token.lit;
			loc_t field_loc = p.token.loc;
			pnext();
			pexpect(TOK_COLON);
			// x: ...
			//    ^^^

			hir_expr_t expr = pexpr(desc, 0);
	
			// ensure no duplicate fields
			for (u32 i = 0; i < arrlenu(sfs); i++) {
				if (sfs[i].field == field) {
					print_err_with_pos(field_loc, "duplicate field `%s`", sv_from(field));
					print_hint_with_pos(sfs[i].field_loc, "previous field `%s`", sv_from(sfs[i].field));
					err_unwind();
				}
			}

			hir_sf_t sf = {
				.field = field,
				.field_loc = field_loc,
				.expr = hir_dup(expr),
			};

			arrpush(sfs, sf);

			if (p.token.kind == TOK_COMMA) {
				pnext();
			} else if (p.token.kind != TOK_CCBR) {
				punexpected("expected `,` or `}`");
			}
		}
		pnext();

		return (hir_expr_t){
			.kind = EXPR_STRUCT,
			.loc = loc, // TODO: should be on `expr` not `{`
			.type = type, // EXPR_STRUCT* must have type
			.d_struct = {
				.fields = sfs,
			},
		};
	} else {
		// T{1, 2}
		//   ^

		hir_expr_t *exprs = NULL;

		while (p.token.kind != TOK_CCBR) {
			hir_expr_t expr = pexpr(desc, 0);
			arrpush(exprs, expr);

			if (p.token.kind == TOK_COMMA) {
				pnext();
			} else if (p.token.kind != TOK_CCBR) {
				punexpected("expected `,` or `}`");
			}
		}
		pnext();

		return (hir_expr_t){
			.kind = EXPR_STRUCT_POSITIONAL,
			.loc = loc,
			.type = type,
			.d_struct_positional = {
				.exprs = exprs,
			},
		};
	}
}

// let without an initialiser won't generate an expr
bool plet(ir_desc_t *desc, hir_expr_t *expr_out) {
	loc_t oloc = p.token.loc;
	pnext();

	// let x = 0
	// let y

	u32 scope_olen = p.scope_entries_len;
	pattern_t pattern = ppattern(desc);
	u32 scope_rlen = p.scope_entries_len;

	// let v: i32 = ...
	if (pattern.kind == PATTERN_LOCAL && p.token.kind == TOK_COLON) {
		pnext();
		loc_t type_loc = p.token.loc;
		type_t type = ptype();
		desc->locals[pattern.d_local].type = type;
		desc->locals[pattern.d_local].type_loc = type_loc;
	}

	// legal syntax
	//
	//     let ident: i32 = ...
	//     let ident
	//     let pattern = ...
	//
	// these below get special consideration, there
	// is no pattern to unwrap basically.
	//
	//     let _ = ...
	//     let ident = ...
	//

	// it isn't the checkers job to assert exhaustiveness, the pattern
	// match is compiled down into a single match pattern which further
	// passes will check for exhaustiveness.
	//
	//     let v: []i32 = ...
	//
	//     if (v.len == 2) {
	//         let [a, b] = v      (fully exhaustive after analysis)
	//     }
	//

	// no let rec here
	pmask_scope(scope_olen, scope_rlen, true);

	bool set = false;

	if (p.token.kind == TOK_ASSIGN) {
		pnext();

		hir_expr_t expr = pexpr(desc, 0);
		
		set = true;
		*expr_out = (hir_expr_t){
			.kind = EXPR_LET,
			.type = TYPE_INFER,
			.loc = oloc,
			.d_let = {
				.pattern = pattern,
				.expr = hir_dup(expr),
			},
		};
	} else if (pattern.kind != PATTERN_LOCAL) {
		// pattern must have an initialiser
		punexpected("expected `=` after pattern");
	}

	// unmask
	pmask_scope(scope_olen, scope_rlen, false);

	return set;
}

// returns true if a value was set
// regardless the block field may or may not be written to
bool pstmt(ir_desc_t *desc, hir_expr_t *expr_out) {
	bool set = false;

	switch (p.token.kind) {
		case TOK_LET: {
			set = plet(desc, expr_out);
			break;
		}
		case TOK_IDENT: {
			if (p.peek.kind == TOK_COLON) {
				assert_not_reached();
				/* pproc(expr, s, previous_exprs);
				set = true;
				break; */
			}
			// fall through
		}
		// case TOK_LET
		default: {
			*expr_out = pexpr(desc, 0);
			set = true;
			break;
		}
	}

	return set;
}

// naive indentation rules, but works for now
// do
//     ...
//     ...
hir_expr_t *pdo_block_impl(ir_desc_t *desc) {
	loc_t oloc = p.token.loc;
	u32 oline_nr = p.line_token.line_nr;
	pnext();
	//
	// TODO: handle the case where the next line token is dedented less than the actual do
	//       would need some lexer stuff (store the start of the line in parser)
	if (p.token.kind == TOK_EOF || p.line_token.line_nr == oline_nr) {
		err_with_pos(oloc, "expected newline after `do`");
	}

	hir_expr_t *exprs = NULL;

	u32 bcol = p.line_token.col;
	ppush_scope();
	while (p.token.kind != TOK_EOF) {
		u32 cln = p.line_token.line_nr;

		hir_expr_t expr;
		bool set = pstmt(desc, &expr);
		if (set) {
			arrpush(exprs, expr);
		}

		if (cln != p.line_token.line_nr && p.line_token.col < bcol) {
			break;
		}
	}
	ppop_scope();

	return exprs;
}

hir_expr_t *pbrace_block_impl(ir_desc_t *desc) {
	pnext();
	
	hir_expr_t *exprs = NULL;

	ppush_scope();
	while (p.token.kind != TOK_CCBR) {
		hir_expr_t expr;
		bool set = pstmt(desc, &expr);
		if (set) {
			arrpush(exprs, expr);
		}
	}
	ppop_scope();
	pnext();

	return exprs;
}

// do exprs and {}
hir_expr_t pdo(ir_desc_t *desc, istr_t opt_label, loc_t opt_loc) {
	loc_t oloc = p.token.loc;
	// TODO: implement `:do` syntax

	pblk_t *blk = &p.blks[p.blks_len++];

	*blk = (pblk_t){
		.label = opt_label,
		.loc = opt_loc,
		.always_brk = false,
	};

	hir_expr_t *exprs;

	if (p.token.kind == TOK_DO) {
		exprs = pdo_block_impl(desc);
	} else if (p.token.kind == TOK_OCBR) {
		exprs = pbrace_block_impl(desc);
	} else {
		assert_not_reached();
	}
	p.blks_len--;

	if (exprs == NULL) {
		return (hir_expr_t){
			.kind = EXPR_TUPLE_UNIT,
			.loc = oloc,
			.type = TYPE_UNIT,
		};
	}

	hir_expr_t block = {
		.kind = EXPR_DO_BLOCK,
		.loc = oloc,
		.type = TYPE_INFER,
		.d_do_block = {
			.exprs = exprs,
			.forward = blk->forward,
			.backward = blk->backward,
		},
	};

	return block;
}

// loop expr
hir_expr_t ploop(ir_desc_t *desc, istr_t opt_label, loc_t opt_loc) {
	loc_t oloc = p.token.loc;
	pnext();

	pblk_t *blk = &p.blks[p.blks_len++];
	*blk = (pblk_t){
		.label = opt_label,
		.loc = opt_loc,
		.always_brk = true,
	};

	hir_expr_t *expr = hir_dup(pexpr(desc, 0));
	p.blks_len--;

	hir_expr_t loop = {
		.kind = EXPR_LOOP,
		.loc = oloc,
		.type = TYPE_INFER,
		.d_loop = {
			.expr = expr,
			.forward = blk->forward,
			.backward = blk->backward,
		},
	};

	return loop;
}

// fallable
static bool pexpr_fallable_unit(ir_desc_t *desc, hir_expr_t *out_expr) {
	struct {
		istr_t name;
		loc_t name_loc;
	} label;

	label.name = ISTR_NONE;

	hir_expr_t expr;

	retry: {}
	token_t token = p.token;
	switch (token.kind) {
		case TOK_VOID: {
			pnext();
			// void expr
			//      ^^^^
			expr = (hir_expr_t){
				.kind = EXPR_VOIDING,
				.loc = token.loc,
				.type = TYPE_UNIT,
				.d_voiding = hir_dup(pexpr(desc, 0)),
			};
			break;
		}
		case TOK_TRUE:
		case TOK_FALSE: {
			pnext();
			expr = (hir_expr_t){
				.kind = EXPR_BOOL_LIT,
				.type = TYPE_BOOL,
				.loc = token.loc,
				.d_bool_lit = token.kind == TOK_TRUE,
			};
			break;
		}
		// TODO: labels
		case TOK_IF: {
			// if (...) ... else ...
			pnext();
			pexpect(TOK_OPAR);
			hir_expr_t *cond = hir_dup(pexpr(desc, 0));
			pexpect(TOK_CPAR);

			// if (...) ... else ...
			//          ^^^

			hir_expr_t *then = hir_dup(pexpr(desc, 0));

			// if (...) ... else ...
			//              ^^^^
			//            optional

			hir_expr_t *els = NULL;
			type_t type = TYPE_INFER;
			if (p.token.kind == TOK_ELSE) {
				pnext();
				els = hir_dup(pexpr(desc, 0));
			} else {
				// if (t) effect
				type = TYPE_UNIT;
				els = NULL;
			}

			expr = (hir_expr_t){
				.kind = EXPR_IF,
				.type = type,
				.loc = token.loc,
				.d_if = {
					.cond = cond,
					.then = then,
					.els = els,
				},
			};
			break;
		}
		// possible syntax ambiguity with `|` operator
		// case TOK_PIPE:
		case TOK_COLON: {
			// label
			pnext();
			pcheck(TOK_IDENT);
			label.name = p.token.lit;
			label.name_loc = p.token.loc;
			pnext();
			switch (p.token.kind) {
				case TOK_DO: {
					break;
				}
				case TOK_LOOP: {
					break;
				}
				default: {
					err_with_pos(p.token.loc, "expected `do` or `loop` after label");
				}
			}
			goto retry;
		}
		case TOK_OCBR:
		case TOK_DO: {
			expr = pdo(desc, label.name, label.name_loc);
			label.name = ISTR_NONE;
			break;
		}
		case TOK_LOOP: {
			expr = ploop(desc, label.name, label.name_loc);
			label.name = ISTR_NONE;
			break;
		}
		case TOK_BREAK: {
			istr_t label = ISTR_NONE;
			loc_t onerror = token.loc;
			pnext();
			// parse label
			if (p.token.kind == TOK_COLON) {
				pnext();
				pcheck(TOK_IDENT);
				label = p.token.lit;
				onerror = p.token.loc;
				pnext();
			}
			u32 branch_level = pblk_locate_label(label, onerror);
			p.blks[p.blks_len - branch_level - 1].forward = true; // brk

			expr = (hir_expr_t){
				.kind = EXPR_BREAK,
				.loc = token.loc,
				.type = TYPE_BOTTOM,
				.d_break = {
					.branch_level = branch_level,
					.expr = hir_dup(pexpr(desc, 0)),
				},
			};
			break;
		}
		case TOK_CONTINUE: {
			istr_t label = ISTR_NONE;
			loc_t onerror = token.loc;
			pnext();
			// parse label
			if (p.token.kind == TOK_COLON) {
				pnext();
				pcheck(TOK_IDENT);
				label = p.token.lit;
				onerror = p.token.loc;
				pnext();
			}
			u32 branch_level = pblk_locate_label(label, onerror);
			p.blks[p.blks_len - branch_level - 1].backward = true; // rep

			expr = (hir_expr_t){
				.kind = EXPR_CONTINUE,
				.loc = token.loc,
				.type = TYPE_BOTTOM,
				.d_continue = {
					.branch_level = branch_level,
				},
			};
			break;
		}
		case TOK_RETURN: {
			if (!p.inside_fn) {
				err_with_pos(p.token.loc, "not inside function");
			}
			pnext();
			expr = (hir_expr_t){
				.kind = EXPR_RETURN,
				.loc = token.loc,
				.type = TYPE_BOTTOM,
				.d_return = {
					.expr = hir_dup(pexpr(desc, 0)),
				},
			};
			break;
		}
		case TOK_IDENT: {
			expr = pident_wstruc(desc);
			break;
		}
		case TOK_OPAR: {
			pnext();
			if (p.token.kind == TOK_CPAR) {
				expr = (hir_expr_t){
					.kind = EXPR_TUPLE_UNIT,
					.loc = token.loc,
					.type = TYPE_UNIT,
				};
				pnext();
				break;
			}
			bool first = true;
			hir_expr_t *elems = NULL;
			while (p.token.kind != TOK_CPAR) {
				if (first) {
					expr = pexpr(desc, 0);
				} else {
					if (elems == NULL) {
						arrpush(elems, expr);
					}
					expr = pexpr(desc, 0);
					arrpush(elems, expr);
				}

				if (p.token.kind == TOK_COMMA) {
					pnext();
				} else if (p.token.kind != TOK_CPAR) {
					punexpected("expected `,` or `)`");
				}

				first = false;
			}
			pnext();
			if (elems != NULL) {
				expr = (hir_expr_t){
					.kind = EXPR_TUPLE,
					.loc = token.loc,
					.type = TYPE_INFER,
					.d_tuple = elems,
				};
			}
			break;
		}
		case TOK_OSQ: {
			hir_expr_t *exprs = NULL;

			// [ 1, 2, 3, 4, 5 ]
			// ^

			pnext();
			while (p.token.kind != TOK_CSQ) {
				hir_expr_t expr = pexpr(desc, 0);
				arrpush(exprs, expr);				

				if (p.token.kind == TOK_COMMA) {
					pnext();
				} else if (p.token.kind != TOK_CSQ) {
					punexpected("expected `,` or `]`");
				}
			}

			// [ 1, 2, 3, 4, 5 ]
			//                 ^
			pnext();

			expr = (hir_expr_t){
				.kind = EXPR_ARRAY,
				.loc = token.loc,
				.type = TYPE_INFER,
				.d_array = exprs,
			};
			break;
		}
		case TOK_INTEGER: {
			u64 lit = pparse_int(token);

			expr = (hir_expr_t){
				.kind = EXPR_INTEGER_LIT,
				.loc = token.loc,
				.type = TYPE_INFER,
				.d_integer = lit,
			};
			pnext();
			break;
		}
		default: {
			if (TOK_IS_PREFIX(token.kind)) {
				pnext();
				switch (token.kind) {
					case TOK_NOT:
					case TOK_SUB: {
						u8 k;
						switch (token.kind) {
							case TOK_NOT: k = EXPR_K_NOT; break;
							case TOK_SUB: k = EXPR_K_SUB; break;
							default: {
								assert_not_reached();
							}
						}

						hir_expr_t rhs = pexpr(desc, PREC_PREFIX);

						expr = (hir_expr_t){
							.kind = EXPR_PREFIX,
							.loc = token.loc,
							.type = TYPE_INFER,
							.d_prefix.expr = hir_dup(rhs),
							.d_prefix.op = k,
						};
						break;
					}
					case TOK_SINGLE_AND: {
						bool is_mut = false;
						if (p.token.kind == TOK_TACK) {
							// &'v = mut ref
							is_mut = true;
							pnext();
						}

						hir_expr_t rhs = pexpr(desc, PREC_PREFIX);

						expr = (hir_expr_t){
							.kind = EXPR_ADDR_OF,
							.loc = token.loc,
							.type = TYPE_INFER,
							.d_addr_of.ref = hir_dup(rhs),
							.d_addr_of.is_mut = is_mut,
						};
						break;
					}
					default: {
						assert_not_reached();
					}
				}
				break;
			} else {
				// failed to parse
				return false;
			}
		}
	}

	*out_expr = expr;
	return true;
}

static bool pexpr_fallable(ir_desc_t *desc, u8 prec, hir_expr_t *out_expr) {
	u32 line_nr = p.line_token.line_nr;

	hir_expr_t expr;

	if (!pexpr_fallable_unit(desc, &expr)) {
		return false;
	}
	
	while (true) {
		if (p.token.kind == TOK_EOF) {
			break;
		}

		u8 nprec = ptok_prec_ambiguities();
		nprec = nprec == 0 ? PREC_CALL : nprec;

		if (prec >= nprec || p.line_token.line_nr != line_nr) {
			goto exit;
		}

		token_t token = p.token;
		switch (token.kind) {
			case TOK_INC:
			case TOK_DEC: {
				pnext();

				u8 k;
				switch (token.kind) {
					case TOK_INC: k = EXPR_K_INC; break;
					case TOK_DEC: k = EXPR_K_DEC; break;
					default: {
						assert_not_reached();
					}
				}
				
				expr = (hir_expr_t){
					.kind = EXPR_POSTFIX,
					.loc = token.loc,
					.type = TYPE_INFER,
					.d_postfix.expr = hir_dup(expr),
					.d_postfix.op = k,
				};
				continue;
			}
			case TOK_OSQ: {
				// x[0..1] and x[0]
				pnext();

				hir_expr_t *expr0;
				if (p.token.kind == TOK_DOUBLE_DOTS) {
					// x[..1]
					expr0 = NULL;
					pnext();
				} else {
					// x[0]
					expr0 = hir_dup(pexpr(desc, 0));
					if (p.token.kind == TOK_CSQ) {
						pnext();
						expr = (hir_expr_t){
							.kind = EXPR_INDEX,
							.loc = token.loc,
							.type = TYPE_INFER,
							.d_index = {
								.expr = hir_dup(expr),
								.index = expr0,
							},
						};
						continue;
					}
					pexpect(TOK_DOUBLE_DOTS);
				}
				//
				// x[?..]
				// x[?..expr]
				//
				if (p.token.kind == TOK_CSQ) {
					pnext();
					expr = (hir_expr_t){
						.kind = EXPR_SLICE,
						.loc = token.loc,
						.type = TYPE_INFER,
						.d_slice = {
							.expr = hir_dup(expr),
							.lo = expr0,
							.hi = NULL,
						},
					};
				} else {
					hir_expr_t expr1 = pexpr(desc, 0);
					pexpect(TOK_CSQ);

					expr = (hir_expr_t){
						.kind = EXPR_SLICE,
						.loc = token.loc,
						.type = TYPE_INFER,
						.d_slice = {
							.expr = hir_dup(expr),
							.lo = expr0,
							.hi = hir_dup(expr1),
						},
					};
				}
				continue;
			}
			default: {
				if (TOK_IS_INFIX(token.kind)) {
					pnext();

					// cast
					if (token.kind == TOK_COLON) {
						loc_t type_loc = p.token.loc;
						type_t type = ptype();
						expr = (hir_expr_t){
							.kind = EXPR_CAST,
							.type = type,
							.loc = token.loc,
							.d_cast = {
								.expr = hir_dup(expr),
								.type_loc = type_loc,
							},
						};
						continue;
					}

					// postfix deref: v.*
					if (token.kind == TOK_DOT && p.token.kind == TOK_MUL) {
						loc_t oloc = token.loc;
						pnext();
						expr = (hir_expr_t){
							.kind = EXPR_DEREF,
							.type = TYPE_INFER,
							.loc = oloc,
							.d_deref = hir_dup(expr),
						};
						continue;
					}

					// field access: v.x
					// tuple access: v.0
					if (token.kind == TOK_DOT) {
						switch (p.token.kind) {
							case TOK_INTEGER: {
								u64 index = strtoull(sv_from(p.token.lit), NULL, 10);
								// TODO: less than u16
								assert(index <= 0xFFFF);

								expr = (hir_expr_t){
									.kind = EXPR_FIELD,
									.loc = p.token.loc,
									.type = TYPE_INFER,
									.d_field = {
										.expr = hir_dup(expr),
										.field_idx = index,
										.field = ISTR_NONE,
									},
								};
								pnext();
								break;
							}
							case TOK_IDENT: {
								istr_t field = p.token.lit;

								expr = (hir_expr_t){
									.kind = EXPR_FIELD,
									.loc = p.token.loc,
									.type = TYPE_INFER,
									.d_field = {
										.expr = hir_dup(expr),
										.field_idx = (u16)-1,
										.field = field,
									},
								};
								pnext();
								break;
							}
							default: {
								punexpected("expected field name or tuple index after `.`");
							}
						}
						continue;
					}

					tok_t kind = token.kind;

					hir_expr_t rhs = pexpr(desc, ptok_prec(kind));

					hir_expr_t *p_expr = hir_dup(expr);
					hir_expr_t *p_rhs = hir_dup(rhs);
			
					bool is_assign_op = kind == TOK_ASSIGN || kind == TOK_ASSIGN_ADD || kind == TOK_ASSIGN_SUB || kind == TOK_ASSIGN_MUL || kind == TOK_ASSIGN_DIV || kind == TOK_ASSIGN_MOD;

					if (is_assign_op) {
						expr = (hir_expr_t){
							.kind = EXPR_ASSIGN,
							.loc = token.loc,
							.type = TYPE_INFER,
							.d_assign.lhs = p_expr,
							.d_assign.rhs = p_rhs,
							.d_assign.kind = kind,
						};
					} else {
						expr = (hir_expr_t){
							.kind = EXPR_INFIX,
							.loc = token.loc,
							.type = TYPE_INFER,
							.d_infix.lhs = p_expr,
							.d_infix.rhs = p_rhs,
							.d_infix.kind = token.kind,
						};
					}
					continue;
				} else {
					hir_expr_t *exprs = NULL;
					
					hir_expr_t situ_expr;
					while (p.line_token.line_nr == line_nr) {
						// don't allow `main()` in expressions
						//                 ^^ next to eachother!

						if (p.token.kind == TOK_OPAR && pprev_next_to()) {
							// a decent enough error message to discourage the user from doing this
							err_with_pos(p.token.loc, "must contain whitespace between argument and function");
						}

						// struct init ambiguity
						// don't allow `2{}` in expressions
						// 			    ^^ next to eachother!

						if (p.token.kind == TOK_OCBR && pprev_next_to()) {
							// a decent enough error message to discourage the user from doing this
							err_with_pos(p.token.loc, "must contain whitespace to avoid ambiguity with struct initialisation");
						}

						if (!pexpr_fallable(desc, PREC_CALL, &situ_expr)) {
							break;
						}

						arrpush(exprs, situ_expr);
					}

					if (exprs == NULL) {
						goto exit;
					}

					expr = (hir_expr_t){
						.kind = EXPR_CALL,
						.loc = token.loc,
						.type = TYPE_INFER,
						.d_call.f = hir_dup(expr),
						.d_call.args = exprs,
					};
					continue;
				}
			}
		}
	}
exit:

	*out_expr = expr;
	return true;
}

// (                           a b                          )
// ^>    cfg = PEXPR_ET_PAREN  ^^^> cfg = PEXPR_ET_PAREN   >^   break
hir_expr_t pexpr(ir_desc_t *desc, u8 prec) {
	hir_expr_t expr;

	if (!pexpr_fallable(desc, prec, &expr)) {
		punexpected("expected expression");
	}

	return expr;
}
