#include "all.h"
#include "parser.h"

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

enum : u8 {
	PEXPR_ET_NONE,
	PEXPR_ET_PAREN,
	PEXPR_ET_ARRAY,
	PEXPR_ET_INDEX_LO,
	PEXPR_ET_INDEX_HI,
	PEXPR_ET_ELSE,
};

// load ident ptr, set lvalue
rexpr_t pident(lir_proc_t *proc, lir_rblock_t block) {
	pcheck(TOK_IDENT);

	istr_t lit = p.token.lit;
	loc_t loc = p.token.loc;

	int id;
	if ((id = pimport_ident(lit)) != -1) {
		pnext();
		if (p.token.kind != TOK_DOT) {
			print_err_with_pos(p.token.loc, "expected `.` after import name `%s`", sv_from(p.is[id].name));
			print_hint_with_pos(loc, "import name `%s` used here", sv_from(p.is[id].name));
			err_unwind();
		}
		pnext();
		pcheck(TOK_IDENT);
		istr_t lit = p.token.lit;
		pnext();

		istr_t qualified_name = fs_module_symbol_sv(p.is[id].mod, lit);

		assert(0 && "TODO: implement import symbols");
		/* return (rexpr_t){
			.block = block,
			.value = lir_ssa_tmp_inst(proc, block, TYPE_INFER, &p.token.loc, (lir_inst_t){
				.kind = INST_,
				.d_symbol = {
					.qualified_name = qualified_name,
				},
			}),
		}; */
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
				pnext();

				// create lvalue
				rexpr_t expr = {
					.block = block,
					.is_lvalue = true,
					.lvalue = {
						.local = entry->d_local.local,
						.loc = loc,
					},
				};
				
				return expr;
			}
		}
	}

	// 2. not found, return a symbol
	//    insert a `PS_DEBUG_REFERENCE` to let further references know that
	//    this variable is unresolved

	pnext();
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

	istr_t qualified_name = fs_module_symbol_sv(p.mod, lit);
	rsym_t symbol = table_resolve(qualified_name);

	rexpr_t expr = {
		.block = block,
		.is_lvalue = true,
		.lvalue = {
			.symbol = symbol,
			.loc = loc,
			.is_sym = true,
		},
	};

	// create lvalue
	return expr;
}

bool plet(lir_proc_t *proc, lir_rblock_t block, rexpr_t *expr_out) {
	pnext();

	// let x = 0
	// let y

	u32 scope_olen = p.scope_entries_len;
	u32 locals_olen = arrlenu(proc->locals);
	pattern_t pattern = ppattern(proc);
	u32 scope_rlen = p.scope_entries_len;
	u32 locals_rlen = arrlenu(proc->locals);

	// let v: i32 = ...
	if (pattern.kind == PATTERN_LOCAL && p.token.kind == TOK_COLON) {
		pnext();
		type_t type = ptype();
		proc->locals[pattern.d_local].type = type;
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
	//
	// let (x, y) = expr       (desugared to a goto_pattern with single branch)
	//
	// entry:
	//     ...
	//     goto_pattern expr
	//       (x, y) -> let.next
	//
	// let.next:
	//     ... use x and y
	//

	// no let rec here
	pmask_scope(scope_olen, scope_rlen, true);

	if (p.token.kind == TOK_ASSIGN) {
		pnext();

		rexpr_t expr = pexpr(proc, block, 0, 0);
		lir_value_t value = pexpr_eu(proc, expr);

		switch (pattern.kind) {
			case PATTERN_LOCAL: {
				// create lvalue
				lir_lvalue_t lvalue = {
					.local = pattern.d_local,
					.loc = pattern.loc,
				};
				
				// assignment
				lir_assign(proc, expr.block, lvalue, value);
				expr_out->block = expr.block;
				break;
			}
			case PATTERN_UNDERSCORE: {
				// user discard
				lir_discard(proc, expr.block, value);
				expr_out->block = expr.block;
				break;
			}
			default: {
				lir_rblock_t let_next = lir_block_new(proc, "let.next");

				lir_block_term(proc, expr.block, (lir_term_t){
					.kind = TERM_GOTO_PATTERN,
					.d_goto_pattern = {
						.value = pexpr_eu(proc, expr),
						.patterns = arr(pattern_t, pattern),
						.blocks = arr(lir_rblock_t, let_next),
					},
				});

				expr_out->block = let_next;
				break;
			}
		}
	} else if (pattern.kind != PATTERN_LOCAL) {
		// pattern must have an initialiser
		punexpected("expected `=` after pattern");
	}

	// unmask
	pmask_scope(scope_olen, scope_rlen, false);

	return false;
}

// returns true if a value was set
// regardless the block field may or may not be written to
bool pstmt(lir_proc_t *proc, lir_rblock_t block, rexpr_t *expr_out) {
	bool set = false;

	switch (p.token.kind) {
		case TOK_LET: {
			set = plet(proc, block, expr_out);
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
			*expr_out = pexpr(proc, block, 0, 0);
			set = true;
			break;
		}
	}

	return set;
}

// TODO: add in expr_cfg ??? (ehh i tried it, it doesn't work somehow)
/*

let v = (do
	40
) + 20

*/
// naive indentation rules, but works for now
// do
//     ...
//     ...
rexpr_t pdo(lir_proc_t *proc, lir_rblock_t block, istr_t opt_label, loc_t opt_loc) {
	loc_t oloc = p.token.loc;
	pnext();
	//
	// TODO: handle the case where the next line token is dedented less than the actual do
	//       would need some lexer stuff (store the start of the line in parser)
	if (p.token.kind == TOK_EOF || p.token.loc.line_nr == oloc.line_nr) {
		err_with_pos(oloc, "expected newline after `do`");
	}

	// chain
	rexpr_t expr = {
		.block = block,
	};

	// TODO: note that the `:do` syntax isn't implemented yet
	//
	// let v = :do       (desugar brk into intermediate variable stores)
	//     brk 20
	//     50
	//     60

	// `do` block introduces control flow as a value, we must nest it
	// creating a special (do block) expression that expands into the
	// proper basic block representation when finished by splicing them
	// together.

	// desugar from the parser:
	//
	// entry:
	//     ...
	//     goto do.exit
	// do.exit:
	//     v = ctrl {entry: do.entry, local: _0}
	//

	// do block expansion here:
	//
	// entry:
	//     goto do.entry
	// do.entry:
	//     _0 = 20
	//     goto do.exit
	// do.0:
	//     _ = 50
	//     _0 = 60
	//     goto do.exit
	// do.exit:
	//     v = _0           (<- _0)
	//

	// NOTE: this applies to `loop` and `do` with a label, they're implemented
	//       using the same mechanisms
	// NOTE: a traversal in reverse postorder would be ruined after splicing
	//       you'll need to recompute and start again from there??
	//       lets hope not. keep an updatable stack or something.
	//

	// when reaching a `VALUE_CTRL_TEMP` you must splice the CFG:
	//
	// 1. term(DOM(current)) = goto(begin)
	//    set all terminators of all blocks that dominate the current
	//    block to a goto that points to the beginning of the do block
	// 2. value = local
	//    the current value should be a lvalue copy referencing
	//    the intermediary local that was created to store the
	//    values of the do block at all brks

	bool branchable = false;

	branchable = opt_label != ISTR_NONE;

	lir_rblock_t origin = block;
	lir_rblock_t do_rep = BLOCK_NONE;
	lir_rblock_t do_brk = BLOCK_NONE;
	rlocal_t do_brk_local;

	if (branchable) {
		do_rep = lir_block_new(proc, "do.entry");
		do_brk = lir_block_new(proc, "do.exit");

		// term(origin) = do_rep
		lir_block_term(proc, origin, (lir_term_t){
			.kind = TERM_GOTO,
			.d_goto = do_rep,
		});

		// TODO: would be nice to name the var after the label
		//       but currently have no way to distinguish compiler
		//       locals from program locals
		do_brk_local = lir_local_new(proc, (local_t){
			.kind = LOCAL_IMM,
			.loc = oloc,
			.name = ISTR_NONE,
			.type = TYPE_INFER,
		});

		expr.block = do_rep;

		u8 blk_id = p.blks_len++;
		p.blks[blk_id] = (pblk_t){
			.label = opt_label,
			.loc = opt_loc,
			.always_brk = false,
			.brk = do_brk,
			.rep = do_rep,
			.brk_local = do_brk_local,
		};
	}

	bool first = true;
	bool set = false;

	u32 bcol = p.token.loc.col;
	ppush_scope();
	while (p.token.kind != TOK_EOF) {
		u32 cln = p.token.loc.line_nr;

		rexpr_t new_expr = expr;
		bool expr_set = pstmt(proc, new_expr.block, &new_expr);

		// TODO: disinguish between user just dumping shit
		//       and the user explicitly going `let _ = ...`

		// insert a discard if the last expression is not used
		if (!first && expr_set) {
			lir_discard(proc, expr.block, pexpr_eu(proc, expr));
		}
		if (expr_set) {
			first = false;
		}

		expr = new_expr;
		set |= expr_set;

		if (cln != p.token.loc.line_nr && p.token.loc.col < bcol) {
			break;
		}
	}
	ppop_scope();
	
	if (branchable) {
		p.blks_len--;
	}

	// not EOF, something was present here but didn't return any value
	// return () to either the brk param or just as a bare expression
	if (!set) {
		expr.is_lvalue = false;
		expr.value = (lir_value_t){
			.kind = VALUE_TUPLE_UNIT,
			.type = TYPE_UNIT,
			.loc = oloc,
		};
		
		// even though there was no branches, we still need to use the variable
		if (branchable) {
			// _0 = ()
			lir_assign_local(proc, expr.block, do_brk_local, expr.value);
			expr.value = lir_local_value(do_brk_local, oloc);
		}
	}

	if (set && branchable) {
		// term(origin) = do_brk
		lir_block_term(proc, origin, (lir_term_t){
			.kind = TERM_GOTO,
			.d_goto = do_brk,
		});

		// write to the brk local, the termination will be spliced anyway
		// _0 = expr
		lir_assign_local(proc, expr.block, do_brk_local, pexpr_eu(proc, expr));

		expr.is_lvalue = false;
		expr.value = (lir_value_t){
			.kind = VALUE_CTRL_TEMP,
			.loc = oloc,
			.type = TYPE_INFER,
			.d_ctrl_temp = {
				.entry = do_rep,
				.local = do_brk_local,
			},
		};
		expr.block = do_brk;
	}
	return expr;
}

// loop expr
rexpr_t ploop(lir_proc_t *proc, lir_rblock_t block, u8 expr_cfg, istr_t opt_label, loc_t opt_loc) {
	loc_t oloc = p.token.loc;
	pnext();

	// use the information from `pdo` to understand the control flow value abstraction

	// let v = loop do
	//     1 + 2
	//     brk 20

	// desugar from the parser:
	//
	// entry:
	//     ...
	//     goto loop.exit
	// loop.exit:
	//     v = ctrl {entry: loop.entry, local: _0}

	// loop block expansion here:
	//
	// entry:
	//     goto loop.entry
	// loop.entry:
	//     _ = 1 + 2
	// loop.0:
	//     _ = 50
	//     _0 = 20
	//     goto loop.exit:
	// !.brk:
	//     _ = !
	//     goto loop.entry
	// loop.exit:
	//     v = _0           (<- _0)
	//

	// for expressions inside the loop, the user might go:
	//
	//     loop ()
	//     loop 1
	//     loop 2 + 4
	//
	// if the value is `()`, don't raise a warn on ignoring the value.
	// this is the idomatic way to write an infinite loop, anything else
	// is a warning on pure values.
	//
	// TODO: don't raise for now though. raise later
	//

	lir_rblock_t origin = block;
	lir_rblock_t loop_rep = lir_block_new(proc, "loop.entry");
	lir_rblock_t loop_brk = lir_block_new(proc, "loop.exit");

	rlocal_t loop_brk_local = lir_local_new(proc, (local_t){
		.kind = LOCAL_IMM,
		.loc = oloc,
		.name = ISTR_NONE,
		.type = TYPE_INFER,
	});
	
	u8 blk_id = p.blks_len++;
	p.blks[blk_id] = (pblk_t){
		.label = opt_label,
		.loc = opt_loc,
		.always_brk = true,
		.brk = loop_brk,
		.rep = loop_rep,
		.brk_local = loop_brk_local,
	};


	rexpr_t expr = pexpr(proc, loop_rep, 0, expr_cfg);

	// ignore value
	lir_ignore(proc, expr.block, pexpr_eu(proc, expr));

	// close the infinite loop
	lir_block_term(proc, expr.block, (lir_term_t){
		.kind = TERM_GOTO,
		.d_goto = loop_rep,
	});

	// term(origin) = loop_brk
	// <- loop_brk

	lir_block_term(proc, origin, (lir_term_t){
		.kind = TERM_GOTO,
		.d_goto = loop_brk,
	});

	// TODO: i am averse to writing ! to a local.
	//       we have `VALUE_CTRL_NORETURN` to construct one.

	p.blks_len--;
	if (!p.blks[p.blks_len].is_brk) {
		assert_not_reached();
	}

	return (rexpr_t){
		.block = loop_brk,
		.value = (lir_value_t){
			.kind = VALUE_CTRL_TEMP,
			.loc = oloc,
			.type = TYPE_INFER,
			.d_ctrl_temp = {
				.entry = loop_rep,
				.local = loop_brk_local,
			},
		},
	};
}

// (                           a b                          )
// ^>    cfg = PEXPR_ET_PAREN  ^^^> cfg = PEXPR_ET_PAREN   >^   break
rexpr_t pexpr(lir_proc_t *proc, lir_rblock_t block, u8 prec, u8 cfg) {
	// will set `is_root` on return path

	token_t token = p.token;
	u32 line_nr = p.token.loc.line_nr;

	struct {
		istr_t name;
		loc_t name_loc;
	} label;

	label.name = ISTR_NONE;

	rexpr_t expr = {
		.block = block,
	};

	retry: switch (p.token.kind) {
		case TOK_VOID: {
			pnext();
			// void expr
			//      ^^^^
			expr = pexpr(proc, expr.block, 0, cfg); // ignore
			lir_discard(proc, expr.block, pexpr_eu(proc, expr));
			expr.value = (lir_value_t){
				.kind = VALUE_TUPLE_UNIT,
				.type = TYPE_UNIT,
				.loc = token.loc,
			};
			break;
		}
		/* case TOK_UNDEFINED: {
			pnext();
			// undefined must be an SSA constant
			// it is also nonsensical to have undefined inside an immutable variable
			expr.value = lir_lvalue(lir_ssa_tmp_inst(proc, expr.block, TYPE_UNDEFINED, token.loc, (lir_inst_t){
				.kind = INST_UNDEFINED,
			}), token.loc);
			break;
		} */
		case TOK_TRUE:
		case TOK_FALSE: {
			pnext();
			expr.value = (lir_value_t){
				.kind = VALUE_BOOL_LIT,
				.type = TYPE_BOOL,
				.loc = token.loc,
				.d_bool_lit = token.kind == TOK_TRUE,
			};
			break;
		}
		case TOK_INTEGER: {
			pnext();
			expr.value = (lir_value_t){
				.kind = VALUE_INTEGER_LIT,
				.type = TYPE_INFER,
				.loc = token.loc,
				.d_integer_lit = token.lit,
			};
			break;
		}
		case TOK_IDENT: {
			expr = pident(proc, expr.block);
			break;
		}
		case TOK_OPAR: {
			loc_t oloc = p.token.loc;
			pnext();
			if (p.token.kind == TOK_CPAR) {
				expr.value = (lir_value_t){
					.kind = VALUE_TUPLE_UNIT,
					.type = TYPE_UNIT,
					.loc = token.loc,
				};
				pnext();
				break;
			}

			// perform late uses here, since we compose them all into a single unit

			// TODO: should commaless containers, we have the option to
			//
			//  let v = (1
			//           1
			//           1)
			//
			bool first = true;
			lir_value_t *elems = NULL;
			while (p.token.kind != TOK_CPAR) {
				if (first) {
					expr = pexpr(proc, expr.block, 0, PEXPR_ET_PAREN);
				} else {
					if (elems == NULL) {
						arrpush(elems, pexpr_lu(proc, expr));
					}
					expr = pexpr(proc, expr.block, 0, PEXPR_ET_PAREN);
					arrpush(elems, pexpr_lu(proc, expr));
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
				expr.value = (lir_value_t){
					.kind = VALUE_TUPLE,
					.type = TYPE_INFER,
					.loc = oloc,
					.d_tuple = elems,
				};
			}
			break;
		}
		case TOK_OSQ: {
			loc_t oloc = p.token.loc;
			lir_value_t *elems = NULL;

			// again, perform late uses here

			// [ 1, 2, 3, 4, 5 ]
			// ^

			pnext();
			while (p.token.kind != TOK_CSQ) {
				rexpr_t nexpr = pexpr(proc, expr.block, 0, PEXPR_ET_ARRAY);
				expr.block = nexpr.block;

				arrpush(elems, pexpr_lu(proc, nexpr));

				if (p.token.kind == TOK_COMMA) {
					pnext();
				} else if (p.token.kind != TOK_CSQ) {
					punexpected("expected `,` or `]`");
				}
			}

			// [ 1, 2, 3, 4, 5 ]
			//                 ^
			pnext();

			expr.value = (lir_value_t){
				.kind = VALUE_ARRAY,
				.type = TYPE_INFER,
				.loc = oloc,
				.d_array = elems,
			};
			break;
		}
		/* case TOK_IF: {
			// if (...) ... else ...
			//
			pnext();
			pexpect(TOK_OPAR);
			rexpr_t cond = pexpr(proc, expr.block, 0, PEXPR_ET_PAREN);
			(void)pexpr_spill(proc, &cond);
			pexpect(TOK_CPAR);

			// if (...) ... else ...
			//          ^^^

			// desugar: if (cond) 20 else 25
			//
			// entry(cond):
			//     s0 = spill cond
			//     goto_pattern s0
			//         true -> if.then
			//         false -> if.else
			// if.then:
			//     s1 = 20
			//     goto if.exit(s1)
			// if.else:
			//     s2 = 25
			//     goto if.exit(s2)
			// if.exit(s3: i32):
			//     <- s3
			//

			// don't forget to setup terminators, sometimes we return a value and others not
			rexpr_t then;
			then.block = lir_block_new(proc, "if.then");
			then = pexpr(proc, then.block, 0, PEXPR_ET_ELSE);
			(void)pexpr_spill(proc, &then);

			// if (...) ... else ...
			//              ^^^^
			//            optional
			//
			// when `else` is not present, we will create a unit tuple
			// to return in the place of the missing else.
			//
			// desugar: return if (cond) 20
			//
			// entry(cond):
			//     s0 = spill cond
			//     goto_pattern s0
			//         true -> if.then
			//         false -> if.else
			// if.then:
			//     s1 = 20
			//     _ = s1
			//     s2 = ()
			//     goto exit(s2)
			// if.else:
			//    s3 = ()
			//    goto exit(s3)
			// exit(s4):
			//     <- s4
			//

			rexpr_t els;
			els.block = lir_block_new(proc, "if.else");

			if (p.token.kind == TOK_ELSE) {
				pnext();
				els = pexpr(proc, els.block, 0, cfg);
				(void)pexpr_spill(proc, &els);
			} else {
				// return empty tuple
				rlocal_t then_unit = lir_ssa_tmp_inst(proc, then.block, TYPE_UNIT, token.loc, (lir_inst_t){
					.kind = INST_TUPLE_UNIT,
				});
				then.value = lir_lvalue(then_unit, token.loc);

				rlocal_t else_unit = lir_ssa_tmp_inst(proc, els.block, TYPE_UNIT, token.loc, (lir_inst_t){
					.kind = INST_TUPLE_UNIT,
				});
				els.value = lir_lvalue(else_unit, token.loc);
			}

			lir_rblock_t exit = lir_block_new(proc, "if.exit");

			// entry -> [if.then, if.else]
			lir_block_term(proc, cond.block, (lir_term_t){
				.kind = TERM_GOTO_PATTERN,
				.d_goto_pattern = {
					.value = cond.value.local,
					.blocks = arr(lir_rblock_t, then.block, els.block),
					.patterns = arr(pattern_t, (pattern_t){
						.kind = PATTERN_BOOL_LIT,
						.loc = cond.value.loc,
						.d_bool_lit = true,
					}, (pattern_t){
						.kind = PATTERN_BOOL_LIT,
						.loc = cond.value.loc,
						.d_bool_lit = false,
					}),
				},
			});

			// if.exit <- if.then
			lir_block_term(proc, then.block, (lir_term_t){
				.kind = TERM_GOTO,
				.d_goto = {
					.block = exit,
					.args = arr(rlocal_t, then.value.local),
				},
			});

			// if.exit <- if.else
			lir_block_term(proc, els.block, (lir_term_t){
				.kind = TERM_GOTO,
				.d_goto = {
					.block = exit,
					.args = arr(rlocal_t, els.value.local),
				}
			});
			
			rlocal_t exit_value = lir_block_new_arg(proc, exit, TYPE_INFER, token.loc);

			expr.block = exit;
			expr.value = lir_lvalue(exit_value, token.loc);
			break;
		} */
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
		case TOK_DO: {
			expr = pdo(proc, expr.block, label.name, label.name_loc);
			label.name = ISTR_NONE;
			break;
		}
		case TOK_LOOP: {
			expr = ploop(proc, expr.block, cfg, label.name, label.name_loc);
			label.name = ISTR_NONE;
			break;
		}
		case TOK_BREAK: {
			istr_t label = ISTR_NONE;
			loc_t onerror = p.token.loc;
			pnext();
			// parse label
			if (p.token.kind == TOK_COLON) {
				pnext();
				pcheck(TOK_IDENT);
				label = p.token.lit;
				onerror = p.token.loc;
				pnext();
			}
			u32 blk_id = pblk_locate(label, onerror);
			expr = pexpr(proc, expr.block, 0, cfg);

			// `brk` always returns a ! type, it's a valid expression

			pblk_t *blk = &p.blks[blk_id];

			// write to the brk local
			// _0 = expr
			lir_assign_local(proc, expr.block, blk->brk_local, pexpr_eu(proc, expr));

			// then branch away

			lir_block_term(proc, expr.block, (lir_term_t){
				.kind = TERM_GOTO,
				.d_goto = blk->brk,
			});

			// we are breaking here
			blk->is_brk = true;
			expr = pnoreturn_value(proc, token.loc, "!.brk");
			break;
		}
		case TOK_CONTINUE: {
			// most of the same, `rep` doesn't take in any expressions.
			// copied code from above to resolve label and locate block
			//
			istr_t label = ISTR_NONE;
			loc_t onerror = token.loc;
			pnext();
			// parse label
			if (p.token.kind == TOK_COLON) {
				pnext();
				pcheck(TOK_IDENT); // TODO: something like: "expected label" ??
				label = p.token.lit;
				onerror = p.token.loc;
				pnext();
			}
			u8 blk_id = pblk_locate(label, onerror);

			lir_block_term(proc, expr.block, (lir_term_t){
				.kind = TERM_GOTO,
				.d_goto = p.blks[blk_id].rep,
			});
			expr = pnoreturn_value(proc, token.loc, "!.rep");
			break;
		}
		case TOK_RETURN: {
			// `ret` evaluates to ! like `brk` and `rep`
			//
			// just end the block with a `ret` terminator
			//
			pnext();
			expr = pexpr(proc, expr.block, 0, cfg);

			lir_block_term(proc, expr.block, (lir_term_t){
				.kind = TERM_RET,
				.d_ret = {
					.value = pexpr_eu(proc, expr),
				},
			});
			expr = pnoreturn_value(proc, token.loc, "!.ret");
			break;
		}
		default: {
			if (TOK_IS_PREFIX(token.kind)) {
				pnext();
				switch (token.kind) {
					case TOK_NOT:
					case TOK_SUB: {
						const u8 prefix_tbl[] = {
							[TOK_NOT] = VALUE_NOT,
							[TOK_SUB] = VALUE_NEG,
						};

						expr = pexpr(proc, expr.block, PREC_PREFIX, cfg);
						//
						// perform unary

						expr.value = (lir_value_t){
							.kind = prefix_tbl[token.kind],
							.type = TYPE_INFER,
							.loc = token.loc,
							.d_unary = {
								.src = lir_dup(pexpr_eu(proc, expr)),
							},
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
						expr = pexpr(proc, expr.block, PREC_PREFIX, cfg);
						expr.value = (lir_value_t){
							.kind = VALUE_ADDRESS_OF,
							.type = TYPE_INFER,
							.loc = token.loc,
							.d_address_of = {
								.lvalue = pexpr_lvalue(proc, expr),
								.is_mut = is_mut,
							},
						};
						break;
					}
					default: {
						assert_not_reached();
					}
				}
				break;
			} else {
				punexpected("expected expression");
			}
		}
	}

	while (true) {
		token_t token = p.token;

		if (token.kind == TOK_EOF) {
			goto exit;
		}

		u8 nprec = ptok_prec_ambiguities();
		nprec = nprec == 0 ? PREC_CALL : nprec;

		if (prec < nprec) {
			switch (cfg) {
				case PEXPR_ET_NONE: break;
				case PEXPR_ET_PAREN: {
					if (p.token.kind == TOK_CPAR || p.token.kind == TOK_COMMA) {
						goto exit;
					}
					break;
				}
				case PEXPR_ET_INDEX_LO: {
					if (p.token.kind == TOK_CSQ || p.token.kind == TOK_DOUBLE_DOTS) {
						goto exit;
					}
					break;
				}
				case PEXPR_ET_INDEX_HI: {
					if (p.token.kind == TOK_CSQ) {
						goto exit;
					}
					break;
				}
				case PEXPR_ET_ARRAY: {
					if (p.token.kind == TOK_CSQ || p.token.kind == TOK_COMMA) {
						goto exit;
					}
					break;
				}
				case PEXPR_ET_ELSE: {
					if (p.token.kind == TOK_ELSE) {
						goto exit;
					}
					break;
				}
				default: {
					assert_not_reached();
				}
			}
			switch (token.kind) {
				case TOK_INC:
				case TOK_DEC: {
					pnext();
					//
					// desugar(lvalue++):
					//     s0 = spill lvalue
					//     s1 = 1
					//     s2 = s0 + s1
					//     lvalue = s2
					//     <- s2
					//
					// will possibly be an SSA local or the actual local
					// remember, spill for reading only
					//
					// s0 = spill lvalue
					/* rlocal_t s0 =  lir_lvalue_spill(proc, expr.block, expr.value);

					// s1 = 1
					rlocal_t s1 = lir_ssa_tmp_inst(proc, expr.block, TYPE_INFER, token.loc, (lir_inst_t){
						.kind = INST_INTEGER_LIT,
						.d_integer_lit = sv_move("1"),
					});

					// s2 = s0 + s1
					rlocal_t s2 = lir_ssa_tmp_inst(proc, expr.block, TYPE_INFER, token.loc, (lir_inst_t){
						.kind = token.kind == TOK_INC ? INST_ADD : INST_SUB,
						.d_infix = {
							.lhs = s0,
							.rhs = s1,
						},
					});

					// lvalue = s2
					lir_inst_lvalue(proc, expr.block, expr.value, s2, token.loc);

					expr.value = lir_lvalue(s2, token.loc);
					//
					// remember, SSA locals are semantically constants and are non lvalues
					// we get this type of checks for free
					//
					// int v;     
					// v++++;      (error: expression is not assignable)
					//    ~~
					//
					continue; */

					// _0 = p
					// p = _0 + 1
					// <- _0

					lir_lvalue_t lvalue = pexpr_lvalue(proc, expr);

					lir_value_t _0 = pexpr_spill(proc, expr.block, lir_value_lvalue(lvalue));

					// loc_t:   p++ 
					//         / ||
					//    p = p + 1
					lir_value_t p_plus_plus = {
						.kind = token.kind == TOK_INC ? VALUE_ADD : VALUE_SUB,
						.type = TYPE_INFER,
						.loc = token.loc,
						.d_infix = {
							.lhs = lir_dup(_0),
							.rhs = lir_dup((lir_value_t){
								.kind = VALUE_INTEGER_LIT,
								.type = TYPE_INFER,
								.loc = token.loc,
								.d_integer_lit = sv_move("1"),
							}),
						},
					};

					// p = _0
					lir_assign(proc, expr.block, lvalue, p_plus_plus);

					// <- _0
					expr = (rexpr_t){
						.block = expr.block,
						.value = _0,
					};
					continue;
				}
				/* case TOK_OSQ: {
					// x[0..1] and x[0]
					pnext();
					//
					// order of evaluation problems
					//
					//     x[expr that mut x] = ...
					//
					// what do you do about this expr? if was a simple use, then we'd
					// just spill it with a late use and use the spilled value.
					// but no, we can't. this is an lvalue assignment.
					//
					// two options:
					// 1. deal with it, ignore left to right evaluation order.
					//    the idea of "left to right" evaluation order falls apart
					//    given an expression like this anyway.
					//    no languages that i know of define this behaviour, they
					//    just go with path `1`.
					//
					// 2. disallow effects inside the index that may cause overwrites
					//    to the lvalue on the left hand side, raise hard errors.
					//    not impossible, it's going to be in our type system anyway.
					//
					// TODO: stick with `1` until a better solution arises, or don't.
					//
					// ```rust
					// pub fn main() {
					//     let mut x = [1, 2, 3];
					// 
					//     x[{
					//         x = [1, 4, 5];         (uhh, where are we writing to again?)
 					//        2
 					//    }] = 5;
					// }
					// ```
					//
					// though, we're kind of royally fucked here. in rust, the expression
					// below respects ordering. in this language though, index expressions
					// are forced lvalues. this means the value `x`'s LATE USE is suddenly
					// invalidated before the index.
					//
					// ```rust
					// let v = x[{
					//     x = [1, 4, 5];      (rust raises warning "value is never read")
					//     2
					// }];
					// ```
					//
					// we'll end up with semantics like this. TODO: this is bad.
					//
					// ```rust
					// x = [1, 4, 5]
					// let v = x[2];
					// ```
					//
					// UPDATE: though rust raises a warning that "value is never read"
					//         it actually READS from the assigned value to `x`.
					//         this is a bug in the rust compiler, not the ordering,
					//         the warning.
					//
					//         this means, that yes, we actually aren't that fucked.
					//         it's fine really.
					//

					// INFO: currently slices AREN'T lvalues
					//
					//     x[0..2] = [1, 2] isn't possible
					//
					// if we were to make it possible, then you'll be able to take
					// the address of a slice, which in reality should be a temporary
					//
					bool expr0_s; // ?rexpr_t would be quite nice here
					rexpr_t expr0;
					if (p.token.kind == TOK_DOUBLE_DOTS) {
						// x[..1]
						expr0_s = false;
						pnext();
					} else {
						// x[0]
						expr0 = pexpr(proc, expr.block, 0, PEXPR_ET_INDEX_LO);
						if (p.token.kind == TOK_CSQ) {
							pnext();
							expr.block = expr0.block;
							lir_lvalue_index(&expr.value, token.loc, lir_lvalue_spill(proc, expr.block, expr0.value));
							continue;
						}
						pexpect(TOK_DOUBLE_DOTS);
						expr0_s = true;
					}
					expr.block = expr0_s ? expr0.block : block;
					//
					// x[?..]
					// x[?..expr]
					//
					if (p.token.kind == TOK_CSQ) {
						pnext();
						// x[0..]
						// x[..]
						expr.value = lir_lvalue(lir_ssa_tmp_inst(proc, expr.block, TYPE_INFER, token.loc, (lir_inst_t){
							.kind = INST_SLICE,
							.d_slice = {
								.src = lir_lvalue_spill(proc, expr.block, expr.value),
								.lo = expr0_s ? lir_lvalue_spill(proc, expr0.block, expr0.value) : LOCAL_NONE,
								.hi = LOCAL_NONE,
							},
						}), token.loc);
					} else {
						if (expr0_s) {
							pexpr_spill(proc, &expr0);
						}
						rexpr_t expr1 = pexpr(proc, expr.block, 0, PEXPR_ET_INDEX_HI);
						pexpect(TOK_CSQ);

						expr.value = lir_lvalue(lir_ssa_tmp_inst(proc, expr1.block, TYPE_INFER, token.loc, (lir_inst_t){
							.kind = INST_SLICE,
							.d_slice = {
								.src = lir_lvalue_spill(proc, expr.block, expr.value),
								.lo = expr0_s ? lir_lvalue_spill(proc, expr0.block, expr0.value) : LOCAL_NONE,
								.hi = lir_lvalue_spill(proc, expr1.block, expr1.value),
							},
						}), token.loc);
						expr.block = expr1.block;
					}
					continue;
				} */
				default: {
					/* if (TOK_IS_INFIX(token.kind)) {
						pnext();
						
						// cast
						if (token.kind == TOK_COLON) {
							type_t type = ptype();
							expr = (rexpr_t){
								.block = expr.block,
								.value = {
									.kind = VALUE_CAST,
									.type = type,
									.loc = token.loc,
									.d_cast = {
										.src = lir_dup(pexpr_eu(proc, expr)),
									},
								},
							};
							continue;
						}

						// TODO: also issues here with possibly invalid lvalues

						// postfix deref: v.*
						if (token.kind == TOK_DOT && p.token.kind == TOK_MUL) {
							// append deref to the lvalue
							(void)pexpr_lvalue(proc, expr); // assert lvalue
							lir_lvalue_deref(&expr.lvalue, p.token.loc);
							pnext();
							continue;
						}

						// field access: v.x
						// tuple access: v.0
						if (token.kind == TOK_DOT) {
							// TODO: read the comments from `pexpr_lvalue_from()`
							//       there is obviously a big issue here

							lir_lvalue_t lvalue = pexpr_lvalue_from(proc, expr);
							
							switch (p.token.kind) {
								case TOK_INTEGER: {
									u64 index = strtoull(sv_from(p.token.lit), NULL, 10);
									// TODO: less than u16
									assert(index <= 0xFFFF);
									lir_lvalue_index_field(&lvalue, p.token.loc, index);
									pnext();
									break;
								}
								case TOK_IDENT: {
									istr_t field = p.token.lit;
									lir_lvalue_index_field(&lvalue, p.token.loc, field);
									break;
								}
								default: {
									punexpected("expected field name or tuple index after `.`");
								}
							}

							expr.is_lvalue = true;
							expr.lvalue = lvalue;
							continue;
						}

						// handles op= and infix op expressions 
						//
						// desugar(lvalue += s0:spilled):
						//     s1 = spill lvalue
						//     s2 = s1 + s0
						//     lvalue = s2 
						//     <- s2
						//
						// desugar(s0:spilled + s1:spilled):
						//     s2 = s0 + s1
						//     <- s2
						//
						// desugar(lvalue = s0:spilled):
						//     lvalue = s0
						//     <- s0
						//
						tok_t kind = token.kind;
						bool is_assign_op = kind == TOK_ASSIGN_ADD || kind == TOK_ASSIGN_SUB || kind == TOK_ASSIGN_MUL || kind == TOK_ASSIGN_DIV || kind == TOK_ASSIGN_MOD;
						//
						rexpr_t lhs_v = expr;

						// remember, spill before pexpr() calls
						rlocal_t lhs;
						if (kind != TOK_ASSIGN) {
							lhs = lir_lvalue_spill(proc, lhs_v.block, lhs_v.value);
						}
						
						rexpr_t rhs_v = pexpr(proc, expr.block, nprec, cfg);
						lir_rblock_t prec_block = rhs_v.block;

						rlocal_t rhs = lir_lvalue_spill(proc, rhs_v.block, rhs_v.value);

						if (is_assign_op) {
							// sparse array
							static const tok_t assign_tok_to_tok[] = {
								[TOK_ASSIGN_ADD] = TOK_ADD,
								[TOK_ASSIGN_SUB] = TOK_SUB,
								[TOK_ASSIGN_MUL] = TOK_MUL,
								[TOK_ASSIGN_DIV] = TOK_DIV,
								[TOK_ASSIGN_MOD] = TOK_MOD,
							};

							kind = assign_tok_to_tok[kind];
						}

						rexpr_t r_expr;

						if (kind != TOK_ASSIGN) {
							static const u8 tok_to_op[] = {
								[TOK_ADD] = INST_ADD,
								[TOK_SUB] = INST_SUB,
								[TOK_MUL] = INST_MUL,
								[TOK_DIV] = INST_DIV,
								[TOK_MOD] = INST_MOD,
								[TOK_EQ] = INST_EQ,
								[TOK_NE] = INST_NE,
								[TOK_LT] = ISNT_LE,
								[TOK_GT] = ISNT_LT,
								[TOK_LE] = ISNT_GE,
								[TOK_GE] = ISNT_GT,
								[TOK_AND] = INST_AND,
								[TOK_OR] = INST_OR,
							};

							// s2 = s0 + s1
							rlocal_t s2 = lir_ssa_tmp_inst(proc, prec_block, TYPE_INFER, token.loc, (lir_inst_t){
								.kind = tok_to_op[kind],
								.d_infix = {
									.lhs = lhs,
									.rhs = rhs,
								},
							});

							r_expr.block = prec_block;
							r_expr.value = lir_value_lvalue(s2, token.loc);
						} else {
							r_expr.block = prec_block;
							r_expr.value = lir_value_lvalue(rhs, token.loc);
						}

						if (is_assign_op || kind == TOK_ASSIGN) {
							// lvalue = ?
							lir_inst_lvalue(proc, r_expr.block, expr.value, lir_lvalue_spill(proc, r_expr.block, r_expr.value), token.loc);
						}

						expr = r_expr;
						continue;
					} else if (token.loc.line_nr == line_nr) {
						// spill as soon as possible, good job
						rlocal_t f = pexpr_spill(proc, &expr);
						//
						rexpr_t expr0 = pexpr(proc, expr.block, PREC_CALL, cfg);
						//
						// don't bloat the IR, merge call expressions if the LHS is already a call expr
						// the checker will unmerge calls based on type information
						//
						// call merging may happen on different basic block boundaries, which means
						// you need to move the call forward into this one
						//
						// there is also an issue with order of evaluation, which is so obviously left
						// to right. however an issue is posed when function arguments have side effects
						// which may affect the previous arguments to a function. ~~this is where forwarding
						// an lvalue to a possible mutable local as the function argument isn't possible~~
						//
						// UPDATE: this has been solved by spilling all mutable lvalues into SSA locals.
						//         as all arguments must go through `lvalue_spill` the change was easy
						//         and the affects were instantly apparent.
						//
						//         assume that all arguments to instructions are IMMUTABLE and without side
						//         effects from their neighbouring operands.
						//
						rlocal_t arg = lir_lvalue_spill(proc, expr0.block, expr0.value);

						// functions are never assigned directly into non SSA lvalues
						//
						//     %10 = %9(%8)
						//     l = %10
						//
						// they're always stored separately, which allows for reordering to work just fine
						// since arguments are always spilled immutably
						//
						u32 inst;
						if (!expr.value.is_sym && (inst = lir_find_inst_ssa_block(proc, expr.block, expr.value.local)) != INST_NONE) {
							// found an SSA local, try merging
							lir_inst_t *instp = &proc->blocks[expr.block].stmts[inst];

							if (instp->kind == INST_CALL) {
								// merge and forward instruction, expr.value stays the same
								lir_inst_t pop_inst = lir_inst_pop(proc, expr.block, inst);
								arrpush(pop_inst.d_call.args, arg);
								lir_inst(proc, expr0.block, pop_inst);
								continue;
							}
						}

						expr.block = expr0.block;
						expr.value = lir_lvalue(lir_ssa_tmp_inst(proc, expr.block, TYPE_INFER, token.loc, (lir_inst_t){
							.kind = INST_CALL,
							.d_call = {
								.f = f,
								.args = arr(rlocal_t, arg),
							},
						}), token.loc);
						continue;
					} */
				}
			}
		}
		goto exit;
	}
exit:

	return expr;
}