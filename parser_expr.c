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

				// load &variable
				rexpr_t expr = (rexpr_t){
					.block = block,
					.value = lir_lvalue(entry->d_local.local),
				};
				
				return expr;
			}
		}
	}

	assert_not_reached();

	// 2. not found, return a symbol
	//    insert a `PS_DEBUG_REFERENCE` to let further references know that
	//    this variable is unresolved

	pnext();
	// used to solve below:
	//
	// let g = x      (error: use before declaration in same scope)
	// let x = 0      (hint: declaration here)
	//
	/* pscope_register((pscope_entry_t){
		.kind = PS_DEBUG_REFERENCE,
		.loc = loc,
		.name = lit,
	});

	istr_t qualified_name = fs_module_symbol_sv(p.mod, lit);

	return (rexpr_t){
		.block = block,
		.value = lir_inst_value(proc, block, TYPE_INFER, loc, (lir_inst_t){
			.kind = INST_SYMBOL,
			.d_symbol = {
				.qualified_name = qualified_name,
			},
		}),
		.is_lvalue = true,
	}; */
}

bool plet(lir_proc_t *proc, lir_rblock_t block, rexpr_t *expr_out) {
	pnext();

	// let x = 0
	// let y

	u32 scope_olen = p.scope_entries_len;
	u32 locals_olen = arrlenu(proc->locals);
	lir_term_pat_t pattern = ppattern(proc);
	u32 scope_rlen = p.scope_entries_len;
	u32 locals_rlen = arrlenu(proc->locals);

	// let x = 0
	//
	//     %0 = 0
	//     goto_pattern next:
	// next(%1):
	//     x = %1
	//
	// let x       (evaluates to a no-op)
	//

	// no let rec here
	pmask_scope(scope_olen, scope_rlen, true);

	if (p.token.kind == TOK_ASSIGN) {
		pnext();

		rexpr_t expr = pexpr(proc, block, 0, 0);

		// create goto_pattern

		lir_rblock_t let_next = lir_block_new(proc, "let.next");
		lir_block_term(proc, expr.block, (lir_term_t){
			.kind = TERM_GOTO_PATTERN,
			.d_goto_pattern = {
				.value = lir_lvalue_spill(proc, expr.block, expr.value),
				.patterns = arr(lir_term_pat_t, pattern),
				.blocks = arr(lir_rblock_t, let_next),
			},
		});

		// create variable bindings as block arguments
		pblock_args_to_vars(proc, let_next, locals_olen, locals_rlen);

		expr_out->block = let_next;
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

	lir_rblock_t do_rep = BLOCK_NONE;
	lir_rblock_t do_brk = BLOCK_NONE;
	lir_rlocal_t do_brk_local;

	if (opt_label != ISTR_NONE) {
		do_rep = lir_block_new(proc, "do.entry");
		do_brk = lir_block_new(proc, "do.exit");

		lir_block_term(proc, expr.block, (lir_term_t){
			.kind = TERM_GOTO,
			.d_goto = {
				.block = do_rep,
			},
		});

		do_brk_local = lir_block_new_arg(proc, do_brk, TYPE_INFER, &oloc);
		expr.block = do_rep;
	}

	u8 blk_id = p.blks_len++;
	p.blks[blk_id] = (pblk_t){
		.label = opt_label,
		.loc = opt_loc,
		.always_brk = false,
		.brk = do_brk,
		.rep = do_rep,
	};

	bool set = false;

	u32 bcol = p.token.loc.col;
	ppush_scope();
	while (p.token.kind != TOK_EOF) {
		u32 cln = p.token.loc.line_nr;

		set |= pstmt(proc, expr.block, &expr);

		if (cln != p.token.loc.line_nr && p.token.loc.col < bcol) {
			break;
		}
	}
	ppop_scope();
	p.blks_len--;

	// not EOF, something was present here but didn't return any value
	// return () to either the brk param or just as a bare expression
	if (!set) {
		expr.value = lir_lvalue(lir_ssa_tmp_inst(proc, expr.block, TYPE_UNIT, &oloc, (lir_inst_t){
			.kind = INST_TUPLE_UNIT,
		}));
	}

	if (opt_label != ISTR_NONE) {
		lir_block_term(proc, expr.block, (lir_term_t){
			.kind = TERM_GOTO,
			.d_goto = {
				.block = do_brk,
				.args = arr(lir_rlocal_t, lir_lvalue_spill(proc, expr.block, expr.value)),
			},
		});
		expr.value = lir_lvalue(do_brk_local);
		expr.block = do_brk;
	}
	return expr;
}

// loop expr
rexpr_t ploop(lir_proc_t *proc, lir_rblock_t block, u8 expr_cfg, istr_t opt_label, loc_t opt_loc) {
	loc_t oloc = p.token.loc;
	pnext();

	lir_rblock_t loop_rep = lir_block_new(proc, "loop.entry");
	lir_rblock_t loop_brk = lir_block_new(proc, "loop.exit");

	lir_rlocal_t loop_brk_value = lir_block_new_arg(proc, loop_brk, TYPE_INFER, &oloc);

	u8 blk_id = p.blks_len++;
	p.blks[blk_id] = (pblk_t){
		.label = opt_label,
		.loc = opt_loc,
		.always_brk = true,
		.brk = loop_brk,
		.rep = loop_rep,
	};

	// block -> loop.entry
	lir_block_term(proc, block, (lir_term_t){
		.kind = TERM_GOTO,
		.d_goto = {
			.block = loop_rep,
		},
	});

	rexpr_t expr = pexpr(proc, loop_rep, 0, expr_cfg);
	// TODO: unused(expr)

	// expr.block -> loop.entry
	// <- loop.exit

	lir_block_term(proc, expr.block, (lir_term_t){
		.kind = TERM_GOTO,
		.d_goto = {
			.block = loop_rep,
		},
	});

	p.blks_len--;
	if (!p.blks[p.blks_len].is_brk) {
		proc->locals[loop_brk_value].type = TYPE_BOTTOM; // infinite loop, expr is !
	}

	rexpr_t expr_ret = {
		.block = loop_brk,
		.value = lir_lvalue(loop_brk_value),
	};

	return expr_ret;
}

// (                           a b                          )
// ^>    cfg = PEXPR_ET_PAREN  ^^^> cfg = PEXPR_ET_PAREN   >^   break
rexpr_t pexpr(lir_proc_t *proc, lir_rblock_t block, u8 prec, u8 cfg) {
	// will set `is_root` on return path

	token_t token = p.token;

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
			// TODO: possibly insert `unused` instruction
			expr.value = lir_lvalue(lir_ssa_tmp_inst(proc, expr.block, TYPE_UNIT, &token.loc, (lir_inst_t){
				.kind = INST_TUPLE_UNIT,
			}));
			break;
		}
		case TOK_UNDEFINED: {
			pnext();
			// undefined must be an SSA constant
			// it is also nonsensical to have undefined inside an immutable variable
			expr.value = lir_lvalue(lir_ssa_tmp_inst(proc, expr.block, TYPE_UNDEFINED, &token.loc, (lir_inst_t){
				.kind = INST_UNDEFINED,
			}));
			break;
		}
		case TOK_TRUE:
		case TOK_FALSE: {
			pnext();
			expr.value = lir_lvalue(lir_ssa_tmp_inst(proc, expr.block, TYPE_BOOL, &token.loc, (lir_inst_t){
				.kind = INST_BOOL_LIT,
				.d_bool_lit = token.kind == TOK_TRUE,
			}));
			break;
		}
		case TOK_INTEGER: {
			pnext();
			expr.value = lir_lvalue(lir_ssa_tmp_inst(proc, expr.block, TYPE_INFER, &token.loc, (lir_inst_t){
				.kind = INST_INTEGER_LIT,
				.d_integer_lit = token.lit,
			}));
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
				expr.value = lir_lvalue(lir_ssa_tmp_inst(proc, expr.block, TYPE_UNIT, &token.loc, (lir_inst_t){
					.kind = INST_TUPLE_UNIT,
				}));
				pnext();
				break;
			}
			bool first = true;
			lir_rlocal_t *elems = NULL;
			while (p.token.kind != TOK_CPAR) {
				if (first) {
					expr = pexpr(proc, expr.block, 0, PEXPR_ET_PAREN);
				} else {
					if (elems == NULL) {
						arrpush(elems, lir_lvalue_spill(proc, expr.block, expr.value));
					}
					expr = pexpr(proc, expr.block, 0, PEXPR_ET_PAREN);
					arrpush(elems, lir_lvalue_spill(proc, expr.block, expr.value));
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
				expr.value = lir_lvalue(lir_ssa_tmp_inst(proc, expr.block, TYPE_INFER, &oloc, (lir_inst_t){
					.kind = INST_TUPLE,
					.d_tuple = elems,
				}));
			}
			break;
		}
		case TOK_IF: {
			loc_t oloc = p.token.loc;
			// if (...) ... else ...
			//
			pnext();
			pexpect(TOK_OPAR);
			rexpr_t cond = pexpr(proc, expr.block, 0, PEXPR_ET_PAREN);
			pexpect(TOK_CPAR);

			// if (...) ... else ...
			//          ^^^

			// desugar: return if (cond) 20 else 25
			//
			// entry(cond):
			//     s0 = spill cond
			//     if s0 goto block_t else block_f
			// block_t:
			//     s1 = 20
			//     goto exit(s1)
			// block_f:
			//     s2 = 25
			//     goto exit(s2)
			// exit(s3: i32):
			//     <- s3
			//

			// don't forget to setup terminators, sometimes we return a value and others not
			lir_rlocal_t block_t = lir_block_new(proc, "if.then");
			rexpr_t then = pexpr(proc, block_t, 0, PEXPR_ET_ELSE);

			// if (...) ... else ...
			//              ^^^^
			//            optional
			//
			// when `else` is not present, we will create a unit tuple
			// to return. just goto the exit label and ret `v0`.
			//
			// desugar: return if (cond) 20
			//
			// entry(cond):
			//     s0 = spill cond
			//     if s0 goto block_t else exit
			// block_t:
			//     s1 = 20          (pure value ignored)
			//     goto exit
			// exit:
			//     s2 = ()
			//     <- s2
			//
			// possibly raise error when a pure value is ignored
			// could be done in two ways:
			//
			// 1. insert unused(v) instruction
			// 2. mark all roots, then check if any roots are unused
			//
			// it's best we mark the expr value of `then` as a root
			//

			lir_rblock_t else_block;
			lir_rblock_t exit;
			lir_rlocal_t exit_value;

			if (p.token.kind == TOK_ELSE) {
				pnext();
				lir_rlocal_t block_f = lir_block_new(proc, "if.else");
				rexpr_t els = pexpr(proc, block_f, 0, cfg);
				
				// cond -> block_t, block_f
				// 'block_t -> exit(v)
				// 'block_f -> exit(v)
				// exit(v) -> return v

				exit = lir_block_new(proc, "if.exit");
				exit_value = lir_block_new_arg(proc, exit, TYPE_INFER, &oloc);

				lir_block_term(proc, then.block, (lir_term_t){
					.kind = TERM_GOTO,
					.d_goto = {
						.block = exit,
						.args = arr(lir_rlocal_t, lir_lvalue_spill(proc, then.block, then.value)),
					},
				});

				lir_block_term(proc, els.block, (lir_term_t){
					.kind = TERM_GOTO,
					.d_goto = {
						.block = exit,
						.args = arr(lir_rlocal_t, lir_lvalue_spill(proc, els.block, els.value)),
					}
				});

				else_block = block_f;
			} else {
				// return empty tuple
				exit = lir_block_new(proc, "if.exit");
				exit_value = lir_ssa_tmp_inst(proc, exit, TYPE_UNIT, &oloc, (lir_inst_t){
					.kind = INST_TUPLE_UNIT,
				});

				lir_block_term(proc, then.block, (lir_term_t){
					.kind = TERM_GOTO,
					.d_goto = {
						.block = exit,
					},
				});
				else_block = exit;
			}

			// join control flow
			//
			// cond -> block_t, block_f
			// cond -> block_t, exit
			//
			lir_block_term(proc, expr.block, (lir_term_t){
				.kind = TERM_IF,
				.d_if = {
					.cond = lir_lvalue_spill(proc, expr.block, cond.value),
					.then = block_t,
					.els = else_block,
				},
			});

			expr.block = exit;
			expr.value = lir_lvalue(exit_value);
			break;
		}
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
			u8 blk_id = pblk_locate(label, onerror);
			expr = pexpr(proc, expr.block, 0, cfg);

			// below unifies just fine. remember that a `brk` always
			// returns a ! type, so it's always a valid expression.
			//
			// returning a ! type is involved though since it requires
			// unreachable control flow. construct a new block with a
			// ! parameter and then use that as the expr return.
			//
			// the checker is aware of any blocks that produce a ! type
			// and will remove them from the CFG after checking.
			//
			// desugar:
			//
			// :t do
			//     20 + brk :t 50
			//
			// entry:
			//     goto do.0
			// do.0:
			//     %0 = 20
			//     %1 = 50
			//     goto do.exit(%1)
			// do.1(%3: !):
			//     %4 = %0 + %3
			//     goto do.exit(%4)
			// do.exit(%2: i32):
			//     (use %2)
			//
			//
			lir_block_term(proc, expr.block, (lir_term_t){
				.kind = TERM_GOTO,
				.d_goto = {
					.block = p.blks[blk_id].brk,
					.args = arr(lir_rlocal_t, lir_lvalue_spill(proc, expr.block, expr.value)),
				},
			});

			// we are breaking here
			p.blks[blk_id].is_brk = true;

			expr = pnoreturn_value(proc, &token.loc);
			break;
		}
		default: {
			if (TOK_IS_PREFIX(token.kind)) {
				pnext();
				switch (token.kind) {
					case TOK_NOT:
					case TOK_SUB: {
						const u8 prefix_tbl[] = {
							[TOK_NOT] = INST_NOT,
							[TOK_SUB] = INST_NEG,
						};

						expr = pexpr(proc, expr.block, PREC_PREFIX, cfg);
						//
						// perform unary
						expr.value = lir_lvalue(lir_ssa_tmp_inst(proc, expr.block, TYPE_INFER, &token.loc, (lir_inst_t){
							.kind = prefix_tbl[token.kind],
							.d_unary = {
								.src = lir_lvalue_spill(proc, expr.block, expr.value),
							},
						}));
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
						expr.value = lir_lvalue(lir_ssa_tmp_inst(proc, expr.block, TYPE_INFER, &token.loc, (lir_inst_t){
							.kind = INST_ADDRESS_OF,
							.d_address_of = {
								.lvalue = expr.value,
								.is_mut = is_mut,
							},
						}));
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
					lir_rlocal_t s0 =  lir_lvalue_spill(proc, expr.block, expr.value);

					// s1 = 1
					lir_rlocal_t s1 = lir_ssa_tmp_inst(proc, expr.block, TYPE_INFER, &token.loc, (lir_inst_t){
						.kind = INST_INTEGER_LIT,
						.d_integer_lit = token.lit,
					});

					// s2 = s0 + s1
					lir_rlocal_t s2 = lir_ssa_tmp_inst(proc, expr.block, TYPE_INFER, &token.loc, (lir_inst_t){
						.kind = token.kind == TOK_INC ? INST_ADD : INST_SUB,
						.d_infix = {
							.lhs = s0,
							.rhs = s1,
						},
					});

					// lvalue = s2
					lir_inst(proc, expr.block, (lir_inst_t){
						.dest = expr.value, // original lvalue
						.kind = INST_LVALUE,
						.d_lvalue = lir_lvalue(s2),
					});

					expr.value = lir_lvalue(s2);
					//
					// remember, SSA locals are semantically constants and are non lvalues
					// we get this type of checks for free
					//
					// int v;     
					// v++++;      (error: expression is not assignable)
					//    ~~
					//
					continue;
				}
				/* case TOK_OSQ: {
					// x[0..1] and x[0]
					loc_t oloc = p.token.loc;
					pnext();

					bool expr0_s;
					rexpr_t expr0;
					if (p.token.kind == TOK_DOUBLE_DOTS) {
						// x[..1]
						expr0_s = false;
						pnext();
					} else {
						// x[0]
						
						expr0 = pexpr(proc, block, PEXPR_ET_INDEX_LO, cfg);
						if (p.token.kind == TOK_CSQ) {
							pnext();
							expr.block = expr0.block;
							expr.value = lir_inst_value(proc, expr.block, TYPE_INFER, oloc, (lir_inst_t){
								.kind = INST_LEA,
								.d_lea = {
									.src = expr.value,
									.index = expr0.value,
								},
							});
							continue;
						}
						expr0_s = true;
					}
					// x[0..]
					//      ^
					// x[0..1]
					//      ^
					if (p.token.kind == TOK_CSQ) {
						pnext();
						expr.block = expr0_s ? expr0.block : block;
						expr.value = lir_inst_value(proc, expr.block, TYPE_INFER, oloc, (lir_inst_t){
							.kind = INST_SLICE,
							.d_slice = {
								.src = expr.value,
								.lo = expr0_s ? expr0.value : LIR_VALUE_NONE,
								.hi = LIR_VALUE_NONE,
							},
						});
					} else {
						lir_rblock_t next = expr0_s ? expr0.block : block;
						rexpr_t expr1 = pexpr(proc, next, PEXPR_ET_INDEX_HI, cfg);
						pexpect(TOK_CSQ);

						expr.is_lvalue = false;
						expr.block = expr1.block;
						expr.value = lir_inst_value(proc, expr.block, TYPE_INFER, oloc, (lir_inst_t){
							.kind = INST_SLICE,
							.d_slice = {
								.src = expr.value,
								.lo = expr0_s ? expr0.value : LIR_VALUE_NONE,
								.hi = expr1.value,
							},
						});
					}
					continue;
				} */
				default: {
					if (!TOK_IS_INFIX(token.kind)) {
						goto exit;
					}
					pnext();
					
					// cast
					if (token.kind == TOK_COLON) {
						type_t type = ptype();
						expr.value = lir_lvalue(lir_ssa_tmp_inst(proc, expr.block, type, &token.loc, (lir_inst_t){
							.kind = INST_CAST,
							.d_cast = {
								.src = lir_lvalue_spill(proc, expr.block, expr.value),
								.type = type,
							},
						}));
						continue;
					}

					// postfix deref: v.*
					if (token.kind == TOK_DOT && p.token.kind == TOK_MUL) {
						// append deref to the lvalue
						lir_lvalue_deref(&expr.value, p.token.loc);
						pnext();
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
								lir_lvalue_index_field(&expr.value, p.token.loc, index);
								pnext();
								continue;
							}
							case TOK_IDENT: {
								istr_t field = p.token.lit;
								lir_lvalue_index_field(&expr.value, p.token.loc, field);
								continue;
							}
							default: {
								punexpected("expected field name or tuple index after `.`");
							}
						}
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
					rexpr_t rhs_v = pexpr(proc, expr.block, nprec, cfg);
					lir_rblock_t prec_block = rhs_v.block;

					lir_rlocal_t lhs;
					lir_rlocal_t rhs = lir_lvalue_spill(proc, rhs_v.block, rhs_v.value);

					if (kind != TOK_ASSIGN) {
						lhs = lir_lvalue_spill(proc, lhs_v.block, lhs_v.value);
					}

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

					rexpr_t r_expr = {};

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
						lir_rlocal_t s2 = lir_ssa_tmp_inst(proc, prec_block, TYPE_INFER, &token.loc, (lir_inst_t){
							.kind = tok_to_op[kind],
							.d_infix = {
								.lhs = lhs,
								.rhs = rhs,
							},
						});

						r_expr.block = prec_block;
						r_expr.value = lir_lvalue(s2);
					} else {
						r_expr.block = prec_block;
						r_expr.value = lir_lvalue(rhs);
					}

					if (is_assign_op || kind == TOK_ASSIGN) {
						// lvalue = ?
						lir_inst(proc, expr.block, (lir_inst_t){
							.dest = expr.value, // original lvalue
							.kind = INST_LVALUE,
							.d_lvalue = r_expr.value,
						});
					}

					expr = r_expr;
					continue;
				}
			}
		}
		goto exit;
	}
exit:

	return expr;
}