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

// remove lvalue
void pexpr_load(lir_proc_t *proc, rexpr_t *expr) {
	if (expr->is_lvalue) {
		expr->is_lvalue = false;
		loc_t loc = proc->values[expr->value].loc;
		expr->value = lir_inst_value(proc, expr->block, TYPE_INFER, loc, (lir_inst_t){
			.kind = INST_LOAD,
			.d_load = {
				.src = expr->value,
			},
		});
	}
}

// INFO: there is a very real risk with representing symbols as unwrapped module strings.
//       sometimes there may be conflicting modules with different filesystem paths
//       but they share the same module path. it's a small risk, but it's there.
//
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

		return (rexpr_t){
			.block = block,
			.value = lir_inst_value(proc, block, TYPE_INFER, p.token.loc, (lir_inst_t){
				.kind = INST_SYMBOL,
				.d_symbol = {
					.qualified_name = qualified_name,
				},
			}),
		};
	}

	// 1. for each scope entry, search for the local `lit`

	for (u32 s = p.scope_len; s-- > 0;) {
		u32 lo = p.scope[s];
		u32 hi;
		if (s == p.scope_len - 1) {
			hi = arrlen(p.scope_entries);
		} else {
			hi = p.scope[s + 1];
		}

		for (u32 i = lo; i < hi; i++) {
			pscope_entry_t *entry = &p.scope_entries[i];
			if (entry->kind == PS_LOCAL && entry->name == lit) {
				pnext();

				// load &variable
				rexpr_t expr = (rexpr_t){
					.block = block,
					.value = lir_local_addr(proc, block, entry->d_local.local),
					.is_lvalue = true,
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

	return (rexpr_t){
		.block = block,
		.value = lir_inst_value(proc, block, TYPE_INFER, loc, (lir_inst_t){
			.kind = INST_SYMBOL,
			.d_symbol = {
				.qualified_name = qualified_name,
			},
		}),
		.is_lvalue = true,
	};
}

void pexpr_assert_lvalue(lir_proc_t *proc, rexpr_t expr, const char *onerror) {
	if (!expr.is_lvalue) {
		print_err_with_pos(proc->values[expr.value].loc, onerror);
		err_unwind();
	}
}

// (                           a b                          )
// ^>    cfg = PEXPR_ET_PAREN  ^^^> cfg = PEXPR_ET_PAREN   >^   break
rexpr_t pexpr(lir_proc_t *proc, lir_rblock_t block, u8 prec, u8 cfg) {
	// will set `is_root` on return path

	token_t token = p.token;
	u32 line_nr = token.loc.line_nr;
	bool is_single = true;
	bool should_continue = true;

	struct {
		istr_t name;
		loc_t name_loc;
	} label;

	label.name = ISTR_NONE;

	rexpr_t expr = {
		.block = block,
	};

	retry: switch (token.kind) {
		case TOK_VOID: {
			pnext();
			// void expr
			//      ^^^^
			expr = pexpr(proc, block, 0, cfg); // ignore
			expr.value = lir_inst_value(proc, block, TYPE_UNIT, token.loc, (lir_inst_t){
				.kind = INST_TUPLE_UNIT,
			});
			break;
		}
		case TOK_TRUE:
		case TOK_FALSE: {
			pnext();
			expr.value = lir_inst_value(proc, block, TYPE_BOOL, token.loc, (lir_inst_t){
				.kind = INST_BOOL_LIT,
				.d_bool_lit = token.kind == TOK_TRUE,
			});
			break;
		}
		case TOK_INTEGER: {
			pnext();
			expr.value = lir_inst_value(proc, block, TYPE_INFER, token.loc, (lir_inst_t){
				.kind = INST_INTEGER_LIT,
				.d_integer_lit = token.lit,
			});
			break;
		}
		case TOK_IDENT: {
			expr = pident(proc, block);
			break;
		}
		/* case TOK_IF: {
			// if (...) ... else ...
			//
			pnext();
			pexpect(TOK_OPAR);
			rexpr_t cond = pexpr(proc, block, PEXPR_ET_PAREN, cfg);
			pexpect(TOK_CPAR);

			// if (...) ... else ...
			//          ^^^

			rexpr_t then = pexpr(proc, block, PEXPR_ET_ELSE, cfg);

			// if (...) ... else ...
			//              ^^^^
			//            optional
			//
			// when `else` is not present, we will create a unit tuple
			// to return. just goto the exit label and ret `v0`.
			//
			// exit:
			//     v0 = ()

			if (p.token.kind == TOK_ELSE) {
				rexpr_t els = pexpr(proc, block, 0, cfg);

				// case : goto
				// case : goto then
				lir_block_term(proc, block, token.loc, (lir_term_t){
					.kind = TERM_IF,
					.d_if = {
						.cond = cond.value,
						.then = then.block,
						.els = els.block,
					},
				});
			} else {

			}
		} */
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

						expr = pexpr(proc, block, PREC_PREFIX, cfg);
						pexpr_load(proc, &expr);
						//
						// perform unary
						expr.value = lir_inst_value(proc, block, TYPE_INFER, token.loc, (lir_inst_t){
							.kind = prefix_tbl[token.kind],
							.d_unary = {
								.src = expr.value,
							},
						});
						break;
					}
					case TOK_SINGLE_AND: {
						bool is_mut = false;
						if (p.token.kind == TOK_TACK) {
							// &'v = mut ref
							is_mut = true;
							pnext();
						}

						expr = pexpr(proc, block, PREC_PREFIX, cfg);
						//
						pexpr_assert_lvalue(proc, expr, "cannot take address of non-lvalue");
						//
						// address of requires no more work, and is no longer an lvalue anymore
						expr.is_lvalue = false;
						if (is_mut) {
							expr.value = lir_mut(proc, expr.block, expr.value);
						}
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
					// desugar(v0:l++):
					//     v1 = load v0
					//     v2 = 1
					//     v3 = v1 + v2
					//     store v0, v3
					//     <- v3
					//
					const char *onerror = token.kind == TOK_INC ? "cannot increment non-lvalue" : "cannot decrement non-lvalue";
					pexpr_assert_lvalue(proc, expr, onerror);
					//
					lir_rvalue_t v1 = lir_inst_value(proc, block, TYPE_INFER, token.loc, (lir_inst_t){
						.kind = INST_LOAD,
						.d_load = {
							.src = expr.value,
						},
					});
					lir_rvalue_t v2 = lir_inst_value(proc, block, TYPE_INFER, token.loc, (lir_inst_t){
						.kind = INST_INTEGER_LIT,
						.d_integer_lit = sv_move("1"),
					});
					lir_rvalue_t v3 = lir_inst_value(proc, block, TYPE_INFER, token.loc, (lir_inst_t){
						.kind = token.kind == TOK_INC ? INST_ADD : INST_SUB,
						.d_infix = {
							.lhs = v1,
							.rhs = v2,
						},
					});
					lir_inst_new(proc, block, (lir_inst_t){
						.target = LIR_VALUE_NONE,
						.kind = INST_STORE,
						.d_store = {
							.dest = expr.value,
							.src = v3,
						},
					});
					expr.value = v3;
					expr.is_lvalue = false; // we set is_lvalue to false here because of below:
					//
					// int v;     
					// v++++;      (error: expression is not assignable)
					//    ~~
					//
					continue;
				}
				case TOK_OSQ: {
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
				}
				default: {
					if (!TOK_IS_INFIX(token.kind)) {
						goto exit;
					}
					pnext();
					
					// cast
					if (token.kind == TOK_COLON) {
						type_t type = ptype();
						pexpr_load(proc, &expr);
						expr.value = lir_inst_value(proc, block, type, token.loc, (lir_inst_t){
							.kind = INST_CAST,
							.d_cast = {
								.src = expr.value,
								.type = type,
							},
						});
						continue;
					}

					// postfix deref: v.*
					if (token.kind == TOK_DOT && p.token.kind == TOK_MUL) {
						pexpr_load(proc, &expr);
						expr.value = lir_inst_value(proc, block, TYPE_INFER, p.token.loc, (lir_inst_t){
							.kind = INST_LOAD,
							.d_load = {
								.src = expr.value,
							},
						});
						pnext();
						continue;						
					}

					// field access: v.x
					// tuple access: v.0
					if (token.kind == TOK_DOT) {
						switch (p.token.kind) {
							case TOK_INTEGER: {
								// assume that this never fails...
								// TODO: fix?
								u64 index = strtoull(sv_from(p.token.lit), NULL, 10);

								pexpr_assert_lvalue(proc, expr, "cannot access tuple field of non-lvalue");
								//
								// will stay lvalue
								expr.value = lir_inst_value(proc, block, TYPE_INFER, p.token.loc, (lir_inst_t){
									.kind = INST_TUPLE_OFFSET,
									.d_tuple_offset = {
										.src = expr.value,
										.index = index,
									},
								});
								pnext();
								continue;
							}
							case TOK_IDENT: {
								istr_t field = p.token.lit;

								pexpr_assert_lvalue(proc, expr, "cannot access tuple field of non-lvalue");
								//
								// will stay lvalue
								expr.value = lir_inst_value(proc, block, TYPE_INFER, p.token.loc, (lir_inst_t){
									.kind = INST_FIELD_OFFSET,
									.d_field_offset = {
										.src = expr.value,
										.field = field,
									},
								});
								pnext();
								continue;
							}
							default: {
								punexpected("expected field name or tuple index after `.`");
							}
						}
					}

					// handles += and infix + expressions 
					//
					// desugar(v0:l += v1):
					//     v2 = load v0
					//     v3 = v2 + v1
					//     store v0, v3
					//     <- v3
					//
					// desugar(v0 + v1):
					//     v3 = v0 + v1
					//     <- v3
					//
					tok_t kind = token.kind;
					bool is_assign_op = kind == TOK_ASSIGN_ADD || kind == TOK_ASSIGN_SUB || kind == TOK_ASSIGN_MUL || kind == TOK_ASSIGN_DIV || kind == TOK_ASSIGN_MOD;
					//
					rexpr_t lhs_v = expr;
					pexpr_load(proc, &lhs_v);
					rexpr_t rhs_v = pexpr(proc, expr.block, nprec, cfg);
					pexpr_load(proc, &rhs_v);					

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

					if (is_assign_op || kind == TOK_ASSIGN) {
						pexpr_assert_lvalue(proc, expr, "cannot assign to non-lvalue");
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

						// v3 = v0 + v1
						lir_rvalue_t v3 = lir_inst_value(proc, block, TYPE_INFER, token.loc, (lir_inst_t){
							.kind = tok_to_op[kind],
							.d_infix = {
								.lhs = lhs_v.value,
								.rhs = rhs_v.value,
							},
						});

						r_expr.block = rhs_v.block;
						r_expr.value = v3;
						r_expr.is_lvalue = false;
					} else {
						r_expr.block = rhs_v.block;
						r_expr.value = rhs_v.value;
						r_expr.is_lvalue = false;
					}

					if (is_assign_op || kind == TOK_ASSIGN) {
						// store v0, v3
						expr.value = lir_mut(proc, expr.block, expr.value);
						lir_inst_new(proc, block, (lir_inst_t){
							.target = LIR_VALUE_NONE,
							.kind = INST_STORE,
							.d_store = {
								.dest = expr.value,
								.src = r_expr.value,
							},
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