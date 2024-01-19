#include "all.h"
#include "parser.h"

enum : u8 {
	PEXPR_ET_NONE,
	PEXPR_ET_PAREN,
	PEXPR_ET_ARRAY,
	PEXPR_ET_INDEX_LO,
	PEXPR_ET_INDEX_HI,
	PEXPR_ET_ELSE,
};

// (                           a b                          )
// ^>    cfg = PEXPR_ET_PAREN  ^^^> cfg = PEXPR_ET_PAREN   >^   break
rexpr_t pexpr(lir_proc_t *proc, lir_rblock_t block, u32 levels, u8 prec, u8 cfg) {
	u32 olevel = levels++;
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

retry:
	switch (token.kind) {
		case TOK_VOID: {
			pnext();
			// void expr
			//      ^^^^
			expr = pexpr(proc, block, levels, 0, cfg);
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
		/* case TOK_IF: {
			// if (...) ... else ...
			//
			pnext();
			pexpect(TOK_OPAR);
			rexpr_t cond = pexpr(proc, block, levels, PEXPR_ET_PAREN, cfg);
			pexpect(TOK_CPAR);

			// if (...) ... else ...
			//          ^^^

			rexpr_t then = pexpr(proc, block, levels, PEXPR_ET_ELSE, cfg);

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
				rexpr_t els = pexpr(proc, block, levels, 0, cfg);

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
			assert_not_reached();
		}
	}

	return expr;
}