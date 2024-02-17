#pragma once

#define TYPE_X_CONCRETE_LITERALS_LIST \
	X(TYPE_I8, "i8") \
	X(TYPE_I16, "i16") \
	X(TYPE_I32, "i32") \
	X(TYPE_I64, "i64") \
	X(TYPE_ISIZE, "isize") \
	X(TYPE_U8, "u8") \
	X(TYPE_U16, "u16") \
	X(TYPE_U32, "u32") \
	X(TYPE_U64, "u64") \
	X(TYPE_USIZE, "usize") \
	X(TYPE_F32, "f32") \
	X(TYPE_F64, "f64") \
	X(TYPE_BOOL, "bool")

#define TYPE_SIGNED_INTEGERS_START TYPE_I8
#define TYPE_SIGNED_INTEGERS_END TYPE_ISIZE
#define TYPE_UNSIGNED_INTEGERS_START TYPE_U8
#define TYPE_UNSIGNED_INTEGERS_END TYPE_USIZE

#define TYPE_X_CONCRETE_LIST \
	TYPE_X_CONCRETE_LITERALS_LIST \
	X(TYPE_UNIT, "()") \
	X(TYPE_BOTTOM, "!")

// NOTE: there is no undefined in the language anymore,
//       there isn't a need for it when you have proper
//       definite assignment analysis.
// X(TYPE_UNDEFINED, "undefined")

#define TOK_X_KEYWORDS_LIST \
	X(TOK_I8, "i8") \
	X(TOK_I16, "i16") \
	X(TOK_I32, "i32") \
	X(TOK_I64, "i64") \
	X(TOK_ISIZE, "isize") \
	X(TOK_U8, "u8") \
	X(TOK_U16, "u16") \
	X(TOK_U32, "u32") \
	X(TOK_U64, "u64") \
	X(TOK_USIZE, "usize") \
	X(TOK_F32, "f32") \
	X(TOK_F64, "f64") \
	X(TOK_BOOL, "bool") \
	X(TOK_TRUE, "true") \
	X(TOK_FALSE, "false") \
	X(TOK_LET, "let") \
	X(TOK_UNDEFINED, "undefined") \
	X(TOK_DO, "do") \
	X(TOK_LOOP, "loop") \
	X(TOK_IO, "io") \
	X(TOK_IMPORT, "import") \
	X(TOK_IMPORT_MAIN, "import_main") \
	X(TOK_PUB, "pub") \
	X(TOK_EXTERN, "extern") \
	X(TOK_BREAK, "brk") \
	X(TOK_CONTINUE, "rep") \
	X(TOK_RETURN, "ret") \
	X(TOK_VOID, "void") \
	X(TOK_IF, "if") \
	X(TOK_ELSE, "else") \
	X(TOK_STRUCT, "struct") \
	X(TOK_TYPE, "type")

//	X(TOK_IN, "in")

// in specific order due to how operators are parsed
#define TOK_X_OPERATOR_LIST \
	X(TOK_TACK, "'") \
	X(TOK_UNDERSCORE, "_") \
	X(TOK_ARROW, "->") \
	X(TOK_INC, "++") \
	X(TOK_ASSIGN_ADD, "+=") \
	X(TOK_ADD, "+") \
	X(TOK_ASSIGN_SUB, "-=") \
	X(TOK_DEC, "--") \
	X(TOK_SUB, "-") \
	X(TOK_ASSIGN_MUL, "*=") \
	X(TOK_MUL, "*") \
	X(TOK_ASSIGN_DIV, "/=") \
	X(TOK_DIV, "/") \
	X(TOK_ASSIGN_MOD, "%=") \
	X(TOK_MOD, "%") \
	X(TOK_EQ, "==") \
	X(TOK_ASSIGN, "=") \
	X(TOK_NE, "!=") \
	X(TOK_NOT, "!") \
	X(TOK_LE, "<=") \
	X(TOK_LT, "<") \
	X(TOK_GE, ">=") \
	X(TOK_GT, ">") \
	X(TOK_AND, "&&") \
	X(TOK_OR, "||") \
	X(TOK_PIPE, "|") \
	X(TOK_TILDE, "~") \
	X(TOK_TRIPLE_DOTS, "...") \
	X(TOK_DOUBLE_DOTS, "..") \
	X(TOK_DOT, ".") \
	X(TOK_COMMA, ",") \
	X(TOK_OPAR, "(") \
	X(TOK_CPAR, ")") \
	X(TOK_OSQ, "[") \
	X(TOK_CSQ, "]") \
	X(TOK_COLON, ":") \
	X(TOK_QUESTION, "?") \
	X(TOK_SINGLE_AND, "&") \
	X(TOK_OCBR, "{") \
	X(TOK_CCBR, "}")

// X(TOK_LSHIFT, "<<")
// X(TOK_RSHIFT, ">>")
// X(TOK_RUSHIFT, ">>>")
// X(TOK_XOR, "^")
// X(TOK_BAND, "&")

#define TOK_HAS_LIT(t) \
	((t) == TOK_IDENT || \
	(t) == TOK_INTEGER)

#define TOK_IS_PREFIX(t) \
	((t) == TOK_SUB || \
	(t) == TOK_NOT || \
	(t) == TOK_SINGLE_AND)

// these tokens will always evaluate to bool
#define TOK_IS_COND(t) \
	((t) == TOK_EQ || \
	(t) == TOK_NE || \
	(t) == TOK_LT || \
	(t) == TOK_GT || \
	(t) == TOK_LE || \
	(t) == TOK_GE || \
	(t) == TOK_AND || \
	(t) == TOK_OR)

/* (t) == TOK_TILDE || \
	(t) == TOK_MUL || \
	(t) == TOK_BAND) */

#define TOK_IS_INFIX(t) \
	((t) == TOK_ADD || \
	(t) == TOK_SUB || \
	(t) == TOK_MUL || \
	(t) == TOK_DIV || \
	(t) == TOK_MOD || \
	(t) == TOK_ASSIGN || \
	(t) == TOK_ASSIGN_ADD || \
	(t) == TOK_ASSIGN_SUB || \
	(t) == TOK_ASSIGN_MUL || \
	(t) == TOK_ASSIGN_DIV || \
	(t) == TOK_ASSIGN_MOD || \
	(t) == TOK_EQ || \
	(t) == TOK_NE || \
	(t) == TOK_LT || \
	(t) == TOK_GT || \
	(t) == TOK_LE || \
	(t) == TOK_GE || \
	(t) == TOK_AND || \
	(t) == TOK_OR || \
	(t) == TOK_DOT || \
	(t) == TOK_COLON)

// TODO: impl pipe
// (t) == TOK_BAND ||
// (t) == TOK_BOR ||
// (t) == TOK_XOR ||
// (t) == TOK_LSHIFT ||
// (t) == TOK_RSHIFT ||
// (t) == TOK_RUSHIFT ||
	

#define TOK_X_LIST \
	X(TOK_NIL, "tok_nil") \
	X(TOK_EOF, "EOF") \
	X(TOK_IDENT, "identifier") \
	X(TOK_INTEGER, "integer") \
	X(TOK_STRING, "string") \
	TOK_X_KEYWORDS_LIST \
	TOK_X_OPERATOR_LIST
