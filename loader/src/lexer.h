#ifndef GAME_LEXER_H
#define GAME_LEXER_H 1

typedef enum {
	TT_OPEN_PAREN,
	TT_CLOSE_PAREN,
	TT_SYMBOL,
	TT_FIXNUM,
	TT_FLOAT
} token_type;

typedef struct {
	token_type ty;
	str lexeme;
} token;

typedef enum {
	LEX_OK = 0,
	LEX_ERR = 1,
	LEX_EOF = -1
} lex_result;

lex_result lex(str*, token*);

#endif
