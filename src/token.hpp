#pragma once

#include "types.hpp"

enum token_type {
	TOKEN_EOF = 0,
	// IDENTIFIER
	TOKEN_ID,
	
	// SYMBOLS
	TOKEN_COMMA, // ,
	TOKEN_COLON, // :
	TOKEN_EXCLAMATION, // !
	TOKEN_SEMI_COLON, // ;
	TOKEN_DOT, //.
	TOKEN_OPEN_CURLY_BRACE, // {
	TOKEN_CLOSE_CURLY_BRACE, // }
	TOKEN_OPEN_PARENTHESIS, // (
	TOKEN_CLOSE_PARENTHESIS, // )


	// OPERATORS
	TOKEN_ASTERISK, // *
	TOKEN_PLUS,  // +
	TOKEN_MINUS, // -
	TOKEN_EQUAL, // =
	TOKEN_EQUAL_EQUAL, // ==
	TOKEN_EXCLAMATION_EQUAL, // ==
	TOKEN_SLASH, // /
	TOKEN_GREATER, // >
	TOKEN_SMALLER, // <
	TOKEN_GREATER_EQUAL, // >=
	TOKEN_SMALLER_EQUAL, // <=

	// UTILS
	TOKEN_ARROW, // ->

	// Keywords
	TOKEN_KEYWORD_FN, // fn
	TOKEN_KEYWORD_EF, // ef
	TOKEN_KEYWORD_HANDLER, // handler
	TOKEN_KEYWORD_RETURN, // RETURN
	TOKEN_KEYWORD_IF, // if
	TOKEN_KEYWORD_ELSE, // else 
	TOKEN_KEYWORD_WITH, // with 
	TOKEN_KEYWORD_CONTINUE, // continue 

	// Types
	TOKEN_I32, // i32

	// Literals
	TOKEN_I32_LIT // 1234
};


struct token {
	token_type type;
	u64 size;
	u64 file;
	u64 row;
	u64 col;
	u64 pos;
	// 64 bits buffer 
	u64 buf;
};

enum lexer_state {
	LEXER_EOF = 0,
	LEXER_OK = 1,
};

struct lexer {
	token curr;
	token prev;
	
	i8* file_buf;
	
	u64 file_pos;
	u64 file_id;
	u64 file_col;
	u64 file_row;
	u64 file_size;
};

void lexer_init(lexer* t, u64 id, i8* buffer, u64 size);

void lexer_destroy(lexer*);

token lexer_read_token(lexer*);

b8 lexer_is_eof(lexer*);

void token_get_id(lexer* t, token tok, i8* buf);

const i8* lexer_get_token_file_buff_ptr(lexer* l, token t);
