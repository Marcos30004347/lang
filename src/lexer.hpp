#pragma once

#include "types.hpp"

enum Token_Type {
  TOKEN_EOF = 0,
  UNDEF_TOKEN,
  // IDENTIFIER
  TOKEN_ID,

  // SYMBOLS
  TOKEN_COMMA,             // ,
  TOKEN_COLON,             // :
  TOKEN_EXCLAMATION,       // !
  TOKEN_SEMI_COLON,        // ;
  TOKEN_DOT,               //.
  TOKEN_OPEN_CURLY_BRACE,  // {
  TOKEN_CLOSE_CURLY_BRACE, // }
  TOKEN_OPEN_PARENTHESIS,  // (
  TOKEN_CLOSE_PARENTHESIS, // )
  TOKEN_APHOSTROPHE,       // '

  // OPERATORS
  TOKEN_PIPE,              // |
  TOKEN_ASTERISK,          // *
  TOKEN_PLUS,              // +
  TOKEN_MINUS,             // -
  TOKEN_EQUAL,             // =
  TOKEN_EQUAL_EQUAL,       // ==
  TOKEN_EXCLAMATION_EQUAL, // ==
  TOKEN_SLASH,             // /
  TOKEN_GREATER,           // >
  TOKEN_SMALLER,           // <
  TOKEN_GREATER_EQUAL,     // >=
  TOKEN_SMALLER_EQUAL,     // <=

  // UTILS
  TOKEN_ARROW, // ->

  // Keywords
  TOKEN_KEYWORD_FN,       // fn
  TOKEN_KEYWORD_EFFECT,   // effect
  TOKEN_KEYWORD_HANDLER,  // handler
  TOKEN_KEYWORD_RETURN,   // RETURN
  TOKEN_KEYWORD_IF,       // if
  TOKEN_KEYWORD_ELSE,     // else
  TOKEN_KEYWORD_WITH,     // with
  TOKEN_KEYWORD_CONTINUE, // continue
  TOKEN_KEYWORD_RESUME,   // resume
  TOKEN_KEYWORD_STRUCT,   // resume
  TOKEN_KEYWORD_THEN,     // then

  // Types
  TOKEN_UNIT, // unit
  TOKEN_I32,  // i32
  TOKEN_TYPE, // type

  // Literals
  TOKEN_I32_LIT // 1234
};

struct Token {
  Token_Type type;

  u32 size;
  u32 file;
  u32 row;
  u32 col;
  u32 pos;

  // 64 bits buffer
  u64 buf;
};

enum Lexer_State {
  LEXER_EOF = 0,
  LEXER_OK  = 1,
};

struct Lexer {
  Token curr;
  Token prev;

  i8* file_buf;

  u32 file_pos;
  u32 file_id;
  u32 file_col;
  u32 file_row;
  u32 file_size;
};

void lexer_init(Lexer* t, u64 id, i8* buffer, u64 size);

void lexer_destroy(Lexer*);

Token lexer_read_token(Lexer*);

Token lexer_undef_token();

b8 lexer_is_eof(Lexer*);

void token_get_id(Lexer* t, Token tok, i8* buf);

const i8* lexer_get_Token_file_buff_ptr(Lexer* l, Token t);
