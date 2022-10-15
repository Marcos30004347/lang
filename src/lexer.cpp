#include "lexer.hpp"
#include "crc64.hpp"

#include "stdlib.h"

b8 is_numeric(i8 c) {
  return c >= '0' && c <= '9';
}

b8 is_alphanumeric(i8 c) {
  return is_numeric(c) || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

i8 lexer_current(Lexer* t) {
  return t->file_buf[t->file_pos];
}

b8 lexer_is_eof(Lexer* t) {
  return t->file_pos == t->file_size || lexer_current(t) == '\0';
}

u8 lexer_eat(Lexer* t) {
  if (t->file_buf[t->file_pos] == '\n') {
    t->file_col = 0;
    t->file_row += 1;
  } else {
    t->file_col += 1;
  }

  t->file_pos += 1;

  return LEXER_OK;
}

// u8 lexer_move_next_line(Lexer* t) {
//   if (lexer_current(t) != '\n') {
//     return LEXER_OK;
//   }

//   lexer_eat(t);

//   t->file_col = 0;
//   t->file_row += 1;

//   return LEXER_OK;
// }

i8 lexer_look_ahead(Lexer* t) {
  if (t->file_pos + 1 != t->file_size) {
    return t->file_buf[t->file_pos + 1];
  }

  return '\0';
}

u8 lexer_skip_whitespaces(Lexer* t) {
  // TODO(marcos): there should probably be a '\n' and '\r' here.
  while (!lexer_is_eof(t) && (lexer_current(t) == ' ' || lexer_current(t) == '\t' || lexer_current(t) == '\n')) {
    lexer_eat(t);
  }

  if (lexer_is_eof(t))
    return LEXER_EOF;

  // if (lexer_current(t) == '\n') {
  //   lexer_move_next_line(t);
  //   lexer_skip_whitespaces(t);
  // }

  return LEXER_OK;
}

u8 lexer_skip_comments(Lexer* t) {
  while (lexer_current(t) == '/' && lexer_look_ahead(t) == '/') {
    while (!lexer_is_eof(t) && lexer_current(t) != '\n') {
      lexer_eat(t);
    }

    lexer_skip_whitespaces(t);
  }

  lexer_skip_whitespaces(t);

  return LEXER_OK;
}

Token token_create(Token_Type ty, u64 pos, u64 col, u64 row, u64 file, u64 size, u64 buf = 0) {
  Token tok;

  tok.pos  = pos;
  tok.file = file;
  tok.col  = col;
  tok.row  = row;
  tok.size = size;
  tok.type = ty;
  tok.buf  = buf;

  return tok;
}

Token lexer_read_i32lit(Lexer* t) {
  u64 col = t->file_col;
  u64 row = t->file_row;
  u64 pos = t->file_pos;
  u64 s   = t->file_pos;

  i32 n = 0;

  while (is_numeric(lexer_current(t))) {
    n = n * 10 + (lexer_current(t) - '0');
    lexer_eat(t);
  }

  Token tok = token_create(TOKEN_I32_LIT, pos, col, row, t->file_id, t->file_pos - s);

  tok.buf = n;

  return tok;
}

b8 lexer_can_read_word(Lexer* t, const char* word, u64* size = 0) {
  if (word == 0)
    return false;

  u64 i = 0;

  while (word[i] != '\0' && !lexer_is_eof(t) && t->file_buf[t->file_pos + i] == word[i]) {
    i = i + 1;
  }

  if (size)
    *size = i;

  if (word[i] == '\0') {
    return true;
  }

  return false;
}

u8 lexer_eat_next_n(Lexer* t, u64 n) {
  for (u64 i = 0; i < n; i++) {
    if (lexer_is_eof(t))
      return LEXER_EOF;
    lexer_eat(t);
  }

  return LEXER_OK;
}

Token lexer_read_id(Lexer* t) {
  u64 col = t->file_col;
  u64 row = t->file_row;
  u64 pos = t->file_pos;

  while (is_alphanumeric(lexer_current(t)) || lexer_current(t) == '_') {
    lexer_eat(t);
  }

  u64 len = t->file_pos - pos;
  u64 buf = crc64(t->file_buf + pos, len);

  return token_create(TOKEN_ID, pos, col, row, t->file_id, len, buf);
}

void lexer_init(Lexer* t, u64 id, const i8* buffer, u64 size) {
  t->file_buf  = buffer;
  t->file_col  = 1;
  t->file_row  = 1;
  t->file_id   = id;
  t->file_size = size;
  t->file_pos  = 0;

  t->prev = token_create(TOKEN_EOF, -1, -1, -1, id, 0);
  t->curr = token_create(TOKEN_EOF, -1, -1, -1, id, 0);

  lexer_read_token(t);
}

void lexer_destroy(Lexer* t) { /*free(t->file_buf);*/
}

Token lexer_set_curr(Lexer* lex, Token tok) {
  lex->prev = lex->curr;
  lex->curr = tok;
  return tok;
}

Token eof(Lexer* lex) {
  return token_create(TOKEN_EOF, lex->file_pos, lex->file_col, lex->file_row, lex->file_id, 0);
}

Token lexer_read_token(Lexer* t) {
  lexer_skip_whitespaces(t);
  lexer_skip_comments(t);

  if (lexer_is_eof(t)) {
    return lexer_set_curr(t, eof(t));
  }

  u64 col = t->file_col;
  u64 row = t->file_row;
  u64 pos = t->file_pos;

  if (lexer_current(t) == '\'' && lexer_eat(t))
    return lexer_set_curr(t, token_create(TOKEN_APHOSTROPHE, pos, col, row, t->file_id, 1));
  if (lexer_current(t) == ':' && lexer_eat(t))
    return lexer_set_curr(t, token_create(TOKEN_COLON, pos, col, row, t->file_id, 1));
  if (lexer_current(t) == ',' && lexer_eat(t))
    return lexer_set_curr(t, token_create(TOKEN_COMMA, pos, col, row, t->file_id, 1));
  if (lexer_current(t) == '!' && lexer_eat(t))
    return lexer_set_curr(t, token_create(TOKEN_EXCLAMATION, pos, col, row, t->file_id, 1));
  if (lexer_current(t) == ';' && lexer_eat(t))
    return lexer_set_curr(t, token_create(TOKEN_SEMI_COLON, pos, col, row, t->file_id, 1));
  if (lexer_current(t) == '.' && lexer_eat(t))
    return lexer_set_curr(t, token_create(TOKEN_DOT, pos, col, row, t->file_id, 1));
  if (lexer_current(t) == '{' && lexer_eat(t))
    return lexer_set_curr(t, token_create(TOKEN_OPEN_CURLY_BRACE, pos, col, row, t->file_id, 1));
  if (lexer_current(t) == '}' && lexer_eat(t))
    return lexer_set_curr(t, token_create(TOKEN_CLOSE_CURLY_BRACE, pos, col, row, t->file_id, 1));
  if (lexer_current(t) == '(' && lexer_eat(t))
    return lexer_set_curr(t, token_create(TOKEN_OPEN_PARENTHESIS, pos, col, row, t->file_id, 1));
  if (lexer_current(t) == ')' && lexer_eat(t))
    return lexer_set_curr(t, token_create(TOKEN_CLOSE_PARENTHESIS, pos, col, row, t->file_id, 1));

  if (lexer_current(t) == '+' && lexer_eat(t))
    return lexer_set_curr(t, token_create(TOKEN_PLUS, pos, col, row, t->file_id, 1));

  if (lexer_current(t) == '-' && lexer_eat(t)) {
    if (lexer_current(t) == '>' && lexer_eat(t)) {
      return lexer_set_curr(t, token_create(TOKEN_ARROW, pos, col, row, t->file_id, 2));
    }

    return lexer_set_curr(t, token_create(TOKEN_MINUS, pos, col, row, t->file_id, 1));
  }

  if (lexer_current(t) == '|' && lexer_eat(t))
    return lexer_set_curr(t, token_create(TOKEN_PIPE, pos, col, row, t->file_id, 1));

  if (lexer_current(t) == '*' && lexer_eat(t))
    return lexer_set_curr(t, token_create(TOKEN_ASTERISK, pos, col, row, t->file_id, 1));

  if (lexer_current(t) == '/' && lexer_eat(t))
    return lexer_set_curr(t, token_create(TOKEN_SLASH, pos, col, row, t->file_id, 1));

  if (lexer_current(t) == '<' && lexer_eat(t)) {
    if (lexer_current(t) == '=' && lexer_eat(t)) {
      return lexer_set_curr(t, token_create(TOKEN_SMALLER_EQUAL, pos, col, row, t->file_id, 2));
    }

    return lexer_set_curr(t, token_create(TOKEN_SMALLER, pos, col, row, t->file_id, 1));
  }

  if (lexer_current(t) == '>' && lexer_eat(t)) {
    if (lexer_current(t) == '=' && lexer_eat(t)) {
      return lexer_set_curr(t, token_create(TOKEN_GREATER_EQUAL, pos, col, row, t->file_id, 2));
    }

    return lexer_set_curr(t, token_create(TOKEN_GREATER, pos, col, row, t->file_id, 1));
  }

  if (lexer_current(t) == '=' && lexer_eat(t)) {
    if (lexer_current(t) == '=' && lexer_eat(t)) {
      return lexer_set_curr(t, token_create(TOKEN_EQUAL_EQUAL, pos, col, row, t->file_id, 2));
    }

    return lexer_set_curr(t, token_create(TOKEN_EQUAL, pos, col, row, t->file_id, 1));
  }

  if (lexer_current(t) == '!' && lexer_eat(t)) {
    if (lexer_current(t) == '=' && lexer_eat(t)) {
      return lexer_set_curr(t, token_create(TOKEN_EXCLAMATION_EQUAL, pos, col, row, t->file_id, 2));
    }

    return lexer_set_curr(t, token_create(TOKEN_EXCLAMATION, pos, col, row, t->file_id, 1));
  }

  // TODO: next character after a numeric literal cannot be alphanumeric
  // unless it is a indicator of the literal type like 10f, 0.5f, 10U.
  if (is_numeric(lexer_current(t)))
    return lexer_set_curr(t, lexer_read_i32lit(t));

  u64 size = 0;

  if (lexer_can_read_word(t, "struct", &size) && lexer_eat_next_n(t, size))
    return lexer_set_curr(t, token_create(TOKEN_KEYWORD_STRUCT, pos, col, row, t->file_id, size));

  if (lexer_can_read_word(t, "then", &size) && lexer_eat_next_n(t, size))
    return lexer_set_curr(t, token_create(TOKEN_KEYWORD_THEN, pos, col, row, t->file_id, size));

  if (lexer_can_read_word(t, "resume", &size) && lexer_eat_next_n(t, size))
    return lexer_set_curr(t, token_create(TOKEN_KEYWORD_RESUME, pos, col, row, t->file_id, size));

  if (lexer_can_read_word(t, "i32", &size) && lexer_eat_next_n(t, size))
    return lexer_set_curr(t, token_create(TOKEN_I32, pos, col, row, t->file_id, size));

  if (lexer_can_read_word(t, "unit", &size) && lexer_eat_next_n(t, size))
    return lexer_set_curr(t, token_create(TOKEN_UNIT, pos, col, row, t->file_id, size));

  if (lexer_can_read_word(t, "type", &size) && lexer_eat_next_n(t, size))
    return lexer_set_curr(t, token_create(TOKEN_TYPE, pos, col, row, t->file_id, size));

  if (lexer_can_read_word(t, "continue", &size) && lexer_eat_next_n(t, size))
    return lexer_set_curr(t, token_create(TOKEN_KEYWORD_CONTINUE, pos, col, row, t->file_id, size));

  if (lexer_can_read_word(t, "with", &size) && lexer_eat_next_n(t, size))
    return lexer_set_curr(t, token_create(TOKEN_KEYWORD_WITH, pos, col, row, t->file_id, size));

  if (lexer_can_read_word(t, "if", &size) && lexer_eat_next_n(t, size))
    return lexer_set_curr(t, token_create(TOKEN_KEYWORD_IF, pos, col, row, t->file_id, size));

  if (lexer_can_read_word(t, "else", &size) && lexer_eat_next_n(t, size))
    return lexer_set_curr(t, token_create(TOKEN_KEYWORD_ELSE, pos, col, row, t->file_id, size));

  if (lexer_can_read_word(t, "return", &size) && lexer_eat_next_n(t, size))
    return lexer_set_curr(t, token_create(TOKEN_KEYWORD_RETURN, pos, col, row, t->file_id, size));

  if (lexer_can_read_word(t, "fn", &size) && lexer_eat_next_n(t, size))
    return lexer_set_curr(t, token_create(TOKEN_KEYWORD_FN, pos, col, row, t->file_id, size));

  if (lexer_can_read_word(t, "effect", &size) && lexer_eat_next_n(t, size))
    return lexer_set_curr(t, token_create(TOKEN_KEYWORD_EFFECT, pos, col, row, t->file_id, size));

  if (lexer_can_read_word(t, "handler", &size) && lexer_eat_next_n(t, size))
    return lexer_set_curr(t, token_create(TOKEN_KEYWORD_HANDLER, pos, col, row, t->file_id, size));

  return lexer_set_curr(t, lexer_read_id(t));
}

void token_get_id(Lexer* t, Token tok, i8* buf) {
  u64 i = 0;

  for (; i < tok.size; i++) {
    buf[i] = t->file_buf[tok.pos + i];
  }

  buf[i] = '\0';
}

const i8* lexer_get_token_file_buff_ptr(Lexer* l, Token t) {
  return l->file_buf + t.pos;
}

Token lexer_undef_token() {
  return token_create(UNDEF_TOKEN, -1, -1, -1, -1, 0);
}

Token lexer_i32_token(i32 nat) {
  Token t;
  t.buf  = nat;
  t.col  = -1;
  t.row  = -1;
  t.file = -1;
  t.size = -1;
  t.type = TOKEN_I32_LIT;
  return t;
}
