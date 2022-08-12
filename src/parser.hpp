#pragma once

#include "ast.hpp"
#include "token.hpp"
#include "types.hpp"

struct parser {
  lexer lex;
};

void parser_init(parser *p, u64 id, i8 *buffer, u64 size);
void parser_destroy(parser *p);

ast *parser_parse(parser *p);

