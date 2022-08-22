#pragma once

#include "ast.hpp"
#include "lexer.hpp"
#include "types.hpp"

struct Parser {
  Lexer lex;
	AST_Manager ast_man;
};

void parser_init(Parser *p, u64 id, i8 *buffer, u64 size);
void parser_destroy(Parser *p);

void print_ast(Parser* p, AST_Node* n);

b8 parser_is_same_symbol(Parser* p, AST_Node* a, AST_Node* b);

AST_Node *parser_parse(Parser *p);

