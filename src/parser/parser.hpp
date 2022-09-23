#pragma once

#include "ast/ast_manager.hpp"
#include "compiler/symbol_table.hpp"

#include "lexer.hpp"

namespace parser {

struct Parser {
  Lexer*        lexer;
  ast::Manager* ast_manager;
};

Parser* parser_create(u64 id, const i8* buffer, u64 size);
void    parser_destroy(Parser* p);

void print_ast(Parser* p, ast::Node* n);
void print_ast_ir(Parser* p, ast::Node* n, u32 scope = 0);
b8   parser_is_same_symbol(Parser* p, ast::Node* a, ast::Node* b);

ast::Node* parser_parse(Parser* p);

void parser_error(Parser* p, Token tok, const i8* msg);

} // namespace parser
