#include "ast_literals.hpp"
#include "ast.hpp"
#include "ast/ast_kind.hpp"
#include "ast/ast_manager.hpp"
#include "compiler/symbol_table.hpp"
#include "lexer.hpp"
#include "parser/parser.hpp"

using namespace compiler;
using namespace parser;

namespace ast {
Literal_False_Node* create_node_literal_false(parser::Parser* parser) {
  return as< Literal_False_Node* >(ast::manager_alloc(parser->ast_manager, AST_FALSE_LITERAL, 0, 0));
}

Literal_True_Node* create_node_literal_true(parser::Parser* parser) {
  return as< Literal_True_Node* >(ast::manager_alloc(parser->ast_manager, AST_TRUE_LITERAL, 0, 0));
}

Literal_Undefined_Node* create_node_literal_undefined(parser::Parser* parser) {
  return as< Literal_Undefined_Node* >(ast::manager_alloc(parser->ast_manager, AST_UNDEFINED_NODE, 0, 0));
}

Literal_Nothing_Node* create_node_literal_nothing(parser::Parser* parser) {
  return as< Literal_Nothing_Node* >(ast::manager_alloc(parser->ast_manager, AST_NULL_NODE, 0, 0));
}

Literal_Natural_Node* literal_natural(parser::Parser* parser, symbol::Symbol val) {
  return as< Literal_Natural_Node* >(ast::manager_alloc(parser->ast_manager, AST_NATURAL_LITERAL, val.id, 0));
}

Literal_Symbol_Node* literal_symbol(parser::Parser* parser, compiler::symbol::Symbol val) {
  return as< Literal_Symbol_Node* >(ast::manager_alloc(parser->ast_manager, AST_SYMBOL_LITERAL, val.id, 0));
}

compiler::symbol::Symbol Literal_Symbol_Node::get_symbol(parser::Parser* parser) {
  symbol::Symbol sym = symbol::get_symbol(parser->symbol_table, left_id_of(parser->ast_manager, this));

  assert(sym.id != symbol::empty(parser->symbol_table).id);

  return sym;
}

compiler::symbol::Symbol Literal_Natural_Node::get_symbol(parser::Parser* parser) {
  symbol::Symbol sym = symbol::get_symbol(parser->symbol_table, left_id_of(parser->ast_manager, this));

  assert(sym.id != symbol::empty(parser->symbol_table).id);

  return sym;
}

Literal_Struct_Node* create_node_literal_struct(parser::Parser* parser, ProgramPoint_List_Node* members) {
  return as< Literal_Struct_Node* >(ast::manager_alloc(parser->ast_manager, AST_STRUCT_LITERAL, members->id, members ? members->id : 0));
}

} // namespace ast
