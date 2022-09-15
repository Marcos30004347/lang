#include "ast_declaration.hpp"
#include "ast/ast_kind.hpp"
#include "ast/ast_literals.hpp"
#include "ast/ast_manager.hpp"

namespace ast {

Declaration_Constant_Node* create_constant_declaration(parser::Parser* parser, Literal_Symbol_Node* name, Node* type) {
  return as< Declaration_Constant_Node* >(ast::manager_alloc(parser->ast_manager, AST_DECLARATION_CONSTANT, 0, 0));
}

Declaration_Variable_Node* create_variable_declaration(parser::Parser* parser, Literal_Symbol_Node* name, Node* type) {
  return as< Declaration_Variable_Node* >(ast::manager_alloc(parser->ast_manager, AST_DECLARATION_VARIABLE, 0, 0));
}

Literal_Symbol_Node* Declaration_Constant_Node::get_symbol(parser::Parser* parser) {
  return is_instance< Literal_Symbol_Node* >(left_of(parser->ast_manager, this));
}

Node* Declaration_Constant_Node::get_type(parser::Parser* parser) {
  return right_of(parser->ast_manager, this);
}

Literal_Symbol_Node* Declaration_Variable_Node::get_symbol(parser::Parser* parser) {
  return is_instance< Literal_Symbol_Node* >(left_of(parser->ast_manager, this));
}

Node* Declaration_Variable_Node::get_type(parser::Parser* parser) {
  return right_of(parser->ast_manager, this);
}

} // namespace ast
