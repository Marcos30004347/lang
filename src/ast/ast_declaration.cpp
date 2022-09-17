#include "ast_declaration.hpp"
#include "ast/ast_kind.hpp"
#include "ast/ast_literals.hpp"
#include "ast/ast_manager.hpp"

namespace ast {

template <> Declaration_Constant_Node* is_instance<>(Node* node) {
  return node->kind == AST_DECLARATION_CONSTANT ? as< Declaration_Constant_Node* >(node) : 0;
}

template <> Declaration_Variable_Node* is_instance<>(Node* node) {
  return node->kind == AST_DECLARATION_VARIABLE ? as< Declaration_Variable_Node* >(node) : 0;
}

Declaration_Constant_Node*
create_constant_declaration(parser::Parser* parser, Literal_Symbol_Node* name, Node* type) {
  return as< Declaration_Constant_Node* >(
      ast::manager_alloc(parser->ast_manager, AST_DECLARATION_CONSTANT, name->id, type->id));
}

Declaration_Variable_Node*
create_variable_declaration(parser::Parser* parser, Literal_Symbol_Node* name, Node* type) {
  return as< Declaration_Variable_Node* >(
      ast::manager_alloc(parser->ast_manager, AST_DECLARATION_VARIABLE, name->id, type->id));
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

b8 is_declaration_node(ast::Node* node) {
  return ast::is_instance< ast::Declaration_Constant_Node* >(node) ||
         ast::is_instance< ast::Declaration_Variable_Node* >(node);
}

} // namespace ast
