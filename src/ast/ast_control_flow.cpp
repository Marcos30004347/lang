#include "ast_control_flow.hpp"
#include "ast/ast_kind.hpp"
#include "ast/ast_manager.hpp"
#include "parser/parser.hpp"

#include "assert.h"

namespace ast {

template <> If_Node_Statement* is_instance<>(Node* node) {
  return node->kind == AST_CTRL_FLOW_IF ? (If_Node_Statement*)node : 0;
}

template <> Elif_List_Node* is_instance<>(Node* node) {
  return node->kind == AST_CTRL_FLOW_IF_ELSE ? (Elif_List_Node*)node : 0;
}

template <> Return_Node_Statement* is_instance<>(Node* node) {
  return node->kind == AST_CTRL_FLOW_RETURN ? (Return_Node_Statement*)node : 0;
}

If_Node_Statement* create_node_if_statement(parser::Parser* parser, Node* condition, Node* body) {
  return as< If_Node_Statement* >(
      ast::manager_alloc(parser->ast_manager, AST_CTRL_FLOW_IF, condition->id, body->id));
}

Elif_List_Node* create_node_elif_list(parser::Parser* parser, If_Node_Statement* branch, Elif_List_Node* tail) {
  assert(branch && is_instance< If_Node_Statement* >(branch));

  return as< Elif_List_Node* >(
      ast::manager_alloc(parser->ast_manager, AST_CTRL_FLOW_IF_ELSE, branch->id, tail ? tail->id : 0));
}

Return_Node_Statement* create_node_return_statement(parser::Parser* parser, Node* expression) {
  return as< Return_Node_Statement* >(
      ast::manager_alloc(parser->ast_manager, AST_CTRL_FLOW_RETURN, expression->id, 0));
}

Node* If_Node_Statement::get_condition(parser::Parser* parser) {
  return left_of(parser->ast_manager, this);
}

Node* If_Node_Statement::get_body(parser::Parser* parser) {
  return right_of(parser->ast_manager, this);
}

If_Node_Statement* Elif_List_Node::get_if(parser::Parser* parser) {
  return is_instance< If_Node_Statement* >(left_of(parser->ast_manager, this));
}

Elif_List_Node* Elif_List_Node::get_elif(parser::Parser* parser) {
  return is_instance< Elif_List_Node* >(right_of(parser->ast_manager, this));
}

} // namespace ast
