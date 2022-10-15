#include "ast_control_flow.hpp"
#include "ast/ast_kind.hpp"
#include "ast/ast_manager.hpp"
#include "ast/ast_program_point.hpp"
#include "parser/parser.hpp"

#include "assert.h"

namespace ast {

template <> If_Node_Statement* is_instance<>(Node* node) {
  return node && node->kind == AST_CTRL_FLOW_IF ? (If_Node_Statement*)node : 0;
}

template <> Elif_List_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_CTRL_FLOW_IF_ELSE ? (Elif_List_Node*)node : 0;
}

template <> Return_Node_Statement* is_instance<>(Node* node) {
  return node && node->kind == AST_CTRL_FLOW_RETURN ? (Return_Node_Statement*)node : 0;
}

template <> Resume_Node_Statement* is_instance<>(Node* node) {
  return node && node->kind == AST_CTRL_FLOW_RESUME ? (Resume_Node_Statement*)node : 0;
}

template <> With_Node_Statement* is_instance<>(Node* node) {
  return node && node->kind == AST_WITH_STATEMENT ? (With_Node_Statement*)node : 0;
}

If_Node_Statement* create_node_if_statement(ast::Manager* manager, Node* condition, Node* body) {
  return as< If_Node_Statement* >(ast::manager_alloc(manager, AST_CTRL_FLOW_IF, condition->id, body->id));
}

Elif_List_Node* create_node_elif_list(ast::Manager* manager, If_Node_Statement* branch, Elif_List_Node* tail) {
  assert(branch && is_instance< If_Node_Statement* >(branch));

  return as< Elif_List_Node* >(ast::manager_alloc(manager, AST_CTRL_FLOW_IF_ELSE, branch->id, tail ? tail->id : 0));
}

Return_Node_Statement* create_node_return_statement(ast::Manager* manager, Node* expression) {
  return as< Return_Node_Statement* >(ast::manager_alloc(manager, AST_CTRL_FLOW_RETURN, expression->id, 0));
}

Resume_Node_Statement* create_node_resume_statement(ast::Manager* manager, Node* expression) {
  return as< Resume_Node_Statement* >(ast::manager_alloc(manager, AST_CTRL_FLOW_RESUME, expression->id, 0));
}

With_Node_Statement* create_node_with_statement(ast::Manager* manager, Declarations_List_Node* list) {
  return as< With_Node_Statement* >(ast::manager_alloc(manager, AST_WITH_STATEMENT, get_id(list), 0));
}

Node* If_Node_Statement::get_condition(ast::Manager* manager) {
  return left_of(manager, this);
}

ProgramPoint_List_Node* If_Node_Statement::get_body(ast::Manager* manager) {
  return is_instance< ProgramPoint_List_Node* >(right_of(manager, this));
}

If_Node_Statement* Elif_List_Node::get_if(ast::Manager* manager) {
  return is_instance< If_Node_Statement* >(left_of(manager, this));
}

Elif_List_Node* Elif_List_Node::get_elif(ast::Manager* manager) {
  return is_instance< Elif_List_Node* >(right_of(manager, this));
}

Node* Return_Node_Statement::get_expression(ast::Manager* manager) {
  return left_of(manager, this);
}

} // namespace ast
