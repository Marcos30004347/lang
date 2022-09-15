#include "ast_control_flow.hpp"
#include "ast/ast_kind.hpp"

namespace ast {
If_Node_Statement* create_node_if_statement(parser::Parser* parser, Node* condition, Node* body) {

  return as< If_Node_Statement* >(ast::manager_alloc(parser->ast_manager, AST_CTRL_FLOW_IF, condition->id, body->id));
}

Elif_List_Node* create_node_elif_list(parser::Parser* parser, If_Node_Statement* branch, Elif_List_Node* tail) {
  return as< Elif_List_Node* >(ast::manager_alloc(parser->ast_manager, AST_CTRL_FLOW_IF_ELSE, branch->id, tail->id));
}

} // namespace ast
