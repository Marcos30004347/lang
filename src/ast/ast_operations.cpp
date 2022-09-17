#include "ast_operations.hpp"
#include "ast/ast_kind.hpp"
#include "parser/parser.hpp"

namespace ast {

Arithmetic_Operation_Add_Node* create_node_arithmetic_add(parser::Parser* parser, Node* a, Node* b) {
  assert(a && b);
  return as< Arithmetic_Operation_Add_Node* >(ast::manager_alloc(parser->ast_manager, AST_OP_BIN_ADD, a->id, b->id));
}

Arithmetic_Operation_Sub_Node* create_node_arithmetic_sub(parser::Parser* parser, Node* a, Node* b) {
  assert(a && b);
  return as< Arithmetic_Operation_Sub_Node* >(ast::manager_alloc(parser->ast_manager, AST_OP_BIN_SUB, a->id, b->id));
}

Arithmetic_Operation_Mul_Node* create_node_arithmetic_mul(parser::Parser* parser, Node* a, Node* b) {
  assert(a && b);
  return as< Arithmetic_Operation_Mul_Node* >(ast::manager_alloc(parser->ast_manager, AST_OP_BIN_MUL, a->id, b->id));
}

Arithmetic_Operation_Div_Node* create_node_arithmetic_div(parser::Parser* parser, Node* a, Node* b) {
  assert(a && b);
  return as< Arithmetic_Operation_Div_Node* >(ast::manager_alloc(parser->ast_manager, AST_OP_BIN_DIV, a->id, b->id));
}

Logical_Operation_Greater_Node* create_node_logical_greater_than(parser::Parser* parser, Node* a, Node* b) {
  assert(a && b);
  return as< Logical_Operation_Greater_Node* >(ast::manager_alloc(parser->ast_manager, AST_OP_BIN_GT, a->id, b->id));
}

Logical_Operation_GreaterEqual_Node* create_node_logical_greater_equal_than(parser::Parser* parser, Node* a, Node* b) {
  assert(a && b);
  return as< Logical_Operation_GreaterEqual_Node* >(ast::manager_alloc(parser->ast_manager, AST_OP_BIN_GE, a->id, b->id));
}

Logical_Operation_Less_Node* create_node_logical_less_than(parser::Parser* parser, Node* a, Node* b) {
  assert(a && b);
  return as< Logical_Operation_Less_Node* >(ast::manager_alloc(parser->ast_manager, AST_OP_BIN_LT, a->id, b->id));
}

Logical_Operation_LessEqual_Node* create_node_logical_less_equal_than(parser::Parser* parser, Node* a, Node* b) {
  assert(a && b);
  return as< Logical_Operation_LessEqual_Node* >(ast::manager_alloc(parser->ast_manager, AST_OP_BIN_LE, a->id, b->id));
}

Logical_Operation_Equal_Node* create_node_logical_equal_to(parser::Parser* parser, Node* a, Node* b) {
  assert(a && b);
  return as< Logical_Operation_Equal_Node* >(ast::manager_alloc(parser->ast_manager, AST_OP_BIN_EQ, a->id, b->id));
}

Logical_Operation_NotEqual_Node* create_node_logical_not_equal_to(parser::Parser* parser, Node* a, Node* b) {
  assert(a && b);
  return as< Logical_Operation_NotEqual_Node* >(ast::manager_alloc(parser->ast_manager, AST_OP_BIN_NE, a->id, b->id));
}

Member_Access_Node* create_node_member_access(parser::Parser* parser, Node* a, Node* b) {
  assert(a && b);
  return as< Member_Access_Node* >(ast::manager_alloc(parser->ast_manager, AST_OP_MEMBER_ACCESS, a->id, b->id));
}

Variable_Assignment_Node* create_node_assignment(parser::Parser* parser, Node* a, Node* b) {
  assert(a && b);
  return as< Variable_Assignment_Node* >(ast::manager_alloc(parser->ast_manager, AST_OP_BIN_ASSIGN, a->id, b->id));
}

Node* Arithmetic_Operation_Add_Node::get_left_operand(parser::Parser* parser) {
  return left_of(parser->ast_manager, this);
}

Node* Arithmetic_Operation_Add_Node::get_right_operand(parser::Parser* parser) {
  return right_of(parser->ast_manager, this);
}

Node* Arithmetic_Operation_Sub_Node::get_left_operand(parser::Parser* parser) {
  return left_of(parser->ast_manager, this);
}

Node* Arithmetic_Operation_Sub_Node::get_right_operand(parser::Parser* parser) {
  return right_of(parser->ast_manager, this);
}

Node* Arithmetic_Operation_Mul_Node::get_left_operand(parser::Parser* parser) {
  return left_of(parser->ast_manager, this);
}

Node* Arithmetic_Operation_Mul_Node::get_right_operand(parser::Parser* parser) {
  return right_of(parser->ast_manager, this);
}
Node* Arithmetic_Operation_Div_Node::get_left_operand(parser::Parser* parser) {
  return left_of(parser->ast_manager, this);
}

Node* Arithmetic_Operation_Div_Node::get_right_operand(parser::Parser* parser) {
  return right_of(parser->ast_manager, this);
}

Node* Logical_Operation_Greater_Node::get_left_operand(parser::Parser* parser) {
  return left_of(parser->ast_manager, this);
}

Node* Logical_Operation_Greater_Node::get_right_operand(parser::Parser* parser) {
  return right_of(parser->ast_manager, this);
}

Node* Logical_Operation_GreaterEqual_Node::get_left_operand(parser::Parser* parser) {
  return left_of(parser->ast_manager, this);
}

Node* Logical_Operation_GreaterEqual_Node::get_right_operand(parser::Parser* parser) {
  return right_of(parser->ast_manager, this);
}

Node* Logical_Operation_Less_Node::get_left_operand(parser::Parser* parser) {
  return left_of(parser->ast_manager, this);
}

Node* Logical_Operation_Less_Node::get_right_operand(parser::Parser* parser) {
  return right_of(parser->ast_manager, this);
}

Node* Logical_Operation_LessEqual_Node::get_left_operand(parser::Parser* parser) {
  return left_of(parser->ast_manager, this);
}

Node* Logical_Operation_LessEqual_Node::get_right_operand(parser::Parser* parser) {
  return right_of(parser->ast_manager, this);
}

Node* Logical_Operation_Equal_Node::get_left_operand(parser::Parser* parser) {
  return left_of(parser->ast_manager, this);
}

Node* Logical_Operation_NotEqual_Node::get_right_operand(parser::Parser* parser) {
  return right_of(parser->ast_manager, this);
}

Node* Member_Access_Node::get_left_operand(parser::Parser* parser) {
  return left_of(parser->ast_manager, this);
}

Node* Member_Access_Node::get_right_operand(parser::Parser* parser) {
  return right_of(parser->ast_manager, this);
}

b8 is_binary_operation(Node* a) {
  return a->kind >= __AST_BINARY_OPERATOR_START && a->kind <= __AST_BINARY_OPERATOR_END;
}

}; // namespace ast
