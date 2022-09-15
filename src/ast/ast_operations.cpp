#include "ast_operations.hpp"
#include "parser/parser.hpp"

namespace ast {

Arithmetic_Operation_Add_Node* create_node_arithmetic_add(parser::Parser* parser, Node* a, Node* b) {
  return as< Arithmetic_Operation_Add_Node* >(ast::manager_alloc(parser->ast_manager, AST_OP_BIN_ADD, 0, 0));
}

Arithmetic_Operation_Sub_Node* create_node_arithmetic_sub(parser::Parser* parser, Node* a, Node* b) {
  return as< Arithmetic_Operation_Sub_Node* >(ast::manager_alloc(parser->ast_manager, AST_OP_BIN_SUB, 0, 0));
}

Arithmetic_Operation_Mul_Node* create_node_arithmetic_mul(parser::Parser* parser, Node* a, Node* b) {
  return as< Arithmetic_Operation_Mul_Node* >(ast::manager_alloc(parser->ast_manager, AST_OP_BIN_MUL, 0, 0));
}

Arithmetic_Operation_Div_Node* create_node_arithmetic_div(parser::Parser* parser, Node* a, Node* b) {
  return as< Arithmetic_Operation_Div_Node* >(ast::manager_alloc(parser->ast_manager, AST_OP_BIN_DIV, 0, 0));
}

Logical_Operation_Greater_Node* create_node_logical_greater_than(parser::Parser* parser, Node* a, Node* b) {
  return as< Logical_Operation_Greater_Node* >(ast::manager_alloc(parser->ast_manager, AST_OP_BIN_GT, 0, 0));
}

Logical_Operation_GreaterEqual_Node* create_node_logical_greater_equa_thanl(parser::Parser* parser, Node* a, Node* b) {
  return as< Logical_Operation_GreaterEqual_Node* >(ast::manager_alloc(parser->ast_manager, AST_OP_BIN_GE, 0, 0));
}

Logical_Operation_Less_Node* create_node_logical_less_than(parser::Parser* parser, Node* a, Node* b) {
  return as< Logical_Operation_Less_Node* >(ast::manager_alloc(parser->ast_manager, AST_OP_BIN_LT, 0, 0));
}

Logical_Operation_LessEqual_Node* create_node_logical_less_equal_than(parser::Parser* parser, Node* a, Node* b) {
  return as< Logical_Operation_LessEqual_Node* >(ast::manager_alloc(parser->ast_manager, AST_OP_BIN_LE, 0, 0));
}

Logical_Operation_Equal_Node* create_node_logical_equal_to(parser::Parser* parser, Node* a, Node* b) {
  return as< Logical_Operation_Equal_Node* >(ast::manager_alloc(parser->ast_manager, AST_OP_BIN_EQ, 0, 0));
}

Logical_Operation_NotEqual_Node* create_node_logical_not_equal_to(parser::Parser* parser, Node* a, Node* b) {
  return as< Logical_Operation_NotEqual_Node* >(ast::manager_alloc(parser->ast_manager, AST_OP_BIN_NE, 0, 0));
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

}; // namespace ast
