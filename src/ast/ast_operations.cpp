#include "ast_operations.hpp"
#include "ast/ast_literals.hpp"
#include "ast_kind.hpp"
#include "ast_manager.hpp"

#include "assert.h"

namespace ast {

template <> Arithmetic_Operation_Add_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_OP_BIN_ADD ? (Arithmetic_Operation_Add_Node*)node : 0;
}

template <> Arithmetic_Operation_Sub_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_OP_BIN_SUB ? (Arithmetic_Operation_Sub_Node*)node : 0;
}

template <> Arithmetic_Operation_Div_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_OP_BIN_DIV ? (Arithmetic_Operation_Div_Node*)node : 0;
}

template <> Logical_Operation_Greater_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_OP_BIN_GT ? (Logical_Operation_Greater_Node*)node : 0;
}

template <> Logical_Operation_GreaterEqual_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_OP_BIN_GE ? (Logical_Operation_GreaterEqual_Node*)node : 0;
}

template <> Logical_Operation_Less_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_OP_BIN_LT ? (Logical_Operation_Less_Node*)node : 0;
}

template <> Logical_Operation_LessEqual_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_OP_BIN_LE ? (Logical_Operation_LessEqual_Node*)node : 0;
}

template <> Logical_Operation_Equal_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_OP_BIN_EQ ? (Logical_Operation_Equal_Node*)node : 0;
}

template <> Logical_Operation_NotEqual_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_OP_BIN_NE ? (Logical_Operation_NotEqual_Node*)node : 0;
}

template <> Variable_Assignment_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_OP_BIN_ASSIGN ? (Variable_Assignment_Node*)node : 0;
}

template <> Member_Access_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_OP_MEMBER_ACCESS ? (Member_Access_Node*)node : 0;
}

Arithmetic_Operation_Add_Node* create_node_arithmetic_add(ast::Manager* manager, Node* a, Node* b) {
  assert(a && b);
  return as< Arithmetic_Operation_Add_Node* >(ast::manager_alloc(manager, AST_OP_BIN_ADD, a->id, b->id));
}

Arithmetic_Operation_Sub_Node* create_node_arithmetic_sub(ast::Manager* manager, Node* a, Node* b) {
  assert(a && b);
  return as< Arithmetic_Operation_Sub_Node* >(ast::manager_alloc(manager, AST_OP_BIN_SUB, a->id, b->id));
}

Arithmetic_Operation_Mul_Node* create_node_arithmetic_mul(ast::Manager* manager, Node* a, Node* b) {
  assert(a && b);
  return as< Arithmetic_Operation_Mul_Node* >(ast::manager_alloc(manager, AST_OP_BIN_MUL, a->id, b->id));
}

Arithmetic_Operation_Div_Node* create_node_arithmetic_div(ast::Manager* manager, Node* a, Node* b) {
  assert(a && b);
  return as< Arithmetic_Operation_Div_Node* >(ast::manager_alloc(manager, AST_OP_BIN_DIV, a->id, b->id));
}

Logical_Operation_Greater_Node* create_node_logical_greater_than(ast::Manager* manager, Node* a, Node* b) {
  assert(a && b);
  return as< Logical_Operation_Greater_Node* >(ast::manager_alloc(manager, AST_OP_BIN_GT, a->id, b->id));
}

Logical_Operation_GreaterEqual_Node*
create_node_logical_greater_equal_than(ast::Manager* manager, Node* a, Node* b) {
  assert(a && b);
  return as< Logical_Operation_GreaterEqual_Node* >(ast::manager_alloc(manager, AST_OP_BIN_GE, a->id, b->id));
}

Logical_Operation_Less_Node* create_node_logical_less_than(ast::Manager* manager, Node* a, Node* b) {
  assert(a && b);
  return as< Logical_Operation_Less_Node* >(ast::manager_alloc(manager, AST_OP_BIN_LT, a->id, b->id));
}

Logical_Operation_LessEqual_Node* create_node_logical_less_equal_than(ast::Manager* manager, Node* a, Node* b) {
  assert(a && b);
  return as< Logical_Operation_LessEqual_Node* >(ast::manager_alloc(manager, AST_OP_BIN_LE, a->id, b->id));
}

Logical_Operation_Equal_Node* create_node_logical_equal_to(ast::Manager* manager, Node* a, Node* b) {
  assert(a && b);
  return as< Logical_Operation_Equal_Node* >(ast::manager_alloc(manager, AST_OP_BIN_EQ, a->id, b->id));
}

Logical_Operation_NotEqual_Node* create_node_logical_not_equal_to(ast::Manager* manager, Node* a, Node* b) {
  assert(a && b);
  return as< Logical_Operation_NotEqual_Node* >(ast::manager_alloc(manager, AST_OP_BIN_NE, a->id, b->id));
}

Member_Access_Node* create_node_member_access(ast::Manager* manager, Node* a, Node* b) {
  return as< Member_Access_Node* >(ast::manager_alloc(manager, AST_OP_MEMBER_ACCESS, get_id(a), get_id(b)));
}

Variable_Assignment_Node* create_node_assignment(ast::Manager* manager, Node* a, Node* b) {
  assert(a && b);
  return as< Variable_Assignment_Node* >(ast::manager_alloc(manager, AST_OP_BIN_ASSIGN, a->id, b->id));
}

Node* Arithmetic_Operation_Add_Node::get_left_operand(ast::Manager* manager) {
  return left_of(manager, this);
}

Node* Arithmetic_Operation_Add_Node::get_right_operand(ast::Manager* manager) {
  return right_of(manager, this);
}

Node* Arithmetic_Operation_Sub_Node::get_left_operand(ast::Manager* manager) {
  return left_of(manager, this);
}

Node* Arithmetic_Operation_Sub_Node::get_right_operand(ast::Manager* manager) {
  return right_of(manager, this);
}

Node* Arithmetic_Operation_Mul_Node::get_left_operand(ast::Manager* manager) {
  return left_of(manager, this);
}

Node* Arithmetic_Operation_Mul_Node::get_right_operand(ast::Manager* manager) {
  return right_of(manager, this);
}
Node* Arithmetic_Operation_Div_Node::get_left_operand(ast::Manager* manager) {
  return left_of(manager, this);
}

Node* Arithmetic_Operation_Div_Node::get_right_operand(ast::Manager* manager) {
  return right_of(manager, this);
}

Node* Logical_Operation_Greater_Node::get_left_operand(ast::Manager* manager) {
  return left_of(manager, this);
}

Node* Logical_Operation_Greater_Node::get_right_operand(ast::Manager* manager) {
  return right_of(manager, this);
}

Node* Logical_Operation_GreaterEqual_Node::get_left_operand(ast::Manager* manager) {
  return left_of(manager, this);
}

Node* Logical_Operation_GreaterEqual_Node::get_right_operand(ast::Manager* manager) {
  return right_of(manager, this);
}

Node* Logical_Operation_Less_Node::get_left_operand(ast::Manager* manager) {
  return left_of(manager, this);
}

Node* Logical_Operation_Less_Node::get_right_operand(ast::Manager* manager) {
  return right_of(manager, this);
}

Node* Logical_Operation_LessEqual_Node::get_left_operand(ast::Manager* manager) {
  return left_of(manager, this);
}

Node* Logical_Operation_LessEqual_Node::get_right_operand(ast::Manager* manager) {
  return right_of(manager, this);
}

Node* Logical_Operation_Equal_Node::get_left_operand(ast::Manager* manager) {
  return left_of(manager, this);
}

Node* Logical_Operation_NotEqual_Node::get_right_operand(ast::Manager* manager) {
  return right_of(manager, this);
}

Node* Member_Access_Node::get_object(ast::Manager* manager) {
  return left_of(manager, this);
}

Node* Member_Access_Node::get_access(ast::Manager* manager) {
  ast::Node* right = right_of(manager, this);

  if (!ast::is_semantic_node(right)) {
    return create_node_literal_nothing(manager);
  }

  return left_of(manager, right);
}

Member_Access_Node* Member_Access_Node::get_next_accesses(ast::Manager* manager) {
  return ast::is_instance< Member_Access_Node* >(right_of(manager, this));
}

Node* Variable_Assignment_Node::get_left_operand(ast::Manager* manager) {
  return left_of(manager, this);
}

Node* Variable_Assignment_Node::get_right_operand(ast::Manager* manager) {
  return right_of(manager, this);
}

void Variable_Assignment_Node::set_left_operand(ast::Manager* manager, ast::Node* node) {
  return set_left(manager, this, node);
}

void Variable_Assignment_Node::set_right_operand(ast::Manager* manager, ast::Node* node) {
  return set_right(manager, this, node);
}

b8 is_binary_operation(Node* a) {
  return a->kind >= __AST_BINARY_OPERATOR_START && a->kind <= __AST_BINARY_OPERATOR_END;
}

}; // namespace ast
