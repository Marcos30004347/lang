#pragma once

#include "ast.hpp"
#include "ast_literals.hpp"
#include "ast_manager.hpp"

#include "compiler/compiler.hpp"
#include "parser/parser.hpp"

namespace ast {

struct Variable_Assignment_Node : Node {
  Node* get_left_operand(parser::Parser* parser);
  Node* get_right_operand(parser::Parser* parser);
};

struct Arithmetic_Operation_Add_Node : Node {
  Node* get_left_operand(parser::Parser* parser);
  Node* get_right_operand(parser::Parser* parser);
};

struct Arithmetic_Operation_Sub_Node : Node {
  Node* get_left_operand(parser::Parser* parser);
  Node* get_right_operand(parser::Parser* parser);
};

struct Arithmetic_Operation_Mul_Node : Node {
  Node* get_left_operand(parser::Parser* parser);
  Node* get_right_operand(parser::Parser* parser);
};

struct Arithmetic_Operation_Div_Node : Node {
  Node* get_left_operand(parser::Parser* parser);
  Node* get_right_operand(parser::Parser* parser);
};

struct Logical_Operation_Greater_Node : Node {
  Node* get_left_operand(parser::Parser* parser);
  Node* get_right_operand(parser::Parser* parser);
};

struct Logical_Operation_GreaterEqual_Node : Node {
  Node* get_left_operand(parser::Parser* parser);
  Node* get_right_operand(parser::Parser* parser);
};

struct Logical_Operation_Less_Node : Node {
  Node* get_left_operand(parser::Parser* parser);
  Node* get_right_operand(parser::Parser* parser);
};

struct Logical_Operation_LessEqual_Node : Node {
  Node* get_left_operand(parser::Parser* parser);
  Node* get_right_operand(parser::Parser* parser);
};

struct Logical_Operation_Equal_Node : Node {
  Node* get_left_operand(parser::Parser* parser);
  Node* get_right_operand(parser::Parser* parser);
};

struct Logical_Operation_NotEqual_Node : Node {
  Node* get_left_operand(parser::Parser* parser);
  Node* get_right_operand(parser::Parser* parser);
};

template <> Arithmetic_Operation_Add_Node* is_instance<>(Node* node) {
  return node->kind == AST_OP_BIN_ADD ? (Arithmetic_Operation_Add_Node*)node : 0;
}

template <> Arithmetic_Operation_Sub_Node* is_instance<>(Node* node) {
  return node->kind == AST_OP_BIN_SUB ? (Arithmetic_Operation_Sub_Node*)node : 0;
}

template <> Arithmetic_Operation_Div_Node* is_instance<>(Node* node) {
  return node->kind == AST_OP_BIN_DIV ? (Arithmetic_Operation_Div_Node*)node : 0;
}

template <> Logical_Operation_Greater_Node* is_instance<>(Node* node) {
  return node->kind == AST_OP_BIN_GT ? (Logical_Operation_Greater_Node*)node : 0;
}

template <> Logical_Operation_GreaterEqual_Node* is_instance<>(Node* node) {
  return node->kind == AST_OP_BIN_GE ? (Logical_Operation_GreaterEqual_Node*)node : 0;
}

template <> Logical_Operation_Less_Node* is_instance<>(Node* node) {
  return node->kind == AST_OP_BIN_LT ? (Logical_Operation_Less_Node*)node : 0;
}

template <> Logical_Operation_LessEqual_Node* is_instance<>(Node* node) {
  return node->kind == AST_OP_BIN_LE ? (Logical_Operation_LessEqual_Node*)node : 0;
}

template <> Logical_Operation_Equal_Node* is_instance<>(Node* node) {
  return node->kind == AST_OP_BIN_EQ ? (Logical_Operation_Equal_Node*)node : 0;
}

template <> Logical_Operation_NotEqual_Node* is_instance<>(Node* node) {
  return node->kind == AST_OP_BIN_NE ? (Logical_Operation_NotEqual_Node*)node : 0;
}

template <> Variable_Assignment_Node* is_instance<>(Node* node) {
  return node->kind == AST_OP_BIN_ASSIGN ? (Variable_Assignment_Node*)node : 0;
}

Arithmetic_Operation_Add_Node* create_node_arithmetic_add(parser::Parser* parser, Node* a, Node* b);

Arithmetic_Operation_Sub_Node* create_node_arithmetic_sub(parser::Parser* parser, Node* a, Node* b);

Arithmetic_Operation_Mul_Node* create_node_arithmetic_mul(parser::Parser* parser, Node* a, Node* b);

Arithmetic_Operation_Div_Node* create_node_arithmetic_div(parser::Parser* parser, Node* a, Node* b);

Logical_Operation_Greater_Node* create_node_logical_greater_than(parser::Parser* parser, Node* a, Node* b);

Logical_Operation_GreaterEqual_Node* create_node_logical_greater_equa_thanl(parser::Parser* parser, Node* a, Node* b);

Logical_Operation_Less_Node* create_node_logical_less_than(parser::Parser* parser, Node* a, Node* b);

Logical_Operation_LessEqual_Node* create_node_logical_less_equal_than(parser::Parser* parser, Node* a, Node* b);

Logical_Operation_Equal_Node* create_node_logical_equal_to(parser::Parser* parser, Node* a, Node* b);

Logical_Operation_NotEqual_Node* create_node_logical_not_equal_to(parser::Parser* parser, Node* a, Node* b);

} // namespace ast
