#pragma once

#include "ast/ast_kind.hpp"
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

struct Member_Access_Node : Node {
  Node* get_left_operand(parser::Parser* parser);
  Node* get_right_operand(parser::Parser* parser);
};

template <> Arithmetic_Operation_Add_Node*       is_instance<>(Node* node);
template <> Arithmetic_Operation_Sub_Node*       is_instance<>(Node* node);
template <> Arithmetic_Operation_Div_Node*       is_instance<>(Node* node);
template <> Logical_Operation_Greater_Node*      is_instance<>(Node* node);
template <> Logical_Operation_GreaterEqual_Node* is_instance<>(Node* node);
template <> Logical_Operation_Less_Node*         is_instance<>(Node* node);
template <> Logical_Operation_LessEqual_Node*    is_instance<>(Node* node);
template <> Logical_Operation_Equal_Node*        is_instance<>(Node* node);
template <> Logical_Operation_NotEqual_Node*     is_instance<>(Node* node);
template <> Variable_Assignment_Node*            is_instance<>(Node* node);
template <> Member_Access_Node*                  is_instance<>(Node* node);

Arithmetic_Operation_Add_Node* create_node_arithmetic_add(parser::Parser* parser, Node* a, Node* b);

Arithmetic_Operation_Sub_Node* create_node_arithmetic_sub(parser::Parser* parser, Node* a, Node* b);

Arithmetic_Operation_Mul_Node* create_node_arithmetic_mul(parser::Parser* parser, Node* a, Node* b);

Arithmetic_Operation_Div_Node* create_node_arithmetic_div(parser::Parser* parser, Node* a, Node* b);

Logical_Operation_Greater_Node* create_node_logical_greater_than(parser::Parser* parser, Node* a, Node* b);

Logical_Operation_GreaterEqual_Node*
create_node_logical_greater_equal_than(parser::Parser* parser, Node* a, Node* b);

Logical_Operation_Less_Node* create_node_logical_less_than(parser::Parser* parser, Node* a, Node* b);

Logical_Operation_LessEqual_Node*
create_node_logical_less_equal_than(parser::Parser* parser, Node* a, Node* b);

Logical_Operation_Equal_Node* create_node_logical_equal_to(parser::Parser* parser, Node* a, Node* b);

Logical_Operation_NotEqual_Node* create_node_logical_not_equal_to(parser::Parser* parser, Node* a, Node* b);

Member_Access_Node* create_node_member_access(parser::Parser* parser, Node* a, Node* b);

Variable_Assignment_Node* create_node_assignment(parser::Parser* parser, Node* a, Node* b);

b8 is_binary_operation(Node* a);

} // namespace ast
