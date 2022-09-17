#pragma once

#include "ast_kind.hpp"
#include "ast_manager.hpp"
#include "ast_program_point.hpp"

#include "compiler/compiler.hpp"
#include "parser/parser.hpp"

namespace ast {

struct If_Node_Statement : Node {
  Node* get_condition(parser::Parser* parser);
  Node* get_body(parser::Parser* parser);
};

struct Return_Node_Statement : Node {
  Node* get_expression(parser::Parser* parser);
};

struct Elif_List_Node : Node {
  If_Node_Statement* get_if(parser::Parser* parser);
  Elif_List_Node*    get_elif(parser::Parser* parser);
};

template <> If_Node_Statement*     is_instance<>(Node* node);
template <> Elif_List_Node*        is_instance<>(Node* node);
template <> Return_Node_Statement* is_instance<>(Node* node);

b8 is_branching(Node* node);

If_Node_Statement* create_node_if_statement(parser::Parser* parser, Node* condition, Node* body);

Return_Node_Statement* create_node_return_statement(parser::Parser* parser, Node* expression);

Elif_List_Node* create_node_elif_list(parser::Parser* parser, If_Node_Statement* branch, Elif_List_Node* tail);

} // namespace ast
