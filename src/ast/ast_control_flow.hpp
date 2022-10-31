#pragma once

#include "ast/ast_literals.hpp"
#include "ast_kind.hpp"
#include "ast_manager.hpp"
#include "ast_program_point.hpp"

#include "compiler/compiler.hpp"
#include "parser/parser.hpp"

namespace ast {

struct If_Node_Statement : Node {
  Node*                   get_condition(ast::Manager* manager);
  ProgramPoint_List_Node* get_body(ast::Manager* manager);
};

struct Return_Node_Statement : Node {
  Node* get_expression(ast::Manager* manager);
};

struct Resume_Node_Statement : Node {
  Node* get_expression(ast::Manager* manager);
};

struct With_Node_Statement : Node {
  Node*                   get_call(ast::Manager* manager);
  Declarations_List_Node* get_list(ast::Manager* manager);
};

struct Elif_List_Node : Node {
  If_Node_Statement* get_if(ast::Manager* manager);
  Elif_List_Node*    get_elif(ast::Manager* manager);
};

struct Prompt_Statement_Node : Node {
  Literal_Symbol_Node* get_handler(ast::Manager* manager);
};

struct Bubble_Handler_Node : Node {
  Literal_Symbol_Node* get_handler(ast::Manager* manager);
};

template <> If_Node_Statement*     is_instance<>(Node* node);
template <> Elif_List_Node*        is_instance<>(Node* node);
template <> Return_Node_Statement* is_instance<>(Node* node);
template <> Resume_Node_Statement* is_instance<>(Node* node);
template <> With_Node_Statement*   is_instance<>(Node* node);
template <> Prompt_Statement_Node* is_instance<>(Node* node);
template <> Bubble_Handler_Node*   is_instance<>(Node* node);

b8 is_branching(Node* node);

Prompt_Statement_Node* create_prompt_statement(ast::Manager* m, ast::Node* h);

Bubble_Handler_Node* create_bubble_handler(ast::Manager* m, ast::Node* h);

If_Node_Statement* create_node_if_statement(ast::Manager* manager, Node* condition, Node* body);

Return_Node_Statement* create_node_return_statement(ast::Manager* manager, Node* expression);

Resume_Node_Statement* create_node_resume_statement(ast::Manager* manager, Node* expression);

With_Node_Statement* create_node_with_statement(ast::Manager* manager, ast::Node* call, Declarations_List_Node* list);

Elif_List_Node* create_node_elif_list(ast::Manager* manager, If_Node_Statement* branch, Elif_List_Node* tail);

} // namespace ast
