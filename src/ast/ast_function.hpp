#pragma once

#include "ast.hpp"
#include "ast/ast_operations.hpp"
#include "ast_declaration.hpp"
#include "ast_kind.hpp"
#include "ast_manager.hpp"

#include "compiler/compiler.hpp"
#include "parser/parser.hpp"

namespace ast {

struct Function_Literal_Node : Node {
  Node* get_return_type(ast::Manager* manager);

  void push_argument(ast::Manager* manager, Declaration_Variable_Node* var);

  Declarations_List_Node* get_arguments(ast::Manager* manager);

  ProgramPoint_List_Node* get_body(ast::Manager* manager);
};

struct Function_Call_Node : Node {
  Node* get_function(ast::Manager* manager);

  void push_argument(ast::Manager* manager, Node* var);

  Declarations_List_Node* get_arguments(ast::Manager* manager);
};

template <> Function_Literal_Node* is_instance<>(Node* node);
template <> Function_Call_Node*    is_instance<>(Node* node);

Function_Literal_Node* create_node_function_literal(
    ast::Manager* manager, Node* arguments, Node* return_type, ProgramPoint_List_Node* body);

Function_Call_Node* create_node_function_call(ast::Manager* manager, Node* function, Node* arguments);

} // namespace ast