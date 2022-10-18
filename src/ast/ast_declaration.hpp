#pragma once

#include "ast_kind.hpp"
#include "ast_literals.hpp"
#include "ast_manager.hpp"

#include "compiler/compiler.hpp"
#include "parser/parser.hpp"

namespace ast {

struct Declaration_Constant_Node : Node {
  Literal_Symbol_Node* get_symbol(ast::Manager* manager);
  void                 set_symbol(ast::Manager* manager, ast::Literal_Symbol_Node* symbol);

  Node* get_type(ast::Manager* manager);
  void  set_type(ast::Manager* manager, ast::Node* type);
};

struct Declaration_Variable_Node : Node {
  Literal_Symbol_Node* get_symbol(ast::Manager* manager);
  void                 set_symbol(ast::Manager* manager, ast::Literal_Symbol_Node* symbol);

  Node* get_type(ast::Manager* manager);
  void  set_type(ast::Manager* manager, ast::Node* type);
};

template <> Declaration_Constant_Node* is_instance<>(Node* node);
template <> Declaration_Variable_Node* is_instance<>(Node* node);

Declaration_Constant_Node* create_constant_declaration(ast::Manager* manager, Literal_Symbol_Node* name, Node* type);
Declaration_Variable_Node* create_variable_declaration(ast::Manager* manager, Literal_Symbol_Node* name, Node* type);

b8 is_declaration_node(ast::Node* node);

} // namespace ast
