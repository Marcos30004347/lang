#pragma once

#include "ast_kind.hpp"
#include "ast_literals.hpp"
#include "ast_manager.hpp"

#include "compiler/compiler.hpp"
#include "parser/parser.hpp"

namespace ast {

struct Declaration_Constant_Node : Node {
  Literal_Symbol_Node* get_symbol(parser::Parser* parser);

  Node* get_type(parser::Parser* parser);
};

struct Declaration_Variable_Node : Node {
  Literal_Symbol_Node* get_symbol(parser::Parser* parser);

  Node* get_type(parser::Parser* parser);
};

template <> Declaration_Constant_Node* is_instance<>(Node* node);
template <> Declaration_Variable_Node* is_instance<>(Node* node);

Declaration_Constant_Node*
create_constant_declaration(parser::Parser* parser, Literal_Symbol_Node* name, Node* type);
Declaration_Variable_Node*
create_variable_declaration(parser::Parser* parser, Literal_Symbol_Node* name, Node* type);

b8 is_declaration_node(ast::Node* node);

} // namespace ast
