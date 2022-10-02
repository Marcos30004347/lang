#pragma once

#include "ast/ast_literals.hpp"
#include "ast_manager.hpp"

#include "ast/ast_program_point.hpp"

#include "compiler/compiler.hpp"
#include "parser/parser.hpp"

namespace ast {

struct Type_Any_Node : Node {};
struct Type_Int32_Node : Node {};
struct Type_Unit_Node : Node {};
struct Type_Struct_Node : Node {};

struct Type_Arrow_Node : Node {
  Node* get_from_type(ast::Manager* parser);
  Node* get_to_type(ast::Manager* parser);
};

struct Type_Variable_Node : Node {
  Literal_Symbol_Node* get_symbol(ast::Manager* parser);
};

template <> Type_Any_Node*      is_instance<>(Node* node);
template <> Type_Arrow_Node*    is_instance<>(Node* node);
template <> Type_Int32_Node*    is_instance<>(Node* node);
template <> Type_Unit_Node*     is_instance<>(Node* node);
template <> Type_Struct_Node*   is_instance<>(Node* node);
template <> Type_Variable_Node* is_instance<>(Node* node);

Type_Any_Node* create_node_type_any(ast::Manager* parser);

Type_Int32_Node* create_node_type_i32(ast::Manager* parser);

Type_Unit_Node* create_node_type_unit(ast::Manager* parser);

Type_Arrow_Node* create_node_type_arrow(ast::Manager* parser, Node* a, Node* b);

Type_Struct_Node* create_node_type_struct(ast::Manager* parser);

Type_Variable_Node* create_node_type_variable(ast::Manager* parser, Literal_Symbol_Node* symbol);

} // namespace ast
