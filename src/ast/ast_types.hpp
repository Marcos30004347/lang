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

struct Type_Evidence_Context_Node : Node {};

struct Type_Arrow_Node : Node {
  Node* get_from_type(ast::Manager* manager);
  Node* get_to_type(ast::Manager* manager);
};

struct Type_Variable_Node : Node {
  Literal_Symbol_Node* get_symbol(ast::Manager* manager);
};

struct Cast_Type_Node : Node {
  Node* get_expr(ast::Manager* manager);
  Node* get_to_type(ast::Manager* manager);
};

template <> Type_Any_Node*              is_instance<>(Node* node);
template <> Type_Arrow_Node*            is_instance<>(Node* node);
template <> Type_Int32_Node*            is_instance<>(Node* node);
template <> Type_Unit_Node*             is_instance<>(Node* node);
template <> Type_Struct_Node*           is_instance<>(Node* node);
template <> Type_Variable_Node*         is_instance<>(Node* node);
template <> Type_Evidence_Context_Node* is_instance<>(Node* node);
template <> Cast_Type_Node*             is_instance<>(Node* node);

Type_Any_Node* create_node_type_any(ast::Manager* manager);

Type_Int32_Node* create_node_type_i32(ast::Manager* manager);

Type_Unit_Node* create_node_type_unit(ast::Manager* manager);

Type_Arrow_Node* create_node_type_arrow(ast::Manager* manager, Node* a, Node* b);

Type_Struct_Node* create_node_type_struct(ast::Manager* manager);

Type_Variable_Node* create_node_type_variable(ast::Manager* manager, Literal_Symbol_Node* symbol);

Type_Evidence_Context_Node* create_node_type_evidence_context(ast::Manager* manager);

Cast_Type_Node* create_node_cast_type(ast::Manager* manager, ast::Node* type, ast::Node* expr);

} // namespace ast
