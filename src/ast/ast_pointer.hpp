#pragma once

#include "ast/ast_kind.hpp"
#include "ast_literals.hpp"
#include "ast_manager.hpp"

namespace ast {

struct Type_Pointer_Node : Node {
  Node* type;
  Node* get_pointer_type(Manager* m);
};

struct Pointer_Value_Node : Node {
  Node* symbol;
  Node* get_variable(ast::Manager* m);
};

template <> Type_Pointer_Node*  is_instance<>(Node* node);
template <> Pointer_Value_Node* is_instance<>(Node* node);

Type_Pointer_Node*  create_node_type_pointer(Manager* m, Node* type);
Pointer_Value_Node* create_node_pointer_value(Manager* m, Node* symbol);

} // namespace ast
