#include "ast_pointer.hpp"
#include "ast/ast_kind.hpp"
#include "ast/ast_manager.hpp"

namespace ast {

template <> Type_Pointer_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_TYPE_POINTER ? (Type_Pointer_Node*)node : 0;
}

template <> Pointer_Value_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_OP_POINTER_VALUE ? (Pointer_Value_Node*)node : 0;
}

Type_Pointer_Node* create_node_type_pointer(Manager* m, Node* type) {
  return as< Type_Pointer_Node* >(ast::manager_alloc(m, AST_TYPE_POINTER, get_id(type), 0));
}

Pointer_Value_Node* create_node_pointer_value(Manager* m, Node* symbol) {
  return as< Pointer_Value_Node* >(ast::manager_alloc(m, AST_OP_POINTER_VALUE, get_id(symbol), 0));
}

Node* Type_Pointer_Node::get_pointer_type(Manager* m) {
  return left_of(m, this);
}

Node* Pointer_Value_Node::get_variable(Manager* m) {
  return left_of(m, this);
}
} // namespace ast
