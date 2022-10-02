#include "ast_types.hpp"
#include "ast/ast_kind.hpp"
#include "ast/ast_literals.hpp"
#include "ast/ast_manager.hpp"
#include "parser/parser.hpp"

#include "assert.h"

using namespace compiler;

namespace ast {

template <> Type_Any_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_TYPE_ANY ? as< Type_Any_Node* >(node) : 0;
}

template <> Type_Arrow_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_TYPE_ARROW ? as< Type_Arrow_Node* >(node) : 0;
}

template <> Type_Int32_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_TYPE_ARROW ? as< Type_Int32_Node* >(node) : 0;
}

template <> Type_Unit_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_TYPE_UNIT ? as< Type_Unit_Node* >(node) : 0;
}

template <> Type_Struct_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_TYPE_STRUCT ? as< Type_Struct_Node* >(node) : 0;
}

template <> Type_Variable_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_TYPE_VARIABLE ? as< Type_Variable_Node* >(node) : 0;
}

Type_Any_Node* create_node_type_any(ast::Manager* manager) {
  return as< Type_Any_Node* >(ast::manager_alloc(manager, AST_TYPE_ANY, 0, 0));
}

Type_Int32_Node* create_node_type_i32(ast::Manager* manager) {
  return as< Type_Int32_Node* >(ast::manager_alloc(manager, AST_TYPE_I32, 0, 0));
}

Type_Unit_Node* create_node_type_unit(ast::Manager* manager) {
  return as< Type_Unit_Node* >(ast::manager_alloc(manager, AST_TYPE_UNIT, 0, 0));
}

Type_Arrow_Node* create_node_type_arrow(ast::Manager* manager, Node* from, Node* to) {
  assert(from && to);
  return as< Type_Arrow_Node* >(ast::manager_alloc(manager, AST_TYPE_ARROW, from->id, to->id));
}

Type_Struct_Node* create_node_type_struct(ast::Manager* manager) {
  return as< Type_Struct_Node* >(ast::manager_alloc(manager, AST_TYPE_STRUCT, 0, 0));
}

Type_Variable_Node* create_node_type_variable(ast::Manager* manager, Literal_Symbol_Node* symbol) {
  assert(symbol);
  return as< Type_Variable_Node* >(ast::manager_alloc(manager, AST_TYPE_VARIABLE, symbol->id, 0));
}

Node* Type_Arrow_Node::get_from_type(ast::Manager* manager) {
  return left_of(manager, this);
}

Node* Type_Arrow_Node::get_to_type(ast::Manager* manager) {
  return right_of(manager, this);
}

} // namespace ast
