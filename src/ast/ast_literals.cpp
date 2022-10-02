#include "ast_literals.hpp"
#include "ast/ast_kind.hpp"
#include "ast/ast_manager.hpp"
#include "ast/ast_program_point.hpp"
#include "compiler/symbol_table.hpp"
#include "lexer.hpp"
#include "parser/parser.hpp"

#include "assert.h"

using namespace compiler;
using namespace parser;

namespace ast {

template <> Literal_Undefined_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_UNDEFINED_NODE ? (Literal_Undefined_Node*)node : 0;
}

template <> Literal_Nothing_Node* is_instance<>(Node* node) {
  return node->kind == AST_NULL_NODE ? (Literal_Nothing_Node*)node : 0;
}

template <> Literal_Symbol_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_SYMBOL_LITERAL ? (Literal_Symbol_Node*)node : 0;
}

template <> Literal_Natural_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_NATURAL_LITERAL ? (Literal_Natural_Node*)node : 0;
}

template <> Literal_True_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_TRUE_LITERAL ? (Literal_True_Node*)node : 0;
}

template <> Literal_False_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_FALSE_LITERAL ? (Literal_False_Node*)node : 0;
}

template <> Literal_Struct_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_STRUCT_LITERAL ? (Literal_Struct_Node*)node : 0;
}

Literal_False_Node* create_node_literal_false(ast::Manager* manager) {
  return as< Literal_False_Node* >(ast::manager_alloc(manager, AST_FALSE_LITERAL, 0, 0));
}

Literal_True_Node* create_node_literal_true(ast::Manager* manager) {
  return as< Literal_True_Node* >(ast::manager_alloc(manager, AST_TRUE_LITERAL, 0, 0));
}

Literal_Undefined_Node* create_node_literal_undefined(ast::Manager* manager) {
  return as< Literal_Undefined_Node* >(ast::manager_alloc(manager, AST_UNDEFINED_NODE, 0, 0));
}

Literal_Nothing_Node* create_node_literal_nothing(ast::Manager* manager) {
  return as< Literal_Nothing_Node* >(ast::manager_alloc(manager, AST_NULL_NODE, 0, 0));
}

Literal_Natural_Node* create_node_literal_natural(ast::Manager* manager, symbol::Symbol val) {
  return as< Literal_Natural_Node* >(ast::manager_alloc(manager, AST_NATURAL_LITERAL, val.id, 0));
}

Literal_Symbol_Node* create_node_literal_symbol(ast::Manager* manager, compiler::symbol::Symbol val) {
  return as< Literal_Symbol_Node* >(ast::manager_alloc(manager, AST_SYMBOL_LITERAL, val.id, 0));
}

compiler::symbol::Symbol Literal_Symbol_Node::get_symbol(ast::Manager* manager) {
  symbol::Symbol sym = symbol::get_symbol(manager->symbol_table, left_id_of(manager, this));

  assert(sym.id != symbol::empty(manager->symbol_table).id);

  return sym;
}

compiler::symbol::Id Literal_Symbol_Node::get_symbol_id() {
  return this->left;
}

compiler::symbol::Symbol Literal_Natural_Node::get_symbol(ast::Manager* manager) {
  symbol::Symbol sym = symbol::get_symbol(manager->symbol_table, left_id_of(manager, this));

  assert(sym.id != symbol::empty(manager->symbol_table).id);

  return sym;
}

Literal_Struct_Node* create_node_literal_struct(ast::Manager* manager, ProgramPoint_List_Node* members) {
  return as< Literal_Struct_Node* >(ast::manager_alloc(manager, AST_STRUCT_LITERAL, members->id, members ? members->id : 0));
}

ProgramPoint_List_Node* Literal_Struct_Node::get_members(ast::Manager* manager) {
  if (this->left == 0) {
    return NULL;
  }

  ProgramPoint_List_Node* members = is_instance< ProgramPoint_List_Node* >(left_of(manager, this));

  assert(members);

  return members;
}

b8 is_semantic_node(Node* n) {
  return n && !is_instance< Literal_Nothing_Node* >(n);
}

void replace_ocurrences(ast::Manager* manager, ast::Node* node, ast::Literal_Symbol_Node* from, ast::Literal_Symbol_Node* to) {
  if (!ast::is_semantic_node(node)) {
    return;
  }

  if (ast::Literal_Symbol_Node* sym = ast::is_instance< Literal_Symbol_Node* >(node)) {
    if (sym->get_symbol_id() == from->get_symbol_id()) {
      node->left = to->id;
    }
  }

  replace_ocurrences(manager, left_of(manager, node), from, to);
  replace_ocurrences(manager, right_of(manager, node), from, to);
}
void replace_ocurrences(ast::Manager* manager, ast::Node* node, symbol::Symbol from, symbol::Symbol to) {
  if (!ast::is_semantic_node(node)) {
    return;
  }

  if (ast::Literal_Symbol_Node* sym = ast::is_instance< Literal_Symbol_Node* >(node)) {
    if (sym->get_symbol_id() == from.id) {
      node->left = to.id;
    }
  }

  replace_ocurrences(manager, left_of(manager, node), from, to);
  replace_ocurrences(manager, right_of(manager, node), from, to);
}

} // namespace ast

#include "assert.h"
