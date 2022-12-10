#include "ast_program_point.hpp"
#include "ast/ast_kind.hpp"
#include "ast/ast_literals.hpp"
#include "ast/ast_manager.hpp"
#include "compiler/symbol_table.hpp"
#include "parser/parser.hpp"

#include "assert.h"

using namespace compiler;

namespace ast {

template <> ProgramPoint* is_instance<>(Node* node) {
  return node && node->kind == AST_PROGRAM_POINT ? as< ProgramPoint* >(node) : 0;
}

template <> Declarations_List_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_DECL_ARGS_LIST ? as< Declarations_List_Node* >(node) : 0;
}

ProgramPoint* ProgramPoint::get_next_program_point(ast::Manager* manager) {
  return is_instance< ProgramPoint* >(right_of(manager, this));
}

Node* ProgramPoint::get_statement(ast::Manager* manager) {
  return left_of(manager, this);
}

ProgramPoint* create_node_program_point(ast::Manager* manager, Node* point, ProgramPoint* tail) {
  return as< ProgramPoint* >(ast::manager_alloc(manager, AST_PROGRAM_POINT, get_id(point), get_id(tail)));
}

Declarations_List_Node* Declarations_List_Node::get_next_declaration(ast::Manager* manager) {
  return is_instance< Declarations_List_Node* >(right_of(manager, this));
}

Node* Declarations_List_Node::get_declaration(ast::Manager* manager) {
  return left_of(manager, this);
}

Declarations_List_Node* create_node_declarations_list(ast::Manager* manager, Node* point, Declarations_List_Node* tail) {
  return as< Declarations_List_Node* >(ast::manager_alloc(manager, AST_DECL_ARGS_LIST, get_id(point), get_id(tail)));
}

ProgramPoint* ProgramPoint::insert(ast::Manager* manager, Node* node) {
  ProgramPoint* pp = create_node_program_point(manager, node, NULL);

  pp->right = this->right;

  this->right = pp->id;

  return pp;
}

ProgramPoint* ProgramPoint::concat(ast::Manager* manager, ProgramPoint* node) {
	if(ast::is_semantic_node(node) == false) {
		return this;
	}
	
  ProgramPoint* right = this;

  while (ast::is_semantic_node(right->get_next_program_point(manager))) {
    right = right->get_next_program_point(manager);
  }

  right->right = node->id;

  return node;
}

ProgramPoint* ProgramPoint::emplace(ast::Manager* manager, ProgramPoint* node) {

  ProgramPoint* right = node;

  while (ast::is_semantic_node(right->get_next_program_point(manager))) {
    right = right->get_next_program_point(manager);
  }

  right->right = this->right;

  this->right = node->id;

  return node;
}

void ProgramPoint::push(ast::Manager* manager, Node* node) {
  ProgramPoint* tail = this;

  while (ast::is_semantic_node(tail->get_next_program_point(manager))) {
    tail = tail->get_next_program_point(manager);
  }

  tail->insert(manager, node);
}

void Declarations_List_Node::set_declaration(ast::Manager* m, ast::Node* n) {
  set_left(m, this, n);
}

void Declarations_List_Node::push(ast::Manager* manager, Node* node) {
  Declarations_List_Node* tail = this;

  if (this->right) {
    tail->get_next_declaration(manager)->push(manager, node);
  } else {
    tail->insert(manager, node);
  }
}

void Declarations_List_Node::insert(ast::Manager* manager, Node* node) {
  Declarations_List_Node* pp = create_node_declarations_list(manager, node, NULL);

  pp->right = this->right;

  this->right = pp->id;
}

void ProgramPoint::set_statement(ast::Manager* m, ast::Node* node) {
  return set_left(m, this, node);
}

ProgramPoint* ProgramPoint::split(ast::Manager* manager) {
  ProgramPoint* n = this->get_next_program_point(manager);
  set_right(manager, this, 0);
  return n;
}

} // namespace ast
