#include "ast_program_point.hpp"
#include "ast/ast_kind.hpp"
#include "ast/ast_manager.hpp"
#include "compiler/symbol_table.hpp"
#include "parser/parser.hpp"

#include "assert.h"

using namespace compiler;

namespace ast {

template <> ProgramPoint_List_Node* is_instance<>(Node* node) {
  return node->kind == AST_PROGRAM_POINT ? as< ProgramPoint_List_Node* >(node) : 0;
}

template <> Declarations_List_Node* is_instance<>(Node* node) {
  return node->kind == AST_DECL_ARGS_LIST ? as< Declarations_List_Node* >(node) : 0;
}

ProgramPoint_List_Node* ProgramPoint_List_Node::get_next_program_point(parser::Parser* parser) {
  return is_instance< ProgramPoint_List_Node* >(right_of(parser->ast_manager, this));
}

Node* ProgramPoint_List_Node::get_statement(parser::Parser* parser) {
  return left_of(parser->ast_manager, this);
}

ProgramPoint_List_Node*
create_node_program_point(parser::Parser* parser, Node* point, ProgramPoint_List_Node* tail) {
  return as< ProgramPoint_List_Node* >(
      ast::manager_alloc(parser->ast_manager, AST_PROGRAM_POINT, point ? point->id : 0, tail ? tail->id : 0));
}

Declarations_List_Node* Declarations_List_Node::get_next_declaration(parser::Parser* parser) {
  return is_instance< Declarations_List_Node* >(right_of(parser->ast_manager, this));
}

Node* Declarations_List_Node::get_declaration(parser::Parser* parser) {
  return left_of(parser->ast_manager, this);
}

Declarations_List_Node*
create_node_declarations_list(parser::Parser* parser, Node* point, Declarations_List_Node* tail) {
  return as< Declarations_List_Node* >(
      ast::manager_alloc(parser->ast_manager, AST_DECL_ARGS_LIST, point ? point->id : 0, tail ? tail->id : 0));
}

void ProgramPoint_List_Node::insert(parser::Parser* parser, Node* node) {
  ProgramPoint_List_Node* pp = create_node_program_point(parser, node, NULL);

  pp->right   = this->right;
  this->right = pp->id;
}

void ProgramPoint_List_Node::push(parser::Parser* parser, Node* node) {
  ProgramPoint_List_Node* tail = this;

  while (this->right) {
    tail = tail->get_next_program_point(parser);
  }

  tail->insert(parser, node);
}

void Declarations_List_Node::push(parser::Parser* parser, Node* node) {
  Declarations_List_Node* tail = this;

  while (this->right) {
    tail = tail->get_next_declaration(parser);
  }

  tail->insert(parser, node);
}

void Declarations_List_Node::insert(parser::Parser* parser, Node* node) {
  ProgramPoint_List_Node* pp = create_node_program_point(parser, node, NULL);

  pp->right   = this->right;
  this->right = pp->id;
}

} // namespace ast
