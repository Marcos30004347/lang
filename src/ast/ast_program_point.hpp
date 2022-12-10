#pragma once

#include "ast/ast_kind.hpp"
#include "ast_manager.hpp"

#include "compiler/compiler.hpp"
#include "parser/parser.hpp"

namespace ast {

struct ProgramPoint : Node {
  Node* get_statement(ast::Manager* m);

  ProgramPoint* get_next_program_point(ast::Manager* p);

  void set_statement(ast::Manager* m, ast::Node* node);

  ProgramPoint* emplace(ast::Manager* manager, ProgramPoint* node);
  ProgramPoint* concat(ast::Manager* manager, ProgramPoint* node);
  ProgramPoint* insert(ast::Manager* manager, Node* node);

  void push(ast::Manager* parsre, Node* node);

  ProgramPoint* split(ast::Manager* manager);
};

struct Declarations_List_Node : Node {
  Node* get_declaration(ast::Manager* p);

  Declarations_List_Node* get_next_declaration(ast::Manager* p);

  void insert(ast::Manager* manager, Node* node);

  void push(ast::Manager* parsre, Node* node);
	void set_declaration(ast::Manager* m, ast::Node* n);
};

template <> ProgramPoint* is_instance<>(Node* node);
template <> Declarations_List_Node* is_instance<>(Node* node);

ProgramPoint* create_node_program_point(ast::Manager* manager, Node* point, ProgramPoint* tail);
Declarations_List_Node* create_node_declarations_list(ast::Manager* manager, Node* point, Declarations_List_Node* tail);

} // namespace ast
