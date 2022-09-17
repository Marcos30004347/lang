#pragma once

#include "ast/ast_kind.hpp"
#include "ast_manager.hpp"

#include "compiler/compiler.hpp"
#include "parser/parser.hpp"

namespace ast {

struct ProgramPoint_List_Node : Node {
  Node*                   get_statement(parser::Parser* p);
  ProgramPoint_List_Node* get_next_program_point(parser::Parser* p);
};

struct Declarations_List_Node : Node {
  Node*                   get_declaration(parser::Parser* p);
  Declarations_List_Node* get_next_declaration(parser::Parser* p);
};

template <> ProgramPoint_List_Node* is_instance<>(Node* node) {
  return node->kind == AST_PROGRAM_POINT ? as< ProgramPoint_List_Node* >(node) : 0;
}

template <> Declarations_List_Node* is_instance<>(Node* node) {
  return node->kind == AST_DECL_ARGS_LIST ? as< Declarations_List_Node* >(node) : 0;
}

ProgramPoint_List_Node* create_node_program_point(parser::Parser* parser, Node* point, ProgramPoint_List_Node* tail);
Declarations_List_Node* create_node_declarations_list(parser::Parser* parser, Node* point, Declarations_List_Node* tail);

} // namespace ast
