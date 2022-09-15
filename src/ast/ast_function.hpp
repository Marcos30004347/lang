#pragma once

#include "ast_declaration.hpp"
#include "ast_kind.hpp"
#include "ast_manager.hpp"
#include "ast_program_point.hpp"

#include "compiler/compiler.hpp"
#include "parser/parser.hpp"

namespace ast {

struct Function_Literal_Node : Node {
  Node* get_return_type(parser::Parser* parser);

  ProgramPoint_List_Node* get_arguments(parser::Parser* parser);

  ProgramPoint_List_Node* get_body(parser::Parser* parser);
};

struct Function_Call_Node : Node {
  Node* get_function(parser::Parser* parser);

  ProgramPoint_List_Node* get_arguments(parser::Parser* parser);
};

template <> Function_Literal_Node* is_instance<>(Node* node) {
  return node->kind == AST_FUNCTION_LITERAL ? as< Function_Literal_Node* >(node) : 0;
}

Function_Literal_Node*
create_node_function_literal(parser::Parser* parser, ProgramPoint_List_Node* arguments, Node* return_type, ProgramPoint_List_Node* body);

Function_Call_Node* create_node_function_call(parser::Parser* parser, Node* function, ProgramPoint_List_Node* body);

} // namespace ast
