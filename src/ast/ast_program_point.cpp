#include "ast_program_point.hpp"
#include "ast/ast_kind.hpp"
#include "ast/ast_manager.hpp"
#include "compiler/symbol_table.hpp"
#include "parser/parser.hpp"

using namespace compiler;

namespace ast {

ProgramPoint_List_Node* ProgramPoint_List_Node::get_next_program_point(parser::Parser* parser) {
  return is_instance< ProgramPoint_List_Node* >(right_of(parser->ast_manager, this));
}

Node* ProgramPoint_List_Node::get_statement(parser::Parser* parser) {
  return left_of(parser->ast_manager, this);
}

ProgramPoint_List_Node* create_node_program_point(parser::Parser* parser, Node* point, ProgramPoint_List_Node* tail) {
  return as< ProgramPoint_List_Node* >(ast::manager_alloc(parser->ast_manager, AST_PROGRAM_POINT, point ? point->id : 0, tail ? tail->id : 0));
}

Declarations_List_Node* Declarations_List_Node::get_next_declaration(parser::Parser* parser) {
  return is_instance< Declarations_List_Node* >(right_of(parser->ast_manager, this));
}

Node* Declarations_List_Node::get_declaration(parser::Parser* parser) {
  return left_of(parser->ast_manager, this);
}

Declarations_List_Node* create_node_declarations_list(parser::Parser* parser, Node* point, Declarations_List_Node* tail) {
  return as< Declarations_List_Node* >(ast::manager_alloc(parser->ast_manager, AST_DECL_ARGS_LIST, point ? point->id : 0, tail ? tail->id : 0));
}

} // namespace ast
