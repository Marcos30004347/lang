#include "ast_function.hpp"
#include "ast/ast_kind.hpp"
#include "ast/ast_manager.hpp"
#include "ast/ast_program_point.hpp"

namespace ast {

template <> Function_Literal_Node* is_instance<>(Node* node) {
  return node->kind == AST_FUNCTION_LITERAL ? as< Function_Literal_Node* >(node) : 0;
}

template <> Function_Call_Node* is_instance<>(Node* node) {
  return node->kind == AST_FUNCTION_CALL ? as< Function_Call_Node* >(node) : 0;
}

Function_Literal_Node*
create_node_function_literal(parser::Parser* parser, Declarations_List_Node* arguments, Node* return_type, ProgramPoint_List_Node* body) {
  Node* signature = ast::manager_alloc(parser->ast_manager, AST_FUN_SIGNATURE, arguments->id, return_type->id);
  return as< Function_Literal_Node* >(ast::manager_alloc(parser->ast_manager, AST_FUNCTION_LITERAL, signature->id, body->id));
}

Function_Call_Node* create_node_function_call(parser::Parser* parser, Node* function, Declarations_List_Node* arguments) {
  return as< Function_Call_Node* >(ast::manager_alloc(parser->ast_manager, AST_FUNCTION_LITERAL, function->id, arguments->id));
}

ProgramPoint_List_Node* Function_Literal_Node::get_arguments(parser::Parser* parser) {
  Node* signature = left_of(parser->ast_manager, this);
  return is_instance< ProgramPoint_List_Node* >(left_of(parser->ast_manager, signature));
}

ProgramPoint_List_Node* Function_Literal_Node::get_body(parser::Parser* parser) {
  return is_instance< ProgramPoint_List_Node* >(right_of(parser->ast_manager, this));
}

Node* Function_Literal_Node::get_return_type(parser::Parser* parser) {
  Node* signature = left_of(parser->ast_manager, this);
  return right_of(parser->ast_manager, signature);
}

ProgramPoint_List_Node* Function_Call_Node::get_arguments(parser::Parser* parser) {
  Node* signature = left_of(parser->ast_manager, this);
  return is_instance< ProgramPoint_List_Node* >(left_of(parser->ast_manager, signature));
}

Node* Function_Call_Node::get_function(parser::Parser* parser) {
  return right_of(parser->ast_manager, this);
}

}; // namespace ast
