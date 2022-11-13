#include "ast_function.hpp"
#include "ast/ast_declaration.hpp"
#include "ast/ast_kind.hpp"
#include "ast/ast_literals.hpp"
#include "ast/ast_manager.hpp"
#include "ast/ast_operations.hpp"
#include "ast/ast_program_point.hpp"

#include <assert.h>

namespace ast {

template <> Function_Literal_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_FUNCTION_LITERAL ? as< Function_Literal_Node* >(node) : 0;
}

template <> Effect_Declaration_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_EFFECT_LITERAL ? as< Effect_Declaration_Node* >(node) : 0;
}

template <> Function_Call_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_FUNCTION_CALL ? as< Function_Call_Node* >(node) : 0;
}

template <> Effect_Call_Node* is_instance<>(Node* node) {
  return node && node->kind == AST_EFFECT_CALL ? as< Effect_Call_Node* >(node) : 0;
}

Function_Literal_Node* create_node_function_literal(ast::Manager* manager, Node* arguments, Node* return_type, ProgramPoint_List_Node* body) {

  assert(!arguments || ast::is_instance< Literal_Nothing_Node* >(arguments) || ast::is_instance< Declarations_List_Node* >(arguments));

  Node* signature = ast::manager_alloc(manager, AST_FUN_SIGNATURE, get_id(arguments), get_id(return_type));

  return as< Function_Literal_Node* >(ast::manager_alloc(manager, AST_FUNCTION_LITERAL, get_id(signature), get_id(body)));
}

void Function_Literal_Node::set_return_type(ast::Manager* manager, Node* ty) {
  Node* signature = left_of(manager, this);
  set_right(manager, signature, ty);
}

Effect_Declaration_Node* create_node_effect_declaration(ast::Manager* manager, Node* arguments, Node* return_type) {
  assert(!arguments || ast::is_instance< Literal_Nothing_Node* >(arguments) || ast::is_instance< Declarations_List_Node* >(arguments));

  Node* signature = ast::manager_alloc(manager, AST_FUN_SIGNATURE, get_id(arguments), get_id(return_type));

  return as< Effect_Declaration_Node* >(ast::manager_alloc(manager, AST_EFFECT_LITERAL, get_id(signature), 0));
}

Function_Call_Node* create_node_function_call(ast::Manager* manager, Node* function, Node* arguments) {
  assert(!arguments || ast::is_instance< Literal_Nothing_Node* >(arguments) || ast::is_instance< Declarations_List_Node* >(arguments));

  return as< Function_Call_Node* >(ast::manager_alloc(manager, AST_FUNCTION_CALL, get_id(function), get_id(arguments)));
}

Effect_Call_Node* create_node_effect_call(ast::Manager* manager, Node* function, Node* arguments) {
  assert(!arguments || ast::is_instance< Literal_Nothing_Node* >(arguments) || ast::is_instance< Declarations_List_Node* >(arguments));

  return as< Effect_Call_Node* >(ast::manager_alloc(manager, AST_EFFECT_CALL, get_id(function), get_id(arguments)));
}

ProgramPoint_List_Node* Function_Literal_Node::get_body(ast::Manager* manager) {
  return is_instance< ProgramPoint_List_Node* >(right_of(manager, this));
}

void Function_Literal_Node::set_body(ast::Manager* manager, ProgramPoint_List_Node* pp) {
  set_right(manager, this, pp);
}

ProgramPoint_List_Node* Effect_Declaration_Node::get_body(ast::Manager* manager) {
  return is_instance< ProgramPoint_List_Node* >(right_of(manager, this));
}

Node* Function_Literal_Node::get_return_type(ast::Manager* manager) {
  Node* signature = left_of(manager, this);
  return right_of(manager, signature);
}

Declarations_List_Node* Function_Literal_Node::get_arguments(ast::Manager* manager) {
  Node* signature = left_of(manager, this);

  return is_instance< Declarations_List_Node* >(left_of(manager, signature));
}

Node* Effect_Declaration_Node::get_return_type(ast::Manager* manager) {
  Node* signature = left_of(manager, this);
  return right_of(manager, signature);
}

Declarations_List_Node* Effect_Declaration_Node::get_arguments(ast::Manager* manager) {
  Node* signature = left_of(manager, this);

  return is_instance< Declarations_List_Node* >(left_of(manager, signature));
}

void Function_Literal_Node::push_argument(ast::Manager* manager, ast::Declaration_Variable_Node* arg) {
  Node* signature = left_of(manager, this);

  Declarations_List_Node* args = is_instance< Declarations_List_Node* >(left_of(manager, signature));

  if (!ast::is_semantic_node(args)) {
    Declarations_List_Node* left = create_node_declarations_list(manager, arg, NULL);

    return set_left(manager, signature, left);
  }

  args->push(manager, arg);
}

void Effect_Declaration_Node::push_argument(ast::Manager* manager, ast::Declaration_Variable_Node* arg) {
  Node* signature = left_of(manager, this);

  Declarations_List_Node* args = is_instance< Declarations_List_Node* >(left_of(manager, signature));

  if (!ast::is_semantic_node(args)) {
    Declarations_List_Node* left = create_node_declarations_list(manager, arg, NULL);

    return set_left(manager, signature, left);
  }

  args->push(manager, arg);
}

Node* Effect_Call_Node::get_effect(ast::Manager* manager) {
  return left_of(manager, this);
}

Declarations_List_Node* Effect_Call_Node::get_arguments(ast::Manager* manager) {
  return is_instance< Declarations_List_Node* >(right_of(manager, this));
}

Node* Function_Call_Node::get_function(ast::Manager* manager) {
  return left_of(manager, this);
}

Declarations_List_Node* Function_Call_Node::get_arguments(ast::Manager* manager) {
  return is_instance< Declarations_List_Node* >(right_of(manager, this));
}

void Function_Call_Node::push_argument(ast::Manager* manager, ast::Node* arg) {
  Declarations_List_Node* args = this->get_arguments(manager);

  if (!ast::is_semantic_node(args)) {
    Declarations_List_Node* args = create_node_declarations_list(manager, arg, NULL);
    return set_right(manager, this, args);
  }

  args->push(manager, arg);
}

void Effect_Call_Node::push_argument(ast::Manager* manager, ast::Node* arg) {
  Declarations_List_Node* args = this->get_arguments(manager);

  if (!ast::is_semantic_node(args)) {
    Declarations_List_Node* args = create_node_declarations_list(manager, arg, NULL);
    return set_right(manager, this, args);
  }

  args->push(manager, arg);
}

void Function_Literal_Node::set_arguments(ast::Manager* manager, Declarations_List_Node* args) {
	Node* signature = left_of(manager, this);
	signature->left = args->id;
}

}; // namespace ast
