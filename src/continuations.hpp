#pragma once

#include "ast.hpp"
#include "context.hpp"
#include "parser.hpp"
#include <unordered_set>

typedef std::unordered_set< AST_Id > Local_Continuations_Set;

void replace_return_with_continuation_call(Parser* p, AST_Node* statement, AST_Node* continuation) {
  if (ast_is_null_node(statement)) return;
  if (statement->kind == AST_TYPE_STRUCT) return;
  if (statement->kind == AST_HANDLER_LITERAL) return;
  if (statement->kind == AST_FUNCTION_LITERAL) return;

  AST_Manager* m         = &p->ast_man;
  Token        undefined = lexer_undef_token();

  if (statement->kind == AST_CTRL_FLOW_RETURN) {
    ast_change_kind(statement, AST_FUNCTION_CALL);

    AST_Node* symbol = ast_symbol(m, undefined);

    AST_Node* expression = ast_manager_get_relative(m, statement, statement->left);

    *symbol = *continuation;

    statement->left  = symbol->id;
    statement->right = ast_decl_args(m, undefined, expression, ast_node_null(m))->id;
  }

  AST_Node* l = ast_manager_get_relative(m, statement, statement->left);
  AST_Node* r = ast_manager_get_relative(m, statement, statement->right);

  replace_return_with_continuation_call(p, l, continuation);
  replace_return_with_continuation_call(p, r, continuation);
}

AST_Node* create_continuation_function(Local_Continuations_Set& continuations, Parser* p, Scope* scope, AST_Node* argument, AST_Node* return_type, AST_Node* body) {
  AST_Manager* m         = &p->ast_man;
  Token        undefined = lexer_undef_token();

  // NOTE(marcos): If the binding dosent have a type set it to be 'any',
  // this is just a quick patch since we dont have type check and
  // inference yet.
  if (!ast_is_null_node(argument)) {
    AST_Node* argument_type = ast_type_bind_get_type(m, argument);

    if (ast_is_null_node(argument_type)) { argument->right = ast_type_any(m, undefined)->id; }
  }

  AST_Node* arguments   = !ast_is_null_node(argument) ? ast_decl_args(m, undefined, argument, ast_node_null(m)) : ast_node_null(m);
  AST_Node* signature   = ast_function_signature(m, undefined, arguments, return_type);
  AST_Node* function    = ast_function_literal(m, undefined, signature, body);
  AST_Node* symbol      = ast_temp_node(m);
  AST_Node* type        = ast_type_arrow(m, undefined, ast_type_any(m, undefined), ast_type_any(m, undefined)); // TODO: infer type from 'arguments' and 'return_type'
  AST_Node* bind        = ast_type_bind(m, undefined, symbol, type);
  AST_Node* declaration = ast_constant_bind(m, undefined, bind, function);

  continuations.insert(bind->id);

  return declaration;
}

AST_Node*
function_call_cps_conversion(Local_Continuations_Set& conv, Parser* p, Scope* scope, AST_Node* program_point, AST_Node* call, AST_Node* continuation, AST_Node* bind);
void function_literal_cps_conversion(Local_Continuations_Set& conv, Parser* p, Scope* scope, AST_Node* function, AST_Node* continuation = NULL);
void program_point_cps_conversion(Local_Continuations_Set& conv, Parser* p, AST_Node* statements, Scope* scope);

void program_point_cps_conversion(Local_Continuations_Set& conv, Parser* p, AST_Node* statements, Scope* scope) {

  AST_Manager* m = &p->ast_man;

  while (!ast_is_null_node(statements)) {

    AST_Node* statement    = ast_program_point_get_decl(m, statements);
    AST_Node* continuation = ast_program_point_get_tail(m, statements);

    if (statement->kind == AST_BIND_CONSTANT || statement->kind == AST_BIND_VARIABLE) {
      AST_Node* left  = ast_bind_get_type_bind(m, statement);
      AST_Node* right = ast_bind_get_expr(m, statement);

      if (right->kind == AST_FUNCTION_LITERAL) {
        // conv.user_functions.insert(left->id);

        Scope* closure_scope = scope_create(scope); // TODO(marcos): fix leak

        function_literal_cps_conversion(conv, p, closure_scope, right, NULL);

        scope_push(scope, statement);
      }

      if (right->kind == AST_FUNCTION_CALL) {
        program_point_cps_conversion(conv, p, continuation, scope);
        statements = function_call_cps_conversion(conv, p, scope, statements, right, continuation, left);
      }
    }

    if (statement->kind == AST_OP_BIN_ASSIGN) {
      AST_Node* expr = ast_manager_get_relative(m, statement, statement->right);

      if (expr->kind == AST_FUNCTION_LITERAL) {
        //	assert(false && "TODO: implement reassignment");
        Scope* closure_scope = scope_create(scope); // TODO(marcos): fix leak
        function_literal_cps_conversion(conv, p, closure_scope, expr, NULL);

        // Promote function declaration to constant
        AST_Node* temp = ast_temp_node(m);
        AST_Node* type = ast_undefined(m); // TODO(marcos): infer function type
        AST_Node* bind = ast_type_bind(m, lexer_undef_token(), temp, type);
        AST_Node* func = ast_constant_bind(m, lexer_undef_token(), bind, expr);

        AST_Node* pp = ast_program_point(m, lexer_undef_token());

        statements->left  = func->id;
        pp->left          = statement->id;
        pp->right         = statements->right;
        statements->right = pp->id;

        statement->right = ast_copy(m, temp)->id;
        statement        = func;
      }
    }

    if (statement->kind == AST_FUNCTION_CALL) {
      // break continuation
      // statements->right = ast_node_null(m)->id;
      program_point_cps_conversion(conv, p, continuation, scope);
      statements = function_call_cps_conversion(conv, p, scope, statements, statement, continuation, ast_node_null(m));
    }

    if (statement->kind == AST_CTRL_FLOW_IF) {
      AST_Node* right = ast_manager_get_relative(m, statement, statement->right);
      program_point_cps_conversion(conv, p, right, scope);
    }

    if (statement->kind == AST_CTRL_FLOW_IF_ELSE) {
      AST_Node* list = statement;

      while (!ast_is_null_node(list) && list->kind == AST_CTRL_FLOW_IF_ELSE) {
        AST_Node* if_statement = ast_manager_get_relative(m, list, list->left);

        AST_Node* body = if_statement;

        if (if_statement->kind == AST_CTRL_FLOW_IF) { body = ast_manager_get_relative(m, if_statement, if_statement->right); }

        program_point_cps_conversion(conv, p, body, scope);

        list = ast_manager_get_relative(m, list, list->right);
      }

      program_point_cps_conversion(conv, p, list, scope);
    }

    statements = ast_program_point_get_tail(m, statements);
  }
}

AST_Node* function_call_cps_conversion(
    Local_Continuations_Set& continuations, Parser* p, Scope* scope, AST_Node* program_point, AST_Node* call, AST_Node* continuation, AST_Node* bind) {
  AST_Manager* m = &p->ast_man;

  AST_Node* function = ast_fun_call_get_call_sym(m, call);

  AST_Node* local = scope_find(scope, p, function);

  Token undefined = lexer_undef_token();

  AST_Node* continuation_symbol = ast_node_null(m);
  AST_Node* declaration         = ast_node_null(m);

  if (!ast_is_null_node(continuation)) {
    AST_Node* type = ast_type_any(m, undefined); // TODO: use a global context to find function and the type

    declaration = create_continuation_function(continuations, p, scope, bind, type, continuation);

    AST_Node* continuation_bind = ast_bind_get_type_bind(m, declaration);

    continuation_symbol = ast_type_bind_get_symbol(m, continuation_bind);
  }

  if (ast_is_null_node(continuation)) { return program_point; }

  ast_call_push_argument(m, undefined, call, continuation_symbol);

  AST_Node* statement = ast_program_point(m, undefined);

  statement->left = call->id;

  program_point->left  = declaration->id;
  program_point->right = statement->id;

  return statement;
}

void function_literal_cps_conversion(Local_Continuations_Set& continuations, Parser* p, Scope* scope, AST_Node* function, AST_Node* continuation_symbol) {
  assert(function->kind == AST_FUNCTION_LITERAL);

  AST_Manager* m         = &p->ast_man;
  Token        undefined = lexer_undef_token();

  AST_Node* statements = ast_function_literal_get_body(m, function);
  AST_Node* signature  = ast_function_literal_get_signature(m, function);

  AST_Node* type = ast_type_any(m, undefined);

  if (continuation_symbol == NULL) {
    AST_Node* symbol    = ast_temp_node(m);
    continuation_symbol = symbol;
  } else {
    AST_Node* symbol    = ast_temp_node(m);
    *symbol             = *continuation_symbol;
    continuation_symbol = symbol;
  }

  AST_Node* bind = ast_type_bind(m, undefined, continuation_symbol, type);

  ast_function_literal_push_argument(m, undefined, function, bind);

  replace_return_with_continuation_call(p, statements, continuation_symbol);

  program_point_cps_conversion(continuations, p, statements, scope);
}

void cps_conversion(Local_Continuations_Set& continuations, Parser* p, AST_Node* root) {
  assert(root->kind == AST_PROGRAM_POINT);

  AST_Manager* m = &p->ast_man;

  while (!ast_is_null_node(root)) {
    AST_Node* prog_point = ast_manager_get_relative(m, root, root->left);

    if (prog_point->kind == AST_BIND_CONSTANT || prog_point->kind == AST_BIND_VARIABLE) {
      AST_Node* decl = ast_bind_get_expr(m, prog_point);

      if (decl->kind == AST_FUNCTION_LITERAL) {
        // FIXME(marcos): memory leak
        Scope* scope = scope_create(NULL);

        function_literal_cps_conversion(continuations, p, scope, decl);
      }
    }

    root = ast_manager_get_relative(m, root, root->right);
  }
}
