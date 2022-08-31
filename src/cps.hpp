#pragma once

// Continuation Passing Style Analysis.
// brief: the analysis transform a program into CPS style.
// The analysis expects :
// 1. The program is using three adress instructions.
// 2. calls are only made on simple assignment.
// 3. All variable types are know and the program typechecks.

// TODO: we need to get the funtions that may escape throught function arguments
// right now since we dont have pointer types, this is not possible, so we're
// just considering that a function may escape on an assignment to an constant
// argument for testing reasons, fix that when the language gets more features.

#include "ast.hpp"
#include "context.hpp"
#include "error.hpp"
#include "lexer.hpp"
#include "parser.hpp"

#include "stdio.h"
#include <__string>
#include <_stdio.h>
#include <assert.h>
#include <cstddef>
#include <cstdio>

#include <iterator>
#include <random>
#include <sys/resource.h>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <vector>

AST_Node* get_last(Parser* p, AST_Node* head, AST_Node** arg = NULL) {
  assert(head->kind == AST_DECL_ARGS_LIST || head->kind == AST_PROGRAM_POINT);

  AST_Manager* m    = &p->ast_man;
  AST_Node*    decl = ast_node_null(m);

  while (!ast_is_null_node(head)) {
    if (arg) { *arg = head; }

    decl = ast_manager_get_relative(m, head, head->left);
    head = ast_manager_get_relative(m, head, head->right);
  }

  return decl;
}

AST_Node* get_call_last_argument(Parser* p, AST_Node* call, AST_Node** arg = NULL) {
  AST_Manager* m = &p->ast_man;

  AST_Node* head = ast_manager_get_relative(m, call, call->right);

  return get_last(p, head, arg);
}

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

AST_Node* create_continuation_function(Parser* p, Scope* scope, AST_Node* argument, AST_Node* return_type, AST_Node* body) {
  AST_Manager* m         = &p->ast_man;
  Token        undefined = lexer_undef_token();

  // NOTE(marcos): If the binding dosent have a type set it to be 'any',
  // this is just a quick patch since we dont have type check and
  // inference yet.
  if (!ast_is_null_node(argument)) {
    AST_Node* argument_type = ast_type_bind_get_type(m, argument);

    if (ast_is_null_node(argument_type)) { argument->right = ast_type_any(m, undefined)->id; }
  }

  AST_Node* arguments = ast_decl_args(m, undefined, argument, ast_node_null(m));
  AST_Node* signature = ast_function_signature(m, undefined, arguments, return_type);
  AST_Node* function  = ast_function_literal(m, undefined, signature, body);
  AST_Node* symbol    = ast_temp_node(m);
  AST_Node* type      = ast_type_any(m, undefined); // TODO: infer type from 'arguments' and 'return_type'
  AST_Node* bind      = ast_type_bind(m, undefined, symbol, type);

  return ast_constant_bind(m, undefined, bind, function);
}

AST_Node* function_call_cps_conversion(Parser* p, Scope* scope, AST_Node* program_point, AST_Node* call, AST_Node* continuation, AST_Node* bind);
void      function_literal_cps_conversion(Parser* p, Scope* scope, AST_Node* function, AST_Node* continuation = NULL);
void      program_point_cps_conversion(Parser* p, AST_Node* statements, Scope* scope);

void program_point_cps_conversion(Parser* p, AST_Node* statements, Scope* scope) {

  AST_Manager* m = &p->ast_man;

  while (!ast_is_null_node(statements)) {

    AST_Node* statement    = ast_program_point_get_decl(m, statements);
    AST_Node* continuation = ast_program_point_get_tail(m, statements);

    if (statement->kind == AST_BIND_CONSTANT || statement->kind == AST_BIND_VARIABLE) {
      AST_Node* left  = ast_bind_get_type_bind(m, statement);
      AST_Node* right = ast_bind_get_expr(m, statement);

      if (right->kind == AST_FUNCTION_LITERAL) { scope_push(scope, statement); }

      if (right->kind == AST_FUNCTION_CALL) {
        program_point_cps_conversion(p, continuation, scope);
        statements = function_call_cps_conversion(p, scope, statements, right, continuation, left);
      }
    }

    if (statement->kind == AST_OP_BIN_ASSIGN) { assert(false && "TODO: implement reassignment"); }

    if (statement->kind == AST_FUNCTION_CALL) {
      // break continuation
      // statements->right = ast_node_null(m)->id;
      program_point_cps_conversion(p, continuation, scope);
      statements = function_call_cps_conversion(p, scope, statements, statement, continuation, ast_node_null(m));
    }

    if (statement->kind == AST_CTRL_FLOW_IF) {
      AST_Node* right = ast_manager_get_relative(m, statement, statement->right);
      program_point_cps_conversion(p, right, scope);
    }

    if (statement->kind == AST_CTRL_FLOW_IF_ELSE) {
      AST_Node* list = statement;

      while (!ast_is_null_node(list) && list->kind == AST_CTRL_FLOW_IF_ELSE) {
        AST_Node* if_statement = ast_manager_get_relative(m, list, list->left);

        AST_Node* body = if_statement;

        if (if_statement->kind == AST_CTRL_FLOW_IF) { body = ast_manager_get_relative(m, if_statement, if_statement->right); }

        program_point_cps_conversion(p, body, scope);

        list = ast_manager_get_relative(m, list, list->right);
      }

      program_point_cps_conversion(p, list, scope);
    }

    statements = ast_program_point_get_tail(m, statements);
  }
}

AST_Node* function_call_cps_conversion(Parser* p, Scope* scope, AST_Node* program_point, AST_Node* call, AST_Node* continuation, AST_Node* bind) {
  AST_Manager* m        = &p->ast_man;
  AST_Node*    function = ast_fun_call_get_call_sym(m, call);

  b8 is_local = false;

  AST_Node* local = scope_find(scope, p, function, &is_local);

  Token undefined = lexer_undef_token();

  AST_Node* continuation_symbol = ast_node_null(m);
  AST_Node* declaration         = ast_node_null(m);

  if (!ast_is_null_node(continuation)) {
    AST_Node* type = ast_type_any(m, undefined); // TODO: use a global context to find function and the type

    declaration = create_continuation_function(p, scope, bind, type, continuation);

    AST_Node* continuation_bind = ast_bind_get_type_bind(m, declaration);

    continuation_symbol = ast_type_bind_get_symbol(m, continuation_bind);
  }

  if (local->kind == AST_BIND_VARIABLE || local->kind == AST_BIND_CONSTANT) {
    AST_Node* closure = ast_bind_get_expr(m, local);

    if (closure->kind == AST_FUNCTION_LITERAL) {
      if (is_local) {
        // FIXME(marcos): memory leak
        Scope* closure_scope = scope_create(scope);

        function_literal_cps_conversion(p, closure_scope, closure, continuation_symbol);

        // AST_Node* old_body = ast_function_literal_get_body(m, closure);
        // AST_Node* new_body = ast_program_point(m, undefined);

        // new_body->left  = declaration->id;
        // new_body->right = old_body->id;

        // closure->right = new_body->id;
      }

      if (!ast_is_null_node(bind)) {
        program_point->left  = call->id;
        program_point->right = ast_node_null(m)->id;
      }

      // return program_point;
    }
  }

  // NOTE(marcos): call to global function
  if (ast_is_null_node(continuation)) { return program_point; }

  ast_call_push_argument(m, undefined, call, continuation_symbol);

  // if (!is_local) {

  AST_Node* statement = ast_program_point(m, undefined);

  statement->left = call->id;

  program_point->left  = declaration->id;
  program_point->right = statement->id;

  return statement;
  // }

  // return program_point;
}

void function_literal_cps_conversion(Parser* p, Scope* scope, AST_Node* function, AST_Node* continuation_symbol) {
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

  program_point_cps_conversion(p, statements, scope);
}

void cps_conversion(Parser* p, AST_Node* root) {
  assert(root->kind == AST_PROGRAM_POINT);

  AST_Manager* m = &p->ast_man;

  while (!ast_is_null_node(root)) {
    AST_Node* prog_point = ast_manager_get_relative(m, root, root->left);

    if (prog_point->kind == AST_BIND_CONSTANT || prog_point->kind == AST_BIND_VARIABLE) {
      AST_Node* decl = ast_bind_get_expr(m, prog_point);

      if (decl->kind == AST_FUNCTION_LITERAL) {
        // FIXME(marcos): memory leak
        Scope* scope = scope_create(NULL);

        function_literal_cps_conversion(p, scope, decl);
      }
    }

    root = ast_manager_get_relative(m, root, root->right);
  }
}

struct CPS_Extended_Call_Edges {
  std::unordered_set< AST_Id > to;
  std::unordered_set< AST_Id > pred;
};

typedef std::unordered_map< AST_Id, CPS_Extended_Call_Edges > CPS_Extended_Call_Graph;

void build_extended_cps_graph_declaration(CPS_Extended_Call_Graph& context, Scope* scope, Parser* p, AST_Node* function, AST_Node* statement) {
  AST_Manager* m = &p->ast_man;

  if (statement->kind == AST_FUNCTION_CALL) {
    AST_Node* symbol    = ast_fun_call_get_call_sym(m, statement);
    AST_Node* arguments = ast_fun_call_get_call_args(m, statement);

    AST_Node* declaration = scope_find(scope, p, symbol);
    if (!ast_is_null_node(declaration)) {
      AST_Node* bind = declaration;

      if (bind->kind == AST_BIND_CONSTANT || bind->kind == AST_BIND_VARIABLE) { bind = ast_bind_get_type_bind(m, bind); }
      assert(bind->kind == AST_BIND_TYPE);

      AST_Node* expr = ast_bind_get_expr(m, declaration);

      if (expr->kind != AST_FUNCTION_LITERAL) { return; }

      context[function->id].to.insert(bind->id);
      context[bind->id].pred.insert(function->id);

    } else {
      // is global function
      AST_Node* continuation = get_call_last_argument(p, statement);
      AST_Node* declaration  = scope_find(scope, p, continuation);

      if (ast_is_null_node(declaration) || (declaration->kind != AST_BIND_CONSTANT && declaration->kind != AST_BIND_VARIABLE)) { return; }

      AST_Node* expr = ast_bind_get_expr(m, declaration);
      if (expr->kind != AST_FUNCTION_LITERAL) { return; }

      AST_Node* bind = declaration;

      if (bind->kind == AST_BIND_CONSTANT || bind->kind == AST_BIND_VARIABLE) { bind = ast_bind_get_type_bind(m, bind); }
      assert(bind->kind == AST_BIND_TYPE);

      context[function->id].to.insert(bind->id);
      context[bind->id].pred.insert(function->id);
    }
  }

  if (statement->kind == AST_CTRL_FLOW_IF) {
    AST_Node* l = ast_manager_get_relative(m, statement, statement->left);
    AST_Node* r = ast_manager_get_relative(m, statement, statement->right);

    build_extended_cps_graph_declaration(context, scope, p, function, l);
    build_extended_cps_graph_declaration(context, scope, p, function, r);
  }

  if (statement->kind == AST_CTRL_FLOW_IF_ELSE) {
    AST_Node* l = ast_manager_get_relative(m, statement, statement->left);
    AST_Node* r = ast_manager_get_relative(m, statement, statement->right);

    build_extended_cps_graph_declaration(context, scope, p, function, l);
    build_extended_cps_graph_declaration(context, scope, p, function, r);
  }

  if (statement->kind == AST_BIND_CONSTANT || statement->kind == AST_BIND_VARIABLE) {

    AST_Node* bind = ast_bind_get_type_bind(m, statement);
    AST_Node* expr = ast_bind_get_expr(m, statement);

    if (expr->kind == AST_FUNCTION_LITERAL) {
      scope_push(scope, statement);

      Scope* closure_scope = scope_create(scope);

      assert(bind->kind == AST_BIND_TYPE);

      context[bind->id] = CPS_Extended_Call_Edges();

      AST_Node* signature = ast_function_literal_get_signature(m, expr);
      AST_Node* arguments = ast_function_signature_get_args(m, signature);
      AST_Node* body      = ast_function_literal_get_body(m, expr);

      build_extended_cps_graph_declaration(context, closure_scope, p, bind, body);
    }
  }

  if (statement->kind == AST_PROGRAM_POINT) {
    while (!ast_is_null_node(statement)) {
      assert(statement->kind == AST_PROGRAM_POINT);

      AST_Node* closure_statement = ast_program_point_get_decl(m, statement);

      build_extended_cps_graph_declaration(context, scope, p, function, closure_statement);

      statement = ast_program_point_get_tail(m, statement);
    }
  }
}

void build_extended_cps_graph(CPS_Extended_Call_Graph& graph, Parser* p, AST_Node* root) {
  assert(root->kind == AST_PROGRAM_POINT);

  AST_Manager* m = &p->ast_man;

  while (!ast_is_null_node(root)) {
    AST_Node* statement = ast_program_point_get_decl(m, root);
    Scope*    scope     = scope_create(NULL); // FIXME(marcos): memory leak
    build_extended_cps_graph_declaration(graph, scope, p, ast_node_null(m), statement);
    root = ast_program_point_get_tail(m, root);
  }
}

struct Closure_Converter {
  enum Escaping_Kind {
    FUNCTION_LOCAL,
    FUNCTION_ESCAPING,
    CONTINUATION_ESCAPING,
    CONTINUATION_LOCAL,
  };

  struct Free_Variable_Data {
    u64 last_use_time;
    u64 first_use_time;
  };

  typedef std::unordered_map< AST_Id, u64 >                Stage_Number_Map;
  typedef std::unordered_map< AST_Id, Free_Variable_Data > Free_Variables_Map;
  typedef std::unordered_map< AST_Id, Free_Variables_Map > Function_Free_Variables_Map;
  typedef std::unordered_map< AST_Id, Escaping_Kind >      Function_Escaping_Map;

  CPS_Extended_Call_Graph     cps_extended_call_graph;
  Stage_Number_Map            stage_number;
  Function_Free_Variables_Map function_free_variables;
  Function_Escaping_Map       escaping_kind;
};

void compute_function_declaration_stage_numbers(Closure_Converter& converter, Parser* p, AST_Node* root, Scope* scope) {
  AST_Manager* m = &p->ast_man;

  if (root->kind == AST_BIND_CONSTANT || root->kind == AST_BIND_VARIABLE) {
    AST_Node* bind = ast_bind_get_type_bind(m, root);
    AST_Node* expr = ast_bind_get_expr(m, root);

    if (expr->kind == AST_FUNCTION_LITERAL) {
      scope_push(scope, root);

      converter.stage_number[bind->id] = get_scope_depth(scope);
      printf("StageNumber(");
      print_ast_to_program(p, ast_manager_get_relative(m, bind, bind->left));
      printf(") = %lu\n", converter.stage_number[bind->id]);

      // FIXME(marcos): scope memory leak
      AST_Node* body          = ast_function_literal_get_body(m, expr);
      Scope*    closure_scope = scope_create(scope);
      compute_function_declaration_stage_numbers(converter, p, body, closure_scope);
    }
  }

  if (root->kind == AST_OP_BIN_ASSIGN) { assert(false && "TODO"); }

  if (root->kind == AST_CTRL_FLOW_IF || root->kind == AST_CTRL_FLOW_IF_ELSE) {
    AST_Node* l = ast_manager_get_relative(m, root, root->left);
    AST_Node* r = ast_manager_get_relative(m, root, root->right);
    compute_function_declaration_stage_numbers(converter, p, l, scope);
    compute_function_declaration_stage_numbers(converter, p, r, scope);
  }

  if (root->kind == AST_PROGRAM_POINT) {
    while (!ast_is_null_node(root)) {
      AST_Node* decl = ast_program_point_get_decl(m, root);
      compute_function_declaration_stage_numbers(converter, p, decl, scope);
      root = ast_program_point_get_tail(m, root);
    }
  }
}
void compute_continuations_stage_numbers(Closure_Converter& converter, Parser* p, AST_Node* root, Scope* scope) {
  AST_Manager* m = &p->ast_man;

  if (root->kind == AST_BIND_CONSTANT || root->kind == AST_BIND_VARIABLE) {
    AST_Node* bind = ast_bind_get_type_bind(m, root);
    AST_Node* expr = ast_bind_get_expr(m, root);

    if (expr->kind == AST_FUNCTION_LITERAL) {
      scope_push(scope, root);

      // FIXME(marcos): scope memory leak
      AST_Node* body          = ast_function_literal_get_body(m, expr);
      Scope*    closure_scope = scope_create(scope);
      compute_continuations_stage_numbers(converter, p, body, closure_scope);
    }
  }

  if (root->kind == AST_CTRL_FLOW_IF || root->kind == AST_CTRL_FLOW_IF_ELSE) {
    AST_Node* l = ast_manager_get_relative(m, root, root->left);
    AST_Node* r = ast_manager_get_relative(m, root, root->right);
    compute_function_declaration_stage_numbers(converter, p, l, scope);
    compute_function_declaration_stage_numbers(converter, p, r, scope);
  }

  if (root->kind == AST_FUNCTION_CALL) {
    AST_Node* continuation = get_call_last_argument(p, root);
    AST_Node* declaration  = scope_find(scope, p, continuation);

    if (ast_is_null_node(declaration)) { assert(false && "declaration not found"); }

    assert(declaration->kind == AST_BIND_CONSTANT || declaration->kind == AST_BIND_VARIABLE);

    AST_Node* bind = ast_bind_get_type_bind(m, declaration);
    AST_Node* expr = ast_bind_get_expr(m, declaration);

    assert(expr->kind == AST_FUNCTION_LITERAL);

    converter.stage_number[bind->id] = 1;

    u64 max_pred = 0;

    std::unordered_set< AST_Id >& pred = converter.cps_extended_call_graph[bind->id].pred;

    for (std::unordered_set< AST_Id >::iterator it = pred.begin(); it != pred.end(); it++) {
      if (converter.stage_number.find(*it) == converter.stage_number.end()) { assert(false && "Something when wrong, not all predecessores have a Stage Number"); }

      max_pred = max_pred < converter.stage_number[*it] ? converter.stage_number[*it] : max_pred;
    }

    converter.stage_number[bind->id] += max_pred;

    converter.stage_number[bind->id] = get_scope_depth(scope);
    printf("StageNumber(");
    print_ast_to_program(p, bind);
    printf(") = %lu\n", converter.stage_number[bind->id]);
  }

  if (root->kind == AST_PROGRAM_POINT) {
    while (!ast_is_null_node(root)) {
      AST_Node* decl = ast_program_point_get_decl(m, root);
      compute_function_declaration_stage_numbers(converter, p, decl, scope);
      root = ast_program_point_get_tail(m, root);
    }
  }
}

void print_cps_extended_graph(CPS_Extended_Call_Graph& ctx, AST_Id node, Parser* p) {
  if (ctx[node].to.size() == 0 && ctx[node].pred.size() == 0) return;

  AST_Manager* m = &p->ast_man;

  AST_Node* bind = ast_manager_get(m, node);

  assert(bind->kind == AST_BIND_TYPE);

  AST_Node* symbol = ast_type_bind_get_symbol(m, bind);

  if (ctx[node].to.size() > 0) {
    printf("succ(");
    print_ast_to_program(p, symbol);
    printf(") = { ");

    u64 i = 0;

    for (std::unordered_set< AST_Id >::iterator it = ctx[node].to.begin(); it != ctx[node].to.end(); it++) {
      i = i + 1;

      AST_Node* to = ast_manager_get(m, *it);

      assert(to->kind == AST_BIND_TYPE);

      AST_Node* symbol = ast_type_bind_get_symbol(m, to);

      print_ast_to_program(p, symbol);

      if (i != ctx[node].to.size()) { printf(", "); }
    }
    printf(" } \n");
  }

  if (ctx[node].pred.size() > 0) {
    printf("pred(");
    print_ast_to_program(p, symbol);
    printf(") = { ");

    u64 i = 0;
    for (std::unordered_set< AST_Id >::iterator it = ctx[node].pred.begin(); it != ctx[node].pred.end(); it++) {
      i            = i + 1;
      AST_Node* to = ast_manager_get(m, *it);

      assert(to->kind == AST_BIND_TYPE);

      AST_Node* symbol = ast_type_bind_get_symbol(m, to);

      print_ast_to_program(p, symbol);

      if (i != ctx[node].pred.size()) { printf(", "); }
    }
    printf(" }");
  }

  printf("\n");
}

void print_cps_extended_graph_context(CPS_Extended_Call_Graph& ctx, Parser* p) {
  AST_Manager* m = &p->ast_man;

  for (CPS_Extended_Call_Graph::iterator it = ctx.begin(); it != ctx.end(); it++) {
    print_cps_extended_graph(ctx, it->first, p);
  }
}

Scope* function_literal_args_to_scope(Parser* p, AST_Node* node, Scope* parent = NULL) {
  Scope* scope = scope_create(parent);

  if (ast_is_null_node(node)) { return scope; }

  AST_Manager* m = &p->ast_man;

  assert(node->kind == AST_DECL_ARGS_LIST);

  while (!ast_is_null_node(node)) {
    AST_Node* arg = ast_decl_list_get_elem(m, node);

    if (arg->kind == AST_BIND_CONSTANT || arg->kind == AST_BIND_VARIABLE) { arg = ast_manager_get_relative(m, arg, arg->left); }

    assert(arg->kind == AST_BIND_TYPE);

    scope_push(scope, arg);

    node = ast_decl_list_get_tail(m, node);
  }

  return scope;
}

struct FunctionDeclaration {

  typedef std::unordered_map< AST_Id, Closure_Converter::Free_Variable_Data > Free_Variables;

  Free_Variables       free_variables;
  AST_Node*            declaration;
  FunctionDeclaration* prev;
};

void compute_use_time_rec(Closure_Converter& converter, Scope* scope, Parser* p, FunctionDeclaration* enclosing, AST_Node* node) {
  if (ast_is_null_node(node)) return;

  AST_Manager* m = &p->ast_man;

  if (node->kind == AST_SYMBOL_LITERAL || ast_is_temporary(m, node)) {
    if (enclosing == NULL) return;

    while (enclosing) {
      b8 is_local = false;

      AST_Node* declaration = scope_find(scope, p, node, &is_local);

      if (is_local) { break; }

      u64 stage_number = converter.stage_number[enclosing->declaration->id];

      if (enclosing->free_variables.find(declaration->id) == enclosing->free_variables.end()) {
        enclosing->free_variables[declaration->id]                = Closure_Converter::Free_Variable_Data();
        enclosing->free_variables[declaration->id].first_use_time = stage_number;
        enclosing->free_variables[declaration->id].last_use_time  = stage_number;
      } else {
        enclosing->free_variables[declaration->id].first_use_time = std::min(enclosing->free_variables[declaration->id].first_use_time, stage_number);
        enclosing->free_variables[declaration->id].last_use_time  = std::max(enclosing->free_variables[declaration->id].last_use_time, stage_number);
      }

      scope     = scope->parent;
      enclosing = enclosing->prev;
    }

    return;
  }

  if (node->kind == AST_BIND_CONSTANT || node->kind == AST_BIND_VARIABLE) {
    scope_push(scope, node);

    AST_Node* right = ast_bind_get_expr(m, node);

    if (right->kind == AST_FUNCTION_LITERAL) {
      FunctionDeclaration declaration;

      declaration.prev           = enclosing;
      declaration.declaration    = node;
      declaration.free_variables = FunctionDeclaration::Free_Variables();

      compute_use_time_rec(converter, scope, p, &declaration, right);

      printf("FreeVariables(");
      print_ast_to_program(p, ast_manager_get_relative(m, ast_manager_get_relative(m, node, node->left), ast_manager_get_relative(m, node, node->left)->left));
      printf(") = { ");

      u64 i = 0;

      for (FunctionDeclaration::Free_Variables::iterator it = declaration.free_variables.begin(); it != declaration.free_variables.end(); it++) {
        i++;
        AST_Node* n = ast_manager_get(m, it->first);

        if (n->kind == AST_BIND_VARIABLE || n->kind == AST_BIND_CONSTANT) { n = ast_manager_get_relative(m, n, n->left); }
        assert(n->kind == AST_BIND_TYPE);
        if (n->kind == AST_BIND_TYPE) { n = ast_manager_get_relative(m, n, n->left); }

        print_ast_to_program(p, n);
        if (i != declaration.free_variables.size()) printf(", ");
      }

      printf(" }\n");

      converter.function_free_variables[node->id] = declaration.free_variables;
      return;
    } else {
      return compute_use_time_rec(converter, scope, p, enclosing, right);
    }
  }

  if (node->kind == AST_FUNCTION_LITERAL) {
    AST_Node* signature = ast_function_literal_get_signature(m, node);
    AST_Node* arguments = ast_function_signature_get_args(m, signature);

    Scope* closure_scope = function_literal_args_to_scope(p, arguments, scope);

    AST_Node* body = ast_function_literal_get_body(m, node);

    return compute_use_time_rec(converter, closure_scope, p, enclosing, body);
  }

  AST_Node* l = ast_manager_get_relative(m, node, node->left);
  AST_Node* r = ast_manager_get_relative(m, node, node->right);

  compute_use_time_rec(converter, scope, p, enclosing, l);
  compute_use_time_rec(converter, scope, p, enclosing, r);
}

struct Alias_Scope {
  typedef std::unordered_set< AST_Id >            Alias_Set;
  typedef std::unordered_map< AST_Id, Alias_Set > Alias_Map;

  Alias_Map    aliases;
  Alias_Scope* parent;
};

Alias_Scope* copy_alias_scope_to_heap(Alias_Scope* alias) {
  if (alias == NULL) return NULL;

  Alias_Scope* a = (Alias_Scope*)malloc(sizeof(Alias_Scope));

  a->aliases = alias->aliases;
  a->parent  = copy_alias_scope_to_heap(alias->parent);

  return a;
}

void free_alias_scope(Alias_Scope* alias) {
  if (alias == NULL) return;

  free_alias_scope(alias->parent);

  free(alias);
}

// void push_alias(Alias_Scope* aliases, AST_Id left, AST_Id right) {
//   while (aliases) {
//     if (aliases->aliases.find(left) != aliases->aliases.end()) {
//       aliases->aliases[left].insert(right);
//       return;
//     }

//     aliases = aliases->parent;
//   }

//   assert(false && "Alias not found");
// }

Alias_Scope::Alias_Set* find_alias_set(Alias_Scope* aliases, AST_Id id) {
  while (aliases) {
    if (aliases->aliases.find(id) != aliases->aliases.end()) { return &aliases->aliases[id]; }

    aliases = aliases->parent;
  }

  return NULL;
}

void assign_left_to_right_alias(Alias_Scope* aliases, AST_Id left, AST_Id right, b8 reset = false) {
  Alias_Scope::Alias_Set* left_aliases  = find_alias_set(aliases, left);
  Alias_Scope::Alias_Set* right_aliases = find_alias_set(aliases, left);

  if (left_aliases == NULL) {
    aliases->aliases[left] = Alias_Scope::Alias_Set();
    left_aliases           = &aliases->aliases[left];
  }

  if (reset) { left_aliases->clear(); }

  if (right_aliases == NULL) {
    left_aliases->insert(right);
    return;
  }

  for (Alias_Scope::Alias_Set::iterator it = right_aliases->begin(); it != right_aliases->end(); it++) {
    left_aliases->insert(*it);
  }
}

b8 is_inconditional_branch(Parser* p, AST_Node* branch) {
  // TODO(marcos): There is more precise analysis to
  // do here for sure.

  AST_Manager* m = &p->ast_man;

  if (branch->kind != AST_CTRL_FLOW_IF) return false;

  return true;
}

void function_literal_return_escape_analysis(Closure_Converter& conv, Parser* p, AST_Node* root, AST_Node* cont, Scope* scope, Alias_Scope* aliases) {
  if (ast_is_null_node(root)) return;
  AST_Manager* m = &p->ast_man;

  if (root->kind == AST_BIND_CONSTANT || root->kind == AST_BIND_VARIABLE) {
    scope_push(scope, root);

    AST_Node* left  = ast_manager_get_relative(m, root, root->left);
    AST_Node* right = ast_manager_get_relative(m, root, root->right);

    assign_left_to_right_alias(aliases, left->id, right->id);

    return;
  }

  if (root->kind == AST_OP_BIN_ASSIGN) {
    AST_Node* left  = ast_manager_get_relative(m, root, root->left);
    AST_Node* bind  = scope_find(scope, p, left);
    AST_Node* right = ast_manager_get_relative(m, root, root->right);

    assign_left_to_right_alias(aliases, bind->id, right->id);

    return;
  }

  if (root->kind == AST_FUNCTION_CALL) {
    AST_Node* symbol      = ast_manager_get_relative(m, root, root->left);
    AST_Node* arguments   = ast_manager_get_relative(m, root, root->right);
    AST_Node* function    = get_last(p, arguments);
    AST_Node* declaration = scope_find(scope, p, function);

    if (ast_is_null_node(declaration)) return;

    AST_Node* literal = ast_bind_get_expr(m, declaration);

    assert(literal->kind == AST_FUNCTION_LITERAL && "not a function definition");

    AST_Node* signature = ast_function_literal_get_signature(m, literal);
    AST_Node* bindings  = ast_function_signature_get_args(m, signature);

    Scope* closure_scope = function_literal_args_to_scope(p, literal, scope);

    Alias_Scope closure_aliases = {.parent = aliases};

    while (!ast_is_null_node(arguments)) {
      assert(!ast_is_null_node(bindings) && "Invalid number of arguments");

      AST_Node* argument = ast_decl_list_get_elem(m, arguments);
      AST_Node* binding  = ast_decl_list_get_elem(m, bindings);

      if (binding->kind == AST_BIND_CONSTANT || binding->kind == AST_BIND_VARIABLE) { binding = ast_bind_get_type_bind(m, binding); }

      assert(binding->kind == AST_BIND_TYPE);

      closure_aliases.aliases[binding->left] = Alias_Scope::Alias_Set();
      closure_aliases.aliases[binding->left].insert(argument->id);

      bindings  = ast_decl_list_get_tail(m, bindings);
      arguments = ast_decl_list_get_tail(m, arguments);
    }

    AST_Node* body = ast_function_literal_get_body(m, literal);

    return function_literal_return_escape_analysis(conv, p, body, cont, closure_scope, &closure_aliases);
  }

  if (root->kind == AST_CTRL_FLOW_IF) {
    AST_Node* condition = ast_manager_get_relative(m, root, root->left);
    AST_Node* body      = ast_manager_get_relative(m, root, root->right);

    Alias_Scope* branch_aliases = copy_alias_scope_to_heap(aliases);

    function_literal_return_escape_analysis(conv, p, condition, cont, scope, aliases);
    function_literal_return_escape_analysis(conv, p, body, cont, scope, branch_aliases);

    return;
  }

  if (root->kind == AST_CTRL_FLOW_IF_ELSE) {
    AST_Node* branch = ast_manager_get_relative(m, root, root->left);
    AST_Node* tail   = ast_manager_get_relative(m, root, root->right);

    Alias_Scope* branch_aliases = copy_alias_scope_to_heap(aliases);
    function_literal_return_escape_analysis(conv, p, branch, cont, scope, branch_aliases);

    while (tail->kind == AST_CTRL_FLOW_IF_ELSE) {
      branch = ast_manager_get_relative(m, tail, tail->left);

      Alias_Scope* _branch_aliases = copy_alias_scope_to_heap(aliases);

      function_literal_return_escape_analysis(conv, p, branch, cont, scope, _branch_aliases);

      for (Alias_Scope::Alias_Map::iterator it0 = _branch_aliases->aliases.begin(); it0 != _branch_aliases->aliases.end(); it0++) {
        for (Alias_Scope::Alias_Set::iterator it1 = it0->second.begin(); it1 != it0->second.end(); it1++) {
          if (branch_aliases->aliases.find(it0->first) == branch_aliases->aliases.end()) { branch_aliases->aliases[it0->first] = Alias_Scope::Alias_Set(); }

          branch_aliases->aliases[it0->first].insert(*it1);
        }
      }

      tail = ast_manager_get_relative(m, tail, tail->right);
    }

    if (!ast_is_null_node(tail)) {
      Alias_Scope* _branch_aliases = copy_alias_scope_to_heap(aliases);

      function_literal_return_escape_analysis(conv, p, tail, cont, scope, _branch_aliases);

      for (Alias_Scope::Alias_Map::iterator it0 = _branch_aliases->aliases.begin(); it0 != _branch_aliases->aliases.end(); it0++) {
        for (Alias_Scope::Alias_Set::iterator it1 = it0->second.begin(); it1 != it0->second.end(); it1++) {
          if (branch_aliases->aliases.find(it0->first) == branch_aliases->aliases.end()) { aliases->aliases[it0->first] = Alias_Scope::Alias_Set(); }

          branch_aliases->aliases[it0->first].insert(*it1);
        }
      }
    }

    Alias_Scope* temp = aliases;

    while (branch_aliases) {
      temp->aliases  = branch_aliases->aliases;
      branch_aliases = branch_aliases->parent;
      temp           = temp->parent;
    }

    return;
  }

  assert(root->kind == AST_PROGRAM_POINT);

  AST_Node* left  = ast_manager_get_relative(m, root, root->left);
  AST_Node* right = ast_manager_get_relative(m, root, root->right);

  function_literal_return_escape_analysis(conv, p, left, cont, scope, aliases);
  function_literal_return_escape_analysis(conv, p, right, cont, scope, aliases);
}

void return_escape_analysis(Closure_Converter& converter, Parser* p, AST_Node* root) {
  assert(root->kind == AST_PROGRAM_POINT);
  AST_Manager* m = &p->ast_man;

  while (!ast_is_null_node(root)) {
    AST_Node* statement = ast_manager_get_relative(m, root, root->left);

    if (statement->kind == AST_BIND_CONSTANT || statement->kind == AST_BIND_VARIABLE) {
      AST_Node* expr = ast_bind_get_expr(m, statement);
      if (expr->kind == AST_FUNCTION_LITERAL) {
        Scope*    scope        = function_literal_args_to_scope(p, expr);
        AST_Node* signature    = ast_function_literal_get_signature(m, expr);
        AST_Node* body         = ast_function_literal_get_body(m, expr);
        AST_Node* arguments    = ast_function_signature_get_args(m, signature);
        AST_Node* continuation = get_last(p, arguments);
        AST_Node* cont_symbol  = ast_type_bind_get_symbol(m, continuation);

        assert(ast_is_temporary(m, cont_symbol));

        Alias_Scope aliases;

        function_literal_return_escape_analysis(converter, p, body, cont_symbol, scope, &aliases);
      }
    }

    root = ast_manager_get_relative(m, root, root->left);
  }
}

void closure_conversion(Parser* p, AST_Node* root) {
  Closure_Converter converter;

  cps_conversion(p, root);

  print_ast_to_program(p, root); // TODO(marcos): remove

  build_extended_cps_graph(converter.cps_extended_call_graph, p, root);

  print_cps_extended_graph_context(converter.cps_extended_call_graph, p);

  Scope* scopeA = scope_create(NULL); // FIXME(marcos): fix leak
  compute_function_declaration_stage_numbers(converter, p, root, scopeA);

  Scope* scopeB = scope_create(NULL); // FIXME(marcos): fix leak
  compute_function_declaration_stage_numbers(converter, p, root, scopeB);

  Scope* scopeC = scope_create(NULL); // FIXME(marcos): fix leak
  compute_use_time_rec(converter, scopeC, p, NULL, root);

  // Compute escaping analysis
  return_escape_analysis(converter, p, root);
}
