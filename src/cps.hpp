#pragma once

// Continuation Passing Style Analysis.
// brief: the analysis transform a program into CPS style.
// The analysis expects :
// 1. The program is using three adress instructions.
// 2. calls are only made on simple assignment.
// 3. All variable types are know and the program typechecks.

/*
 TODO(marcos): Scope and Aliases are leaking right now, there is already new implementations
 planned for those data structures, so a fix is planned.
*/

#include "ast.hpp"
#include "context.hpp"
#include "error.hpp"
#include "lexer.hpp"
#include "parser.hpp"

#include <unordered_map>
#include <unordered_set>
#include <vector>

struct CPS_Extended_Call_Edges {
  std::unordered_set< AST_Id > to;
  std::unordered_set< AST_Id > pred;
};

typedef std::unordered_map< AST_Id, CPS_Extended_Call_Edges > CPS_Extended_Call_Graph;

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

  typedef std::unordered_set< AST_Id > Continuations;
  typedef std::unordered_set< AST_Id > User_Functions;

  typedef std::unordered_map< AST_Id, u64 >                Stage_Number_Map;
  typedef std::unordered_map< AST_Id, Free_Variable_Data > Free_Variables_Map;
  typedef std::unordered_map< AST_Id, Free_Variables_Map > Function_Free_Variables_Map;
  typedef std::unordered_map< AST_Id, Escaping_Kind >      Function_Escaping_Map;

  User_Functions user_functions;
  Continuations  continuations;

  CPS_Extended_Call_Graph     cps_extended_call_graph;
  Stage_Number_Map            stage_number;
  Function_Free_Variables_Map function_free_variables;
  Function_Escaping_Map       escaping_kind;
};

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

AST_Node* create_continuation_function(Closure_Converter& conv, Parser* p, Scope* scope, AST_Node* argument, AST_Node* return_type, AST_Node* body) {
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
  AST_Node* type        = ast_type_any(m, undefined); // TODO: infer type from 'arguments' and 'return_type'
  AST_Node* bind        = ast_type_bind(m, undefined, symbol, type);
  AST_Node* declaration = ast_constant_bind(m, undefined, bind, function);

  conv.continuations.insert(bind->id);

  return declaration;
}

AST_Node* function_call_cps_conversion(Closure_Converter& conv, Parser* p, Scope* scope, AST_Node* program_point, AST_Node* call, AST_Node* continuation, AST_Node* bind);
void      function_literal_cps_conversion(Closure_Converter& conv, Parser* p, Scope* scope, AST_Node* function, AST_Node* continuation = NULL);
void      program_point_cps_conversion(Closure_Converter& conv, Parser* p, AST_Node* statements, Scope* scope);

void program_point_cps_conversion(Closure_Converter& conv, Parser* p, AST_Node* statements, Scope* scope) {

  AST_Manager* m = &p->ast_man;

  while (!ast_is_null_node(statements)) {

    AST_Node* statement    = ast_program_point_get_decl(m, statements);
    AST_Node* continuation = ast_program_point_get_tail(m, statements);

    if (statement->kind == AST_BIND_CONSTANT || statement->kind == AST_BIND_VARIABLE) {
      AST_Node* left  = ast_bind_get_type_bind(m, statement);
      AST_Node* right = ast_bind_get_expr(m, statement);

      if (right->kind == AST_FUNCTION_LITERAL) {
        conv.user_functions.insert(left->id);

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

AST_Node*
function_call_cps_conversion(Closure_Converter& conv, Parser* p, Scope* scope, AST_Node* program_point, AST_Node* call, AST_Node* continuation, AST_Node* bind) {
  AST_Manager* m        = &p->ast_man;
  AST_Node*    function = ast_fun_call_get_call_sym(m, call);

  // b8 is_local = false;

  AST_Node* local = scope_find(scope, p, function);

  Token undefined = lexer_undef_token();

  AST_Node* continuation_symbol = ast_node_null(m);
  AST_Node* declaration         = ast_node_null(m);

  if (!ast_is_null_node(continuation)) {
    AST_Node* type = ast_type_any(m, undefined); // TODO: use a global context to find function and the type

    declaration = create_continuation_function(conv, p, scope, bind, type, continuation);

    AST_Node* continuation_bind = ast_bind_get_type_bind(m, declaration);

    continuation_symbol = ast_type_bind_get_symbol(m, continuation_bind);
  }

  // if (local->kind == AST_BIND_VARIABLE || local->kind == AST_BIND_CONSTANT) {
  //   AST_Node* closure = ast_bind_get_expr(m, local);

  //   if (closure->kind == AST_FUNCTION_LITERAL) {
  //     //if (is_local) {
  //       // FIXME(marcos): memory leak
  //       // Scope* closure_scope = scope_create(scope);

  //       // function_literal_cps_conversion(p, closure_scope, closure, continuation_symbol);

  //       // AST_Node* old_body = ast_function_literal_get_body(m, closure);
  //       // AST_Node* new_body = ast_program_point(m, undefined);

  //       // new_body->left  = declaration->id;
  //       // new_body->right = old_body->id;

  //       // closure->right = new_body->id;
  // 		// }

  //     if (!ast_is_null_node(bind)) {
  //       program_point->left  = call->id;
  //       program_point->right = ast_node_null(m)->id;
  //     }

  //     // return program_point;
  //   }
  // }

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

void function_literal_cps_conversion(Closure_Converter& conv, Parser* p, Scope* scope, AST_Node* function, AST_Node* continuation_symbol) {
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

  program_point_cps_conversion(conv, p, statements, scope);
}

void cps_conversion(Closure_Converter& conv, Parser* p, AST_Node* root) {
  assert(root->kind == AST_PROGRAM_POINT);

  AST_Manager* m = &p->ast_man;

  while (!ast_is_null_node(root)) {
    AST_Node* prog_point = ast_manager_get_relative(m, root, root->left);

    if (prog_point->kind == AST_BIND_CONSTANT || prog_point->kind == AST_BIND_VARIABLE) {
      AST_Node* decl = ast_bind_get_expr(m, prog_point);

      if (decl->kind == AST_FUNCTION_LITERAL) {
        // FIXME(marcos): memory leak
        Scope* scope = scope_create(NULL);

        function_literal_cps_conversion(conv, p, scope, decl);
      }
    }

    root = ast_manager_get_relative(m, root, root->right);
  }
}

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

Scope* declaration_arguments_to_scope(Parser* p, AST_Node* node, Scope* parent = NULL) {
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

      assert(!ast_is_null_node(declaration) && "Declaration not found");

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
        // print_ast(p, n);
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

    Scope* closure_scope = declaration_arguments_to_scope(p, arguments, scope);

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
  Alias_Scope* a = new Alias_Scope(); //(Alias_Scope*)malloc(sizeof(Alias_Scope));

  a->aliases = Alias_Scope::Alias_Map();
  a->aliases = alias->aliases;
  a->parent  = copy_alias_scope_to_heap(alias->parent);
  return a;
}

void free_alias_scope(Alias_Scope* alias) {
  if (alias == NULL) return;

  free_alias_scope(alias->parent);

  free(alias);
}

Alias_Scope::Alias_Set* find_alias_set(Alias_Scope* aliases, AST_Id id) {
  while (aliases) {
    if (aliases->aliases.find(id) != aliases->aliases.end()) { return &aliases->aliases[id]; }

    aliases = aliases->parent;
  }

  return NULL;
}

void assign_left_to_right_alias(Alias_Scope* aliases, AST_Id left, AST_Id right, b8 reset = true) {
  Alias_Scope::Alias_Set* left_aliases  = find_alias_set(aliases, left);
  Alias_Scope::Alias_Set* right_aliases = find_alias_set(aliases, right);

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

void print_aliases(Parser* p, Alias_Scope* aliases, AST_Id l) {
  AST_Manager* m = &p->ast_man;
  AST_Node*    n = ast_manager_get(m, l);
  printf("[| ");
  print_ast_to_program(p, ast_type_bind_get_symbol(m, n));
  printf(" |] := ");
  printf("{ ");
  u64 i = 0;

  Alias_Scope::Alias_Set* s = find_alias_set(aliases, l);

  for (Alias_Scope::Alias_Set::iterator it = s->begin(); it != s->end(); it++) {
    i = i + 1;

    AST_Node* n = ast_manager_get(m, *it);

    print_ast_to_program(p, n);

    if (i != s->size()) printf(", ");
  }
  printf(" }\n");
}

void print_all_aliases(Parser* p, Alias_Scope* aliases) {
  if (aliases == NULL) return;

  for (Alias_Scope::Alias_Map::iterator it = aliases->aliases.begin(); it != aliases->aliases.end(); it++) {
    print_aliases(p, aliases, it->first);
  }

  print_all_aliases(p, aliases->parent);
}

void replace_aliases(Alias_Scope* a, Alias_Scope* b) {
  while (a) {
    assert(a && b);
    a->aliases = b->aliases;
    a          = a->parent;
    b          = b->parent;
  }
}

void merge_aliases(Alias_Scope* a, Alias_Scope* b) {
  while (b) {
    assert(b && a);
    for (Alias_Scope::Alias_Map::iterator it0 = b->aliases.begin(); it0 != b->aliases.end(); it0++) {
      for (Alias_Scope::Alias_Set::iterator it1 = it0->second.begin(); it1 != it0->second.end(); it1++) {
        if (a->aliases.find(it0->first) == a->aliases.end()) { a->aliases[it0->first] = Alias_Scope::Alias_Set(); }

        a->aliases[it0->first].insert(*it1);
      }
    }

    a = a->parent;
    b = b->parent;
  }
}

void insert_all_function_literal_aliases(Alias_Scope* aliases, Scope* scope, Parser* p, AST_Node* node, Alias_Scope::Alias_Set* literals) {
  AST_Manager* m = &p->ast_man;

  if (ast_is_null_node(node)) return;

  if (node->kind == AST_FUNCTION_LITERAL) {
    literals->insert(node->id);
    return;
  }

  assert(node->kind == AST_BIND_TYPE);

  Alias_Scope::Alias_Set* values = find_alias_set(aliases, node->id);

  if (values == NULL) return;

  for (Alias_Scope::Alias_Set::iterator it = values->begin(); it != values->end(); it++) {
    AST_Id    alias = *it;
    AST_Node* value = ast_manager_get(m, alias);
    if (value->kind == AST_FUNCTION_LITERAL) {
      literals->insert(value->id);
    } else {
      AST_Node* declaration = scope_find(scope, p, value);

      if (declaration->kind == AST_BIND_CONSTANT || declaration->kind == AST_BIND_VARIABLE) { declaration = ast_bind_get_type_bind(m, declaration); }

      insert_all_function_literal_aliases(aliases, scope, p, declaration, literals);
    }
  }
}
void insert_all_aliases_to_function_literals(Alias_Scope* aliases, Scope* scope, Parser* p, AST_Node* node, Alias_Scope::Alias_Set* literals) {
  AST_Manager* m = &p->ast_man;

  if (ast_is_null_node(node)) return;

  Alias_Scope::Alias_Set* set = find_alias_set(aliases, node->id);

  if (set) {
    for (Alias_Scope::Alias_Set::iterator it = set->begin(); it != set->end(); it++) {
      AST_Node* value = ast_manager_get(m, *it);
      if (value->kind == AST_FUNCTION_LITERAL) { literals->insert(node->id); }
    }
  }

  assert(node->kind == AST_BIND_TYPE);

  Alias_Scope::Alias_Set* values = find_alias_set(aliases, node->id);

  if (values == NULL) return;

  for (Alias_Scope::Alias_Set::iterator it = values->begin(); it != values->end(); it++) {
    AST_Id    alias = *it;
    AST_Node* value = ast_manager_get(m, alias);

    AST_Node* declaration = scope_find(scope, p, value);

    if (declaration->kind == AST_BIND_CONSTANT || declaration->kind == AST_BIND_VARIABLE) { declaration = ast_bind_get_type_bind(m, declaration); }

    insert_all_aliases_to_function_literals(aliases, scope, p, declaration, literals);
  }
}

void get_function_literals(Parser* p, Alias_Scope* alias, AST_Node* call, Scope* scope, Alias_Scope::Alias_Set* literals) {
  AST_Manager* m = &p->ast_man;

  AST_Node* symbol      = ast_fun_call_get_call_sym(m, call);
  AST_Node* declaration = scope_find(scope, p, symbol);

  // NOTE(marcos): if a declaration is a type bind it is a argument.
  if (ast_is_null_node(declaration)) {
    // TODO(marcos): assume that all closures being passed as argumens are escaping
    AST_Node* arguments    = ast_fun_call_get_call_args(m, call);
    AST_Node* continuation = get_last(p, arguments);
    declaration            = scope_find(scope, p, continuation);
  }

  if (declaration->kind == AST_BIND_CONSTANT || declaration->kind == AST_BIND_VARIABLE) { declaration = ast_bind_get_type_bind(m, declaration); }

  return insert_all_function_literal_aliases(alias, scope, p, declaration, literals);
}

void get_aliases_to_function_literals(Parser* p, Alias_Scope* alias, AST_Node* node, Scope* scope, Alias_Scope::Alias_Set* literals) {
  AST_Manager* m = &p->ast_man;

  AST_Node* declaration = scope_find(scope, p, node);

  if (declaration->kind == AST_BIND_CONSTANT || declaration->kind == AST_BIND_VARIABLE) { declaration = ast_bind_get_type_bind(m, declaration); }

  return insert_all_aliases_to_function_literals(alias, scope, p, declaration, literals);
}

u64 function_literal_arguments_count(Parser* p, AST_Node* literal) {
  AST_Manager* m = &p->ast_man;

  AST_Node* signature = ast_function_literal_get_signature(m, literal);
  AST_Node* arguments = ast_function_signature_get_args(m, signature);

  u64 count = 0;

  while (!ast_is_null_node(arguments)) {
    count += 1;
    arguments = ast_decl_list_get_tail(m, arguments);
  }

  return count;
}

u64 function_call_arguments_count(Parser* p, AST_Node* call) {
  AST_Manager* m = &p->ast_man;

  AST_Node* arguments = ast_fun_call_get_call_args(m, call);

  u64 count = 0;

  while (!ast_is_null_node(arguments)) {
    count += 1;
    arguments = ast_decl_list_get_tail(m, arguments);
  }

  return count;
}

b8 is_symbols_aliases(Alias_Scope* aliases, Scope* scope, Parser* p, AST_Node* a, AST_Node* b) {
  // NOTE(marcos): the efficience of this is pretty bad

  AST_Manager* m = &p->ast_man;

  if (a->kind != b->kind) return false;

  assert(a->kind == AST_BIND_TYPE);

  AST_Node* l = ast_type_bind_get_symbol(m, a);
  AST_Node* r = ast_type_bind_get_symbol(m, b);

  if (parser_is_same_symbol(p, l, r)) { return true; }

  Alias_Scope::Alias_Set* set_b = find_alias_set(aliases, b->id);

  if (set_b) {
    for (Alias_Scope::Alias_Set::iterator it = set_b->begin(); it != set_b->end(); it++) {
      AST_Node* c = ast_manager_get(m, *it);
      if (is_symbols_aliases(aliases, scope, p, a, c)) return true;
    }
  }

  return false;
}

void bind_function_call_arguments(Alias_Scope* aliases, Scope* scope, Parser* p, AST_Node* literal, AST_Node* call) {
  AST_Manager* m = &p->ast_man;

  u64 call_arguments_count    = function_call_arguments_count(p, call);
  u64 literal_arguments_count = function_literal_arguments_count(p, literal);

  AST_Node* signature = ast_function_literal_get_signature(m, literal);
  AST_Node* bindings  = ast_function_signature_get_args(m, signature);

  if (call_arguments_count == literal_arguments_count) {
    AST_Node* arguments = ast_fun_call_get_call_args(m, call);

    while (!ast_is_null_node(bindings)) {
      AST_Node* binding = ast_decl_list_get_elem(m, bindings);
      AST_Node* value   = ast_undefined(m);

      if (!ast_is_null_node(arguments)) {
        value     = ast_decl_list_get_elem(m, arguments);
        arguments = ast_decl_list_get_tail(m, arguments);
      }

      scope_push(scope, binding);

      assign_left_to_right_alias(aliases, binding->id, value->id);

      bindings = ast_decl_list_get_tail(m, bindings);
    }
  }

  // NOTE(marcos): assuming continuation is going to be called
  if (literal_arguments_count) {
    while (!ast_is_null_node(bindings)) {
      AST_Node* binding = ast_decl_list_get_elem(m, bindings);

      scope_push(scope, binding);

      aliases->aliases[binding->left].insert(ast_undefined(m)->id);
      bindings = ast_program_point_get_tail(m, bindings);
    }
  }
}

void push_argument_aliases(Parser* p, AST_Node* function, AST_Node* values, Alias_Scope* aliases) {
  AST_Manager* m         = &p->ast_man;
  AST_Node*    signature = ast_function_literal_get_signature(m, function);
  AST_Node*    arguments = ast_function_signature_get_args(m, signature);

  while (!ast_is_null_node(arguments)) {
    AST_Node* binding = ast_decl_list_get_elem(m, arguments);

    if (ast_is_null_node(values)) {
      aliases->aliases[binding->id].insert(ast_undefined(m)->id);
    } else {
      AST_Node* value = ast_decl_list_get_elem(m, values);
      aliases->aliases[binding->id].insert(value->id);
      values = ast_decl_list_get_tail(m, values);
    }

    arguments = ast_decl_list_get_tail(m, arguments);
  }
}

void function_literal_escape_analysis(
    Closure_Converter& conv, Parser* p, AST_Node* root, AST_Node* cont, Scope* scope, Alias_Scope* aliases, Alias_Scope::Alias_Set* escapes) {
  if (ast_is_null_node(root)) return;

  AST_Manager* m = &p->ast_man;

  if (root->kind == AST_BIND_CONSTANT || root->kind == AST_BIND_VARIABLE) {

    AST_Node* left = ast_bind_get_type_bind(m, root);

    scope_push(scope, left);

    AST_Node* right = ast_bind_get_expr(m, root);

    // if (right->kind == AST_FUNCTION_CALL) {
    //   printf("HAHAHAHA\n");
    //   Alias_Scope::Alias_Set literals;

    //   get_function_literals(p, aliases, right, scope, &literals);

    //   Closure_Converter converter = conv;

    //   aliases->aliases[left->id] = Alias_Scope::Alias_Set();

    //   for (Alias_Scope::Alias_Set::iterator it = literals.begin(); it != literals.end(); it++) {
    //     AST_Node* expr = ast_manager_get(m, *it);

    //     AST_Node* signature = ast_function_literal_get_signature(m, expr);
    //     AST_Node* body      = ast_function_literal_get_body(m, expr);
    //     AST_Node* arguments = ast_function_signature_get_args(m, signature);

    //     Scope* scope = declaration_arguments_to_scope(p, arguments, scope);

    //     AST_Node* continuation = get_last(p, arguments);

    //     // Alias_Scope function_aliases = {.parent = NULL, .aliases = Alias_Scope::Alias_Map()};

    //     push_argument_aliases(p, expr, ast_fun_call_get_call_args(m, right), aliases);

    //     Alias_Scope::Alias_Set escapes;

    //     function_literal_escape_analysis(converter, p, body, continuation, scope, aliases, &escapes);

    //     for (Alias_Scope::Alias_Set::iterator it = escapes.begin(); it != escapes.end(); it++) {
    //       aliases->aliases[left->id].insert(*it);
    //     }
    //   }
    // } else {
    // }
    return assign_left_to_right_alias(aliases, left->id, right->id);
  }

  if (root->kind == AST_OP_BIN_ASSIGN) {
    AST_Node* left  = ast_manager_get_relative(m, root, root->left);
    AST_Node* bind  = scope_find(scope, p, left);
    AST_Node* right = ast_manager_get_relative(m, root, root->right);

    if (bind->kind == AST_BIND_CONSTANT || bind->kind == AST_BIND_VARIABLE) { bind = ast_bind_get_type_bind(m, bind); }

    assert(bind->kind == AST_BIND_TYPE);

    return assign_left_to_right_alias(aliases, bind->id, right->id);
  }

  if (root->kind == AST_FUNCTION_CALL) {
    AST_Node* symbol = ast_fun_call_get_call_sym(m, root);

    AST_Node* declaration = scope_find(scope, p, symbol);

    if (declaration->kind == AST_BIND_CONSTANT || declaration->kind == AST_BIND_VARIABLE) { declaration = ast_bind_get_type_bind(m, declaration); }

    if (is_symbols_aliases(aliases, scope, p, declaration, cont)) {
      AST_Node* arguments = ast_fun_call_get_call_args(m, root);

      while (!ast_is_null_node(arguments)) {
        AST_Node* arg = ast_decl_list_get_elem(m, arguments);

        get_aliases_to_function_literals(p, aliases, arg, scope, escapes);

        arguments = ast_decl_list_get_tail(m, arguments);
      }
      return;
    }

    Alias_Scope::Alias_Set literals;

    get_function_literals(p, aliases, root, scope, &literals);

    for (Alias_Scope::Alias_Set::iterator it = literals.begin(); it != literals.end(); it++) {
      // Alias_Scope temp = {.aliases = Alias_Scope::Alias_Map(), .parent = aliases};

      Alias_Scope* call_aliases = copy_alias_scope_to_heap(aliases);

      AST_Node* literal    = ast_manager_get(m, *it);
      AST_Node* statements = ast_function_literal_get_body(m, literal);
      AST_Node* signature  = ast_function_literal_get_signature(m, literal);
      AST_Node* arguments  = ast_function_signature_get_args(m, signature);

      Scope* call_scope = scope_create(scope); // TODO(marcos): fix leaks

      bind_function_call_arguments(call_aliases, call_scope, p, literal, root);

      function_literal_escape_analysis(conv, p, statements, cont, call_scope, call_aliases, escapes);

      if (it == literals.begin()) {
        replace_aliases(aliases, call_aliases);
      } else {
        merge_aliases(aliases, call_aliases);
      }
    }

    return;
  }

  if (root->kind == AST_CTRL_FLOW_IF) {
    AST_Node* body = ast_manager_get_relative(m, root, root->right);

    Alias_Scope* branch_aliases = copy_alias_scope_to_heap(aliases);

    function_literal_escape_analysis(conv, p, body, cont, scope, branch_aliases, escapes);

    merge_aliases(aliases, branch_aliases);
    return;
  }

  if (root->kind == AST_CTRL_FLOW_IF_ELSE) {
    AST_Node* branch = ast_manager_get_relative(m, root, root->left);
    AST_Node* tail   = ast_manager_get_relative(m, root, root->right);

    Alias_Scope* branch_aliases = copy_alias_scope_to_heap(aliases);

    function_literal_escape_analysis(conv, p, branch, cont, scope, branch_aliases, escapes);

    while (tail->kind == AST_CTRL_FLOW_IF_ELSE) {
      branch = ast_manager_get_relative(m, tail, tail->left);

      Alias_Scope* _branch_aliases = copy_alias_scope_to_heap(aliases);

      function_literal_escape_analysis(conv, p, branch, cont, scope, _branch_aliases, escapes);

      merge_aliases(branch_aliases, _branch_aliases);

      tail = ast_manager_get_relative(m, tail, tail->right);
    }

    if (!ast_is_null_node(tail)) {
      Alias_Scope* _branch_aliases = copy_alias_scope_to_heap(aliases);

      function_literal_escape_analysis(conv, p, tail, cont, scope, _branch_aliases, escapes);

      merge_aliases(branch_aliases, _branch_aliases);
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

  function_literal_escape_analysis(conv, p, left, cont, scope, aliases, escapes);
  function_literal_escape_analysis(conv, p, right, cont, scope, aliases, escapes);
}

void escape_analysis(Closure_Converter& converter, Parser* p, AST_Node* root) {
  assert(root->kind == AST_PROGRAM_POINT);
  AST_Manager* m = &p->ast_man;

  while (!ast_is_null_node(root)) {
    AST_Node* statement = ast_program_point_get_decl(m, root);

    if (statement->kind == AST_BIND_CONSTANT || statement->kind == AST_BIND_VARIABLE) {
      AST_Node* expr = ast_bind_get_expr(m, statement);
      if (expr->kind == AST_FUNCTION_LITERAL) {
        AST_Node* signature    = ast_function_literal_get_signature(m, expr);
        AST_Node* body         = ast_function_literal_get_body(m, expr);
        AST_Node* arguments    = ast_function_signature_get_args(m, signature);
        Scope*    scope        = declaration_arguments_to_scope(p, arguments);
        AST_Node* continuation = get_last(p, arguments);

        Alias_Scope function_aliases = {.parent = NULL, .aliases = Alias_Scope::Alias_Map()};

        push_argument_aliases(p, expr, ast_node_null(m), &function_aliases);

        Alias_Scope::Alias_Set escapes;

        // Alias_Scope aliases = {.parent = &function_aliases, .aliases = Alias_Scope::Alias_Map()};

        function_literal_escape_analysis(converter, p, body, continuation, scope, &function_aliases, &escapes);

        for (Alias_Scope::Alias_Map::iterator it = function_aliases.aliases.begin(); it != function_aliases.aliases.end(); it++) {
          for (Alias_Scope::Alias_Set::iterator val = it->second.begin(); val != it->second.end(); val++) {
            if (converter.continuations.find(*val) != converter.continuations.end()) { converter.escaping_kind[*val] = Closure_Converter::CONTINUATION_ESCAPING; }
            if (converter.user_functions.find(*val) != converter.continuations.end()) { converter.escaping_kind[*val] = Closure_Converter::FUNCTION_ESCAPING; }
          }
        }

        for (Alias_Scope::Alias_Set::iterator it = escapes.begin(); it != escapes.end(); it++) {
          AST_Node* n = ast_manager_get(m, *it);
          if (converter.continuations.find(*it) != converter.continuations.end()) { converter.escaping_kind[*it] = Closure_Converter::CONTINUATION_ESCAPING; }
          if (converter.user_functions.find(*it) != converter.continuations.end()) { converter.escaping_kind[*it] = Closure_Converter::FUNCTION_ESCAPING; }
        }
      }
    }

    root = ast_program_point_get_tail(m, root);
  }

  for (Closure_Converter::Continuations::iterator it = converter.continuations.begin(); it != converter.continuations.end(); it++) {
    if (converter.escaping_kind.find(*it) == converter.escaping_kind.end()) { converter.escaping_kind[*it] = Closure_Converter::CONTINUATION_LOCAL; }
  }

  for (Closure_Converter::User_Functions::iterator it = converter.user_functions.begin(); it != converter.user_functions.end(); it++) {
    if (converter.escaping_kind.find(*it) == converter.escaping_kind.end()) { converter.escaping_kind[*it] = Closure_Converter::FUNCTION_LOCAL; }
  }

  for (Closure_Converter::Continuations::iterator it = converter.continuations.begin(); it != converter.continuations.end(); it++) {
    AST_Node* node = ast_manager_get(m, *it);
    printf("[| ");
    print_ast_to_program(p, node);
    printf(" |] = ");
    if (converter.escaping_kind[*it] == Closure_Converter::CONTINUATION_LOCAL) printf("local_continuation");
    if (converter.escaping_kind[*it] == Closure_Converter::CONTINUATION_ESCAPING) printf("escaping_continuation");
    printf("\n");
  }

  for (Closure_Converter::User_Functions::iterator it = converter.user_functions.begin(); it != converter.user_functions.end(); it++) {
    AST_Node* node = ast_manager_get(m, *it);
    printf("[| ");
    print_ast_to_program(p, node);
    printf(" |] = ");
    if (converter.escaping_kind[*it] == Closure_Converter::FUNCTION_LOCAL) printf("local_function");
    if (converter.escaping_kind[*it] == Closure_Converter::FUNCTION_ESCAPING) printf("escaping_function");
    printf("\n");
  }
}

void closure_conversion(Parser* p, AST_Node* root) {
  Closure_Converter converter;

  cps_conversion(converter, p, root);

  print_ast_to_program(p, root); // TODO(marcos): remove

  escape_analysis(converter, p, root);

  build_extended_cps_graph(converter.cps_extended_call_graph, p, root);

  print_cps_extended_graph_context(converter.cps_extended_call_graph, p);

  Scope* scopeA = scope_create(NULL); // FIXME(marcos): fix leak
  compute_function_declaration_stage_numbers(converter, p, root, scopeA);

  Scope* scopeB = scope_create(NULL); // FIXME(marcos): fix leak
  compute_function_declaration_stage_numbers(converter, p, root, scopeB);

  Scope* scopeC = scope_create(NULL); // FIXME(marcos): fix leak
  compute_use_time_rec(converter, scopeC, p, NULL, root);
}
