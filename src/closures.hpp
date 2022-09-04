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
 TODO(marcos): All continuations needs to be passed as a closure
 TODO(marcos): Any function being passed as an argument needs to be passed as a closure with null environment

 FIXME(marcos): Recursive functions are present on free variables
*/

#include "ast.hpp"
#include "context.hpp"
#include "error.hpp"
#include "lexer.hpp"
#include "parser.hpp"

#include "continuations.hpp"

#include <unordered_map>
#include <unordered_set>
#include <vector>

struct Call_Graph_Edges {
  std::unordered_set< AST_Id > to;
  std::unordered_set< AST_Id > pred;
};

typedef std::unordered_map< AST_Id, Call_Graph_Edges > Call_Graph;

enum Escaping_Kind {
  FUNCTION_LOCAL,
  FUNCTION_ESCAPING,
  CONTINUATION_ESCAPING,
  CONTINUATION_LOCAL,
};

struct Lifetime {
  u64 last_use_time;
  u64 first_use_time;
};

typedef std::unordered_set< AST_Id >                      Local_Functions_Set;
typedef std::unordered_map< AST_Id, u64 >                 AST_To_U64_Map;
typedef std::unordered_map< AST_Id, Lifetime >            Lifetimes;
typedef std::unordered_map< AST_Id, Local_Functions_Set > AST_Collection;
typedef std::unordered_map< AST_Id, Escaping_Kind >       Escaping_Kind_Map;

struct Closure_Analysis {

  Local_Functions_Set     user_functions;
  Local_Continuations_Set continuations;

  Call_Graph        call_graph;
  AST_To_U64_Map    strategy_number;
  AST_To_U64_Map    stage_number;
  AST_Collection    free_variables;
  Escaping_Kind_Map escaping_kind;
  Lifetimes         lifetimes;
  AST_Collection    escaping_place;

  // Functions and Continuations of escaping functions will be
  // duplicated for a optmized
  b8 lift_escaping_functions_of_local_calls;
};

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
  if (ast_is_null_node(node)) return;

  AST_Manager* m           = &p->ast_man;
  AST_Node*    declaration = scope_find(scope, p, node);

  if (declaration->kind == AST_BIND_CONSTANT || declaration->kind == AST_BIND_VARIABLE) { declaration = ast_bind_get_type_bind(m, declaration); }

  if (declaration->kind == AST_BIND_TYPE) { return insert_all_aliases_to_function_literals(alias, scope, p, declaration, literals); }

  AST_Node* left  = ast_manager_get_relative(m, node, node->left);
  AST_Node* right = ast_manager_get_relative(m, node, node->right);

  get_aliases_to_function_literals(p, alias, left, scope, literals);
  get_aliases_to_function_literals(p, alias, right, scope, literals);
}

AST_Node* get_call_last_argument(Parser* p, AST_Node* call, AST_Node** arg = NULL) {
  AST_Manager* m = &p->ast_man;

  AST_Node* head = ast_manager_get_relative(m, call, call->right);

  return get_last(p, head, arg);
}
void build_extended_cps_graph_declaration(
    Call_Graph& context, Alias_Scope* aliases, Scope* scope, Parser* p, AST_Node* function, AST_Node* statement, AST_Node* cont_symbol) {
  // TODO(marcos): this functions is not considering aliases

  AST_Manager* m = &p->ast_man;

  if (statement->kind == AST_FUNCTION_CALL) {
    Alias_Scope::Alias_Set functions;

    AST_Node* symbol = ast_fun_call_get_call_sym(m, statement);

    get_aliases_to_function_literals(p, aliases, symbol, scope, &functions);

    AST_Node* arguments = ast_fun_call_get_call_args(m, statement);

    // AST_Node* declaration = scope_find(scope, p, symbol);
    if (functions.size()) {
      for (Alias_Scope::Alias_Set::iterator it = functions.begin(); it != functions.end(); it++) {
        AST_Node* bind = ast_manager_get(m, *it);

        context[function->id].to.insert(bind->id);
        context[bind->id].pred.insert(function->id);
      }
    } else {
      if (parser_is_same_symbol(p, symbol, cont_symbol)) return;
      // NOTE(marcos): extern function

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
    Alias_Scope branch_aliases = {.parent = NULL, .aliases = aliases->aliases};

    AST_Node* expr = ast_manager_get_relative(m, statement, statement->right);

    build_extended_cps_graph_declaration(context, &branch_aliases, scope, p, function, expr, cont_symbol);

    return merge_aliases(aliases, &branch_aliases);
  }

  if (statement->kind == AST_CTRL_FLOW_IF_ELSE) {
    Alias_Scope branches_aliases = {.parent = NULL, .aliases = aliases->aliases};

    AST_Node* expr = ast_node_null(m);

    while (!ast_is_null_node(statement)) {
      expr = ast_manager_get_relative(m, statement, statement->left);

      Alias_Scope branch_aliases = {.parent = NULL, .aliases = aliases->aliases};

      build_extended_cps_graph_declaration(context, &branch_aliases, scope, p, function, expr, cont_symbol);

      merge_aliases(&branches_aliases, &branch_aliases);

      statement = ast_manager_get_relative(m, statement, statement->right);
    }

    if (!ast_is_null_node(statement)) {
      Alias_Scope branch_aliases = {.parent = NULL, .aliases = aliases->aliases};

      build_extended_cps_graph_declaration(context, &branch_aliases, scope, p, function, statement, cont_symbol);

      replace_aliases(aliases, &branch_aliases);
    }

    return replace_aliases(aliases, &branches_aliases);
  }

  if (statement->kind == AST_BIND_CONSTANT || statement->kind == AST_BIND_VARIABLE) {

    AST_Node* bind = ast_bind_get_type_bind(m, statement);
    AST_Node* expr = ast_bind_get_expr(m, statement);

    scope_push(scope, statement);

    assign_left_to_right_alias(aliases, bind->id, expr->id);

    if (expr->kind == AST_FUNCTION_LITERAL) {

      assert(bind->kind == AST_BIND_TYPE);

      context[bind->id] = Call_Graph_Edges();

      AST_Node* signature     = ast_function_literal_get_signature(m, expr);
      AST_Node* arguments     = ast_function_signature_get_args(m, signature);
      AST_Node* body          = ast_function_literal_get_body(m, expr);
      Scope*    closure_scope = scope_create(scope); // declaration_arguments_to_scope(p, arguments, scope);

      build_extended_cps_graph_declaration(context, aliases, closure_scope, p, bind, body, cont_symbol);
    }
  }

  if (statement->kind == AST_OP_BIN_ASSIGN) {
    AST_Node* left  = ast_manager_get_relative(m, statement, statement->left);
    AST_Node* right = ast_manager_get_relative(m, statement, statement->right);

    AST_Node* bind = scope_find(scope, p, left);

    if (ast_is_null_node(bind)) assert(false && "Declaration not found");

    assign_left_to_right_alias(aliases, bind->id, right->id);
  }

  if (statement->kind == AST_PROGRAM_POINT) {
    while (!ast_is_null_node(statement)) {
      assert(statement->kind == AST_PROGRAM_POINT);

      AST_Node* closure_statement = ast_program_point_get_decl(m, statement);

      build_extended_cps_graph_declaration(context, aliases, scope, p, function, closure_statement, cont_symbol);

      statement = ast_program_point_get_tail(m, statement);
    }
  }
}

void build_extended_cps_graph(Closure_Analysis& analysis, Parser* p, AST_Node* root) {
  assert(root->kind == AST_PROGRAM_POINT);

  AST_Manager* m = &p->ast_man;

  while (!ast_is_null_node(root)) {
    AST_Node*   statement = ast_program_point_get_decl(m, root);
    Scope*      scope     = scope_create(NULL); // FIXME(marcos): memory leak
    Alias_Scope aliases   = {.parent = NULL, .aliases = Alias_Scope::Alias_Map()};

    if (statement->kind == AST_BIND_CONSTANT || statement->kind == AST_BIND_VARIABLE) {
      AST_Node* right = ast_bind_get_expr(m, statement);
      if (right->kind == AST_FUNCTION_LITERAL) {
        AST_Node* left = ast_bind_get_type_bind(m, statement);

        AST_Node* signature    = ast_function_literal_get_signature(m, right);
        AST_Node* arguments    = ast_function_signature_get_args(m, signature);
        AST_Node* continuation = get_last(p, arguments);
        AST_Node* cont_symbol  = ast_type_bind_get_symbol(m, continuation);

        build_extended_cps_graph_declaration(analysis.call_graph, &aliases, scope, p, ast_node_null(m), statement, cont_symbol);
      }
    }
    root = ast_program_point_get_tail(m, root);
  }
}

void compute_function_declaration_stage_numbers(Closure_Analysis& converter, Parser* p, AST_Node* root, Scope* scope) {
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

void compute_continuations_stage_numbers(Closure_Analysis& converter, Parser* p, AST_Node* root, Scope* scope) {
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

    std::unordered_set< AST_Id >& pred = converter.call_graph[bind->id].pred;

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

void print_cps_extended_graph(Call_Graph& ctx, AST_Id node, Parser* p) {
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

void print_cps_extended_graph_context(Call_Graph& ctx, Parser* p) {
  AST_Manager* m = &p->ast_man;

  for (Call_Graph::iterator it = ctx.begin(); it != ctx.end(); it++) {
    print_cps_extended_graph(ctx, it->first, p);
  }
}

struct FunctionDeclaration {
  AST_Node*            declaration;
  FunctionDeclaration* prev;
};

void compute_free_variables_lifetimes(Closure_Analysis& analysis, Scope* scope, Parser* p, FunctionDeclaration* enclosing, AST_Node* node) {
  if (ast_is_null_node(node)) return;

  AST_Manager* m = &p->ast_man;

  if (node->kind == AST_SYMBOL_LITERAL || ast_is_temporary(m, node)) {
    // if (enclosing == NULL) return;

    b8 is_local = false;

    while (enclosing != NULL) {
      AST_Node* declaration = scope_find(scope, p, node, &is_local);

      assert(!ast_is_null_node(declaration) && "Declaration not found");

      if (is_local) { break; }

      u64 stage_number = analysis.stage_number[enclosing->declaration->id];

      Local_Functions_Set& fv = analysis.free_variables[enclosing->declaration->id];

      fv.insert(declaration->id);

      if (declaration->kind == AST_BIND_CONSTANT || declaration->kind == AST_BIND_VARIABLE) { declaration = ast_bind_get_type_bind(m, declaration); }

      assert(declaration->kind == AST_BIND_TYPE);

      if (analysis.lifetimes.find(declaration->id) == analysis.lifetimes.end()) {
        analysis.lifetimes[declaration->id].first_use_time = stage_number;
        analysis.lifetimes[declaration->id].last_use_time  = stage_number;
      } else {
        analysis.lifetimes[declaration->id].first_use_time = std::min(analysis.lifetimes[declaration->id].first_use_time, stage_number);
        analysis.lifetimes[declaration->id].last_use_time  = std::max(analysis.lifetimes[declaration->id].last_use_time, stage_number);
      }

      scope     = scope->parent;
      enclosing = enclosing->prev;
    }

    return;
  }

  if (node->kind == AST_BIND_CONSTANT || node->kind == AST_BIND_VARIABLE) {
    scope_push(scope, node);

    AST_Node* right = ast_bind_get_expr(m, node);
    AST_Node* left  = ast_bind_get_type_bind(m, node);

    if (right->kind == AST_FUNCTION_LITERAL) {
      FunctionDeclaration declaration;

      declaration.prev        = enclosing;
      declaration.declaration = left;

      analysis.free_variables[left->id] = Local_Functions_Set();

      // declaration.free_variables = FunctionDeclaration::Free_Variables();

      compute_free_variables_lifetimes(analysis, scope, p, &declaration, right);

      printf("FreeVariables(");
      print_ast_to_program(p, ast_manager_get_relative(m, ast_manager_get_relative(m, node, node->left), ast_manager_get_relative(m, node, node->left)->left));
      printf(") = { ");

      u64 i = 0;

      for (Local_Functions_Set::iterator it = analysis.free_variables[left->id].begin(); it != analysis.free_variables[left->id].end(); it++) {
        i++;
        AST_Node* n = ast_manager_get(m, *it);
        if (n->kind == AST_BIND_VARIABLE || n->kind == AST_BIND_CONSTANT) { n = ast_manager_get_relative(m, n, n->left); }
        // print_ast(p, n);
        AST_Id id = n->id;
        assert(n->kind == AST_BIND_TYPE);
        if (n->kind == AST_BIND_TYPE) { n = ast_manager_get_relative(m, n, n->left); }

        print_ast_to_program(p, n);
        printf("[%lu, %lu]", analysis.lifetimes[id].first_use_time, analysis.lifetimes[id].last_use_time);
        if (i != analysis.free_variables[left->id].size()) printf(", ");
      }

      printf(" }\n");

      // converter.free_variables[node->id] = declaration.free_variables;
      return;
    } else {
      return compute_free_variables_lifetimes(analysis, scope, p, enclosing, right);
    }
  }

  if (node->kind == AST_FUNCTION_LITERAL) {
    AST_Node* signature = ast_function_literal_get_signature(m, node);
    AST_Node* arguments = ast_function_signature_get_args(m, signature);

    Scope* closure_scope = declaration_arguments_to_scope(p, arguments, scope);

    AST_Node* body = ast_function_literal_get_body(m, node);

    return compute_free_variables_lifetimes(analysis, closure_scope, p, enclosing, body);
  }

  AST_Node* l = ast_manager_get_relative(m, node, node->left);
  AST_Node* r = ast_manager_get_relative(m, node, node->right);

  compute_free_variables_lifetimes(analysis, scope, p, enclosing, l);
  compute_free_variables_lifetimes(analysis, scope, p, enclosing, r);
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
    Closure_Analysis& conv, Parser* p, AST_Node* root, AST_Node* cont, Scope* scope, Alias_Scope* aliases, Alias_Scope::Alias_Set* escapes, AST_Node* parent) {
  if (ast_is_null_node(root)) return;

  AST_Manager* m = &p->ast_man;

  if (root->kind == AST_BIND_CONSTANT || root->kind == AST_BIND_VARIABLE) {

    AST_Node* left = ast_bind_get_type_bind(m, root);

    scope_push(scope, left);

    AST_Node* right = ast_bind_get_expr(m, root);

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

    Alias_Scope::Alias_Set* escapings = new Alias_Scope::Alias_Set();

    if (is_symbols_aliases(aliases, scope, p, declaration, cont)) {
      AST_Node* arguments = ast_fun_call_get_call_args(m, root);

      while (!ast_is_null_node(arguments)) {
        AST_Node* arg = ast_decl_list_get_elem(m, arguments);

        get_aliases_to_function_literals(p, aliases, arg, scope, escapings);

        arguments = ast_decl_list_get_tail(m, arguments);
      }

      for (Alias_Scope::Alias_Set::iterator it = escapings->begin(); it != escapings->end(); it++) {
        AST_Id literal = *it;

        assert(parent->kind == AST_PROGRAM_POINT);

        conv.escaping_place[literal].insert(parent->id);

        escapes->insert(literal);
      }

      delete escapings;

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

      function_literal_escape_analysis(conv, p, statements, cont, call_scope, call_aliases, escapes, literal);

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

    function_literal_escape_analysis(conv, p, body, cont, scope, branch_aliases, escapes, root);

    merge_aliases(aliases, branch_aliases);
    return;
  }

  if (root->kind == AST_CTRL_FLOW_IF_ELSE) {
    AST_Node* branch = ast_manager_get_relative(m, root, root->left);
    AST_Node* tail   = ast_manager_get_relative(m, root, root->right);

    Alias_Scope* branch_aliases = copy_alias_scope_to_heap(aliases);

    function_literal_escape_analysis(conv, p, branch, cont, scope, branch_aliases, escapes, root);

    while (tail->kind == AST_CTRL_FLOW_IF_ELSE) {
      branch = ast_manager_get_relative(m, tail, tail->left);

      Alias_Scope* _branch_aliases = copy_alias_scope_to_heap(aliases);

      function_literal_escape_analysis(conv, p, branch, cont, scope, _branch_aliases, escapes, root);

      merge_aliases(branch_aliases, _branch_aliases);

      root = tail;
      tail = ast_manager_get_relative(m, tail, tail->right);
    }

    if (!ast_is_null_node(tail)) {
      Alias_Scope* _branch_aliases = copy_alias_scope_to_heap(aliases);

      function_literal_escape_analysis(conv, p, tail, cont, scope, _branch_aliases, escapes, root);

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

  function_literal_escape_analysis(conv, p, left, cont, scope, aliases, escapes, root);
  function_literal_escape_analysis(conv, p, right, cont, scope, aliases, escapes, root);
}

void escape_analysis(Closure_Analysis& converter, Parser* p, AST_Node* root) {
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

        function_literal_escape_analysis(converter, p, body, continuation, scope, &function_aliases, &escapes, expr);

        for (Alias_Scope::Alias_Map::iterator it = function_aliases.aliases.begin(); it != function_aliases.aliases.end(); it++) {
          for (Alias_Scope::Alias_Set::iterator val = it->second.begin(); val != it->second.end(); val++) {
            if (converter.continuations.find(*val) != converter.continuations.end()) { converter.escaping_kind[*val] = CONTINUATION_ESCAPING; }
            if (converter.user_functions.find(*val) != converter.continuations.end()) { converter.escaping_kind[*val] = FUNCTION_ESCAPING; }
          }
        }

        for (Alias_Scope::Alias_Set::iterator it = escapes.begin(); it != escapes.end(); it++) {
          AST_Node* n = ast_manager_get(m, *it);
          if (converter.continuations.find(*it) != converter.continuations.end()) { converter.escaping_kind[*it] = CONTINUATION_ESCAPING; }
          if (converter.user_functions.find(*it) != converter.continuations.end()) { converter.escaping_kind[*it] = FUNCTION_ESCAPING; }
        }
      }
    }

    root = ast_program_point_get_tail(m, root);
  }

  for (Local_Functions_Set::iterator it = converter.continuations.begin(); it != converter.continuations.end(); it++) {
    if (converter.escaping_kind.find(*it) == converter.escaping_kind.end()) { converter.escaping_kind[*it] = CONTINUATION_LOCAL; }
  }

  for (Local_Functions_Set::iterator it = converter.user_functions.begin(); it != converter.user_functions.end(); it++) {
    if (converter.escaping_kind.find(*it) == converter.escaping_kind.end()) { converter.escaping_kind[*it] = FUNCTION_LOCAL; }
  }

  for (Local_Functions_Set::iterator it = converter.continuations.begin(); it != converter.continuations.end(); it++) {
    AST_Node* node = ast_manager_get(m, *it);
    printf("[| ");
    print_ast_to_program(p, node);
    printf(" |] = ");
    if (converter.escaping_kind[*it] == CONTINUATION_LOCAL) printf("local_continuation");
    if (converter.escaping_kind[*it] == CONTINUATION_ESCAPING) printf("escaping_continuation");
    printf("\n");
  }

  for (Local_Functions_Set::iterator it = converter.user_functions.begin(); it != converter.user_functions.end(); it++) {
    AST_Node* node = ast_manager_get(m, *it);
    printf("[| ");
    print_ast_to_program(p, node);
    printf(" |] = ");
    if (converter.escaping_kind[*it] == FUNCTION_LOCAL) printf("local_function");
    if (converter.escaping_kind[*it] == FUNCTION_ESCAPING) printf("escaping_function");
    printf("\n");
  }
}

// void closure_strategy_analysis(Closure_Analysis& converter, Parser* p, AST_Node* root) {
//   AST_Manager* m = &p->ast_man;

//   if (ast_is_null_node(root)) return;

//   if (root->kind == AST_BIND_CONSTANT || root->kind == AST_BIND_VARIABLE) {
//     AST_Node* left  = ast_bind_get_type_bind(m, root);
//     AST_Node* right = ast_bind_get_expr(m, root);

//     if (right->kind == AST_FUNCTION_LITERAL) {
//       if (converter.escaping_kind.find(left->id) != converter.escaping_kind.end()) {
//         if (converter.escaping_kind[left->id] == Closure_Analysis::CONTINUATION_ESCAPING) { converter.strategy_number[left->id] = 0; }
//         if (converter.escaping_kind[left->id] == Closure_Analysis::FUNCTION_ESCAPING) { converter.strategy_number[left->id] = 0; }
//         if (converter.escaping_kind[left->id] == Closure_Analysis::FUNCTION_LOCAL) {
//           // TODO(marcos): implement accordingly
//           converter.strategy_number[left->id] = converter.free_variables[left->id].size();
//         }
//         if (converter.escaping_kind[left->id] == Closure_Analysis::CONTINUATION_LOCAL) {
//           converter.strategy_number[left->id] = converter.free_variables[left->id].size();
//         }
//       }
//     }
//   }

//   AST_Node* left  = ast_manager_get_relative(m, root, root->left);
//   AST_Node* right = ast_manager_get_relative(m, root, root->right);

//   closure_strategy_analysis(converter, p, left);
//   closure_strategy_analysis(converter, p, right);
// }

void get_transitive_closure(Closure_Analysis& conv, Parser* p, AST_Node* root) { AST_Manager* m = &p->ast_man; }

struct Environment {
  // Id of the struct that represents the values stored on this environment
  AST_Id env_struct;
  // Id of the evironment shared by this
  AST_Id env_shared;
};

struct Environment_Map {
  // map of function declaration to environment
  typedef std::unordered_map< AST_Id, Environment > environment_of;

  Environment_Map* previous;
};

struct Closures_Map {
  typedef std::unordered_set< AST_Id > closures;

  Closures_Map* previous;
};

AST_Node* remove_continuation_argument(Closure_Analysis& analysis, Parser* p, AST_Node* arguments, AST_Node* continuation, AST_Node* prev) {
  AST_Manager* m = &p->ast_man;

  if (ast_is_null_node(arguments)) { return ast_node_null(m); }

  AST_Node* right = ast_decl_list_get_tail(m, arguments);

  if (ast_is_null_node(right)) {
    if (!ast_is_null_node(prev)) { prev->right = ast_node_null(m)->id; }

    if (analysis.continuations.find(continuation->id) == analysis.continuations.end()) { return ast_decl_list_get_elem(m, arguments); }

    return ast_node_null(m);
  }

  return remove_continuation_argument(analysis, p, right, continuation, arguments);
}

void lift_local_function_body_pointer_loads(Closure_Analysis& analysis, Parser* p, AST_Node* node, Scope* scope, AST_Node* parent) {
  if (ast_is_null_node(node)) return;

  if (node->kind == AST_FUNCTION_LITERAL) return;

  AST_Manager* m = &p->ast_man;

  if (node->kind == AST_SYMBOL_LITERAL || ast_is_temporary(m, node)) {
    AST_Node* decl = scope_find(scope, p, node);

    if (ast_is_null_node(decl)) {
      AST_Node* load = ast_pointer_load(m, lexer_undef_token(), ast_copy(m, node));

      if (parent->left == node->id) {
        parent->left = load->id;
      } else if (parent->right == node->id) {
        parent->right = load->id;
      }
    }

    return;
  }

  if (node->kind == AST_BIND_CONSTANT || node->kind == AST_BIND_VARIABLE) {
    AST_Node* expr = ast_bind_get_expr(m, node);
    AST_Node* bind = ast_bind_get_type_bind(m, node);
    AST_Node* type = ast_type_bind_get_type(m, bind);

    lift_local_function_body_pointer_loads(analysis, p, type, scope, bind);
    lift_local_function_body_pointer_loads(analysis, p, expr, scope, expr);

    return scope_push(scope, node);
  }

  AST_Node* l = ast_manager_get_relative(m, node, node->left);
  AST_Node* r = ast_manager_get_relative(m, node, node->right);

  lift_local_function_body_pointer_loads(analysis, p, l, scope, node);
  lift_local_function_body_pointer_loads(analysis, p, r, scope, node);
}

void lift_local_function_declaration_arguments(Closure_Analysis& analysis, Parser* p, AST_Node* declaration) {
  AST_Manager* m = &p->ast_man;

  AST_Node* bind    = ast_bind_get_type_bind(m, declaration);
  AST_Node* literal = ast_bind_get_expr(m, declaration);

  assert(literal->kind == AST_FUNCTION_LITERAL);
  assert(analysis.free_variables.find(bind->id) != analysis.free_variables.end());

  Local_Functions_Set& fv = analysis.free_variables[bind->id];

  AST_Node* signature = ast_function_literal_get_signature(m, literal);
  AST_Node* arguments = ast_function_signature_get_args(m, signature);
  AST_Node* body      = ast_function_literal_get_body(m, literal);

  Scope* scope = declaration_arguments_to_scope(p, arguments);

  lift_local_function_body_pointer_loads(analysis, p, body, scope, literal);

  AST_Node* continuation = remove_continuation_argument(analysis, p, arguments, bind, ast_node_null(m));

  for (Local_Functions_Set::iterator it = fv.begin(); it != fv.end(); it++) {
    AST_Node* variable = ast_manager_get(m, *it);

    if (variable->kind == AST_BIND_CONSTANT || variable->kind == AST_BIND_VARIABLE) { variable = ast_bind_get_type_bind(m, variable); }
    assert(variable->kind == AST_BIND_TYPE);

    AST_Node* symbol = ast_type_bind_get_symbol(m, variable);
    AST_Node* type   = ast_type_bind_get_type(m, variable);

    AST_Node* ptr = ast_type_pointer(m, lexer_undef_token(), ast_copy(m, type));
    AST_Node* arg = ast_type_bind(m, lexer_undef_token(), ast_copy(m, symbol), ptr);

    ast_function_literal_push_argument(m, lexer_undef_token(), literal, arg);
  }

  if (!ast_is_null_node(continuation)) ast_function_literal_push_argument(m, lexer_undef_token(), literal, continuation);
}

void lift_local_function_body_environment_access(Closure_Analysis& analysis, Parser* p, AST_Node* node, Scope* scope, AST_Node* env, AST_Node* parent) {
  if (ast_is_null_node(node)) return;

  if (node->kind == AST_FUNCTION_LITERAL) return;

  AST_Manager* m = &p->ast_man;

  if (node->kind == AST_SYMBOL_LITERAL || ast_is_temporary(m, node)) {
    AST_Node* decl = scope_find(scope, p, node);

    if (ast_is_null_node(decl)) {
      AST_Node* access = ast_member_access(m, lexer_undef_token(), ast_copy(m, env), ast_copy(m, node));

      if (parent->left == node->id) {
        parent->left = access->id;
      } else if (parent->right == node->id) {
        parent->right = access->id;
      }
    }

    return;
  }

  if (node->kind == AST_BIND_CONSTANT || node->kind == AST_BIND_VARIABLE) {
    AST_Node* expr = ast_bind_get_expr(m, node);
    AST_Node* bind = ast_bind_get_type_bind(m, node);
    AST_Node* type = ast_type_bind_get_type(m, bind);

    lift_local_function_body_environment_access(analysis, p, type, scope, env, bind);
    lift_local_function_body_environment_access(analysis, p, expr, scope, env, expr);

    return scope_push(scope, node);
  }

  AST_Node* l = ast_manager_get_relative(m, node, node->left);
  AST_Node* r = ast_manager_get_relative(m, node, node->right);

  lift_local_function_body_environment_access(analysis, p, l, scope, env, node);
  lift_local_function_body_environment_access(analysis, p, r, scope, env, node);
}

void lift_local_function_declaration_environment_struct(Closure_Analysis& analysis, Parser* p, AST_Node* declaration, AST_Node** environment) {
  AST_Manager* m = &p->ast_man;

  AST_Node* bind    = ast_bind_get_type_bind(m, declaration);
  AST_Node* literal = ast_bind_get_expr(m, declaration);

  assert(literal->kind == AST_FUNCTION_LITERAL);
  assert(analysis.free_variables.find(bind->id) != analysis.free_variables.end());

  Local_Functions_Set& fv = analysis.free_variables[bind->id];

  AST_Node* signature = ast_function_literal_get_signature(m, literal);
  AST_Node* arguments = ast_function_signature_get_args(m, signature);
  AST_Node* body      = ast_function_literal_get_body(m, literal);

  Scope* scope = declaration_arguments_to_scope(p, arguments);

  AST_Node* continuation = remove_continuation_argument(analysis, p, arguments, bind, ast_node_null(m));

  AST_Node* members = ast_node_null(m);

  for (Local_Functions_Set::iterator it = fv.begin(); it != fv.end(); it++) {
    AST_Node* variable = ast_manager_get(m, *it);

    if (variable->kind == AST_BIND_CONSTANT || variable->kind == AST_BIND_VARIABLE) { variable = ast_bind_get_type_bind(m, variable); }

    assert(variable->kind == AST_BIND_TYPE);

    AST_Node* symbol = ast_type_bind_get_symbol(m, variable);
    AST_Node* type   = ast_type_bind_get_type(m, variable);

    AST_Node* ptr = ast_type_pointer(m, lexer_undef_token(), ast_copy(m, type));
    AST_Node* arg = ast_type_bind(m, lexer_undef_token(), ast_copy(m, symbol), ptr);

    members = ast_struct_member(m, lexer_undef_token(), arg, members);
  }

  AST_Node* symbol     = ast_temp_node(m);
  AST_Node* env_struct = ast_type_struct(m, lexer_undef_token(), members);
  AST_Node* env_bind   = ast_constant_bind(m, lexer_undef_token(), symbol, env_struct);
  AST_Node* arg_type   = ast_type_pointer(m, lexer_undef_token(), symbol);
  AST_Node* arg_symb   = ast_temp_node(m);
  AST_Node* arg_bind   = ast_type_bind(m, lexer_undef_token(), arg_symb, arg_type);

  ast_function_literal_push_argument(m, lexer_undef_token(), literal, arg_bind);

  *environment = env_bind;

  lift_local_function_body_environment_access(analysis, p, body, scope, arg_symb, literal);

  if (!ast_is_null_node(continuation)) ast_function_literal_push_argument(m, lexer_undef_token(), literal, continuation);
}

void closure_representation_analysis_declaration(
    Closure_Analysis& analysis, Scope* scope, Parser* p, AST_Node* root, AST_Node* parent, AST_Node* continuation, AST_Node* last_program_point) {
  if (ast_is_null_node(root)) return;

  AST_Manager* m = &p->ast_man;

  if (root->kind == AST_BIND_CONSTANT || root->kind == AST_BIND_VARIABLE) {
    scope_push(scope, root);

    AST_Node* left  = ast_bind_get_type_bind(m, root);
    AST_Node* right = ast_bind_get_expr(m, root);

    // closure_representation_analysis(analysis, envs, closures, aliases, scope, p, left, root, continuation);
    return closure_representation_analysis_declaration(analysis, scope, p, right, root, continuation, last_program_point);
  }

  // if (root->kind == AST_OP_BIN_ASSIGN) { assign_left_to_right_alias(aliases, root->left, root->right); }

  if (root->kind == AST_FUNCTION_LITERAL) {
    AST_Node* bind = ast_bind_get_type_bind(m, parent);

    assert(analysis.escaping_kind.find(bind->id) != analysis.escaping_kind.end());

    // if (analysis.escaping_kind[bind->id] == CONTINUATION_LOCAL || analysis.escaping_kind[bind->id] == FUNCTION_LOCAL) {
    AST_Node* struct_bind = ast_node_null(m);

    lift_local_function_declaration_environment_struct(analysis, p, parent, &struct_bind);
    print_ast(p, struct_bind);

    AST_Node* pp = ast_program_point(m, lexer_undef_token());

    pp->left  = parent->id;
    pp->right = last_program_point->right;

    last_program_point->left  = struct_bind->id;
    last_program_point->right = pp->id;

    //}

    AST_Node* signature    = ast_function_literal_get_signature(m, root);
    AST_Node* arguments    = ast_function_signature_get_args(m, signature);
    AST_Node* body         = ast_function_literal_get_body(m, root);
    AST_Node* continuation = get_last(p, arguments);
    Scope*    scope        = declaration_arguments_to_scope(p, arguments);

    return closure_representation_analysis_declaration(analysis, scope, p, body, ast_node_null(m), continuation, last_program_point);
  }

  if (root->kind == AST_FUNCTION_CALL) {
    if (analysis.escaping_place.find(parent->id) != analysis.escaping_place.end()) {
      // TODO(marcos): alocate escaping closure
    }

    return;
  }

  if (root->kind == AST_PROGRAM_POINT) last_program_point = root;

  AST_Node* l = ast_manager_get_relative(m, root, root->left);
  AST_Node* r = ast_manager_get_relative(m, root, root->right);

  closure_representation_analysis_declaration(analysis, scope, p, l, root, continuation, last_program_point);
  closure_representation_analysis_declaration(analysis, scope, p, r, root, continuation, last_program_point);
}

void closure_representation_analysis(Closure_Analysis& analysis, Parser* p, AST_Node* root) {
  AST_Manager* m = &p->ast_man;

  while (!ast_is_null_node(root)) {
    AST_Node* declaration = ast_program_point_get_decl(m, root);

    if (declaration->kind == AST_BIND_CONSTANT || declaration->kind == AST_BIND_VARIABLE) {
      AST_Node* literal = ast_bind_get_expr(m, declaration);

      if (literal->kind == AST_FUNCTION_LITERAL) {
        AST_Node* signature    = ast_function_literal_get_signature(m, literal);
        AST_Node* arguments    = ast_function_signature_get_args(m, signature);
        AST_Node* body         = ast_function_literal_get_body(m, literal);
        AST_Node* continuation = get_last(p, arguments);
        Scope*    scope        = declaration_arguments_to_scope(p, arguments);

        closure_representation_analysis_declaration(analysis, scope, p, body, ast_node_null(m), continuation, root);
      }
    }

    root = ast_program_point_get_tail(m, root);
  }
}

void get_local_functions_rec(Closure_Analysis& analysis, Parser* p, AST_Node* node) {
  if (ast_is_null_node(node)) return;

  AST_Manager* m = &p->ast_man;

  if (node->kind == AST_BIND_CONSTANT || node->kind == AST_BIND_VARIABLE) {
    AST_Node* right = ast_bind_get_expr(m, node);

    if (right->kind == AST_FUNCTION_LITERAL) {
      AST_Node* left = ast_bind_get_type_bind(m, node);
      if (analysis.continuations.find(left->id) == analysis.continuations.end()) { analysis.user_functions.insert(left->id); }
    }
  }

  AST_Node* l = ast_manager_get_relative(m, node, node->left);
  AST_Node* r = ast_manager_get_relative(m, node, node->right);

  get_local_functions_rec(analysis, p, l);
  get_local_functions_rec(analysis, p, r);
}
void get_local_functions(Closure_Analysis& analysis, Parser* p, AST_Node* node) {
  if (ast_is_null_node(node)) return;

  AST_Manager* m = &p->ast_man;

  AST_Node* l = ast_manager_get_relative(m, node, node->left);
  AST_Node* r = ast_manager_get_relative(m, node, node->right);

  get_local_functions_rec(analysis, p, l);

  get_local_functions(analysis, p, r);
}

void closure_conversion(Parser* p, AST_Node* root) {
  Closure_Analysis analysis;

  analysis.lift_escaping_functions_of_local_calls = true;

  cps_conversion(analysis.continuations, p, root);

  print_ast_to_program(p, root); // TODO(marcos): remove

  get_local_functions(analysis, p, root);

  build_extended_cps_graph(analysis, p, root);

  escape_analysis(analysis, p, root);

  print_cps_extended_graph_context(analysis.call_graph, p);

  Scope* scopeA = scope_create(NULL); // FIXME(marcos): fix leak
  compute_function_declaration_stage_numbers(analysis, p, root, scopeA);

  Scope* scopeB = scope_create(NULL); // FIXME(marcos): fix leak
  compute_continuations_stage_numbers(analysis, p, root, scopeB);

  Scope* scopeC = scope_create(NULL); // FIXME(marcos): fix leak
  compute_free_variables_lifetimes(analysis, scopeC, p, NULL, root);

  closure_representation_analysis(analysis, p, root);

  print_ast_to_program(p, root); // TODO(marcos): remove
  // closure_strategy_analysis(analysis, p, root);
}
