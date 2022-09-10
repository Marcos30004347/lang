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

#include <sys/_types/_size_t.h>
#include <unordered_map>
#include <unordered_set>
#include <vector>

// struct Call_Graph_Edges {
//   std::unordered_set< AST_Id > to;
//   std::unordered_set< AST_Id > pred;
// };

// typedef std::unordered_map< AST_Id, Call_Graph_Edges > Call_Graph;

enum Escaping_Kind {
  FUNCTION_LOCAL,
  FUNCTION_ESCAPING,
  CONTINUATION_ESCAPING,
  CONTINUATION_LOCAL,
};

// struct Lifetime {
//   u64 last_use_time;
//   u64 first_use_time;
// };

struct Escaping {
  AST_Id closure; // the function actualy escaping
  u64    index;
};

typedef std::vector< Escaping >              Escapings;
typedef std::unordered_set< AST_Id >         AST_Set;
typedef std::unordered_map< AST_Id, AST_Id > AST_Map;
typedef std::unordered_map< AST_Id, u64 >    AST_Discover_Time;
// typedef std::unordered_map< AST_Id, Lifetime >      Lifetimes;
typedef std::unordered_map< AST_Id, AST_Set >       AST_Collection;
typedef std::unordered_map< AST_Id, Escapings >     Return_Escapings;
typedef std::unordered_map< AST_Id, Escaping_Kind > Escaping_Kind_Map;

struct Closure_Analysis {
  AST_Set escapes;
  AST_Set user_functions;
  // Lifetimes               lifetimes;
  // Call_Graph              call_graph;
  // AST_To_U64_Map          stage_number;
  // AST_Set           escaping_variables;
  AST_Collection    closure_free_variables;
  AST_Discover_Time free_variable_time;
  AST_Map           closure_variables_bitset;

  // AST_To_U64_Map          strategy_number;

  Return_Escapings        local_escapes;
  Return_Escapings        escaping_place;
  Escaping_Kind_Map       escaping_kind;
  Local_Continuations_Set continuations;
  // AST_Map                 environment_struct;
  AST_Map environment_argument;
};

Context* declaration_arguments_to_scope(Parser* p, AST_Node* node, Context* parent = NULL) {
  Context* ctx = context_create(parent);

  if (ast_is_null_node(node)) { return ctx; }

  AST_Manager* m = &p->ast_man;

  assert(node->kind == AST_DECL_ARGS_LIST);

  while (!ast_is_null_node(node)) {
    AST_Node* arg = ast_decl_list_get_elem(m, node);

    if (arg->kind == AST_BIND_CONSTANT || arg->kind == AST_BIND_VARIABLE) { arg = ast_manager_get_relative(m, arg, arg->left); }

    assert(arg->kind == AST_BIND_TYPE);

    context_declare(ctx, p, arg, node);

    node = ast_decl_list_get_tail(m, node);
  }
  return ctx;
}

// struct Alias_Scope {
//   typedef std::unordered_set< AST_Id >            Alias_Set;
//   typedef std::unordered_map< AST_Id, Alias_Set > Alias_Map;

//   Alias_Map    aliases;
//   Alias_Scope* parent;
// };

// Alias_Scope* copy_alias_scope_to_heap(Alias_Scope* alias) {
//   if (alias == NULL) return NULL;
//   Alias_Scope* a = new Alias_Scope(); //(Alias_Scope*)malloc(sizeof(Alias_Scope));

//   a->aliases = Alias_Scope::Alias_Map();
//   a->aliases = alias->aliases;
//   a->parent  = copy_alias_scope_to_heap(alias->parent);
//   return a;
// }

// void free_alias_scope(Alias_Scope* alias) {
//   if (alias == NULL) return;

//   free_alias_scope(alias->parent);

//   free(alias);
// }

// Alias_Scope::Alias_Set* find_alias_set(Alias_Scope* aliases, AST_Id id) {
//   while (aliases) {
//     if (aliases->aliases.find(id) != aliases->aliases.end()) { return &aliases->aliases[id]; }

//     aliases = aliases->parent;
//   }

//   return NULL;
// }

// void assign_left_to_right_alias(Alias_Scope* aliases, AST_Id left, AST_Id right, b8 reset = true) {
//   Alias_Scope::Alias_Set* left_aliases  = find_alias_set(aliases, left);
//   Alias_Scope::Alias_Set* right_aliases = find_alias_set(aliases, right);

//   if (left_aliases == NULL) {
//     aliases->aliases[left] = Alias_Scope::Alias_Set();
//     left_aliases           = &aliases->aliases[left];
//   }

//   if (reset) { left_aliases->clear(); }

//   if (right_aliases == NULL) {
//     left_aliases->insert(right);
//     return;
//   }

//   for (Alias_Scope::Alias_Set::iterator it = right_aliases->begin(); it != right_aliases->end(); it++) {
//     left_aliases->insert(*it);
//   }
// }

// void print_aliases(Parser* p, Alias_Scope* aliases, AST_Id l) {
//   AST_Manager* m = &p->ast_man;
//   AST_Node*    n = ast_manager_get(m, l);
//   printf("[| ");
//   print_ast_to_program(p, ast_type_bind_get_symbol(m, n));
//   printf(" |] := ");
//   printf("{ ");
//   u64 i = 0;

//   Alias_Scope::Alias_Set* s = find_alias_set(aliases, l);

//   for (Alias_Scope::Alias_Set::iterator it = s->begin(); it != s->end(); it++) {
//     i = i + 1;

//     AST_Node* n = ast_manager_get(m, *it);

//     print_ast_to_program(p, n);

//     if (i != s->size()) printf(", ");
//   }
//   printf(" }\n");
// }

// void print_all_aliases(Parser* p, Alias_Scope* aliases) {
//   if (aliases == NULL) return;

//   for (Alias_Scope::Alias_Map::iterator it = aliases->aliases.begin(); it != aliases->aliases.end(); it++) {
//     print_aliases(p, aliases, it->first);
//   }

//   print_all_aliases(p, aliases->parent);
// }

// void replace_aliases(Alias_Scope* a, Alias_Scope* b) {
//   while (a) {
//     assert(a && b);
//     a->aliases = b->aliases;
//     a          = a->parent;
//     b          = b->parent;
//   }
// }

// void merge_aliases(Alias_Scope* a, Alias_Scope* b) {
//   while (b) {
//     assert(b && a);
//     for (Alias_Scope::Alias_Map::iterator it0 = b->aliases.begin(); it0 != b->aliases.end(); it0++) {
//       for (Alias_Scope::Alias_Set::iterator it1 = it0->second.begin(); it1 != it0->second.end(); it1++) {
//         if (a->aliases.find(it0->first) == a->aliases.end()) { a->aliases[it0->first] = Alias_Scope::Alias_Set(); }

//         a->aliases[it0->first].insert(*it1);
//       }
//     }

//     a = a->parent;
//     b = b->parent;
//   }
// }

typedef std::unordered_set< AST_Id > AST_Set;

void insert_all_function_literal_aliases(Context* ctx, Parser* p, Declaration* declaration, AST_Set* literals) {
  AST_Manager* m = &p->ast_man;
  if (declaration == NULL) return;

  Assignments* assignments = &declaration->assignments;

  for (Assignments::iterator val = assignments->begin(); val != assignments->end(); val++) {
    AST_Node* assignment = *val;
    if (assignment->kind == AST_FUNCTION_LITERAL) { literals->insert(declaration->bind->id); }
    if (assignment->kind == AST_SYMBOL_LITERAL || ast_is_temporary(m, assignment)) {
      Declaration* declaration = context_declaration_of(ctx, p, assignment);
      insert_all_function_literal_aliases(ctx, p, declaration, literals);
    }
  }
}

void insert_all_aliases_to_function_literals(Context* ctx, Parser* p, AST_Node* node, u64 arg_index, Escapings* literals) {
  AST_Manager* m = &p->ast_man;

  if (ast_is_null_node(node)) return;

  assert(node->kind == AST_BIND_TYPE);

  AST_Node* symbol = ast_type_bind_get_symbol(m, node);

  Declaration* decl = context_declaration_of(ctx, p, symbol);

  if (decl) {
    Assignments* assignments = &decl->assignments;

    for (Assignments::iterator val = assignments->begin(); val != assignments->end(); val++) {
      AST_Node* value = *val;

      if (value->kind == AST_FUNCTION_LITERAL) {
        Escaping escaping = {.closure = node->id, .index = arg_index};

        literals->push_back(escaping);
      }

      if (value->kind == AST_SYMBOL_LITERAL) {
        Declaration* decl = context_declaration_of(ctx, p, value);

        assert(decl != NULL);

        insert_all_aliases_to_function_literals(ctx, p, decl->bind, arg_index, literals);
      }
    }
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

void get_function_literals(Parser* p, AST_Node* call, Context* ctx, AST_Set* literals) {
  AST_Manager* m = &p->ast_man;

  AST_Node* symbol = ast_fun_call_get_call_sym(m, call);

  Declaration* declaration = context_declaration_of(ctx, p, symbol);

  if (declaration == NULL) {
    // NOTE(marcos): if a declaration is a type bind it is a argument.
    AST_Node* arguments    = ast_fun_call_get_call_args(m, call);
    AST_Node* continuation = get_last(p, arguments);

    declaration = context_declaration_of(ctx, p, continuation);
  }
  // printf("adasda\n");
  // context_print(ctx, p);
  assert(declaration != NULL);

  return insert_all_function_literal_aliases(ctx, p, declaration, literals);
}

void get_aliases_to_function_literals(Parser* p, AST_Node* node, u64 index, Context* ctx, Escapings* literals) {
  if (ast_is_null_node(node)) return;

  AST_Manager* m = &p->ast_man;

  Declaration* declaration = context_declaration_of(ctx, p, node);

  if (declaration) { return insert_all_aliases_to_function_literals(ctx, p, declaration->bind, index, literals); }

  AST_Node* left  = ast_manager_get_relative(m, node, node->left);
  AST_Node* right = ast_manager_get_relative(m, node, node->right);

  get_aliases_to_function_literals(p, left, index, ctx, literals);
  get_aliases_to_function_literals(p, right, index, ctx, literals);
}

AST_Node* get_call_last_argument(Parser* p, AST_Node* call, AST_Node** arg = NULL) {
  AST_Manager* m = &p->ast_man;

  AST_Node* head = ast_manager_get_relative(m, call, call->right);

  return get_last(p, head, arg);
}

// void build_extended_cps_graph_declaration(Call_Graph& context, Context* scope, Parser* p, AST_Node* function, AST_Node* statement, AST_Node* cont_symbol) {
//   // TODO(marcos): this functions is not considering aliases

//   AST_Manager* m = &p->ast_man;

//   if (statement->kind == AST_FUNCTION_CALL) {
//     AST_Set functions = AST_Set();

//     AST_Node* symbol = ast_fun_call_get_call_sym(m, statement);

//     get_aliases_to_function_literals(p, symbol, scope, &functions);

//     AST_Node* arguments = ast_fun_call_get_call_args(m, statement);

//     // AST_Node* declaration = scope_find(scope, p, symbol);
//     if (functions.size()) {
//       for (AST_Set::iterator it = functions.begin(); it != functions.end(); it++) {
//         AST_Node* bind = ast_manager_get(m, *it);

//         context[function->id].to.insert(bind->id);
//         context[bind->id].pred.insert(function->id);
//       }
//     } else {
//       if (parser_is_same_symbol(p, symbol, cont_symbol)) return;
//       // NOTE(marcos): extern function

//       AST_Node* continuation = get_call_last_argument(p, statement);

//       Declaration* declaration = context_declaration_of(scope, p, continuation);

//       if (declaration == NULL || (declaration->bind->kind != AST_BIND_CONSTANT && declaration->bind->kind != AST_BIND_VARIABLE)) { return; }

//       Assignment* assignment = declaration->assignments;

//       while (assignment) {
//         AST_Node* expr = assignment->value;

//         if (expr->kind != AST_FUNCTION_LITERAL) { return; }

//         AST_Node* bind = declaration->bind;

//         assert(bind->kind == AST_BIND_TYPE);

//         context[function->id].to.insert(bind->id);
//         context[bind->id].pred.insert(function->id);

//         assignment = assignment->previous;
//       }
//     }
//   }

//   if (statement->kind == AST_CTRL_FLOW_IF) {
//     AST_Node* expr = ast_manager_get_relative(m, statement, statement->right);

//     Context* branch_ctx = context_copy(scope);

//     build_extended_cps_graph_declaration(context, branch_ctx, p, function, expr, cont_symbol);

//     context_merge(p, scope, branch_ctx);

//     context_destroy(branch_ctx);

//     return;
//   }

//   if (statement->kind == AST_CTRL_FLOW_IF_ELSE) {
//     Context* branches_aliases = context_copy(scope);

//     AST_Node* expr = ast_node_null(m);

//     while (!ast_is_null_node(statement)) {
//       expr = ast_manager_get_relative(m, statement, statement->left);

//       Context* branch_aliases = context_copy(scope);

//       build_extended_cps_graph_declaration(context, branch_aliases, p, function, expr, cont_symbol);

//       context_merge(p, branches_aliases, branch_aliases);

//       context_destroy(branch_aliases);

//       statement = ast_manager_get_relative(m, statement, statement->right);
//     }

//     if (!ast_is_null_node(statement)) {
//       Context* branch_aliases = context_copy(scope);

//       build_extended_cps_graph_declaration(context, branch_aliases, p, function, statement, cont_symbol);

//       context_replace(scope, branch_aliases);

//       context_destroy(branch_aliases);
//     }

//     context_merge(p, scope, branches_aliases);

//     context_destroy(branches_aliases);

//     return;
//   }

//   if (statement->kind == AST_BIND_CONSTANT || statement->kind == AST_BIND_VARIABLE) {

//     AST_Node* bind = ast_bind_get_type_bind(m, statement);
//     AST_Node* expr = ast_bind_get_expr(m, statement);

//     context_declare(scope, p, statement, ast_node_null(m));

//     if (expr->kind == AST_FUNCTION_LITERAL) {

//       assert(bind->kind == AST_BIND_TYPE);

//       context[bind->id] = Call_Graph_Edges();

//       AST_Node* signature     = ast_function_literal_get_signature(m, expr);
//       AST_Node* arguments     = ast_function_signature_get_args(m, signature);
//       AST_Node* body          = ast_function_literal_get_body(m, expr);
//       Context*  closure_scope = context_create(scope); // declaration_arguments_to_scope(p, arguments, scope);

//       build_extended_cps_graph_declaration(context, closure_scope, p, bind, body, cont_symbol);

//       context_destroy(closure_scope);
//     }
//   }

//   if (statement->kind == AST_OP_BIN_ASSIGN) { context_assign(scope, p, statement, ast_node_null(m)); }

//   if (statement->kind == AST_PROGRAM_POINT) {
//     while (!ast_is_null_node(statement)) {
//       assert(statement->kind == AST_PROGRAM_POINT);

//       AST_Node* closure_statement = ast_program_point_get_decl(m, statement);

//       build_extended_cps_graph_declaration(context, scope, p, function, closure_statement, cont_symbol);

//       statement = ast_program_point_get_tail(m, statement);
//     }
//   }
// }

// void build_extended_cps_graph(Closure_Analysis& analysis, Parser* p, AST_Node* root) {
//   assert(root->kind == AST_PROGRAM_POINT);

//   AST_Manager* m = &p->ast_man;

//   while (!ast_is_null_node(root)) {
//     AST_Node* statement = ast_program_point_get_decl(m, root);
//     Context*  scope     = context_create(NULL); // FIXME(marcos): memory leak

//     if (statement->kind == AST_BIND_CONSTANT || statement->kind == AST_BIND_VARIABLE) {
//       AST_Node* right = ast_bind_get_expr(m, statement);
//       if (right->kind == AST_FUNCTION_LITERAL) {
//         AST_Node* left = ast_bind_get_type_bind(m, statement);

//         AST_Node* signature    = ast_function_literal_get_signature(m, right);
//         AST_Node* arguments    = ast_function_signature_get_args(m, signature);
//         AST_Node* continuation = get_last(p, arguments);
//         AST_Node* cont_symbol  = ast_type_bind_get_symbol(m, continuation);

//         build_extended_cps_graph_declaration(analysis.call_graph, scope, p, ast_node_null(m), statement, cont_symbol);
//       }
//     }

//     root = ast_program_point_get_tail(m, root);
//   }
// }

// void compute_function_declaration_stage_numbers(Closure_Analysis& converter, Parser* p, AST_Node* root, Context* scope) {
//   AST_Manager* m = &p->ast_man;

//   if (root->kind == AST_BIND_CONSTANT || root->kind == AST_BIND_VARIABLE) {
//     AST_Node* bind = ast_bind_get_type_bind(m, root);
//     AST_Node* expr = ast_bind_get_expr(m, root);

//     if (expr->kind == AST_FUNCTION_LITERAL) {
//       context_declare(scope, p, root, ast_node_null(m));

//       converter.stage_number[bind->id] = -1; // TODO(marcos):get_scope_depth(scope);
//       printf("StageNumber(");
//       print_ast_to_program(p, ast_manager_get_relative(m, bind, bind->left));
//       printf(") = %lu\n", converter.stage_number[bind->id]);

//       // FIXME(marcos): scope memory leak
//       AST_Node* body          = ast_function_literal_get_body(m, expr);
//       Context*  closure_scope = context_create(scope);
//       compute_function_declaration_stage_numbers(converter, p, body, closure_scope);
//     }
//   }

//   if (root->kind == AST_CTRL_FLOW_IF || root->kind == AST_CTRL_FLOW_IF_ELSE) {
//     AST_Node* l = ast_manager_get_relative(m, root, root->left);
//     AST_Node* r = ast_manager_get_relative(m, root, root->right);
//     compute_function_declaration_stage_numbers(converter, p, l, scope);
//     compute_function_declaration_stage_numbers(converter, p, r, scope);
//   }

//   if (root->kind == AST_PROGRAM_POINT) {
//     while (!ast_is_null_node(root)) {
//       AST_Node* decl = ast_program_point_get_decl(m, root);
//       compute_function_declaration_stage_numbers(converter, p, decl, scope);
//       root = ast_program_point_get_tail(m, root);
//     }
//   }
// }

// void compute_continuations_stage_numbers(Closure_Analysis& converter, Parser* p, AST_Node* root, Context* scope) {
//   AST_Manager* m = &p->ast_man;

//   if (root->kind == AST_BIND_CONSTANT || root->kind == AST_BIND_VARIABLE) {
//     AST_Node* bind = ast_bind_get_type_bind(m, root);
//     AST_Node* expr = ast_bind_get_expr(m, root);

//     if (expr->kind == AST_FUNCTION_LITERAL) {
//       context_declare(scope, p, root, ast_node_null(m));

//       // FIXME(marcos): scope memory leak
//       AST_Node* body          = ast_function_literal_get_body(m, expr);
//       Context*  closure_scope = context_create(scope);
//       compute_continuations_stage_numbers(converter, p, body, closure_scope);
//     }
//   }

//   if (root->kind == AST_CTRL_FLOW_IF || root->kind == AST_CTRL_FLOW_IF_ELSE) {
//     AST_Node* l = ast_manager_get_relative(m, root, root->left);
//     AST_Node* r = ast_manager_get_relative(m, root, root->right);
//     compute_function_declaration_stage_numbers(converter, p, l, scope);
//     compute_function_declaration_stage_numbers(converter, p, r, scope);
//   }

//   if (root->kind == AST_FUNCTION_CALL) {
//     AST_Node*    continuation = get_call_last_argument(p, root);
//     Declaration* declaration  = context_declaration_of(scope, p, continuation);

//     assert(declaration != NULL && "declaration not found");

//     Assignment* assignment = declaration->assignments;

//     while (assignment) {

//       AST_Node* expr = assignment->value;

//       assert(expr->kind == AST_FUNCTION_LITERAL);

//       converter.stage_number[declaration->bind->id] = 1;

//       u64 max_pred = 0;

//       std::unordered_set< AST_Id >& pred = converter.call_graph[declaration->bind->id].pred;

//       for (std::unordered_set< AST_Id >::iterator it = pred.begin(); it != pred.end(); it++) {
//         max_pred = max_pred < converter.stage_number[*it] ? converter.stage_number[*it] : max_pred;
//       }

//       // converter.stage_number[declaration->bind->id] += max_pred;

//       converter.stage_number[declaration->bind->id] = -1; // TODO(marcos): get_scope_depth(scope);
//       printf("StageNumber(");
//       print_ast_to_program(p, declaration->bind);
//       printf(") = %lu\n", converter.stage_number[declaration->bind->id]);

//       assignment = assignment->previous;
//     }
//   }

//   if (root->kind == AST_PROGRAM_POINT) {
//     while (!ast_is_null_node(root)) {
//       AST_Node* decl = ast_program_point_get_decl(m, root);
//       compute_function_declaration_stage_numbers(converter, p, decl, scope);
//       root = ast_program_point_get_tail(m, root);
//     }
//   }
// }

// void print_cps_extended_graph(Call_Graph& ctx, AST_Id node, Parser* p) {
//   if (ctx[node].to.size() == 0 && ctx[node].pred.size() == 0) return;

//   AST_Manager* m = &p->ast_man;

//   AST_Node* bind = ast_manager_get(m, node);

//   assert(bind->kind == AST_BIND_TYPE);

//   AST_Node* symbol = ast_type_bind_get_symbol(m, bind);

//   if (ctx[node].to.size() > 0) {
//     printf("succ(");
//     print_ast_to_program(p, symbol);
//     printf(") = { ");

//     u64 i = 0;

//     for (std::unordered_set< AST_Id >::iterator it = ctx[node].to.begin(); it != ctx[node].to.end(); it++) {
//       i = i + 1;

//       AST_Node* to = ast_manager_get(m, *it);

//       assert(to->kind == AST_BIND_TYPE);

//       AST_Node* symbol = ast_type_bind_get_symbol(m, to);

//       print_ast_to_program(p, symbol);

//       if (i != ctx[node].to.size()) { printf(", "); }
//     }
//     printf(" } \n");
//   }

//   if (ctx[node].pred.size() > 0) {
//     printf("pred(");
//     print_ast_to_program(p, symbol);
//     printf(") = { ");

//     u64 i = 0;
//     for (std::unordered_set< AST_Id >::iterator it = ctx[node].pred.begin(); it != ctx[node].pred.end(); it++) {
//       i            = i + 1;
//       AST_Node* to = ast_manager_get(m, *it);

//       assert(to->kind == AST_BIND_TYPE);

//       AST_Node* symbol = ast_type_bind_get_symbol(m, to);

//       print_ast_to_program(p, symbol);

//       if (i != ctx[node].pred.size()) { printf(", "); }
//     }
//     printf(" }");
//   }

//   printf("\n");
// }

// void print_cps_extended_graph_context(Call_Graph& ctx, Parser* p) {
//   AST_Manager* m = &p->ast_man;

//   for (Call_Graph::iterator it = ctx.begin(); it != ctx.end(); it++) {
//     print_cps_extended_graph(ctx, it->first, p);
//   }
// }

struct FunctionDeclaration {
  AST_Node*            declaration;
  FunctionDeclaration* prev;
};

void closure_free_variables_analysis_rec(
    Closure_Analysis& analysis, Context* scope, Parser* p, FunctionDeclaration* enclosing, AST_Node* node, AST_Set& escaping_variables) {
  if (ast_is_null_node(node)) return;

  AST_Manager* m = &p->ast_man;

  if (node->kind == AST_SYMBOL_LITERAL || ast_is_temporary(m, node)) {
    // if (enclosing == NULL) return;

    b8 is_local = false;

    while (enclosing != NULL) {
      Declaration* declaration = context_declaration_of(scope, p, node, &is_local);

      assert(declaration != NULL && "Declaration not found");

      if (is_local) { break; }

      AST_Set& fv = analysis.closure_free_variables[enclosing->declaration->id];

      fv.insert(declaration->bind->id);

      escaping_variables.insert(declaration->bind->id);

      scope     = scope->parent;
      enclosing = enclosing->prev;
    }

    return;
  }

  if (node->kind == AST_BIND_CONSTANT || node->kind == AST_BIND_VARIABLE) {
    context_declare(scope, p, node, ast_node_null(m));

    AST_Node* right = ast_bind_get_expr(m, node);
    AST_Node* left  = ast_bind_get_type_bind(m, node);

    if (right->kind == AST_FUNCTION_LITERAL) {
      FunctionDeclaration declaration;

      declaration.prev        = enclosing;
      declaration.declaration = left;

      analysis.closure_free_variables[left->id] = AST_Set();

      // declaration.free_variables = FunctionDeclaration::Free_Variables();
      // if (node->kind == AST_FUNCTION_LITERAL) {
      AST_Node* signature = ast_function_literal_get_signature(m, right);
      AST_Node* arguments = ast_function_signature_get_args(m, signature);

      Context* closure_scope = declaration_arguments_to_scope(p, arguments, scope);

      context_declare(closure_scope, p, node, ast_node_null(m));

      AST_Node* body = ast_function_literal_get_body(m, right);

      closure_free_variables_analysis_rec(analysis, closure_scope, p, &declaration, body, escaping_variables);
      //}

      // closure_free_variables_analysis(analysis, scope, p, &declaration, right);

      printf("FreeVariables(");
      print_ast_to_program(p, ast_manager_get_relative(m, ast_manager_get_relative(m, node, node->left), ast_manager_get_relative(m, node, node->left)->left));
      printf(") = { ");

      u64 i = 0;

      for (AST_Set::iterator it = analysis.closure_free_variables[left->id].begin(); it != analysis.closure_free_variables[left->id].end(); it++) {
        i++;
        AST_Node* n = ast_manager_get(m, *it);
        if (n->kind == AST_BIND_VARIABLE || n->kind == AST_BIND_CONSTANT) { n = ast_manager_get_relative(m, n, n->left); }
        // print_ast(p, n);
        AST_Id id = n->id;
        assert(n->kind == AST_BIND_TYPE);
        if (n->kind == AST_BIND_TYPE) { n = ast_manager_get_relative(m, n, n->left); }

        print_ast_to_program(p, n);
        // printf("[%lu, %lu]", analysis.lifetimes[id].first_use_time, analysis.lifetimes[id].last_use_time);
        if (i != analysis.closure_free_variables[left->id].size()) printf(", ");
      }

      printf(" }\n");

      // converter.free_variables[node->id] = declaration.free_variables;
      return;
    } else {
      return closure_free_variables_analysis_rec(analysis, scope, p, enclosing, right, escaping_variables);
    }
  }

  AST_Node* l = ast_manager_get_relative(m, node, node->left);
  AST_Node* r = ast_manager_get_relative(m, node, node->right);

  closure_free_variables_analysis_rec(analysis, scope, p, enclosing, l, escaping_variables);
  closure_free_variables_analysis_rec(analysis, scope, p, enclosing, r, escaping_variables);
}

void closure_free_variables_discovery_time_rec(Closure_Analysis& analysis, Context* scope, Parser* p, AST_Node* node, AST_Set& escaping_variables) {
  AST_Manager* m = &p->ast_man;

  if (ast_is_null_node(node)) { return; }

  if (node->kind == AST_BIND_TYPE) {
    context_declare(scope, p, node, ast_node_null(m));

    if (escaping_variables.find(node->id) != escaping_variables.end()) {
      u64 size = analysis.free_variable_time.size();

      analysis.free_variable_time[node->id] = size;
    }
  }

  if (node->kind == AST_FUNCTION_LITERAL) {
    AST_Node* signature = ast_function_literal_get_signature(m, node);
    AST_Node* arguments = ast_function_signature_get_args(m, signature);
    AST_Node* body      = ast_function_literal_get_body(m, node);
    Context*  ctx       = declaration_arguments_to_scope(p, arguments, scope);

    closure_free_variables_discovery_time_rec(analysis, ctx, p, arguments, escaping_variables);
    closure_free_variables_discovery_time_rec(analysis, ctx, p, body, escaping_variables);

    context_destroy(ctx);

    return;
  }

  AST_Node* l = ast_manager_get_relative(m, node, node->left);
  AST_Node* r = ast_manager_get_relative(m, node, node->right);

  closure_free_variables_discovery_time_rec(analysis, scope, p, l, escaping_variables);
  closure_free_variables_discovery_time_rec(analysis, scope, p, r, escaping_variables);
}

AST_Node* closure_insert_environment_declaration(Closure_Analysis& analysis, Parser* p, AST_Set& escaping_variables, AST_Node* pp) {
  AST_Manager* m = &p->ast_man;

  AST_Node* struct_members = ast_node_null(m);

  for (AST_Set::iterator it = escaping_variables.begin(); it != escaping_variables.end(); it++) {
    AST_Node* decl = ast_manager_get(m, *it);
    assert(decl->kind == AST_BIND_TYPE);

    AST_Node* symbol = ast_copy(m, ast_manager_get_relative(m, decl, decl->left));
    AST_Node* type   = ast_copy(m, ast_manager_get_relative(m, decl, decl->right));
    AST_Node* bind   = ast_type_bind(m, lexer_undef_token(), symbol, type);

    struct_members = ast_struct_member(m, lexer_undef_token(), bind, struct_members);
  }

  AST_Node* struct_symbol      = ast_temp_node(m);
  AST_Node* struct_literal     = ast_type_struct(m, lexer_undef_token(), struct_members);
  AST_Node* struct_bind        = ast_type_bind(m, lexer_undef_token(), struct_symbol, struct_literal);
  AST_Node* struct_declaration = ast_constant_bind(m, lexer_undef_token(), struct_bind, struct_literal);

  AST_Node* environment_members = ast_node_null(m);

  // TODO(marcos): this should be a 'size' symbol and a u64 type
  AST_Node* environment_size_symbol = ast_temp_node(m);
  AST_Node* environment_size_bind   = ast_type_bind(m, lexer_undef_token(), environment_size_symbol, ast_type_i32(m, lexer_undef_token()));

  // TODO(marcos): this should be a 'variables' symbol
  AST_Node* environment_layout_symbol = ast_temp_node(m);
  AST_Node* environment_layout_bind   = ast_type_bind(m, lexer_undef_token(), environment_layout_symbol, _internal_ast_bitset(m, escaping_variables.size() - 1));

  // TODO(marcos): this should be a 'free_variables' symbol
  AST_Node* environment_free_variables_symbol = ast_temp_node(m);
  AST_Node* environment_free_variables_bind   = ast_type_bind(m, lexer_undef_token(), environment_free_variables_symbol, struct_symbol);

  environment_members = ast_struct_member(m, lexer_undef_token(), environment_size_bind, environment_members);
  environment_members = ast_struct_member(m, lexer_undef_token(), environment_layout_bind, environment_members);
  environment_members = ast_struct_member(m, lexer_undef_token(), environment_free_variables_bind, environment_members);

  AST_Node* environment_symbol      = ast_temp_node(m);
  AST_Node* environment_literal     = ast_type_struct(m, lexer_undef_token(), environment_members);
  AST_Node* environment_bind        = ast_type_bind(m, lexer_undef_token(), environment_symbol, environment_literal);
  AST_Node* environment_declaration = ast_constant_bind(m, lexer_undef_token(), environment_bind, environment_literal);

  AST_Id left  = pp->left;
  AST_Id right = pp->right;

  AST_Node* pp0 = ast_program_point(m, lexer_undef_token());
  AST_Node* pp1 = ast_program_point(m, lexer_undef_token());

  pp->left   = struct_declaration->id;
  pp->right  = pp0->id;
  pp0->left  = environment_declaration->id;
  pp0->right = pp1->id;
  pp1->left  = left;
  pp1->right = right;

  return pp1;
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

b8 is_symbols_aliases(Context* scope, Parser* p, AST_Node* a, AST_Node* b) {
  // NOTE(marcos): the efficience of this is pretty bad

  AST_Manager* m = &p->ast_man;

  if (a->kind != b->kind) return false;

  assert(a->kind == AST_BIND_TYPE);

  AST_Node* l = ast_type_bind_get_symbol(m, a);
  AST_Node* r = ast_type_bind_get_symbol(m, b);

  if (parser_is_same_symbol(p, l, r)) { return true; }

  Declaration* declaration = context_declaration_of(scope, p, b);

  if (declaration) {
    Assignments* assignments = &declaration->assignments;

    for (Assignments::iterator val = assignments->begin(); val != assignments->end(); val++) {
      AST_Node* assignment = *val;

      if (is_symbols_aliases(scope, p, a, assignment)) return true;
    }
  }

  return false;
}

void bind_function_call_arguments(Context* scope, Parser* p, AST_Node* literal, AST_Node* call) {
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

      AST_Node* assignment = ast_variable_bind(m, lexer_undef_token(), binding, value);

      context_declare(scope, p, assignment, bindings);

      bindings = ast_decl_list_get_tail(m, bindings);
    }
  }

  // NOTE(marcos): assuming continuation is going to be called
  if (literal_arguments_count) {
    while (!ast_is_null_node(bindings)) {
      AST_Node* binding = ast_decl_list_get_elem(m, bindings);

      AST_Node* assignment = ast_variable_bind(m, lexer_undef_token(), binding, ast_undefined(m));

      context_declare(scope, p, assignment, ast_node_null(m));

      bindings = ast_program_point_get_tail(m, bindings);
    }
  }
}

// void push_argument_aliases(Parser* p, AST_Node* function, AST_Node* values, Context* aliases) {
//   AST_Manager* m         = &p->ast_man;
//   AST_Node*    signature = ast_function_literal_get_signature(m, function);
//   AST_Node*    arguments = ast_function_signature_get_args(m, signature);

//   while (!ast_is_null_node(arguments)) {
//     AST_Node* binding = ast_decl_list_get_elem(m, arguments);

//     if (ast_is_null_node(values)) {
//       aliases->aliases[binding->id].insert(ast_undefined(m)->id);
//     } else {
//       AST_Node* value = ast_decl_list_get_elem(m, values);
//       aliases->aliases[binding->id].insert(value->id);
//       values = ast_decl_list_get_tail(m, values);
//     }

//     arguments = ast_decl_list_get_tail(m, arguments);
//   }
// }

void escape_analysis_rec(Closure_Analysis& conv, Parser* p, AST_Node* root, AST_Node* cont, Context* context, Escapings* escapes) {
  // TODO(marcos): this function is entering on loops when called with recusrsive functions

  if (ast_is_null_node(root)) return;

  AST_Manager* m = &p->ast_man;

  if (root->kind == AST_BIND_CONSTANT || root->kind == AST_BIND_VARIABLE) {
    context_declare(context, p, root, ast_node_null(m));

    AST_Node* bind = ast_bind_get_type_bind(m, root);
    AST_Node* expr = ast_bind_get_expr(m, root);

    if (expr->kind == AST_FUNCTION_LITERAL) {
      AST_Node* signature    = ast_function_literal_get_signature(m, expr);
      AST_Node* body         = ast_function_literal_get_body(m, expr);
      AST_Node* arguments    = ast_function_signature_get_args(m, signature);
      Context*  _context     = declaration_arguments_to_scope(p, arguments, context);
      AST_Node* continuation = ast_node_null(m);

      if (conv.continuations.find(bind->id) == conv.continuations.end()) { continuation = get_last(p, arguments); }
      conv.local_escapes[bind->id] = Escapings();

      // print_ast_to_program(p, body);
      // printf("\n");

      escape_analysis_rec(conv, p, body, continuation, _context, &conv.local_escapes[bind->id]);

      context_destroy(_context);

      printf("escaping(");
      print_ast_to_program(p, bind);
      printf(") = { ");

      u64 i = 0;
      for (Escapings::iterator it = conv.local_escapes[bind->id].begin(); it != conv.local_escapes[bind->id].end(); it++) {
        i = i + 1;

        AST_Node* f = ast_manager_get(m, it->closure);

        print_ast_to_program(p, f);
        if (i < conv.local_escapes[bind->id].size()) { printf(", "); }
      }
      printf(" }\n");
    }

    return;
  }

  if (root->kind == AST_OP_BIN_ASSIGN) {
    context_assign(context, p, root, ast_node_null(m));
    return;
  }

  if (root->kind == AST_FUNCTION_CALL) {
    AST_Node*    symbol      = ast_fun_call_get_call_sym(m, root);
    Declaration* declaration = context_declaration_of(context, p, symbol);
    Escapings*   escapings   = new Escapings();

    if (is_symbols_aliases(context, p, declaration->bind, cont)) {
      AST_Node* arguments = ast_fun_call_get_call_args(m, root);
      u64       index     = 0;

      while (!ast_is_null_node(arguments)) {
        AST_Node* arg = ast_decl_list_get_elem(m, arguments);

        if (arg->kind == AST_SYMBOL_LITERAL) { get_aliases_to_function_literals(p, arg, index, context, escapings); }
        if (arg->kind == AST_DECL_ARGS_LIST) {
          while (!ast_is_null_node(arg)) {
            AST_Node* curr = ast_decl_list_get_elem(m, arg);
            if (curr->kind == AST_SYMBOL_LITERAL) { get_aliases_to_function_literals(p, curr, index, context, escapings); }
            arg = ast_decl_list_get_tail(m, arg);
          }
        }
        index     = index + 1;
        arguments = ast_decl_list_get_tail(m, arguments);
      }

      for (Escapings::iterator it = escapings->begin(); it != escapings->end(); it++) {
        conv.escaping_place[root->id].push_back(*it);
        escapes->push_back(*it);
      }

      delete escapings;

      return;
    }

    AST_Set literals;

    get_function_literals(p, root, context, &literals);

    for (AST_Set::iterator it = literals.begin(); it != literals.end(); it++) {
      AST_Node* symbol = ast_manager_get(m, *it);

      Declaration* declaration = context_declaration_of(context, p, symbol);

      assert(declaration->assignments.size() == 1);

      AST_Node* literal    = *declaration->assignments.begin();
      AST_Node* statements = ast_function_literal_get_body(m, literal);
      AST_Node* signature  = ast_function_literal_get_signature(m, literal);
      AST_Node* arguments  = ast_function_signature_get_args(m, signature);

      Context* call_scope = context_create(context); // TODO(marcos): fix leaks

      bind_function_call_arguments(call_scope, p, literal, root);

      // context_print(call_scope, p);

      escape_analysis_rec(conv, p, statements, cont, call_scope, escapes);

      if (it == literals.begin()) {
        context_replace(context, call_scope->parent);
      } else {
        context_merge(p, context, call_scope->parent);
      }

      context_destroy(call_scope);
    }

    return;
  }

  if (root->kind == AST_CTRL_FLOW_IF) {
    AST_Node* body = ast_manager_get_relative(m, root, root->right);

    // Alias_Scope* branch_aliases = copy_alias_scope_to_heap(aliases);
    Context* _ctx = context_copy(context);

    escape_analysis_rec(conv, p, body, cont, _ctx, escapes);

    context_merge(p, context, _ctx);

    context_destroy(_ctx);

    return;
  }

  if (root->kind == AST_CTRL_FLOW_IF_ELSE) {
    // AST_Node* branch = ast_manager_get_relative(m, root, root->left);
    // AST_Node* tail   = ast_manager_get_relative(m, root, root->right);

    Context* branch_aliases = context_copy(context);
    u64      i              = 0;
    while (root->kind == AST_CTRL_FLOW_IF_ELSE) { // 'if', 'else if' branches
      AST_Node* branch = ast_manager_get_relative(m, root, root->left);
      AST_Node* body   = ast_manager_get_relative(m, branch, branch->right);

      Context* body_context = context_copy(context);

      escape_analysis_rec(conv, p, body, cont, body_context, escapes);

      if (i == 0) context_replace(branch_aliases, body_context);
      else context_merge(p, branch_aliases, body_context);

      i = i + 1;

      context_destroy(body_context);

      root = ast_manager_get_relative(m, root, root->right);
    }

    if (!ast_is_null_node(root)) { // else branch
      escape_analysis_rec(conv, p, root, cont, context, escapes);
    }

    context_merge(p, context, branch_aliases);
    context_destroy(branch_aliases);

    return;
  }

  assert(root->kind == AST_PROGRAM_POINT);

  AST_Node* left  = ast_manager_get_relative(m, root, root->left);
  AST_Node* right = ast_manager_get_relative(m, root, root->right);

  escape_analysis_rec(conv, p, left, cont, context, escapes);
  escape_analysis_rec(conv, p, right, cont, context, escapes);
}

void closure_escape_analysis(Closure_Analysis& conv, Parser* p, AST_Node* root) {
  AST_Manager* m = &p->ast_man;

  Context* ctx = context_create(NULL);

  escape_analysis_rec(conv, p, root, ast_node_null(m), ctx, NULL);

  for (auto it : conv.escaping_place) {
    AST_Node* node = ast_manager_get(m, it.first);
    print_ast_to_program(p, node);
    printf(" may escape : { ");
    u32 i = 0;
    for (auto j : it.second) {
      // AST_Node* arg  = ast_manager_get(m, j.root);
      AST_Node* node = ast_manager_get(m, j.closure);
      printf("%lu/", j.index);
      print_ast_to_program(p, node);
      i = i + 1;
      if (i < it.second.size()) printf(", ");
    }
    printf(" }\n");
  }

  context_destroy(ctx);
}

// void escape_analysis(Closure_Analysis& converter, Parser* p, AST_Node* root) {
//   assert(root->kind == AST_PROGRAM_POINT);
//   AST_Manager* m = &p->ast_man;

//   while (!ast_is_null_node(root)) {
//     AST_Node* statement = ast_program_point_get_decl(m, root);

//     if (statement->kind == AST_BIND_CONSTANT || statement->kind == AST_BIND_VARIABLE) {
//       AST_Node* expr = ast_bind_get_expr(m, statement);
//       if (expr->kind == AST_FUNCTION_LITERAL) {
//         AST_Node* signature    = ast_function_literal_get_signature(m, expr);
//         AST_Node* body         = ast_function_literal_get_body(m, expr);
//         AST_Node* arguments    = ast_function_signature_get_args(m, signature);
//         Context*  scope        = declaration_arguments_to_scope(p, arguments);
//         AST_Node* continuation = get_last(p, arguments);

//         // push_argument_aliases(p, expr, ast_node_null(m), &function_aliases);

//         AST_Set escapes;

//         // Alias_Scope aliases = {.parent = &function_aliases, .aliases = Alias_Scope::Alias_Map()};

//         function_literal_escape_analysis(converter, p, body, continuation, scope, &escapes);
//         // Declaration* declaration = scope->last_declaration;
//         // while(declaration) {
//         // 	Assignment* assignment = declaration->assignments;

//         // 	while(assignment) {
//         //     if (converter.continuations.find(*val) != converter.continuations.end()) { converter.escaping_kind[*val] = CONTINUATION_ESCAPING; }
//         //     if (converter.user_functions.find(*val) != converter.continuations.end()) { converter.escaping_kind[*val] = FUNCTION_ESCAPING; }

//         // 		assignment = assignment->previous;
//         // 	}
//         // 	declaration = declaration->previous_declaration;
//         // }

//         // for (Alias_Scope::Alias_Map::iterator it = function_aliases.aliases.begin(); it != function_aliases.aliases.end(); it++) {
//         //   for (Alias_Scope::Alias_Set::iterator val = it->second.begin(); val != it->second.end(); val++) {
//         //   }
//         // }

//         // for (Alias_Scope::Alias_Set::iterator it = escapes.begin(); it != escapes.end(); it++) {
//         //   AST_Node* n = ast_manager_get(m, *it);
//         //   if (converter.continuations.find(*it) != converter.continuations.end()) { converter.escaping_kind[*it] = CONTINUATION_ESCAPING; }
//         //   if (converter.user_functions.find(*it) != converter.continuations.end()) { converter.escaping_kind[*it] = FUNCTION_ESCAPING; }
//         // }
//       }
//     }

//     root = ast_program_point_get_tail(m, root);
//   }

//   for (AST_Set::iterator it = converter.continuations.begin(); it != converter.continuations.end(); it++) {
//     if (converter.escaping_kind.find(*it) == converter.escaping_kind.end()) { converter.escaping_kind[*it] = CONTINUATION_LOCAL; }
//   }

//   for (AST_Set::iterator it = converter.user_functions.begin(); it != converter.user_functions.end(); it++) {
//     if (converter.escaping_kind.find(*it) == converter.escaping_kind.end()) { converter.escaping_kind[*it] = FUNCTION_LOCAL; }
//   }

//   for (AST_Set::iterator it = converter.continuations.begin(); it != converter.continuations.end(); it++) {
//     AST_Node* node = ast_manager_get(m, *it);
//     printf("[| ");
//     print_ast_to_program(p, node);
//     printf(" |] = ");
//     if (converter.escaping_kind[*it] == CONTINUATION_LOCAL) printf("local_continuation");
//     if (converter.escaping_kind[*it] == CONTINUATION_ESCAPING) printf("escaping_continuation");
//     printf("\n");
//   }

//   for (AST_Set::iterator it = converter.user_functions.begin(); it != converter.user_functions.end(); it++) {
//     AST_Node* node = ast_manager_get(m, *it);
//     printf("[| ");
//     print_ast_to_program(p, node);
//     printf(" |] = ");
//     if (converter.escaping_kind[*it] == FUNCTION_LOCAL) printf("local_function");
//     if (converter.escaping_kind[*it] == FUNCTION_ESCAPING) printf("escaping_function");
//     printf("\n");
//   }
// }

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

AST_Node* remove_continuation_argument_rec(Closure_Analysis& analysis, Parser* p, AST_Node* arguments, AST_Node* continuation, AST_Node* prev) {
  AST_Manager* m = &p->ast_man;

  if (ast_is_null_node(arguments)) { return ast_node_null(m); }

  AST_Node* right = ast_decl_list_get_tail(m, arguments);

  if (ast_is_null_node(right)) {
    if (!ast_is_null_node(prev)) { prev->right = ast_node_null(m)->id; }

    if (analysis.continuations.find(continuation->id) == analysis.continuations.end()) { return ast_decl_list_get_elem(m, arguments); }

    return ast_node_null(m);
  }

  return remove_continuation_argument_rec(analysis, p, right, continuation, arguments);
}

AST_Node* remove_continuation_argument(Closure_Analysis& analysis, Parser* p, AST_Node* signature, AST_Node* continuation) {
  AST_Manager* m = &p->ast_man;

  AST_Node* arguments = ast_function_signature_get_args(m, signature);

  AST_Node* right = ast_manager_get_relative(m, arguments, arguments->right);

  if (ast_is_null_node(right)) {

    if (analysis.continuations.find(continuation->id) == analysis.continuations.end()) {
      AST_Node* cont  = ast_decl_list_get_elem(m, arguments);
      signature->left = ast_node_null(m)->id; // FIXME(marcos): this may insert bugs
      return cont;
    }

    return ast_node_null(m);
  }

  return remove_continuation_argument_rec(analysis, p, arguments, continuation, ast_node_null(m));
}

// void lift_local_function_body_pointer_loads(Closure_Analysis& analysis, Parser* p, AST_Node* node, Scope* scope, AST_Node* parent) {
//   if (ast_is_null_node(node)) return;

//   if (node->kind == AST_FUNCTION_LITERAL) return;

//   AST_Manager* m = &p->ast_man;

//   if (node->kind == AST_SYMBOL_LITERAL || ast_is_temporary(m, node)) {
//     AST_Node* decl = scope_find(scope, p, node);

//     if (ast_is_null_node(decl)) {
//       AST_Node* load = ast_pointer_load(m, lexer_undef_token(), ast_copy(m, node));

//       if (parent->left == node->id) {
//         parent->left = load->id;
//       } else if (parent->right == node->id) {
//         parent->right = load->id;
//       }
//     }

//     return;
//   }

//   if (node->kind == AST_BIND_CONSTANT || node->kind == AST_BIND_VARIABLE) {
//     AST_Node* expr = ast_bind_get_expr(m, node);
//     AST_Node* bind = ast_bind_get_type_bind(m, node);
//     AST_Node* type = ast_type_bind_get_type(m, bind);

//     lift_local_function_body_pointer_loads(analysis, p, type, scope, bind);
//     lift_local_function_body_pointer_loads(analysis, p, expr, scope, expr);

//     return scope_push(scope, node);
//   }

//   AST_Node* l = ast_manager_get_relative(m, node, node->left);
//   AST_Node* r = ast_manager_get_relative(m, node, node->right);

//   lift_local_function_body_pointer_loads(analysis, p, l, scope, node);
//   lift_local_function_body_pointer_loads(analysis, p, r, scope, node);
// }

// void lift_local_function_declaration_arguments(Closure_Analysis& analysis, Parser* p, AST_Node* declaration) {
//   AST_Manager* m = &p->ast_man;

//   AST_Node* bind    = ast_bind_get_type_bind(m, declaration);
//   AST_Node* literal = ast_bind_get_expr(m, declaration);

//   assert(literal->kind == AST_FUNCTION_LITERAL);
//   assert(analysis.free_variables.find(bind->id) != analysis.free_variables.end());

//   AST_Set& fv = analysis.free_variables[bind->id];

//   AST_Node* signature = ast_function_literal_get_signature(m, literal);
//   AST_Node* arguments = ast_function_signature_get_args(m, signature);
//   AST_Node* body      = ast_function_literal_get_body(m, literal);

//   Scope* scope = declaration_arguments_to_scope(p, arguments);

//   lift_local_function_body_pointer_loads(analysis, p, body, scope, literal);

//   AST_Node* continuation = remove_continuation_argument(analysis, p, arguments, bind, ast_node_null(m));

//   for (AST_Set::iterator it = fv.begin(); it != fv.end(); it++) {
//     AST_Node* variable = ast_manager_get(m, *it);

//     if (variable->kind == AST_BIND_CONSTANT || variable->kind == AST_BIND_VARIABLE) { variable = ast_bind_get_type_bind(m, variable); }
//     assert(variable->kind == AST_BIND_TYPE);

//     AST_Node* symbol = ast_type_bind_get_symbol(m, variable);
//     AST_Node* type   = ast_type_bind_get_type(m, variable);

//     AST_Node* ptr = ast_type_pointer(m, lexer_undef_token(), ast_copy(m, type));
//     AST_Node* arg = ast_type_bind(m, lexer_undef_token(), ast_copy(m, symbol), ptr);

//     ast_function_literal_push_argument(m, lexer_undef_token(), literal, arg);
//   }

//   if (!ast_is_null_node(continuation)) ast_function_literal_push_argument(m, lexer_undef_token(), literal, continuation);
// }

void lift_local_function_body_environment_access(Closure_Analysis& analysis, Parser* p, AST_Node* node, Context* scope, AST_Node* env, AST_Node* parent) {
  if (ast_is_null_node(node)) return;

  if (node->kind == AST_FUNCTION_LITERAL) return;

  AST_Manager* m = &p->ast_man;

  if (node->kind == AST_SYMBOL_LITERAL || ast_is_temporary(m, node)) {
    Declaration* decl = context_declaration_of(scope, p, node);

    if (decl == NULL) {
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

    context_declare(scope, p, node, ast_node_null(m));

    return;
  }

  AST_Node* l = ast_manager_get_relative(m, node, node->left);
  AST_Node* r = ast_manager_get_relative(m, node, node->right);

  lift_local_function_body_environment_access(analysis, p, l, scope, env, node);
  lift_local_function_body_environment_access(analysis, p, r, scope, env, node);
}

void lift_local_function_declaration_environment_struct(Closure_Analysis& analysis, Context* ctx, Parser* p, AST_Node* declaration, AST_Node* environment) {
  AST_Manager* m = &p->ast_man;

  AST_Node* bind    = ast_bind_get_type_bind(m, declaration);
  AST_Node* literal = ast_bind_get_expr(m, declaration);

  assert(literal->kind == AST_FUNCTION_LITERAL);
  assert(analysis.closure_free_variables.find(bind->id) != analysis.closure_free_variables.end());

  AST_Set& fv = analysis.closure_free_variables[bind->id];

  AST_Node* signature = ast_function_literal_get_signature(m, literal);
  AST_Node* arguments = ast_function_signature_get_args(m, signature);
  AST_Node* body      = ast_function_literal_get_body(m, literal);

  Context* scope = declaration_arguments_to_scope(p, arguments);

  AST_Node* continuation = remove_continuation_argument(analysis, p, signature, bind);

  // AST_Node* members = ast_node_null(m);

  // for (AST_Set::iterator it = fv.begin(); it != fv.end(); it++) {
  //   AST_Node* variable = ast_manager_get(m, *it);

  //   if (variable->kind == AST_BIND_CONSTANT || variable->kind == AST_BIND_VARIABLE) { variable = ast_bind_get_type_bind(m, variable); }

  //   assert(variable->kind == AST_BIND_TYPE);

  //   AST_Node* symbol = ast_type_bind_get_symbol(m, variable);
  //   AST_Node* type   = ast_type_bind_get_type(m, variable);

  //   AST_Node* ptr = ast_type_pointer(m, lexer_undef_token(), ast_copy(m, type));
  //   AST_Node* arg = ast_type_bind(m, lexer_undef_token(), ast_copy(m, symbol), ptr);

  //   members = ast_struct_member(m, lexer_undef_token(), arg, members);
  // }

  AST_Node* env_decl_bind = ast_manager_get_relative(m, environment, environment->left);
  AST_Node* env_struct    = ast_manager_get_relative(m, env_decl_bind, env_decl_bind->left);

  AST_Node* arg_type = ast_type_pointer(m, lexer_undef_token(), env_struct);
  AST_Node* arg_symb = ast_temp_node(m);
  AST_Node* arg_bind = ast_type_bind(m, lexer_undef_token(), arg_symb, arg_type);

  ast_function_literal_push_argument(m, lexer_undef_token(), literal, arg_bind);

  analysis.environment_argument[bind->id] = arg_bind->id;

  // context_declare(scope, p, env_decl, ast_node_null(m));

  lift_local_function_body_environment_access(analysis, p, body, scope, arg_symb, literal);

  if (!ast_is_null_node(continuation)) ast_function_literal_push_argument(m, lexer_undef_token(), literal, continuation);
}

// AST_Node* alocate_stack_closure(Parser* p, Context* context, AST_Node* function_symbol, AST_Node* env_argument, AST_Node* environment, AST_Node* pp) {
//   AST_Manager* m = &p->ast_man;

//   assert(environment->kind == AST_BIND_CONSTANT);
//   assert(env_argument->kind == AST_BIND_TYPE);

//   AST_Node* env_struct = ast_bind_get_expr(m, environment);
//   AST_Node* env_bind   = ast_bind_get_type_bind(m, environment);
//   AST_Node* members    = ast_manager_get_relative(m, env_struct, env_struct->left);
//   AST_Node* env_symbol = ast_type_bind_get_symbol(m, env_bind);
//   AST_Node* symbol     = ast_temp_node(m);
//   AST_Node* bind       = ast_type_bind(m, lexer_undef_token(), symbol, ast_copy(m, env_symbol));
//   AST_Node* allocation = ast_constant_bind(m, lexer_undef_token(), bind, ast_unitialized(m));

//   AST_Id assignment_id   = pp->left;
//   AST_Id continuation_id = pp->right;

//   AST_Node* point = pp; // ast_program_point(m, lexer_undef_token());

//   pp->right = point->id;

//   point->left = allocation->id;

//   while (!ast_is_null_node(members)) {
//     AST_Node* member = ast_program_point_get_decl(m, members);

//     assert(member->kind == AST_BIND_TYPE);

//     AST_Node* member_symbol = ast_type_bind_get_symbol(m, member);

//     AST_Node* access = ast_member_access(m, lexer_undef_token(), symbol, ast_copy(m, member_symbol));

//     b8 is_local = true;

//     Declaration* decl = context_declaration_of(context, p, member_symbol, &is_local);

//     AST_Node* value = ast_node_null(m);

//     if (decl && is_local) {
//       value = ast_address_of(m, lexer_undef_token(), ast_copy(m, member_symbol));
//     } else {
//       AST_Node* env_arg_symbol = ast_type_bind_get_symbol(m, env_argument);
//       AST_Node* member_access  = ast_member_access(m, lexer_undef_token(), ast_copy(m, env_arg_symbol), ast_copy(m, member_symbol));
//       value                    = ast_address_of(m, lexer_undef_token(), member_access);
//     }

//     AST_Node* assignment = ast_assignment(m, lexer_undef_token(), access, value);

//     AST_Node* next = ast_program_point(m, lexer_undef_token());

//     next->left   = assignment->id;
//     point->right = next->id;
//     point        = next;

//     members = ast_program_point_get_tail(m, members);
//   }

//   AST_Node* next = ast_program_point(m, lexer_undef_token());

//   next->left  = assignment_id;
//   next->right = continuation_id;

//   point->right = next->id;

//   AST_Node* assignment = ast_manager_get(m, next->left);
//   AST_Node* function   = ast_manager_get(m, assignment->right);

//   assignment->right = _internal_ast_build_stack_closure_object(m, function, symbol)->id;

//   // TODO: call _build_closure_object(assignment->right, env_symbol)
//   return next;
// }

// AST_Node* alocate_heap_closure(Parser* p, Context* context, AST_Node* function_symbol, AST_Node* env_argument, AST_Node* environment, AST_Node* pp, AST_Node* arg) {
//   AST_Manager* m = &p->ast_man;

//   assert(environment->kind == AST_BIND_CONSTANT);
//   assert(env_argument->kind == AST_BIND_TYPE);

//   AST_Node* env_struct = ast_bind_get_expr(m, environment);
//   AST_Node* env_bind   = ast_bind_get_type_bind(m, environment);
//   AST_Node* members    = ast_manager_get_relative(m, env_struct, env_struct->left);
//   AST_Node* env_symbol = ast_type_bind_get_symbol(m, env_bind);
//   AST_Node* symbol     = ast_temp_node(m);
//   AST_Node* bind       = ast_type_bind(m, lexer_undef_token(), symbol, ast_copy(m, env_symbol));
//   AST_Node* allocation = ast_constant_bind(m, lexer_undef_token(), bind, ast_unitialized(m));

//   AST_Id call_id         = pp->left;
//   AST_Id continuation_id = pp->right;

//   AST_Node* point = pp; // ast_program_point(m, lexer_undef_token());

//   pp->right = point->id;

//   point->left = allocation->id;

//   while (!ast_is_null_node(members)) {
//     AST_Node* member = ast_program_point_get_decl(m, members);

//     assert(member->kind == AST_BIND_TYPE);

//     AST_Node* member_symbol = ast_type_bind_get_symbol(m, member);

//     AST_Node* access = ast_member_access(m, lexer_undef_token(), symbol, ast_copy(m, member_symbol));

//     b8 is_local = true;

//     Declaration* decl = context_declaration_of(context, p, member_symbol, &is_local);

//     AST_Node* value = ast_node_null(m);

//     if (decl && is_local) {
//       // TODO: should be heap
//       value = ast_address_of(m, lexer_undef_token(), ast_copy(m, member_symbol));
//     } else if (decl) {
//       AST_Node* env_arg_symbol = ast_type_bind_get_symbol(m, env_argument);
//       AST_Node* member_access  = ast_member_access(m, lexer_undef_token(), ast_copy(m, env_arg_symbol), ast_copy(m, member_symbol));
//       value                    = ast_address_of(m, lexer_undef_token(), member_access);
//     }

//     AST_Node* assignment = ast_assignment(m, lexer_undef_token(), access, value);

//     AST_Node* next = ast_program_point(m, lexer_undef_token());

//     next->left   = assignment->id;
//     point->right = next->id;
//     point        = next;

//     members = ast_program_point_get_tail(m, members);
//   }

//   AST_Node* call = ast_manager_get(m, call_id);
//   AST_Node* func = ast_manager_get(m, call->right);
//   AST_Node* prev = ast_program_point(m, lexer_undef_token());
//   AST_Node* symb = ast_temp_node(m);
//   AST_Node* clos = ast_constant_bind(m, lexer_undef_token(), symb, _internal_ast_build_heap_closure_object(m, func, symbol));
//   prev->left     = clos->id;

//   AST_Node* next = ast_program_point(m, lexer_undef_token());

//   next->left   = call_id;
//   next->right  = continuation_id;
//   prev->right  = next->id;
//   point->right = prev->id;

//   // TODO: call _build_closure_object(assignment->right, env_symbol)
//   return next;
// }

AST_Node* create_binary_mask_free_variables(Closure_Analysis& analysis, Parser* p, AST_Node* pp, AST_Set& escaping_variables) {
  AST_Manager* m = &p->ast_man;

  AST_Node* declaration = ast_manager_get_relative(m, pp, pp->left);
  assert(declaration->kind == AST_BIND_CONSTANT);

  AST_Node* bind = ast_manager_get_relative(m, declaration, declaration->left);
  assert(bind->kind == AST_BIND_TYPE);

  AST_Node* literal = ast_manager_get_relative(m, declaration, declaration->right);
  assert(literal->kind == AST_FUNCTION_LITERAL);

  if (analysis.closure_free_variables.find(bind->id) != analysis.closure_free_variables.end() && analysis.closure_free_variables[bind->id].size() > 0) {
    AST_Set& fv = analysis.closure_free_variables[bind->id];

    AST_Node* bitset_decl        = _internal_ast_bitset(m, escaping_variables.size() - 1);
    AST_Node* bitset             = ast_temp_node(m);
    AST_Node* bitset_bind        = ast_type_bind(m, lexer_undef_token(), bitset, bitset_decl);
    AST_Node* bitset_declaration = ast_variable_bind(m, lexer_undef_token(), bitset_bind, bitset_decl);

    AST_Id declaration_id  = pp->left;
    AST_Id continuation_id = pp->right;

    pp->left = bitset_declaration->id;

    for (AST_Set::iterator var = fv.begin(); var != fv.end(); var++) {
      u64 index = analysis.free_variable_time[*var];

      AST_Node* statement = ast_program_point(m, lexer_undef_token());
      statement->left     = _internal_ast_bitset_set_bit_on(m, bitset, index)->id;
      statement->right    = ast_node_null(m)->id;
      pp->right           = statement->id;
      pp                  = statement;
    }

    AST_Node* statement = ast_program_point(m, lexer_undef_token());

    statement->left  = declaration_id;
    statement->right = continuation_id;

    pp->right = statement->id;

    analysis.closure_variables_bitset[bind->id] = bitset->id;

    return statement;
  }

  return pp;
}

void closure_representation_analysis_rec(Closure_Analysis& analysis,
                                         Context*          context,
                                         Parser*           p,
                                         AST_Node*         root,
                                         AST_Node*         parent,
                                         AST_Node*         cont,
                                         AST_Node*         env_arg,
                                         AST_Node*         last_pp,
                                         AST_Set&          escaping_variables,
                                         AST_Node*         buffer_struct,
                                         AST_Node*         environment) {
  if (ast_is_null_node(root)) return;

  AST_Manager* m = &p->ast_man;

  if (root->kind == AST_BIND_CONSTANT) {
    context_declare(context, p, root, last_pp);

    closure_representation_analysis_rec(analysis, context, p, ast_bind_get_expr(m, root), root, cont, env_arg, last_pp, escaping_variables, buffer_struct, environment);

    return;
  }

  if (root->kind == AST_OP_BIN_ASSIGN) {
    context_assign(context, p, root, last_pp);
    return;
  }

  if (root->kind == AST_BIND_VARIABLE) {
    context_declare(context, p, root, last_pp);
    return;
  }

  if (root->kind == AST_FUNCTION_LITERAL) {
    AST_Node* bind = ast_bind_get_type_bind(m, parent);

    lift_local_function_declaration_environment_struct(analysis, context, p, parent, environment);

    last_pp = create_binary_mask_free_variables(analysis, p, last_pp, escaping_variables);

    AST_Node* signature    = ast_function_literal_get_signature(m, root);
    AST_Node* arguments    = ast_function_signature_get_args(m, signature);
    AST_Node* body         = ast_function_literal_get_body(m, root);
    AST_Node* continuation = get_last(p, arguments);
    Context*  _context     = declaration_arguments_to_scope(p, arguments, context);

    AST_Node* environment_argument = ast_manager_get(m, analysis.environment_argument[bind->id]);

    return closure_representation_analysis_rec(
        analysis, _context, p, body, ast_node_null(m), continuation, environment_argument, last_pp, escaping_variables, buffer_struct, environment);
  }

  if (root->kind == AST_FUNCTION_CALL) {

    AST_Node* function  = ast_fun_call_get_call_sym(m, root);
    AST_Node* arguments = ast_fun_call_get_call_args(m, root);

    if (analysis.escaping_place.find(root->id) != analysis.escaping_place.end()) {
      Escapings* escapings = &analysis.escaping_place[root->id];

      AST_Node* capturing = ast_node_null(m);

      for (u64 i = 0; i < escapings->size(); i++) {
        Escaping  escaping = escapings->at(i);
        AST_Node* closure  = ast_manager_get(m, escaping.closure);

        AST_Node* bitset = ast_manager_get(m, analysis.closure_variables_bitset[escaping.closure]);

        if (ast_is_null_node(capturing)) capturing = bitset;
        else capturing = _internal_ast_bitset_union(m, capturing, bitset);
      }

      if (!ast_is_null_node(capturing)) {
        AST_Id call = last_pp->left;
        AST_Id cont = last_pp->right;

        AST_Node* t = ast_temp_node(m);
        AST_Node* b = ast_type_bind(m, lexer_undef_token(), t, _internal_ast_bitset(m, escaping_variables.size() - 1));
        AST_Node* a = ast_variable_bind(m, lexer_undef_token(), b, capturing);

        AST_Node* environment_bind   = ast_manager_get_relative(m, environment, environment->left);
        AST_Node* environment_symbol = ast_manager_get_relative(m, environment_bind, environment_bind->left);

        Token zero;

        zero.buf  = 0;
        zero.type = TOKEN_I32_LIT;
        zero.col = zero.row = zero.file = zero.size = -1;

        AST_Node* size      = ast_temp_node(m);
        AST_Node* size_type = ast_type_i32(m, lexer_undef_token());
        AST_Node* size_bind = ast_type_bind(m, lexer_undef_token(), size, size_type);
        AST_Node* size_assi = ast_variable_bind(m, lexer_undef_token(), size_bind, ast_i32_lit(m, zero));

        AST_Node* env      = ast_temp_node(m);
        AST_Node* env_bind = ast_type_bind(m, lexer_undef_token(), env, environment_symbol);
        AST_Node* env_assi = ast_variable_bind(m, lexer_undef_token(), env_bind, ast_unitialized(m));

        AST_Node* pp0 = ast_program_point(m, lexer_undef_token());
        AST_Node* pp1 = ast_program_point(m, lexer_undef_token());
        AST_Node* pp2 = ast_program_point(m, lexer_undef_token());

        pp2->left  = call;
        pp2->right = cont;
        pp1->left  = size_assi->id;
        pp1->right = pp2->id;
        pp0->left  = env_assi->id;
        pp0->right = pp1->id;

        last_pp->left  = a->id;
        last_pp->right = pp0->id;

        last_pp = pp2;

        for (AST_Set::iterator var_id = escaping_variables.begin(); var_id != escaping_variables.end(); var_id++) {
          AST_Node* bind = ast_manager_get(m, *var_id);

          assert(bind->kind == AST_BIND_TYPE);

          AST_Node* symbol = ast_manager_get_relative(m, bind, bind->left);
          AST_Node* type   = ast_manager_get_relative(m, bind, bind->right);

          b8 is_local = true;

          if (context_declaration_of(context, p, symbol, &is_local) && is_local) {
            AST_Node* condition = _internal_ast_bitset_is_bit_up(m, t, analysis.free_variable_time[bind->id]);

            AST_Node* body_incc = ast_program_point(m, lexer_undef_token());

            AST_Node* incc = ast_bin_add(m, lexer_undef_token(), size, _internal_ast_sizeof(m, type));
            AST_Node* assi = ast_assignment(m, lexer_undef_token(), size, incc);

            body_incc->left = assi->id;

            AST_Node* if_statement = ast_ctrl_flow_if(m, lexer_undef_token(), condition, body_incc, ast_node_null(m));

            AST_Id call = last_pp->left;
            AST_Id cont = last_pp->right;

            AST_Node* pp = ast_program_point(m, lexer_undef_token());
            pp->left     = call;
            pp->right    = cont;

            last_pp->left  = if_statement->id;
            last_pp->right = pp->id;

            last_pp = pp;
          }
        }

        AST_Node* buffer_struct_bind   = ast_bind_get_type_bind(m, buffer_struct);
        AST_Node* buffer_struct_symbol = ast_type_bind_get_symbol(m, buffer_struct_bind);

        AST_Node* allocation_size = ast_bin_add(m, lexer_undef_token(), size, _internal_ast_sizeof(m, buffer_struct_symbol));
        AST_Node* allocation      = _internal_ast_allocate_heap_buffer(m, allocation_size);
        AST_Node* buffer          = ast_temp_node(m);
        AST_Node* buffer_bind     = ast_type_bind(m, lexer_undef_token(), buffer, ast_type_pointer(m, lexer_undef_token(), ast_type_any(m, lexer_undef_token())));
        AST_Node* buffer_assign   = ast_variable_bind(m, lexer_undef_token(), buffer_bind, allocation);

        AST_Node* pp3 = ast_program_point(m, lexer_undef_token());
        AST_Node* pp4 = ast_program_point(m, lexer_undef_token());

        AST_Node* i      = ast_temp_node(m);
        AST_Node* i_type = ast_type_i32(m, lexer_undef_token());
        AST_Node* i_bind = ast_type_bind(m, lexer_undef_token(), i, i_type);
        AST_Node* i_assi = ast_variable_bind(m, lexer_undef_token(), i_bind, ast_i32_lit(m, zero));

        pp4->left  = last_pp->left;
        pp4->right = last_pp->right;

        pp3->left  = i_assi->id;
        pp3->right = pp4->id;

        last_pp->right = pp3->id;
        last_pp->left  = buffer_assign->id;

        last_pp = pp4;

        for (AST_Set::iterator var_id = escaping_variables.begin(); var_id != escaping_variables.end(); var_id++) {
          AST_Node* bind = ast_manager_get(m, *var_id);

          assert(bind->kind == AST_BIND_TYPE);

          AST_Node* symbol         = ast_manager_get_relative(m, bind, bind->left);
          AST_Node* type           = ast_manager_get_relative(m, bind, bind->right);
          AST_Node* env_arg_symbol = ast_type_bind_get_symbol(m, env_arg);

          b8 is_local = true;

          if (context_declaration_of(context, p, symbol, &is_local) && is_local) {
            AST_Node* condition = _internal_ast_bitset_is_bit_up(m, t, analysis.free_variable_time[bind->id]);

            AST_Node* body_incc    = ast_program_point(m, lexer_undef_token());
            AST_Node* body_capture = ast_program_point(m, lexer_undef_token());

            AST_Node* incc    = ast_bin_add(m, lexer_undef_token(), i, _internal_ast_sizeof(m, type));
            AST_Node* assi    = ast_assignment(m, lexer_undef_token(), i, incc);
            AST_Node* capture = _internal_ast_capture_variable_into_environment(m, environment_symbol, buffer_struct_symbol, buffer, symbol, i, env_arg_symbol);

            body_incc->left  = assi->id;
            body_incc->right = body_capture->id;

            body_capture->left = capture->id;

            AST_Node* if_statement = ast_ctrl_flow_if(m, lexer_undef_token(), condition, body_incc, ast_node_null(m));

            AST_Id call = last_pp->left;
            AST_Id cont = last_pp->right;

            AST_Node* pp = ast_program_point(m, lexer_undef_token());

            pp->left  = call;
            pp->right = cont;

            last_pp->left  = if_statement->id;
            last_pp->right = pp->id;

            last_pp = pp;
          } else {
            AST_Node* condition = _internal_ast_bitset_is_bit_up(m, t, analysis.free_variable_time[bind->id]);

            AST_Node* body_borrow = ast_program_point(m, lexer_undef_token());

            AST_Node* capture = _internal_ast_borrow_variable_into_environment(m, environment_symbol, buffer_struct_symbol, buffer, symbol, i, env_arg_symbol);

            body_borrow->left = capture->id;

            AST_Node* if_statement = ast_ctrl_flow_if(m, lexer_undef_token(), condition, body_borrow, ast_node_null(m));

            AST_Id call = last_pp->left;
            AST_Id cont = last_pp->right;

            AST_Node* pp = ast_program_point(m, lexer_undef_token());

            pp->left  = call;
            pp->right = cont;

            last_pp->left  = if_statement->id;
            last_pp->right = pp->id;

            last_pp = pp;
          }
        }
      }
    }

    return;
  }

  if (root->kind == AST_PROGRAM_POINT) last_pp = root;

  AST_Node* l = ast_manager_get_relative(m, root, root->left);
  AST_Node* r = ast_manager_get_relative(m, root, root->right);

  closure_representation_analysis_rec(analysis, context, p, l, root, cont, env_arg, last_pp, escaping_variables, buffer_struct, environment);
  closure_representation_analysis_rec(analysis, context, p, r, root, cont, env_arg, last_pp, escaping_variables, buffer_struct, environment);
}

void closure_representation_analysis(
    Closure_Analysis& analysis, Parser* p, AST_Node* pp, AST_Node* root, AST_Set& escaping_variables, AST_Node* environment, AST_Node* closure_buffer_struct) {
  Context* ctx = context_create(NULL);

  AST_Manager* m = &p->ast_man;

  closure_representation_analysis_rec(
      analysis, ctx, p, root, ast_node_null(m), ast_node_null(m), ast_node_null(m), pp, escaping_variables, closure_buffer_struct, environment);

  context_destroy(ctx);

  for (Return_Escapings::iterator it = analysis.escaping_place.begin(); it != analysis.escaping_place.end(); it++) {
    for (Escapings::iterator j = it->second.begin(); j != it->second.end(); j++) {
      AST_Node* n = ast_manager_get(m, j->closure);
      print_ast_to_program(p, n);
      printf("\n");
    }
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

void closure_free_variables_analysis(Closure_Analysis& analysis, Parser* p, AST_Node* node) {
  AST_Manager* m = &p->ast_man;

  Context* context;
  while (!ast_is_null_node(node)) {
    AST_Node* decl = ast_program_point_get_decl(m, node);

    if (decl->kind == AST_BIND_CONSTANT || decl->kind == AST_BIND_VARIABLE) {
      AST_Node* expr = ast_bind_get_expr(m, decl);
      AST_Set   escaping_variables;
      if (expr->kind == AST_FUNCTION_LITERAL) {
        context = context_create(NULL);
        closure_free_variables_analysis_rec(analysis, context, p, NULL, decl, escaping_variables);
        context_destroy(context);

        context = context_create(NULL);
        closure_free_variables_discovery_time_rec(analysis, context, p, decl, escaping_variables);
        context_destroy(context);

        for (auto i : escaping_variables) {
          AST_Node* n = ast_manager_get(m, i);
          print_ast_to_program(p, n);
          printf("[%lu]", analysis.free_variable_time[n->id]);
          printf(" ");
        }

        printf("\n");

        AST_Node* new_pp = closure_insert_environment_declaration(analysis, p, escaping_variables, node);

        AST_Node* fv_struct     = ast_manager_get_relative(m, node, node->left);
        AST_Node* env_struct_pp = ast_manager_get_relative(m, node, node->right);
        AST_Node* environment   = ast_manager_get_relative(m, env_struct_pp, env_struct_pp->left);

        node = new_pp;

        closure_escape_analysis(analysis, p, decl);

        closure_representation_analysis(analysis, p, node, decl, escaping_variables, fv_struct, environment);
      }
    }

    node = ast_program_point_get_tail(m, node);
  }
}

void closure_conversion(Parser* p, AST_Node* root) {
  Closure_Analysis analysis;

  AST_Manager* m = &p->ast_man;

  cps_conversion(analysis.continuations, p, root);

  print_ast_to_program(p, root); // TODO(marcos): remove

  get_local_functions(analysis, p, root);

  // build_extended_cps_graph(analysis, p, root);

  // print_cps_extended_graph_context(analysis.call_graph, p);

  // Context* scopeA = context_create(NULL); // FIXME(marcos): fix leak
  // compute_function_declaration_stage_numbers(analysis, p, root, scopeA);

  // Context* scopeB = context_create(NULL); // FIXME(marcos): fix leak
  // compute_continuations_stage_numbers(analysis, p, root, scopeB);

  closure_free_variables_analysis(analysis, p, root);

  print_ast_to_program(p, root); // TODO(marcos): remove
  // closure_strategy_analysis(analysis, p, root);
}
