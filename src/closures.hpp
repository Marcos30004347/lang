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
  AST_Id id;
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

  Return_Escapings  local_escapes;
  Return_Escapings  escaping_place;
  Escaping_Kind_Map escaping_kind;

  AST_Set continuation_declarations;
  AST_Set continuation_arguments;

  // AST_Map                 environment_struct;
  AST_Map environment_argument;
};

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

void insert_all_aliases_to_function_literals(Context* ctx, Parser* p, AST_Node* node, AST_Node* point, u64 arg_index, Escapings* literals) {
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
        Escaping escaping = {.closure = node->id, .id = point->id, .index = arg_index};

        literals->push_back(escaping);
      }

      if (value->kind == AST_SYMBOL_LITERAL) {
        Declaration* decl = context_declaration_of(ctx, p, value);

        assert(decl != NULL);

        insert_all_aliases_to_function_literals(ctx, p, decl->bind, point, arg_index, literals);
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

  if (declaration) { return insert_all_aliases_to_function_literals(ctx, p, declaration->bind, node, index, literals); }

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

      if (analysis.continuation_declarations.find(declaration->bind->id) != analysis.continuation_declarations.end()) return;
      if (analysis.continuation_arguments.find(declaration->bind->id) != analysis.continuation_arguments.end()) return;
      if (analysis.user_functions.find(declaration->bind->id) != analysis.user_functions.end()) return;
      if (analysis.closure_variables_bitset.find(declaration->bind->id) != analysis.closure_variables_bitset.end()) return;

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

      Context* closure_scope = declaration_arguments_to_context(p, arguments, scope);

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
    Context*  ctx       = declaration_arguments_to_context(p, arguments, scope);

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
  AST_Node* environment_layout_bind   = ast_type_bind(m, lexer_undef_token(), environment_layout_symbol, _internal_ast_bitset(m, escaping_variables.size()));

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
      Context*  _context     = declaration_arguments_to_context(p, arguments, context);
      AST_Node* continuation = ast_node_null(m);

      if (conv.continuation_declarations.find(bind->id) == conv.continuation_declarations.end()) { continuation = get_last(p, arguments); }
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

  if (root->kind >= __AST_INTERNAL_START && root->kind <= __AST_INTERNAL_END) { return; }

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
      AST_Node* node  = ast_manager_get(m, j.closure);
      AST_Node* place = ast_manager_get(m, j.id);
      printf("%lu/", j.index);
      print_ast_to_program(p, node);
      printf("/");
      print_ast_to_program(p, place);
      i = i + 1;
      if (i < it.second.size()) printf(", ");
    }
    printf(" }\n");
  }

  context_destroy(ctx);
}

void get_transitive_closure(Closure_Analysis& conv, Parser* p, AST_Node* root) { AST_Manager* m = &p->ast_man; }

AST_Node* remove_continuation_argument_rec(Closure_Analysis& analysis, Parser* p, AST_Node* arguments, AST_Node* continuation, AST_Node* prev) {
  AST_Manager* m = &p->ast_man;

  if (ast_is_null_node(arguments)) { return ast_node_null(m); }

  AST_Node* right = ast_decl_list_get_tail(m, arguments);

  if (ast_is_null_node(right)) {
    if (!ast_is_null_node(prev)) { prev->right = ast_node_null(m)->id; }

    if (analysis.continuation_declarations.find(continuation->id) == analysis.continuation_declarations.end()) { return ast_decl_list_get_elem(m, arguments); }

    return ast_node_null(m);
  }

  return remove_continuation_argument_rec(analysis, p, right, continuation, arguments);
}

AST_Node* remove_continuation_argument(Closure_Analysis& analysis, Parser* p, AST_Node* signature, AST_Node* continuation) {
  AST_Manager* m = &p->ast_man;

  AST_Node* arguments = ast_function_signature_get_args(m, signature);

  AST_Node* right = ast_manager_get_relative(m, arguments, arguments->right);

  if (ast_is_null_node(right)) {

    // if (analysis.continuation_declarations.find(continuation->id) == analysis.continuation_declarations.end()) {
    AST_Node* cont  = ast_decl_list_get_elem(m, arguments);
    signature->left = ast_node_null(m)->id; // FIXME(marcos): this may insert bugs
    return cont;
    //}
  }

  return remove_continuation_argument_rec(analysis, p, arguments, continuation, ast_node_null(m));
}

void lift_local_function_body_environment_access(
    Closure_Analysis& analysis, Parser* p, AST_Node* function, AST_Node* node, Context* scope, AST_Node* env, AST_Node* parent) {
  if (ast_is_null_node(node)) return;

  if (node->kind == AST_FUNCTION_LITERAL) return;

  AST_Manager* m = &p->ast_man;

  if (node->kind == AST_SYMBOL_LITERAL || ast_is_temporary(m, node)) {
    Declaration* decl = context_declaration_of(scope, p, node);

    if (decl == NULL || analysis.closure_free_variables[function->id].count(decl->bind->id)) {
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

    lift_local_function_body_environment_access(analysis, p, function, type, scope, env, bind);
    lift_local_function_body_environment_access(analysis, p, function, expr, scope, env, expr);

    context_declare(scope, p, node, ast_node_null(m));

    return;
  }

  AST_Node* l = ast_manager_get_relative(m, node, node->left);
  AST_Node* r = ast_manager_get_relative(m, node, node->right);

  lift_local_function_body_environment_access(analysis, p, function, l, scope, env, node);
  lift_local_function_body_environment_access(analysis, p, function, r, scope, env, node);
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

  Context* scope = declaration_arguments_to_context(p, arguments, ctx);

  AST_Node* continuation = remove_continuation_argument(analysis, p, signature, bind);

  AST_Node* env_decl_bind = ast_manager_get_relative(m, environment, environment->left);
  AST_Node* env_struct    = ast_manager_get_relative(m, env_decl_bind, env_decl_bind->left);

  AST_Node* arg_type = ast_type_pointer(m, lexer_undef_token(), env_struct);
  AST_Node* arg_symb = ast_temp_node(m);
  AST_Node* arg_bind = ast_type_bind(m, lexer_undef_token(), arg_symb, arg_type);

  ast_function_literal_push_argument(m, lexer_undef_token(), literal, arg_bind);

  analysis.environment_argument[bind->id] = arg_bind->id;

  // context_declare(scope, p, env_decl, ast_node_null(m));

  lift_local_function_body_environment_access(analysis, p, bind, body, scope, arg_symb, literal);

  context_destroy(scope);

  if (!ast_is_null_node(continuation)) ast_function_literal_push_argument(m, lexer_undef_token(), literal, continuation);
}

AST_Node* create_binary_mask_free_variables(Closure_Analysis& analysis, Context* ctx, Parser* p, AST_Node* pp, AST_Set& escaping_variables) {
  AST_Manager* m = &p->ast_man;

  AST_Node* declaration = ast_manager_get_relative(m, pp, pp->left);
  assert(declaration->kind == AST_BIND_CONSTANT);

  AST_Node* bind = ast_manager_get_relative(m, declaration, declaration->left);
  assert(bind->kind == AST_BIND_TYPE);

  AST_Node* literal = ast_manager_get_relative(m, declaration, declaration->right);
  assert(literal->kind == AST_FUNCTION_LITERAL);

  AST_Node* bitset_decl        = _internal_ast_bitset(m, escaping_variables.size());
  AST_Node* bitset             = ast_temp_node(m);
  AST_Node* bitset_bind        = ast_type_bind(m, lexer_undef_token(), bitset, bitset_decl);
  AST_Node* bitset_declaration = ast_variable_bind(m, lexer_undef_token(), bitset_bind, bitset_decl);

  AST_Id declaration_id  = pp->left;
  AST_Id continuation_id = pp->right;

  pp->left = bitset_declaration->id;

  context_declare(ctx, p, bitset_declaration, pp);

  // TODO(marcos): this if statement can be moved up to the bitset declaration if we only want to define bitsets for functions that capture variables
  if (analysis.closure_free_variables.find(bind->id) != analysis.closure_free_variables.end() && analysis.closure_free_variables[bind->id].size() > 0) {
    AST_Set& fv = analysis.closure_free_variables[bind->id];

    for (AST_Set::iterator var = fv.begin(); var != fv.end(); var++) {
      u64 index = analysis.free_variable_time[*var];

      AST_Node* statement = ast_program_point(m, lexer_undef_token());
      statement->left     = _internal_ast_bitset_set_bit_on(m, bitset, index)->id;
      statement->right    = ast_node_null(m)->id;
      pp->right           = statement->id;
      pp                  = statement;
    }
  }

  AST_Node* statement = ast_program_point(m, lexer_undef_token());

  statement->left  = declaration_id;
  statement->right = continuation_id;

  pp->right = statement->id;

  analysis.closure_variables_bitset[bind->id] = bitset->id;

  return statement;

  return pp;
}

void construct_closure_object(Parser* p, AST_Node* place, AST_Node* buffer) {
  assert(place->kind == AST_SYMBOL_LITERAL);

  AST_Manager* m      = &p->ast_man;
  AST_Node*    symbol = ast_copy(m, place);

  ast_change_kind(place, _AST_BUILD_CLOSURE_OBJECT);

  AST_Node* list = ast_decl_args(m, lexer_undef_token(), symbol, ast_decl_args(m, lexer_undef_token(), buffer, ast_node_null(m)));

  place->left  = list->id;
  place->right = ast_node_null(m)->id;
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

    last_pp = create_binary_mask_free_variables(analysis, context, p, last_pp, escaping_variables);

    AST_Node* signature    = ast_function_literal_get_signature(m, root);
    AST_Node* arguments    = ast_function_signature_get_args(m, signature);
    AST_Node* body         = ast_function_literal_get_body(m, root);
    AST_Node* continuation = get_last(p, arguments);

    Context* _context = declaration_arguments_to_context(p, arguments, context);

    AST_Node* environment_argument = ast_manager_get(m, analysis.environment_argument[bind->id]);

    closure_representation_analysis_rec(
        analysis, _context, p, body, ast_node_null(m), continuation, environment_argument, last_pp, escaping_variables, buffer_struct, environment);

    context_destroy(_context);

    return;
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
        AST_Node* b = ast_type_bind(m, lexer_undef_token(), t, _internal_ast_bitset(m, escaping_variables.size()));
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

        // context_declare(context, p, size_assi, pp1);
        // context_declare(context, p, env_assi, pp2);

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

        // context_declare(context, p, buffer_assign, last_pp);
        // context_declare(context, p, i_assi, pp3);

        last_pp = pp4;

        AST_Node* env_arg_symbol = ast_type_bind_get_symbol(m, env_arg);

        for (AST_Set::iterator var_id = escaping_variables.begin(); var_id != escaping_variables.end(); var_id++) {
          AST_Node* bind = ast_manager_get(m, *var_id);

          assert(bind->kind == AST_BIND_TYPE);

          AST_Node* symbol = ast_manager_get_relative(m, bind, bind->left);
          AST_Node* type   = ast_manager_get_relative(m, bind, bind->right);

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

        AST_Node* pp5 = ast_program_point(m, lexer_undef_token());

        pp5->left  = last_pp->left;
        pp5->right = last_pp->right;

        last_pp->right = pp5->id;
        last_pp->left  = _internal_ast_setup_closure_environment_buffer_header(m, environment_symbol, buffer_struct_symbol, buffer, i, env_arg_symbol)->id;

        last_pp = pp4;

        for (u64 i = 0; i < escapings->size(); i++) {
          Escaping  escaping = escapings->at(i);
          AST_Node* place    = ast_manager_get(m, escaping.id);
          construct_closure_object(p, place, buffer);
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

void closure_representation_analysis(Closure_Analysis& analysis,
                                     Parser*           p,
                                     Context*          ctx,
                                     AST_Node*         pp,
                                     AST_Node*         root,
                                     AST_Set&          escaping_variables,
                                     AST_Node*         environment,
                                     AST_Node*         closure_buffer_struct) {

  AST_Manager* m = &p->ast_man;

  closure_representation_analysis_rec(
      analysis, ctx, p, root, ast_node_null(m), ast_node_null(m), ast_node_null(m), pp, escaping_variables, closure_buffer_struct, environment);
}
void get_local_functions_rec(Closure_Analysis& analysis, Parser* p, AST_Node* node) {
  if (ast_is_null_node(node)) return;

  AST_Manager* m = &p->ast_man;

  if (node->kind == AST_BIND_CONSTANT || node->kind == AST_BIND_VARIABLE) {
    AST_Node* right = ast_bind_get_expr(m, node);

    if (right->kind == AST_FUNCTION_LITERAL) {
      AST_Node* left = ast_bind_get_type_bind(m, node);
      if (analysis.continuation_declarations.find(left->id) == analysis.continuation_declarations.end()) { analysis.user_functions.insert(left->id); }
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

  Context* context = context_create(NULL);
  while (!ast_is_null_node(node)) {
    AST_Node* decl = ast_program_point_get_decl(m, node);
    if (decl->kind == AST_BIND_CONSTANT || decl->kind == AST_BIND_VARIABLE) {
      context_declare(context, p, decl, node);

      AST_Node* expr = ast_bind_get_expr(m, decl);
      AST_Set   escaping_variables;

      if (expr->kind == AST_FUNCTION_LITERAL) {
        closure_free_variables_analysis_rec(analysis, context, p, NULL, decl, escaping_variables);

        closure_free_variables_discovery_time_rec(analysis, context, p, decl, escaping_variables);

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

        context_declare(context, p, fv_struct, node);
        context_declare(context, p, environment, env_struct_pp);

        node = new_pp;

        closure_escape_analysis(analysis, p, decl);
        AST_Id decl_id = decl->id;

        closure_representation_analysis(analysis, p, context, node, decl, escaping_variables, fv_struct, environment);

        while (!ast_is_null_node(node) && node->left != decl_id)
          node = ast_program_point_get_tail(m, node);
      }
    }

    node = ast_program_point_get_tail(m, node);
  }

  context_destroy(context);
}

void closure_conversion(Parser* p, AST_Node* root) {
  Closure_Analysis analysis;

  AST_Manager* m = &p->ast_man;

  cps_conversion(analysis.continuation_arguments, analysis.continuation_declarations, p, root);

  print_ast_to_program(p, root); // TODO(marcos): remove

  get_local_functions(analysis, p, root);

  closure_free_variables_analysis(analysis, p, root);

  print_ast_to_program(p, root); // TODO(marcos): remove
}
