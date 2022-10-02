#include "closures.hpp"
#include "ast/ast_control_flow.hpp"
#include "ast/ast_declaration.hpp"
#include "ast/ast_function.hpp"
#include "ast/ast_literals.hpp"
#include "ast/ast_manager.hpp"
#include "ast/ast_operations.hpp"
#include "ast/ast_program_point.hpp"
#include "compiler/symbol_table.hpp"
#include "context/context.hpp"
#include "continuations/continuations.hpp"
#include "lib/set.hpp"
#include "lib/table.hpp"
#include "parser/parser.hpp"

#include <assert.h>
#include <cstdio>
#include <stdio.h>

namespace closures {

struct CPS_Closure_Data {
  cps::CPS_Data* cps_data;

  lib::Table< compiler::symbol::Id, lib::Set< ast::Declaration_Variable_Node* >* >* closure_free_varaibles;
};

void delete_fv_set(lib::Set< ast::Declaration_Variable_Node* >* set) {
  lib::set_delete(set);
}

CPS_Closure_Data* cps_closure_data_create(cps::CPS_Data* cps_data) {
  CPS_Closure_Data* data = new CPS_Closure_Data();

  data->cps_data               = cps_data;
  data->closure_free_varaibles = lib::table_create< compiler::symbol::Id, lib::Set< ast::Declaration_Variable_Node* >* >();

  return data;
}

void cps_closure_data_destroy(CPS_Closure_Data* data) {
  cps::cps_result_destroy(data->cps_data);

  lib::table_delete(data->closure_free_varaibles, delete_fv_set);

  delete data;
}
void print_set_rec(ast::Manager* m, lib::SetNode< compiler::symbol::Id >* child_fv) {
  if (child_fv == NULL) {
    return;
  }

  printf("  ");
  compiler::symbol::print_symbol(m->symbol_table, compiler::symbol::get_symbol(m->symbol_table, child_fv->key));
  printf("\n");

  print_set_rec(m, child_fv->left);
  print_set_rec(m, child_fv->right);
}

void print_set(ast::Manager* m, lib::Set< compiler::symbol::Id >* child_fv) {
  printf("{\n");
  print_set_rec(m, child_fv->root);
  printf("}\n");
}

void push_fv_rec(lib::Set< compiler::symbol::Id >* fv, context::Context* ctx, lib::SetNode< compiler::symbol::Id >* child_fv) {
  if (child_fv == NULL) {
    return;
  }

  if (context::context_is_defined(ctx, child_fv->key) == false) {
    lib::insert(fv, child_fv->key);
  }

  push_fv_rec(fv, ctx, child_fv->left);
  push_fv_rec(fv, ctx, child_fv->right);
}

void push_fv(lib::Set< compiler::symbol::Id >* fv, context::Context* ctx, lib::Set< compiler::symbol::Id >* child_fv) {
  push_fv_rec(fv, ctx, child_fv->root);
}

void collect_free_variables(ast::Manager* m, ast::Node* node, lib::Set< compiler::symbol::Id >* fv, context::Context* ctx, context::Context* globals) {
  if (!ast::is_semantic_node(node)) {
    return;
  }

  if (ast::Function_Literal_Node* fun = ast::is_instance< ast::Function_Literal_Node* >(node)) {
    context::Context* closure_globals = context::context_create(globals);
    context::Context* closure_ctx     = context::context_from_declarations_list(m, fun->get_arguments(m), 0);

    lib::Set< compiler::symbol::Id >* closure_fv = lib::set_create< compiler::symbol::Id >();

    collect_free_variables(m, fun->get_body(m), closure_fv, closure_ctx, closure_globals);

    push_fv(fv, ctx, closure_fv);

    context::context_destroy(closure_ctx);
    context::context_destroy(closure_globals);

    return;
  }

  if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(node)) {
    if (ast::Function_Literal_Node* func = ast::is_instance< ast::Function_Literal_Node* >(assignment->get_right_operand(m))) {
      if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(assignment->get_left_operand(m))) {
        context::context_declare(globals, m, var);
      }

      if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(assignment->get_left_operand(m))) {
        context::context_declare(globals, m, var);
      }
    }
  }

  if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(node)) {
    context::context_declare(ctx, m, var);
  }

  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(node)) {
    context::context_declare(ctx, m, var);
  }

  if (ast::Literal_Symbol_Node* sym = ast::is_instance< ast::Literal_Symbol_Node* >(node)) {
    if (context::context_is_defined(globals, sym) == false && context::context_is_defined(ctx, sym) == false) {
      lib::insert< compiler::symbol::Id >(fv, sym->get_symbol_id());
    }
  }

  collect_free_variables(m, ast::left_of(m, node), fv, ctx, globals);
  collect_free_variables(m, ast::right_of(m, node), fv, ctx, globals);
}

void push_fv_to_env_rec(lib::SetNode< compiler::symbol::Id >* fv, lib::Set< ast::Declaration_Variable_Node* >* env, ast::Manager* m, context::Context* ctx) {
  if (fv == NULL) {
    return;
  }

  ast::Node* type = context::context_type_of(ctx, m, fv->key);

  assert(type);

  ast::Literal_Symbol_Node* symbol = ast::create_node_literal_symbol(m, compiler::symbol::get_symbol(m->symbol_table, fv->key));

  ast::Declaration_Variable_Node* var = ast::create_variable_declaration(m, symbol, type);

  lib::insert(env, var);

  push_fv_to_env_rec(fv->left, env, m, ctx);
  push_fv_to_env_rec(fv->right, env, m, ctx);
}

void push_fv_to_env(lib::Set< compiler::symbol::Id >* fv, lib::Set< ast::Declaration_Variable_Node* >* env, ast::Manager* m, context::Context* ctx) {
  return push_fv_to_env_rec(fv->root, env, m, ctx);
}

lib::Set< ast::Declaration_Variable_Node* >*
build_closure_env(CPS_Closure_Data* data, ast::Manager* m, ast::Function_Literal_Node* fun, context::Context* ctx, context::Context* globals) {
  lib::Set< compiler::symbol::Id >* fv = lib::set_create< compiler::symbol::Id >();

  context::Context* fun_ctx         = context::context_from_declarations_list(m, fun->get_arguments(m), NULL);
  context::Context* fun_globals_ctx = context::context_from_declarations_list(m, fun->get_arguments(m), globals);

  collect_free_variables(m, fun, fv, fun_ctx, fun_globals_ctx);

  context::context_destroy(fun_ctx);
  context::context_destroy(fun_globals_ctx);

  lib::Set< ast::Declaration_Variable_Node* >* env = lib::set_create< ast::Declaration_Variable_Node* >();

  push_fv_to_env(fv, env, m, ctx);

  lib::set_delete(fv);

  return env;
}

void build_closures_environments(CPS_Closure_Data* data, ast::Manager* m, ast::Node* root, context::Context* ctx, context::Context* globals) {
  if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(root)) {

    if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(assignment->get_left_operand(m))) {
      context::context_declare(ctx, m, var);
    }

    if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(assignment->get_left_operand(m))) {
      context::context_declare(ctx, m, var);

      if (ast::Function_Literal_Node* closure = ast::is_instance< ast::Function_Literal_Node* >(assignment->get_right_operand(m))) {
        context::context_declare(globals, m, var);

        if (cps::is_continuation_closure(data->cps_data, m, var)) {
          lib::Set< ast::Declaration_Variable_Node* >* env = build_closure_env(data, m, closure, ctx, globals);
          lib::insert(data->closure_free_varaibles, var->get_symbol(m)->get_symbol_id(), env);
        }
      }

      build_closures_environments(data, m, assignment->get_right_operand(m), ctx, globals);
    }

    return;
  }

  if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(root)) {
    context::context_declare(ctx, m, var);
  }

  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(root)) {
    context::context_declare(ctx, m, var);
  }

  if (ast::Function_Literal_Node* fun = ast::is_instance< ast::Function_Literal_Node* >(root)) {
    context::Context* fun_ctx         = context::context_create(ctx);
    context::Context* fun_globals_ctx = context::context_create(globals);

    build_closures_environments(data, m, fun->get_body(m), fun_ctx, fun_globals_ctx);

    context::context_destroy(fun_ctx);
    context::context_destroy(fun_globals_ctx);
    return;
  }

  if (ast::If_Node_Statement* if_stmt = ast::is_instance< ast::If_Node_Statement* >(root)) {
    context::Context* if_ctx         = context::context_create(ctx);
    context::Context* if_globals_ctx = context::context_create(globals);

    build_closures_environments(data, m, if_stmt->get_body(m), if_ctx, if_globals_ctx);

    context::context_destroy(if_ctx);
    return;
  }

  if (ast::Elif_List_Node* elif = ast::is_instance< ast::Elif_List_Node* >(root)) {
    build_closures_environments(data, m, elif->get_if(m), ctx, globals);
    build_closures_environments(data, m, elif->get_elif(m), ctx, globals);
    return;
  }

  if (ast::ProgramPoint_List_Node* pp = ast::is_instance< ast::ProgramPoint_List_Node* >(root)) {
    build_closures_environments(data, m, pp->get_statement(m), ctx, globals);
    build_closures_environments(data, m, pp->get_next_program_point(m), ctx, globals);
    return;
  }
}

void print_fvs_vars_rec(ast::Manager* m, lib::SetNode< ast::Declaration_Variable_Node* >* data) {
  if (data == NULL) {
    return;
  }

  printf("  ");
  parser::print_ast_ir(m, data->key->get_symbol(m));
  printf(" : ");
  parser::print_ast_ir(m, data->key->get_type(m));
  printf(";\n");

  print_fvs_vars_rec(m, data->left);
  print_fvs_vars_rec(m, data->right);
}

void print_fvs_rec(ast::Manager* m, lib::TableNode< compiler::symbol::Id, lib::Set< ast::Declaration_Variable_Node* >* >* env) {
  if (env == NULL) {
    return;
  }
  compiler::symbol::Symbol sym = compiler::symbol::get_symbol(m->symbol_table, env->key);
  compiler::symbol::print_symbol(m->symbol_table, sym);

  printf(" = {\n");
  print_fvs_vars_rec(m, env->val->root);
  printf("}\n");

  print_fvs_rec(m, env->left);
  print_fvs_rec(m, env->right);
}

void print_fvs(ast::Manager* m, CPS_Closure_Data* data) {
  print_fvs_rec(m, data->closure_free_varaibles->root);
}

void convert_cps_closures(ast::Manager* m, ast::Node* root, CPS_Closure_Data* data) {
  context::Context* ctx     = context::context_create(NULL);
  context::Context* globals = context::context_create(NULL);

  build_closures_environments(data, m, root, ctx, globals);

  context::context_destroy(ctx);
  context::context_destroy(globals);

  print_fvs(m, data);
}

} // namespace closures
