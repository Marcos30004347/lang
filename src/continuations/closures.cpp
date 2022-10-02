#include "closures.hpp"

#include "ast/ast.hpp"
#include "ast/ast_pointer.hpp"
#include "compiler/symbol_table.hpp"
#include "context/context.hpp"

#include "lib/set.hpp"
#include "lib/table.hpp"

#include <assert.h>
#include <cstdio>
#include <stdio.h>

// TODO(marcos): handle calls

namespace closures {

struct CPS_Closure_Data {
  cps::CPS_Data* cps_data;

  lib::Table< ast::Function_Literal_Node*, lib::Set< ast::Declaration_Variable_Node* >* >* closure_free_varaibles;
};

void delete_fv_set(lib::Set< ast::Declaration_Variable_Node* >* set) {
  lib::set_delete(set);
}

CPS_Closure_Data* cps_closure_data_create(cps::CPS_Data* cps_data) {
  CPS_Closure_Data* data = new CPS_Closure_Data();

  data->cps_data               = cps_data;
  data->closure_free_varaibles = lib::table_create< ast::Function_Literal_Node*, lib::Set< ast::Declaration_Variable_Node* >* >();

  return data;
}

void cps_closure_data_destroy(CPS_Closure_Data* data) {
  cps::cps_result_destroy(data->cps_data);

  lib::table_delete(data->closure_free_varaibles, delete_fv_set);

  delete data;
}

// TODO(marcos): remove
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

// TODO(marcos): remove
void print_set(ast::Manager* m, lib::Set< compiler::symbol::Id >* child_fv) {
  printf("{\n");
  print_set_rec(m, child_fv->root);
  printf("}\n");
}

void push_fv_rec(lib::Set< compiler::symbol::Id >* fv, ast::Manager* m, context::Context* ctx, lib::SetNode< ast::Declaration_Variable_Node* >* child_fv) {
  if (child_fv == NULL) {
    return;
  }

  if (context::context_is_local(ctx, child_fv->key->get_symbol(m)) == false) {
    lib::insert(fv, child_fv->key->get_symbol(m)->get_symbol_id());
  }

  push_fv_rec(fv, m, ctx, child_fv->left);
  push_fv_rec(fv, m, ctx, child_fv->right);
}

void push_fv(lib::Set< compiler::symbol::Id >* fv, ast::Manager* m, context::Context* ctx, lib::Set< ast::Declaration_Variable_Node* >* child_fv) {
  push_fv_rec(fv, m, ctx, child_fv->root);
}

b8 collect_free_variables(CPS_Closure_Data* data, ast::Manager* m, ast::Node* node, lib::Set< compiler::symbol::Id >* fv, context::Context* ctx, context::Context* globals) {
  if (!ast::is_semantic_node(node)) {
    return false;
  }

  if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(node)) {
    if (ast::Function_Literal_Node* func = ast::is_instance< ast::Function_Literal_Node* >(assignment->get_right_operand(m))) {
      if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(assignment->get_left_operand(m))) {
        context::context_declare(globals, m, var);
      }

      if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(assignment->get_left_operand(m))) {
        context::context_declare(globals, m, var);

        if (ast::Function_Literal_Node* fun = ast::is_instance< ast::Function_Literal_Node* >(assignment->get_right_operand(m))) {
          context::Context* closure_ctx     = context::context_from_declarations_list(m, fun->get_arguments(m), ctx);
          context::Context* closure_globals = context::context_create(globals);

          build_closures_environments(data, m, assignment, closure_ctx, closure_globals);

          if (lib::Set< ast::Declaration_Variable_Node* >** fvs = lib::search(data->closure_free_varaibles, fun)) {
            push_fv(fv, m, ctx, *fvs);
          }

          context::context_destroy(closure_ctx);
          context::context_destroy(closure_globals);

          return false;
        }
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
    if (context::context_is_defined(globals, sym) == false && context::context_is_local(ctx, sym) == false) {
      lib::insert< compiler::symbol::Id >(fv, sym->get_symbol_id());

      return true;
    }
  }

  ast::Node* l = ast::left_of(m, node);

  if (collect_free_variables(data, m, l, fv, ctx, globals)) {
    ast::Literal_Symbol_Node* env_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "env"));

    // NOTE(marcos): May need to be a ptr access
    ast::Member_Access_Node* access = ast::create_node_member_access(m, env_symbol, ast::deep_copy(m, l));

    ast::Pointer_Value_Node* ptr_val = ast::create_node_pointer_value(m, access);

    ast::set_left(m, node, ptr_val);
  }

  ast::Node* r = ast::right_of(m, node);

  if (collect_free_variables(data, m, r, fv, ctx, globals)) {
    ast::Literal_Symbol_Node* env_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "env"));

    // NOTE(marcos): May need to be a ptr access
    ast::Member_Access_Node* access = ast::create_node_member_access(m, env_symbol, ast::deep_copy(m, r));

    ast::Pointer_Value_Node* ptr_val = ast::create_node_pointer_value(m, access);

    ast::set_right(m, node, ptr_val);
  }

  return false;
}

void push_fv_to_env_rec(lib::SetNode< compiler::symbol::Id >* fv, lib::Set< ast::Declaration_Variable_Node* >* env, ast::Manager* m, context::Context* ctx) {
  if (fv == NULL) {
    return;
  }
  // printf("----> ");
  // compiler::symbol::Symbol s = compiler::symbol::get_symbol(m->symbol_table, fv->key);
  // compiler::symbol::print_symbol(m->symbol_table, s);
  // printf("\n");

  // context::context_print(ctx, m);
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

  context::Context* fun_ctx         = context::context_from_declarations_list(m, fun->get_arguments(m), ctx);
  context::Context* fun_globals_ctx = context::context_create(globals);

  collect_free_variables(data, m, fun->get_body(m), fv, fun_ctx, fun_globals_ctx);

  context::context_destroy(fun_ctx);
  context::context_destroy(fun_globals_ctx);

  lib::Set< ast::Declaration_Variable_Node* >* env = lib::set_create< ast::Declaration_Variable_Node* >();

  push_fv_to_env(fv, env, m, ctx);

  lib::set_delete(fv);

  return env;
}

void build_closures_environments(CPS_Closure_Data* data, ast::Manager* m, ast::Node* root, context::Context* ctx, context::Context* globals) {
  if (!ast::is_semantic_node(root)) {
    return;
  }

  if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(root)) {
    if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(assignment->get_left_operand(m))) {
      context::context_declare(ctx, m, var);
    }

    if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(assignment->get_left_operand(m))) {
      context::context_declare(ctx, m, var);

      if (ast::Function_Literal_Node* closure = ast::is_instance< ast::Function_Literal_Node* >(assignment->get_right_operand(m))) {
        context::context_declare(globals, m, var);

        if (cps::is_continuation_closure(data->cps_data, m, var->get_symbol(m))) {
          // printf("=======\n");
          // parser::print_ast_ir(m, root);

          // context::Context* closure_ctx         = context::context_from_declarations_list(m, closure->get_arguments(m), ctx);
          // context::Context* closure_globals_ctx = context::context_create(globals);

          lib::Set< ast::Declaration_Variable_Node* >* env = build_closure_env(data, m, closure, ctx, globals);

          // TODO(marcos): create an struct environment from 'env'
          ast::Node* type = ast::create_node_type_any(m);

          ast::Literal_Symbol_Node* env_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "env"));

          ast::Declaration_Variable_Node* env_arg = ast::create_variable_declaration(m, env_symbol, type);

          closure->push_argument(m, env_arg);

          lib::insert(data->closure_free_varaibles, closure, env);
          // context::context_destroy(closure_ctx);
          // context::context_destroy(closure_globals_ctx);

          return;
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
    context::Context* fun_ctx         = context::context_from_declarations_list(m, fun->get_arguments(m), ctx);
    context::Context* fun_globals_ctx = context::context_create(globals);

    build_closures_environments(data, m, fun->get_body(m), fun_ctx, fun_globals_ctx);

    context::context_destroy(fun_ctx);
    context::context_destroy(fun_globals_ctx);

    return;
  }

  if (ast::If_Node_Statement* if_stmt = ast::is_instance< ast::If_Node_Statement* >(root)) {
    context::context_push_scope(ctx);

    build_closures_environments(data, m, if_stmt->get_body(m), ctx, globals);

    context::context_pop_scope(ctx);

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

// TODO(marcos): remove
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

// TODO(marcos): remove
void print_fvs_rec(ast::Manager* m, lib::TableNode< ast::Function_Literal_Node*, lib::Set< ast::Declaration_Variable_Node* >* >* env) {
  if (env == NULL) {
    return;
  }

  // compiler::symbol::Symbol sym = compiler::symbol::get_symbol(m->symbol_table, env->key);
  // compiler::symbol::print_symbol(m->symbol_table, sym);

  printf(" env = {\n");
  print_fvs_vars_rec(m, env->val->root);
  printf("}\n");

  print_fvs_rec(m, env->left);
  print_fvs_rec(m, env->right);
}

// TODO(marcos): remove
void print_fvs(ast::Manager* m, CPS_Closure_Data* data) {
  print_fvs_rec(m, data->closure_free_varaibles->root);
}

// void closure_add_environment_access(ast::Manager* m, ast::Node* root, context::Context* ctx) {
// 	if(ast::Declaration_Variable_Node* var = ast::is_instance<ast::Declaration_Variable_Node*>(root)) {
// 		context::context_declare(ctx, m, var);
// 	}

// 	if(ast::Declaration_Constant_Node* var = ast::is_instance<ast::Declaration_Constant_Node*>(root)) {
// 		context::context_declare(ctx, m, var);
// 	}
// }

// void closure_add_environment(ast::Manager* m, lib::TableNode< ast::Function_Literal_Node*, lib::Set< ast::Declaration_Variable_Node* >* >* env) {
//   if (env == NULL) {
//     return;
//   }

//   print_fvs_rec(m, env->left);
//   print_fvs_rec(m, env->right);
// }

void convert_cps_closures(ast::Manager* m, ast::Node* root, CPS_Closure_Data* data) {
  context::Context* ctx     = context::context_create(NULL);
  context::Context* globals = context::context_create(NULL);

  build_closures_environments(data, m, root, ctx, globals);

  context::context_destroy(ctx);
  context::context_destroy(globals);

  print_fvs(m, data);
}

} // namespace closures
