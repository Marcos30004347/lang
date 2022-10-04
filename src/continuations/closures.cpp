#include "closures.hpp"

#include "ast/ast.hpp"
#include "ast/ast_control_flow.hpp"
#include "ast/ast_declaration.hpp"
#include "ast/ast_function.hpp"
#include "ast/ast_literals.hpp"
#include "ast/ast_manager.hpp"
#include "ast/ast_operations.hpp"
#include "ast/ast_pointer.hpp"
#include "ast/ast_program_point.hpp"
#include "ast/ast_types.hpp"
#include "compiler/symbol_table.hpp"
#include "context/context.hpp"

#include "continuations/continuations.hpp"
#include "lib/set.hpp"
#include "lib/table.hpp"
#include "parser/parser.hpp"

#include <assert.h>
#include <cstdio>
#include <stdio.h>

// TODO(marcos): handle calls

namespace closures {

struct CPS_Closure_Data {
  cps::CPS_Data* cps_data;

  u64 structures_allocated;

  lib::Table< ast::ProgramPoint_List_Node*, ast::ProgramPoint_List_Node* >* environment_allocation_point;

  lib::Table< compiler::symbol::Id, ast::Function_Literal_Node* >* symbol_to_closure;
  lib::Table< compiler::symbol::Id, ast::Literal_Struct_Node* >*   symbol_to_structure;
  lib::Table< ast::Function_Literal_Node*, compiler::symbol::Id >* closure_environment;
};

CPS_Closure_Data* cps_closure_data_create(cps::CPS_Data* cps_data) {
  CPS_Closure_Data* data = new CPS_Closure_Data();

  data->structures_allocated = 0;

  data->cps_data            = cps_data;
  data->symbol_to_closure   = lib::table_create< compiler::symbol::Id, ast::Function_Literal_Node* >();
  data->symbol_to_structure = lib::table_create< compiler::symbol::Id, ast::Literal_Struct_Node* >();
  data->closure_environment = lib::table_create< ast::Function_Literal_Node*, compiler::symbol::Id >();

  data->environment_allocation_point = lib::table_create< ast::ProgramPoint_List_Node*, ast::ProgramPoint_List_Node* >();

  return data;
}

void cps_closure_data_destroy(CPS_Closure_Data* data) {
  cps::cps_result_destroy(data->cps_data);

  lib::table_delete(data->closure_environment);
  lib::table_delete(data->symbol_to_closure);
  lib::table_delete(data->symbol_to_structure);
  lib::table_delete(data->environment_allocation_point);

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

void push_fv(lib::Set< compiler::symbol::Id >* fv, ast::Manager* m, context::Context* ctx, ast::Literal_Struct_Node* child_fv) {
  if (child_fv == NULL) {
    return;
  }

  ast::ProgramPoint_List_Node* members = child_fv->get_members(m);

  while (ast::is_semantic_node(members)) {
    if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(members->get_statement(m))) {
      if (context::context_is_local(ctx, var->get_symbol(m)) == false) {
        lib::insert(fv, var->get_symbol(m)->get_symbol_id());
      }
    }

    if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(members->get_statement(m))) {
      if (context::context_is_local(ctx, var->get_symbol(m)) == false) {
        lib::insert(fv, var->get_symbol(m)->get_symbol_id());
      }
    }

    members = members->get_next_program_point(m);
  }
}

ast::Function_Call_Node* find_function_call(ast::Manager* m, ast::Node* node) {
  if (!ast::is_semantic_node(node)) {
    return NULL;
  }

  if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(node)) {
    return call;
  }

  if (ast::Function_Call_Node* call = find_function_call(m, ast::left_of(m, node))) {
    return call;
  }

  if (ast::Function_Call_Node* call = find_function_call(m, ast::right_of(m, node))) {
    return call;
  }

  return NULL;
}

ast::Effect_Call_Node* find_effect_call(ast::Manager* m, ast::Node* node) {
  if (!ast::is_semantic_node(node)) {
    return NULL;
  }

  if (ast::Effect_Call_Node* call = ast::is_instance< ast::Effect_Call_Node* >(node)) {
    return call;
  }

  if (ast::Effect_Call_Node* call = find_effect_call(m, ast::left_of(m, node))) {
    return call;
  }

  if (ast::Effect_Call_Node* call = find_effect_call(m, ast::right_of(m, node))) {
    return call;
  }

  return NULL;
}

b8 closure_call_add_local_env_to_call(CPS_Closure_Data* data, ast::ProgramPoint_List_Node* point, ast::Manager* m, context::Context* ctx, context::Context* globals) {
  ast::Node* call = find_function_call(m, point->get_statement(m));

  if (!call) {
    call = find_effect_call(m, point->get_statement(m));
  }

  if (call) {
    ast::Node* s = NULL;

    if (ast::Function_Call_Node* c = ast::is_instance< ast::Function_Call_Node* >(call)) {
      s = c->get_function(m);
    }

    if (ast::Effect_Call_Node* c = ast::is_instance< ast::Effect_Call_Node* >(call)) {
      s = c->get_effect(m);
    }

    if (ast::Literal_Symbol_Node* sym = ast::is_instance< ast::Literal_Symbol_Node* >(s)) {
      if (cps::is_continuation_closure(data->cps_data, m, sym)) {
        ast::Function_Literal_Node** closure_ref = lib::search(data->symbol_to_closure, sym->get_symbol_id());

        assert(closure_ref);

        ast::Function_Literal_Node* closure = *closure_ref;

        compiler::symbol::Id* environment_structure_symbol_id = lib::search(data->closure_environment, closure);

        assert(environment_structure_symbol_id);

        compiler::symbol::Symbol environment_structure_symbol = compiler::symbol::get_symbol(m->symbol_table, *environment_structure_symbol_id);

        ast::Literal_Struct_Node** environment_structure = lib::search(data->symbol_to_structure, *environment_structure_symbol_id);

        assert(environment_structure);

        compiler::symbol::Symbol env_arg_id = compiler::symbol::symbol_with_prefix(m->symbol_table, sym->get_symbol(m), "env_");

        ast::Literal_Symbol_Node* env_symbol = ast::create_node_literal_symbol(m, env_arg_id);

        ast::ProgramPoint_List_Node* members = (*environment_structure)->get_members(m);

        ast::Declaration_Variable_Node* env_var = ast::create_variable_declaration(m, env_symbol, ast::create_node_literal_symbol(m, environment_structure_symbol));

        ast::ProgramPoint_List_Node* members_assignments = NULL;

        while (ast::is_semantic_node(members)) {
          if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(members->get_statement(m))) {
            ast::Member_Access_Node* access = ast::create_node_member_access(m, env_symbol, var->get_symbol(m));

            if (context::context_is_defined(globals, var->get_symbol(m)) == false && context::context_is_local(ctx, var->get_symbol(m)) == false) {
              // NOTE(marcos): May need to be a ptr access
              ast::Literal_Symbol_Node* env_symbol_access = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "env"));
              ast::Member_Access_Node*  env_access        = ast::create_node_member_access(m, env_symbol_access, ast::deep_copy(m, var->get_symbol(m)));

              ast::Variable_Assignment_Node* assignment = ast::create_node_assignment(m, access, env_access);
              members_assignments                       = ast::create_node_program_point(m, assignment, members_assignments);
            } else {
              ast::Variable_Assignment_Node* assignment = ast::create_node_assignment(m, access, ast::create_node_value_address(m, ast::deep_copy(m, var->get_symbol(m))));
              members_assignments                       = ast::create_node_program_point(m, assignment, members_assignments);
            }
          }

          if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(members->get_statement(m))) {
            ast::Member_Access_Node* access = ast::create_node_member_access(m, env_symbol, var->get_symbol(m));

            if (context::context_is_defined(globals, var->get_symbol(m)) == false && context::context_is_local(ctx, var->get_symbol(m)) == false) {
              // NOTE(marcos): May need to be a ptr access
              ast::Literal_Symbol_Node* env_symbol_access = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "env"));
              ast::Member_Access_Node*  env_access        = ast::create_node_member_access(m, env_symbol_access, ast::deep_copy(m, var->get_symbol(m)));

              ast::Variable_Assignment_Node* assignment = ast::create_node_assignment(m, access, env_access);
              members_assignments                       = ast::create_node_program_point(m, assignment, members_assignments);
            } else {
              ast::Variable_Assignment_Node* assignment = ast::create_node_assignment(m, access, ast::create_node_value_address(m, ast::deep_copy(m, var->get_symbol(m))));
              members_assignments                       = ast::create_node_program_point(m, assignment, members_assignments);
            }
          }

          members = members->get_next_program_point(m);
        }

        members_assignments = ast::create_node_program_point(m, env_var, members_assignments);

        lib::insert(data->environment_allocation_point, point, members_assignments);

        if (ast::Function_Call_Node* c = ast::is_instance< ast::Function_Call_Node* >(call)) {
          c->push_argument(m, ast::create_node_value_address(m, env_symbol));
        }

        if (ast::Effect_Call_Node* c = ast::is_instance< ast::Effect_Call_Node* >(call)) {
          c->push_argument(m, ast::create_node_value_address(m, env_symbol));
        }

        return true;
      }
    }
  }
  return false;
}

b8 collect_free_variables(
    CPS_Closure_Data*                 data,
    ast::Manager*                     m,
    ast::Node*                        node,
    lib::Set< compiler::symbol::Id >* fv,
    context::Context*                 ctx,
    context::Context*                 globals,
    ast::ProgramPoint_List_Node*      parent) {
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

          build_closures_environments(data, m, assignment, closure_ctx, closure_globals, NULL);

          if (compiler::symbol::Id* env_symbol = lib::search(data->closure_environment, fun)) {
            ast::Literal_Struct_Node** env_structure = lib::search(data->symbol_to_structure, *env_symbol);

            assert(*env_structure);

            push_fv(fv, m, ctx, *env_structure);
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

  if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(node)) {
    ast::Node* l = call->get_function(m);

    if (collect_free_variables(data, m, l, fv, ctx, globals, parent)) {
      ast::Literal_Symbol_Node* env_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "env"));

      // NOTE(marcos): May need to be a ptr access
      ast::Member_Access_Node* access = ast::create_node_member_access(m, env_symbol, ast::deep_copy(m, l));

      ast::Pointer_Value_Node* ptr_val = ast::create_node_pointer_value(m, access);

      ast::set_left(m, node, ptr_val);
    }

    collect_free_variables(data, m, call->get_arguments(m), fv, ctx, globals, parent);

    closure_call_add_local_env_to_call(data, parent, m, ctx, globals);

    return false;
  }

  if (ast::Effect_Call_Node* call = ast::is_instance< ast::Effect_Call_Node* >(node)) {
    ast::Node* l = call->get_effect(m);

    if (collect_free_variables(data, m, l, fv, ctx, globals, parent)) {
      ast::Literal_Symbol_Node* env_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "env"));

      // NOTE(marcos): May need to be a ptr access
      ast::Member_Access_Node* access = ast::create_node_member_access(m, env_symbol, ast::deep_copy(m, l));

      ast::Pointer_Value_Node* ptr_val = ast::create_node_pointer_value(m, access);

      ast::set_left(m, node, ptr_val);
    }

    collect_free_variables(data, m, call->get_arguments(m), fv, ctx, globals, parent);

    closure_call_add_local_env_to_call(data, parent, m, ctx, globals);

    return false;
  }

  if (ast::ProgramPoint_List_Node* pp = ast::is_instance< ast::ProgramPoint_List_Node* >(node)) {
    collect_free_variables(data, m, pp->get_statement(m), fv, ctx, globals, pp);
    collect_free_variables(data, m, pp->get_next_program_point(m), fv, ctx, globals, pp->get_next_program_point(m));

    return false;
  }

  ast::Node* l = ast::left_of(m, node);

  if (collect_free_variables(data, m, l, fv, ctx, globals, parent)) {
    ast::Literal_Symbol_Node* env_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "env"));

    // NOTE(marcos): May need to be a ptr access
    ast::Member_Access_Node* access = ast::create_node_member_access(m, env_symbol, ast::deep_copy(m, l));

    ast::Pointer_Value_Node* ptr_val = ast::create_node_pointer_value(m, access);

    ast::set_left(m, node, ptr_val);
  }

  ast::Node* r = ast::right_of(m, node);

  if (collect_free_variables(data, m, r, fv, ctx, globals, parent)) {
    ast::Literal_Symbol_Node* env_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "env"));

    // NOTE(marcos): May need to be a ptr access
    ast::Member_Access_Node* access = ast::create_node_member_access(m, env_symbol, ast::deep_copy(m, r));

    ast::Pointer_Value_Node* ptr_val = ast::create_node_pointer_value(m, access);

    ast::set_right(m, node, ptr_val);
  }

  return false;
}

ast::ProgramPoint_List_Node* push_fv_to_env_rec(lib::SetNode< compiler::symbol::Id >* fv, ast::Manager* m, context::Context* ctx, ast::ProgramPoint_List_Node* tail) {
  if (fv == NULL) {
    return tail;
  }

  ast::Node* type = context::context_type_of(ctx, m, fv->key);

  assert(type);

  ast::Literal_Symbol_Node* symbol = ast::create_node_literal_symbol(m, compiler::symbol::get_symbol(m->symbol_table, fv->key));

  ast::Declaration_Variable_Node* var = ast::create_variable_declaration(m, symbol, ast::create_node_type_pointer(m, type));

  ast::ProgramPoint_List_Node* members = ast::create_node_program_point(m, var, tail);

  members = push_fv_to_env_rec(fv->left, m, ctx, members);
  return push_fv_to_env_rec(fv->right, m, ctx, members);
}

ast::ProgramPoint_List_Node* push_fv_to_env(lib::Set< compiler::symbol::Id >* fv, ast::Manager* m, context::Context* ctx) {
  return push_fv_to_env_rec(fv->root, m, ctx, NULL);
}

ast::Literal_Struct_Node* build_closure_env(CPS_Closure_Data* data, ast::Manager* m, ast::Function_Literal_Node* fun, context::Context* ctx, context::Context* globals) {
  lib::Set< compiler::symbol::Id >* fv = lib::set_create< compiler::symbol::Id >();

  context::Context* fun_ctx         = context::context_from_declarations_list(m, fun->get_arguments(m), ctx);
  context::Context* fun_globals_ctx = context::context_create(globals);

  collect_free_variables(data, m, fun->get_body(m), fv, fun_ctx, fun_globals_ctx, NULL);

  context::context_destroy(fun_ctx);
  context::context_destroy(fun_globals_ctx);

  ast::ProgramPoint_List_Node* members = push_fv_to_env(fv, m, ctx);

  lib::set_delete(fv);

  return ast::create_node_literal_struct(m, members);
}

void build_closures_environments(CPS_Closure_Data* data, ast::Manager* m, ast::Node* root, context::Context* ctx, context::Context* globals, ast::ProgramPoint_List_Node* parent) {
  if (!ast::is_semantic_node(root)) {
    return;
  }

  if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(root)) {
    b8 is_closure = false;

    if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(assignment->get_left_operand(m))) {
      context::context_declare(ctx, m, var);
    }

    if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(assignment->get_left_operand(m))) {
      context::context_declare(ctx, m, var);
      if (ast::Function_Literal_Node* closure = ast::is_instance< ast::Function_Literal_Node* >(assignment->get_right_operand(m))) {
        context::context_declare(globals, m, var);

        if (cps::is_continuation_closure(data->cps_data, m, var->get_symbol(m))) {

          lib::insert(data->symbol_to_closure, var->get_symbol(m)->get_symbol_id(), closure);

          ast::Literal_Struct_Node* env = build_closure_env(data, m, closure, ctx, globals);

          compiler::symbol::Symbol env_id = compiler::symbol::symbol_with_prefix(m->symbol_table, var->get_symbol(m)->get_symbol(m), "env_");

          ast::Literal_Symbol_Node* env_symbol = ast::create_node_literal_symbol(m, env_id);

          lib::insert(data->symbol_to_structure, env_id.id, env);
          lib::insert(data->closure_environment, closure, env_id.id);

          ast::Node* type = ast::create_node_type_pointer(m, ast::deep_copy(m, env_symbol));

          ast::Literal_Symbol_Node*       env_arg_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "env"));
          ast::Declaration_Variable_Node* env_arg        = ast::create_variable_declaration(m, env_arg_symbol, type);

          closure->push_argument(m, env_arg);
          is_closure = true;
          // return;
        }
      }
    }

    if (!is_closure) {
      build_closures_environments(data, m, assignment->get_left_operand(m), ctx, globals, parent);
      build_closures_environments(data, m, assignment->get_right_operand(m), ctx, globals, parent);
    }
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

    build_closures_environments(data, m, fun->get_body(m), fun_ctx, fun_globals_ctx, parent);

    context::context_destroy(fun_ctx);
    context::context_destroy(fun_globals_ctx);

    return;
  }

  if (ast::If_Node_Statement* if_stmt = ast::is_instance< ast::If_Node_Statement* >(root)) {
    context::context_push_scope(ctx);

    build_closures_environments(data, m, if_stmt->get_body(m), ctx, globals, parent);

    context::context_pop_scope(ctx);

    return;
  }

  if (ast::Elif_List_Node* elif = ast::is_instance< ast::Elif_List_Node* >(root)) {
    build_closures_environments(data, m, elif->get_if(m), ctx, globals, parent);
    build_closures_environments(data, m, elif->get_elif(m), ctx, globals, parent);
    return;
  }

  if (ast::ProgramPoint_List_Node* pp = ast::is_instance< ast::ProgramPoint_List_Node* >(root)) {
    build_closures_environments(data, m, pp->get_statement(m), ctx, globals, pp);
    build_closures_environments(data, m, pp->get_next_program_point(m), ctx, globals, pp->get_next_program_point(m));
    return;
  }

  if (ast::Return_Node_Statement* ret = ast::is_instance< ast::Return_Node_Statement* >(root)) {
    build_closures_environments(data, m, ret->get_expression(m), ctx, globals, parent);
    return;
  }

  if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(root)) {
    closure_call_add_local_env_to_call(data, parent, m, ctx, globals);
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
void print_fvs_rec(CPS_Closure_Data* data, ast::Manager* m, lib::TableNode< ast::Function_Literal_Node*, compiler::symbol::Id >* env) {
  if (env == NULL) {
    return;
  }

  compiler::symbol::Symbol sym = compiler::symbol::get_symbol(m->symbol_table, env->val);
  compiler::symbol::print_symbol(m->symbol_table, sym);

  ast::Literal_Struct_Node** strucutre = lib::search(data->symbol_to_structure, env->val);
  printf(" = ");
  parser::print_ast_ir(m, *strucutre);
  printf("\n");
}

// TODO(marcos): remove
void print_fvs(CPS_Closure_Data* data, ast::Manager* m) {
  print_fvs_rec(data, m, data->closure_environment->root);
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

void add_stack_environment_allocations_rec(ast::Manager* m, lib::TableNode< ast::ProgramPoint_List_Node*, ast::ProgramPoint_List_Node* >* node) {
  if (node == NULL) {
    return;
  }

  ast::ProgramPoint_List_Node* pp = node->val;

  node->key->insert(m, node->key->get_statement(m));

  node->key->set_statement(m, pp->get_statement(m));

  pp = pp->get_next_program_point(m);

  while (ast::is_semantic_node(pp)) {
    node->key->insert(m, pp->get_statement(m));

    pp = pp->get_next_program_point(m);
  }

  add_stack_environment_allocations_rec(m, node->left);
  add_stack_environment_allocations_rec(m, node->right);
}

void add_stack_environment_allocations(ast::Manager* m, CPS_Closure_Data* data) {
  add_stack_environment_allocations_rec(m, data->environment_allocation_point->root);
}

void add_environment_structures_to_global_context_rec(
    ast::Manager* m, ast::ProgramPoint_List_Node* root, CPS_Closure_Data* data, lib::TableNode< ast::Function_Literal_Node*, compiler::symbol::Id >* node) {
  if (node == NULL) {
    return;
  }

  compiler::symbol::Symbol structure_symbol = compiler::symbol::get_symbol(m->symbol_table, node->val);

  ast::Literal_Struct_Node** structure = lib::search(data->symbol_to_structure, node->val);

  assert(structure);

  ast::Declaration_Constant_Node* declaration = ast::create_constant_declaration(m, ast::create_node_literal_symbol(m, structure_symbol), ast::create_node_type_struct(m));

  ast::Variable_Assignment_Node* assignment = ast::create_node_assignment(m, declaration, *structure);

  root->insert(m, root->get_statement(m));

  root->set_statement(m, assignment);

  add_environment_structures_to_global_context_rec(m, root, data, node->left);
  add_environment_structures_to_global_context_rec(m, root, data, node->right);
}
void add_environment_structures_to_global_context(ast::Manager* m, ast::Node* root, CPS_Closure_Data* data) {
  add_environment_structures_to_global_context_rec(m, ast::is_instance< ast::ProgramPoint_List_Node* >(root), data, data->closure_environment->root);
}

void add_closures_to_global_context_rec(CPS_Closure_Data* data, ast::Manager* m, ast::ProgramPoint_List_Node* root, ast::Node* node, ast::Node* parent) {
  if (!ast::is_semantic_node(node) || !ast::is_semantic_node(root)) {
    return;
  }

  if (ast::ProgramPoint_List_Node* pp = ast::is_instance< ast::ProgramPoint_List_Node* >(node)) {
    if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(pp->get_statement(m))) {
      if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(assignment->get_left_operand(m))) {
        if (cps::is_continuation_closure(data->cps_data, m, var->get_symbol(m))) {
          if (parent->left == pp->id) {
            parent->left = pp->right;
          }

          if (parent->right == pp->id) {
            parent->right = pp->right;
          }

          root->insert(m, root->get_statement(m));
          root->set_statement(m, assignment);
        }
      }
    }
  }

  add_closures_to_global_context_rec(data, m, root, ast::left_of(m, node), node);
  add_closures_to_global_context_rec(data, m, root, ast::right_of(m, node), node);
}

void add_closures_to_global_context(CPS_Closure_Data* data, ast::Manager* m, ast::Node* root) {
  add_closures_to_global_context_rec(data, m, ast::is_instance< ast::ProgramPoint_List_Node* >(root), root, NULL);
}

void convert_cps_closures(ast::Manager* m, ast::Node* root, CPS_Closure_Data* data) {
  context::Context* ctx     = context::context_create(NULL);
  context::Context* globals = context::context_create(NULL);

  build_closures_environments(data, m, root, ctx, globals, NULL);

  context::context_destroy(ctx);
  context::context_destroy(globals);

  add_stack_environment_allocations(m, data);
  add_closures_to_global_context(data, m, root);
  add_environment_structures_to_global_context(m, root, data);

  print_fvs(data, m);
}

} // namespace closures
