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
#include "compiler/compiler.hpp"
#include "compiler/symbol_table.hpp"
#include "context/context.hpp"

#include "continuations/continuations.hpp"
#include "lib/set.hpp"
#include "lib/table.hpp"
#include "parser/parser.hpp"
#include "stackframe/stackframe.hpp"

#include <assert.h>
#include <cstdio>
#include <stdio.h>

namespace closures {

struct Environment_Dependency_Node {
  compiler::symbol::Id id;

  lib::Set< compiler::symbol::Id >* to;
  lib::Set< compiler::symbol::Id >* from;
};

Environment_Dependency_Node* create_dependency_node(compiler::symbol::Id env_id) {
  Environment_Dependency_Node* d = new Environment_Dependency_Node();

  d->id = env_id;

  d->to   = lib::set_create< compiler::symbol::Id >();
  d->from = lib::set_create< compiler::symbol::Id >();

  return d;
}

void delete_dependency_node(Environment_Dependency_Node* d) {
  lib::set_delete(d->from);
  lib::set_delete(d->to);
  delete d;
}

struct CPS_Closure_Data {
  stackframe::Stack_Frame_Data* sf_data;

  lib::Table< ast::ProgramPoint_List_Node*, ast::ProgramPoint_List_Node* >*      environment_allocation_point;
  lib::Table< compiler::symbol::Id, lib::Set< ast::ProgramPoint_List_Node* >* >* symbol_environment_to_allocations;

  lib::Table< compiler::symbol::Id, ast::Function_Literal_Node* >* symbol_to_closure;
  lib::Table< compiler::symbol::Id, ast::Literal_Struct_Node* >*   symbol_to_structure;
  lib::Table< ast::Function_Literal_Node*, compiler::symbol::Id >* closure_environment;

  lib::Table< ast::Function_Literal_Node*, ast::Declaration_Variable_Node* >* closure_stackframe_argument;

  lib::Table< compiler::symbol::Id, Environment_Dependency_Node* >* env_dependency_graph;
};

stackframe::Stack_Frame_Data* cps_closure_data_get_stackframe_data(CPS_Closure_Data* data) {
  return data->sf_data;
}

ast::Declaration_Variable_Node* cps_closure_data_get_stackframe_argument(CPS_Closure_Data* data, ast::Function_Literal_Node* lit) {
  ast::Declaration_Variable_Node** d = lib::search(data->closure_stackframe_argument, lit);

  if (d) {
    return *d;
  }

  return NULL;
}

void build_closures_environments(
    CPS_Closure_Data* data, ast::Manager* m, ast::Node* root, context::Context* ctx, context::Context* globals, ast::ProgramPoint_List_Node* parent, ast::Function_Literal_Node*);

CPS_Closure_Data* cps_closure_data_create(stackframe::Stack_Frame_Data* sf_data) {
  CPS_Closure_Data* data = new CPS_Closure_Data();

  data->sf_data             = sf_data;
  data->symbol_to_closure   = lib::table_create< compiler::symbol::Id, ast::Function_Literal_Node* >();
  data->symbol_to_structure = lib::table_create< compiler::symbol::Id, ast::Literal_Struct_Node* >();
  data->closure_environment = lib::table_create< ast::Function_Literal_Node*, compiler::symbol::Id >();

  data->closure_stackframe_argument       = lib::table_create< ast::Function_Literal_Node*, ast::Declaration_Variable_Node* >();
  data->environment_allocation_point      = lib::table_create< ast::ProgramPoint_List_Node*, ast::ProgramPoint_List_Node* >();
  data->symbol_environment_to_allocations = lib::table_create< compiler::symbol::Id, lib::Set< ast::ProgramPoint_List_Node* >* >();

  data->env_dependency_graph = lib::table_create< compiler::symbol::Id, Environment_Dependency_Node* >();

  return data;
}

void set_delete(lib::Set< ast::ProgramPoint_List_Node* >* set) {
  lib::set_delete(set);
}

void cps_closure_data_destroy(CPS_Closure_Data* data) {
  stackframe::destroy_stack_frame_data(data->sf_data);

  lib::table_delete(data->closure_environment);
  lib::table_delete(data->symbol_to_closure);
  lib::table_delete(data->symbol_to_structure);
  lib::table_delete(data->environment_allocation_point);
  lib::table_delete(data->closure_stackframe_argument);
  lib::table_delete(data->symbol_environment_to_allocations, set_delete);
  lib::table_delete(data->env_dependency_graph, delete_dependency_node);

  delete data;
}

ast::Function_Literal_Node* cps_closure_data_get_closure_handler(CPS_Closure_Data* data, ast::Manager* m, compiler::symbol::Id id) {
  ast::Function_Literal_Node** f = lib::search(data->symbol_to_closure, id);
  return f ? *f : NULL;
}

ast::Function_Literal_Node* cps_closure_data_get_closure_handler(CPS_Closure_Data* data, ast::Manager* m, ast::Literal_Symbol_Node* id) {
  return cps_closure_data_get_closure_handler(data, m, id->get_symbol_id());
}

ast::Literal_Struct_Node* cps_closure_data_get_environment_struct_from_symbol(CPS_Closure_Data* data, ast::Manager* m, compiler::symbol::Id id) {
  ast::Literal_Struct_Node** f = lib::search(data->symbol_to_structure, id);
  return f ? *f : NULL;
}

ast::Literal_Struct_Node* cps_closure_data_get_environment_struct_from_symbol(CPS_Closure_Data* data, ast::Manager* m, ast::Literal_Symbol_Node* id) {
  return cps_closure_data_get_environment_struct_from_symbol(data, m, id->get_symbol_id());
}

compiler::symbol::Id cps_closure_data_get_environment_symbol_id(CPS_Closure_Data* data, ast::Manager* m, ast::Function_Literal_Node* id) {
  compiler::symbol::Id* i = lib::search(data->closure_environment, id);

  if (i == NULL) {
    return compiler::symbol::empty(m->symbol_table).id;
  }
  return *i;
}

ast::Literal_Struct_Node* cps_closure_data_get_environment_struct_from_function(CPS_Closure_Data* data, ast::Manager* m, ast::Function_Literal_Node* id) {
  compiler::symbol::Id* i = lib::search(data->closure_environment, id);

  if (i == NULL) {
    return NULL;
  }

  return cps_closure_data_get_environment_struct_from_symbol(data, m, *i);
}

ast::Literal_Struct_Node* cps_closure_data_get_environment_struct_from_function(CPS_Closure_Data* data, ast::Manager* m, compiler::symbol::Id id) {
  ast::Function_Literal_Node* lit = cps_closure_data_get_closure_handler(data, m, id);

  if (lit == NULL) {
    return NULL;
  }

  return cps_closure_data_get_environment_struct_from_function(data, m, lit);
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

b8 closure_call_add_local_env_to_call(
    CPS_Closure_Data* data, ast::ProgramPoint_List_Node* point, ast::Manager* m, context::Context* ctx, context::Context* globals, ast::Function_Literal_Node* function) {
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
      if (cps::is_continuation_closure(stackframe::stack_frame_data_get_cps_data(data->sf_data), m, sym)) {

        assert(function);

        u64 depth = stackframe::stack_frame_get_function_depth(data->sf_data, function);

        compiler::symbol::Symbol env_arg_id = compiler::symbol::number_to_symbol(m->symbol_table, depth, "sp");

        ast::Literal_Symbol_Node* env_symbol = ast::create_node_literal_symbol(m, env_arg_id);

        if (ast::Function_Call_Node* c = ast::is_instance< ast::Function_Call_Node* >(call)) {
          c->push_argument(m, ast::deep_copy(m, env_symbol));
        }

        if (ast::Effect_Call_Node* c = ast::is_instance< ast::Effect_Call_Node* >(call)) {
          c->push_argument(m, ast::deep_copy(m, env_symbol));
        }

        return true;
      }
    }
  }

  return false;
}

b8 collect_free_variables(
    CPS_Closure_Data*            data,
    ast::Manager*                m,
    ast::Node*                   node,
    context::Context*            ctx,
    context::Context*            globals,
    ast::ProgramPoint_List_Node* parent,
    ast::Function_Literal_Node*  function) {
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

          build_closures_environments(data, m, assignment, closure_ctx, closure_globals, NULL, fun);

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

  if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(node)) {
    ast::Node* l = call->get_function(m);

    collect_free_variables(data, m, l, ctx, globals, parent, function);

    collect_free_variables(data, m, call->get_arguments(m), ctx, globals, parent, function);

    closure_call_add_local_env_to_call(data, parent, m, ctx, globals, function);

    return false;
  }

  if (ast::Effect_Call_Node* call = ast::is_instance< ast::Effect_Call_Node* >(node)) {
    ast::Node* l = call->get_effect(m);

    collect_free_variables(data, m, l, ctx, globals, parent, function);

    collect_free_variables(data, m, call->get_arguments(m), ctx, globals, parent, function);

    closure_call_add_local_env_to_call(data, parent, m, ctx, globals, function);

    return false;
  }

  if (ast::ProgramPoint_List_Node* pp = ast::is_instance< ast::ProgramPoint_List_Node* >(node)) {
    collect_free_variables(data, m, pp->get_statement(m), ctx, globals, pp, function);
    collect_free_variables(data, m, pp->get_next_program_point(m), ctx, globals, pp->get_next_program_point(m), function);

    return false;
  }

  ast::Node* l = ast::left_of(m, node);

  collect_free_variables(data, m, l, ctx, globals, parent, function);

  ast::Node* r = ast::right_of(m, node);

  collect_free_variables(data, m, r, ctx, globals, parent, function);

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

void build_closure_env(CPS_Closure_Data* data, ast::Manager* m, ast::Function_Literal_Node* fun, context::Context* ctx, context::Context* globals) {
  context::Context* fun_ctx         = context::context_from_declarations_list(m, fun->get_arguments(m), ctx);
  context::Context* fun_globals_ctx = context::context_create(globals);

  collect_free_variables(data, m, fun->get_body(m), fun_ctx, fun_globals_ctx, NULL, fun);

  context::context_destroy(fun_ctx);
  context::context_destroy(fun_globals_ctx);

  return;
}

void build_closures_environments(
    CPS_Closure_Data*            data,
    ast::Manager*                m,
    ast::Node*                   root,
    context::Context*            ctx,
    context::Context*            globals,
    ast::ProgramPoint_List_Node* parent,
    ast::Function_Literal_Node*  function) {
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

        build_closure_env(data, m, closure, ctx, globals);

        if (cps::is_continuation_closure(stackframe::stack_frame_data_get_cps_data(data->sf_data), m, var->get_symbol(m))) {

          lib::insert(data->symbol_to_closure, var->get_symbol(m)->get_symbol_id(), closure);

          assert(closure);

          u64 depth = stackframe::stack_frame_get_function_depth(data->sf_data, closure);

          ast::Literal_Symbol_Node*       env_arg_symbol = ast::create_node_literal_symbol(m, compiler::symbol::number_to_symbol(m->symbol_table, depth, "sp"));
          ast::Declaration_Variable_Node* env_arg        = ast::create_variable_declaration(m, env_arg_symbol, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));

          closure->push_argument(m, env_arg);

          lib::insert(data->closure_stackframe_argument, closure, env_arg);
        }

        return;
      }
    }

    build_closures_environments(data, m, assignment->get_left_operand(m), ctx, globals, parent, function);
    build_closures_environments(data, m, assignment->get_right_operand(m), ctx, globals, parent, function);
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

    build_closures_environments(data, m, fun->get_body(m), fun_ctx, fun_globals_ctx, parent, function);

    context::context_destroy(fun_ctx);
    context::context_destroy(fun_globals_ctx);

    return;
  }

  if (ast::If_Node_Statement* if_stmt = ast::is_instance< ast::If_Node_Statement* >(root)) {
    context::context_push_scope(ctx);

    build_closures_environments(data, m, if_stmt->get_body(m), ctx, globals, parent, function);

    context::context_pop_scope(ctx);

    return;
  }

  if (ast::Elif_List_Node* elif = ast::is_instance< ast::Elif_List_Node* >(root)) {
    build_closures_environments(data, m, elif->get_if(m), ctx, globals, parent, function);
    build_closures_environments(data, m, elif->get_elif(m), ctx, globals, parent, function);
    return;
  }

  if (ast::ProgramPoint_List_Node* pp = ast::is_instance< ast::ProgramPoint_List_Node* >(root)) {
    build_closures_environments(data, m, pp->get_statement(m), ctx, globals, pp, function);
    build_closures_environments(data, m, pp->get_next_program_point(m), ctx, globals, pp->get_next_program_point(m), function);
    return;
  }

  if (ast::Return_Node_Statement* ret = ast::is_instance< ast::Return_Node_Statement* >(root)) {
    build_closures_environments(data, m, ret->get_expression(m), ctx, globals, parent, function);
    return;
  }

  if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(root)) {
    closure_call_add_local_env_to_call(data, parent, m, ctx, globals, function);
    return;
  }
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
        if (cps::is_continuation_closure(stackframe::stack_frame_data_get_cps_data(data->sf_data), m, var->get_symbol(m))) {
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

void build_env_dependency_graph_rec(CPS_Closure_Data* data, ast::Manager* m, ast::Node* root, compiler::symbol::Id from_env) {
  if (!ast::is_semantic_node(root)) {
    return;
  }

  if (ast::Function_Literal_Node* f = ast::is_instance< ast::Function_Literal_Node* >(root)) {
    compiler::symbol::Id* id = lib::search(data->closure_environment, f);

    if (id) {
      build_env_dependency_graph_rec(data, m, f->get_body(m), *id);
    }
  }

  if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(root)) {
    if (ast::Literal_Symbol_Node* symbol = ast::is_instance< ast::Literal_Symbol_Node* >(call->get_function(m))) {
      if (cps::is_continuation_closure(stackframe::stack_frame_data_get_cps_data(data->sf_data), m, symbol)) {
        if (from_env == compiler::symbol::empty(m->symbol_table).id) {
          return;
        }

        Environment_Dependency_Node* from_graph = NULL;
        Environment_Dependency_Node* to_graph   = NULL;

        if (Environment_Dependency_Node** node = lib::search(data->env_dependency_graph, from_env)) {
          from_graph = *node;
        } else {
          from_graph = create_dependency_node(from_env);
          lib::insert(data->env_dependency_graph, from_env, from_graph);
        }

        assert(from_graph);

        ast::Function_Literal_Node** closure_ref = lib::search(data->symbol_to_closure, symbol->get_symbol_id());

        assert(closure_ref);

        ast::Function_Literal_Node* closure = *closure_ref;

        compiler::symbol::Id* to_env_ref = lib::search(data->closure_environment, closure);

        assert(to_env_ref);

        compiler::symbol::Id to_env = *to_env_ref;

        lib::insert(from_graph->to, to_env);

        if (Environment_Dependency_Node** node = lib::search(data->env_dependency_graph, to_env)) {
          to_graph = *node;
        } else {
          to_graph = create_dependency_node(to_env);
          lib::insert(data->env_dependency_graph, to_env, to_graph);
        }

        lib::insert(to_graph->from, from_env);

        return;
      }
    }
  }

  build_env_dependency_graph_rec(data, m, ast::left_of(m, root), from_env);
  build_env_dependency_graph_rec(data, m, ast::right_of(m, root), from_env);
}

void convert_cps_closures(ast::Manager* m, ast::Node* root, CPS_Closure_Data* data) {
  context::Context* ctx     = context::context_create(NULL);
  context::Context* globals = context::context_create(NULL);

  build_closures_environments(data, m, root, ctx, globals, NULL, NULL);

  context::context_destroy(ctx);
  context::context_destroy(globals);

  add_closures_to_global_context(data, m, root);
}

} // namespace closures
