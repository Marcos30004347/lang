#include "bubbling.hpp"
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
#include "continuations/closures.hpp"
#include "lib/table.hpp"
#include "parser/parser.hpp"
#include "stackframe/stackframe.hpp"

#include <assert.h>
#include <stdio.h>

namespace bubbling {

struct Bubbling_Data {
  closures::CPS_Closure_Data* cps_closures_data;

  lib::Table< ast::Function_Literal_Node*, ast::Variable_Assignment_Node* >* continuation_handler;
};

Bubbling_Data* bubbling_data_create(closures::CPS_Closure_Data* cps_closures_data) {
  Bubbling_Data* data = new Bubbling_Data();

  data->cps_closures_data    = cps_closures_data;
  data->continuation_handler = lib::table_create< ast::Function_Literal_Node*, ast::Variable_Assignment_Node* >();

  return data;
}

void bubbling_data_delete(Bubbling_Data* data) {
  closures::cps_closure_data_destroy(data->cps_closures_data);

  lib::table_delete(data->continuation_handler);

  delete data;
}

ast::Declaration_Constant_Node* create_handler_function(Bubbling_Data* data, ast::Manager* m, ast::Literal_Symbol_Node* func, ast::Function_Literal_Node* lit) {
  ast::Variable_Assignment_Node** d = lib::search(data->continuation_handler, lit);

  if (d) {
    return ast::as< ast::Declaration_Constant_Node* >((*d)->get_left_operand(m));
  }

  ast::Literal_Symbol_Node* in_symbol      = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "in"));
  ast::Literal_Symbol_Node* out_symbol     = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "out"));
  ast::Literal_Symbol_Node* sp_symbol      = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "sp"));
  ast::Literal_Symbol_Node* ctx_symbol     = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "ctx"));
  ast::Literal_Symbol_Node* context_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "context"));

  ast::Declaration_Variable_Node* in_arg  = ast::create_variable_declaration(m, in_symbol, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
  ast::Declaration_Variable_Node* out_arg = ast::create_variable_declaration(m, out_symbol, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
  ast::Declaration_Variable_Node* sp_arg  = ast::create_variable_declaration(m, sp_symbol, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
  ast::Declaration_Variable_Node* ctx_arg = ast::create_variable_declaration(m, ctx_symbol, ast::create_node_type_pointer(m, context_symbol));

  ast::Declaration_Variable_Node* first_arg = ast::as< ast::Declaration_Variable_Node* >(lit->get_arguments(m)->get_declaration(m));

  assert(first_arg);

  ast::Cast_Type_Node* cast_out = ast::create_node_cast_type(m, lit->get_return_type(m), ast::deep_copy(m, out_symbol));
  ast::Cast_Type_Node* cast_in  = ast::create_node_cast_type(m, first_arg->get_type(m), ast::deep_copy(m, in_symbol));

  ast::Declarations_List_Node* args = ast::create_node_declarations_list(m, ast::deep_copy(m, ctx_symbol), NULL);
  args                              = ast::create_node_declarations_list(m, ast::deep_copy(m, sp_symbol), args);
  args                              = ast::create_node_declarations_list(m, cast_in, args);

  ast::Function_Call_Node* call = ast::create_node_function_call(m, ast::deep_copy(m, func), args);

  ast::Variable_Assignment_Node* assignment = ast::create_node_assignment(m, ast::create_node_pointer_value(m, cast_out), call);

  ast::ProgramPoint_List_Node* body = ast::create_node_program_point(m, assignment, NULL);

  ast::Declarations_List_Node* arguments = ast::create_node_declarations_list(m, ctx_arg, NULL);
  arguments                              = ast::create_node_declarations_list(m, sp_arg, arguments);
  arguments                              = ast::create_node_declarations_list(m, out_arg, arguments);
  arguments                              = ast::create_node_declarations_list(m, in_arg, arguments);

  ast::Function_Literal_Node* handler = ast::create_node_function_literal(m, arguments, ast::create_node_type_unit(m), body);

  ast::Literal_Symbol_Node* handler_declaration_symbol = ast::create_node_literal_symbol(m, compiler::symbol::symbol_with_prefix(m->symbol_table, func->get_symbol(m), "handler_"));

  // TODO(marcos): if in the future we got a tuple type, use it instead of this
  ast::Node* handler_args_type = ast::create_node_type_pointer(m, ast::create_node_type_any(m));
  //    ast::create_node_arithmetic_mul(m, ast::create_node_type_pointer(m, ast::create_node_type_any(m)), ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
  // handler_args_type = ast::create_node_arithmetic_mul(m, handler_args_type, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
  // handler_args_type = ast::create_node_arithmetic_mul(m, handler_args_type, ast::create_node_type_pointer(m, ast::deep_copy(m, context_symbol)));

  ast::Type_Arrow_Node* handler_type = ast::create_node_type_arrow(m, handler_args_type, ast::create_node_type_unit(m));

  ast::Declaration_Constant_Node* handler_declaration = ast::create_constant_declaration(m, handler_declaration_symbol, handler_type);
  // TODO(marcos): define tuple type for the arguments
  ast::Variable_Assignment_Node* handler_assignment = ast::create_node_assignment(m, handler_declaration, handler);

  lib::insert(data->continuation_handler, lit, handler_assignment);

  return handler_declaration;
}

void insert_is_yielding_check(Bubbling_Data* data, ast::Manager* m, ast::Node* func, ast::ProgramPoint_List_Node* pp, context::Context* ctx) {
  ast::Node*                     type       = 0;
  ast::Variable_Assignment_Node* stackframe = 0;

  ast::Variable_Assignment_Node*  stackframe_assignment = NULL;
  ast::Declaration_Variable_Node* stack_frame_argument  = NULL;

  assert(func);

  if (ast::Function_Literal_Node* lit = ast::is_instance< ast::Function_Literal_Node* >(func)) {
    type = lit->get_return_type(m);

    stackframe_assignment = stackframe::stack_frame_get_function_local_stack_frame_allocation(closures::cps_closure_data_get_stackframe_data(data->cps_closures_data), lit);
    stack_frame_argument  = closures::cps_closure_data_get_stackframe_argument(data->cps_closures_data, lit);
  }

  if (ast::Effect_Declaration_Node* lit = ast::is_instance< ast::Effect_Declaration_Node* >(func)) {
    // type = lit->get_return_type(m);
    assert(false && "Not implemented yet");
  }

  assert(type);

  ast::Literal_Symbol_Node*    default_value_symbol    = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "unitialized"));
  ast::Declarations_List_Node* default_value_arguments = ast::create_node_declarations_list(m, type, NULL);
  ast::Function_Call_Node*     default_value           = ast::create_node_function_call(m, default_value_symbol, default_value_arguments);

  ast::Return_Node_Statement* return_devault_value = ast::create_node_return_statement(m, default_value);

  ast::ProgramPoint_List_Node* if_body = ast::create_node_program_point(m, return_devault_value, NULL);

  ast::ProgramPoint_List_Node* point = pp;

  while (ast::is_semantic_node(point)) {
    if (ast::Return_Node_Statement* ret = ast::is_instance< ast::Return_Node_Statement* >(point->get_statement(m))) {
      if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(ret->get_expression(m))) {
        if (ast::Literal_Symbol_Node* sym = ast::is_instance< ast::Literal_Symbol_Node* >(call->get_function(m))) {
          if (ast::Function_Literal_Node* lit = closures::cps_closure_data_get_closure_handler(data->cps_closures_data, m, sym)) {
            ast::Declaration_Constant_Node* handler_decl = create_handler_function(data, m, sym, lit);

            b8 owner = false;

            ast::Node* stack_frame_allocation = NULL;
            if (stackframe_assignment) {
              ast::Declaration_Variable_Node* sf_decl = ast::is_instance< ast::Declaration_Variable_Node* >(stackframe_assignment->get_left_operand(m));

              assert(sf_decl);

              ast::Literal_Symbol_Node*    alloc_func = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "memcpy"));
              ast::Declarations_List_Node* alloc_args = ast::create_node_declarations_list(m, ast::deep_copy(m, sf_decl->get_symbol(m)), NULL);

              stack_frame_allocation = ast::create_node_function_call(m, alloc_func, alloc_args);

              owner = true;

            } else {
              assert(stack_frame_argument);

              stack_frame_allocation = ast::deep_copy(m, stack_frame_argument->get_symbol(m));
            }

            ast::Literal_Symbol_Node* closure_symbol      = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "k"));
            ast::Literal_Symbol_Node* closure_type_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "continuation_t"));
            ast::Type_Pointer_Node*   closure_type        = ast::create_node_type_pointer(m, closure_type_symbol);

            ast::Literal_Symbol_Node*    sizeof_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "sizeof"));
            ast::Declarations_List_Node* sizeof_args   = ast::create_node_declarations_list(m, ast::deep_copy(m, closure_type_symbol), NULL);
            ast::Function_Call_Node*     sizeof_call   = ast::create_node_function_call(m, sizeof_symbol, sizeof_args);

            ast::Literal_Symbol_Node*    sizeof_type_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "sizeof"));
            ast::Declarations_List_Node* sizeof_type_args   = ast::create_node_declarations_list(m, ast::deep_copy(m, lit->get_return_type(m)), NULL);
            ast::Function_Call_Node*     sizeof_type_call   = ast::create_node_function_call(m, sizeof_type_symbol, sizeof_type_args);

            ast::Literal_Symbol_Node*       sizeof_var_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "s"));
            ast::Declaration_Constant_Node* sizeof_var_decl   = ast::create_constant_declaration(m, sizeof_var_symbol, ast::create_node_type_i32(m));
            ast::Variable_Assignment_Node*  sizeof_var_assignment =
                ast::create_node_assignment(m, sizeof_var_decl, ast::create_node_arithmetic_add(m, sizeof_call, sizeof_type_call));

            ast::Literal_Symbol_Node*    closure_alloc_func = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "malloc"));
            ast::Declarations_List_Node* closure_alloc_args = ast::create_node_declarations_list(m, ast::deep_copy(m, sizeof_var_symbol), NULL);
            ast::Function_Call_Node*     closure_alloc_call = ast::create_node_function_call(m, closure_alloc_func, closure_alloc_args);

            // TODO(marcos): create a type for closures
            // ast::Type_Any_Node*             closure_type   = ast::create_node_type_any(m);
            ast::Declaration_Variable_Node* closure_decl       = ast::create_variable_declaration(m, closure_symbol, closure_type);
            ast::Variable_Assignment_Node*  closure_assignment = ast::create_node_assignment(m, closure_decl, closure_alloc_call);

            ast::Member_Access_Node* handler_access =
                ast::create_node_member_access(m, ast::deep_copy(m, closure_symbol), ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "handler")));
            ast::Member_Access_Node* owner_access =
                ast::create_node_member_access(m, ast::deep_copy(m, closure_symbol), ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "own_frame")));
            ast::Member_Access_Node* frame_access =
                ast::create_node_member_access(m, ast::deep_copy(m, closure_symbol), ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "frame")));
            ast::Member_Access_Node* prompt_access =
                ast::create_node_member_access(m, ast::deep_copy(m, closure_symbol), ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "is_prompt")));

            ast::Node* is_owner = NULL;

            if (owner) {
              is_owner = ast::create_node_literal_true(m);
            } else {
              is_owner = ast::create_node_literal_false(m);
            }

            ast::Variable_Assignment_Node* closure_handler_assignment   = ast::create_node_assignment(m, handler_access, ast::deep_copy(m, handler_decl->get_symbol(m)));
            ast::Variable_Assignment_Node* closure_own_frame_assignment = ast::create_node_assignment(m, owner_access, is_owner);
            ast::Variable_Assignment_Node* closure_frame_assignment     = ast::create_node_assignment(m, frame_access, stack_frame_allocation);
            ast::Variable_Assignment_Node* closure_is_prompt_assignment = ast::create_node_assignment(m, prompt_access, ast::create_node_literal_false(m));

            // ast::Variable_Assignment_Node*  closure_assign = ast::create_node_assignment(m, closure_decl, closure_alloc_call);

            ast::Literal_Symbol_Node* context_symbol        = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "ctx"));
            ast::Literal_Symbol_Node* register_closure_func = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "compose"));

            ast::Declarations_List_Node* register_closure_args = ast::create_node_declarations_list(m, closure_symbol, NULL);
            register_closure_args                              = ast::create_node_declarations_list(m, context_symbol, register_closure_args);

            ast::Function_Call_Node* register_closure_call = ast::create_node_function_call(m, register_closure_func, register_closure_args);

            if_body = ast::create_node_program_point(m, register_closure_call, if_body);
            if_body = ast::create_node_program_point(m, closure_handler_assignment, if_body);
            if_body = ast::create_node_program_point(m, closure_frame_assignment, if_body);
            if_body = ast::create_node_program_point(m, closure_own_frame_assignment, if_body);
            if_body = ast::create_node_program_point(m, closure_is_prompt_assignment, if_body);
            if_body = ast::create_node_program_point(m, closure_assignment, if_body);
            if_body = ast::create_node_program_point(m, sizeof_var_assignment, if_body);

            break;
          }
        }
      }
    }

    point = point->get_next_program_point(m);
  }

  assert(if_body);

  ast::Literal_Symbol_Node*    ctx_symbol         = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "ctx"));
  ast::Literal_Symbol_Node*    is_yielding_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "is_yielding"));
  ast::Declarations_List_Node* arguments          = ast::create_node_declarations_list(m, ctx_symbol, NULL);

  ast::Function_Call_Node* is_yielding = ast::create_node_function_call(m, is_yielding_symbol, arguments);

  pp->insert(m, ast::create_node_if_statement(m, is_yielding, if_body));
}

void bubbling_yields_rec(Bubbling_Data* data, ast::Manager* m, ast::Node* root, ast::Node* func, ast::ProgramPoint_List_Node* pp, context::Context* ctx) {
  if (!ast::is_semantic_node(root) || !ast::is_semantic_node(pp)) {
    return;
  }

  if (ast::is_instance< ast::Return_Node_Statement* >(root)) {
    return;
  }

  if (ast::If_Node_Statement* if_stmt = ast::is_instance< ast::If_Node_Statement* >(root)) {
    if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(if_stmt->get_condition(m))) {
      if (ast::Literal_Symbol_Node* sym = ast::is_instance< ast::Literal_Symbol_Node* >(call->get_function(m))) {
        compiler::symbol::Symbol id = sym->get_symbol(m);

        if (compiler::symbol::is_equal(m->symbol_table, &id, "is_yielding")) {
          return;
        }
      }
    }
  }

  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(root)) {
    context::context_declare(ctx, m, var);
  }

  if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(root)) {
    context::context_declare(ctx, m, var);
  }

  if (ast::Function_Literal_Node* lit = ast::is_instance< ast::Function_Literal_Node* >(root)) {
    context::Context* local_context = context::context_create(NULL); // context::context_from_declarations_list(m, lit->get_arguments(m), NULL);

    bubbling_yields_rec(data, m, lit->get_body(m), lit, pp, local_context);

    context::context_destroy(local_context);

    return;
  }

  if (ast::Effect_Declaration_Node* lit = ast::is_instance< ast::Effect_Declaration_Node* >(root)) {
    return bubbling_yields_rec(data, m, lit->get_return_type(m), lit, pp, ctx);
  }

  if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(root)) {
    insert_is_yielding_check(data, m, func, pp, ctx);
    return;
  }

  if (ast::Effect_Call_Node* call = ast::is_instance< ast::Effect_Call_Node* >(root)) {
    insert_is_yielding_check(data, m, func, pp, ctx);
    return;
  }

  if (ast::ProgramPoint_List_Node* pp = ast::is_instance< ast::ProgramPoint_List_Node* >(root)) {
    bubbling_yields_rec(data, m, pp->get_statement(m), func, pp, ctx);
    bubbling_yields_rec(data, m, pp->get_next_program_point(m), func, pp, ctx);
    return;
  }

  bubbling_yields_rec(data, m, ast::left_of(m, root), func, pp, ctx);
  bubbling_yields_rec(data, m, ast::right_of(m, root), func, pp, ctx);
}

void add_context_argument(Bubbling_Data* data, ast::Manager* m, ast::Node* root) {
  if (!ast::is_semantic_node(root)) {
    return;
  }

  if (ast::Function_Literal_Node* fun = ast::is_instance< ast::Function_Literal_Node* >(root)) {
    ast::Literal_Symbol_Node* symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "ctx"));
    ast::Type_Pointer_Node*   type   = ast::create_node_type_pointer(m, ast::create_node_type_evidence_context(m));

    ast::Declaration_Variable_Node* argument = ast::create_variable_declaration(m, symbol, type);
    fun->push_argument(m, argument);
  }

  if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(root)) {
    ast::Literal_Symbol_Node* symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "ctx"));
    call->push_argument(m, symbol);
  }

  if (ast::Effect_Call_Node* call = ast::is_instance< ast::Effect_Call_Node* >(root)) {
    ast::Literal_Symbol_Node* symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "ctx"));
    call->push_argument(m, symbol);
  }

  add_context_argument(data, m, ast::left_of(m, root));
  add_context_argument(data, m, ast::right_of(m, root));
}

void add_handlers_to_global_context_rec(
    ast::Manager* m, ast::ProgramPoint_List_Node* root, Bubbling_Data* data, lib::TableNode< ast::Function_Literal_Node*, ast::Variable_Assignment_Node* >* node) {
  if (node == NULL) {
    return;
  }

  ast::Variable_Assignment_Node* assignment = node->val;

  root->insert(m, root->get_statement(m));

  root->set_statement(m, assignment);

  add_handlers_to_global_context_rec(m, root, data, node->left);
  add_handlers_to_global_context_rec(m, root, data, node->right);
}

void add_handlers_to_global_context(ast::Manager* m, ast::Node* root, Bubbling_Data* data) {
  add_handlers_to_global_context_rec(m, ast::is_instance< ast::ProgramPoint_List_Node* >(root), data, data->continuation_handler->root);
}

void add_bubbling_yields(Bubbling_Data* data, ast::Manager* m, ast::Node* root) {
  add_context_argument(data, m, root);

  context::Context* context = context::context_create(NULL);

  bubbling_yields_rec(data, m, root, NULL, ast::is_instance< ast::ProgramPoint_List_Node* >(root), context);

  context::context_destroy(context);

  add_handlers_to_global_context(m, root, data);
}

} // namespace bubbling
