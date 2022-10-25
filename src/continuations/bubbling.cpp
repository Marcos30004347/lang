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
#include "continuations/continuations.hpp"
#include "continuations/handler.hpp"
#include "lib/set.hpp"
#include "lib/table.hpp"
#include "parser/parser.hpp"
#include "stackframe/stackframe.hpp"

#include <assert.h>
#include <stdio.h>

namespace bubbling {

struct Bubbling_Data {
  stackframe::Stack_Frame_Data* stack_frame_data;

  lib::Set< ast::Function_Literal_Node* >*                                   continuations_handlers;
  lib::Table< compiler::symbol::Id, ast::Function_Literal_Node* >*           symbol_to_literal;
  lib::Table< ast::Function_Literal_Node*, ast::Variable_Assignment_Node* >* continuation_handler;
};

Bubbling_Data* bubbling_data_create(stackframe::Stack_Frame_Data* stack_frame_data) {
  Bubbling_Data* data = new Bubbling_Data();

  data->stack_frame_data       = stack_frame_data;
  data->continuation_handler   = lib::table_create< ast::Function_Literal_Node*, ast::Variable_Assignment_Node* >();
  data->symbol_to_literal      = lib::table_create< compiler::symbol::Id, ast::Function_Literal_Node* >();
  data->continuations_handlers = lib::set_create< ast::Function_Literal_Node* >();

  return data;
}

void bubbling_data_delete(Bubbling_Data* data) {
  stackframe::destroy_stack_frame_data(data->stack_frame_data);

  lib::table_delete(data->continuation_handler);
  lib::table_delete(data->symbol_to_literal);
  lib::set_delete(data->continuations_handlers);

  delete data;
}

ast::Variable_Assignment_Node* get_handler_assignment(Bubbling_Data* data, ast::Function_Literal_Node* lit) {

  ast::Variable_Assignment_Node** d = lib::search(data->continuation_handler, lit);

  if (d) {
    return *d;
  }

  return NULL;
}

b8 is_internal_function(ast::Manager* m, ast::Literal_Symbol_Node* symbol) {
  compiler::symbol::Symbol s = symbol->get_symbol(m);

  if (compiler::symbol::is_equal(m->symbol_table, &s, "sizeof")) {
    return true;
  }
  if (compiler::symbol::is_equal(m->symbol_table, &s, "escape_frame")) {
    return true;
  }
  if (compiler::symbol::is_equal(m->symbol_table, &s, "bubble")) {
    return true;
  }
  if (compiler::symbol::is_equal(m->symbol_table, &s, "push_frame")) {
    return true;
  }
  if (compiler::symbol::is_equal(m->symbol_table, &s, "is_yielding")) {
    return true;
  }
  if (compiler::symbol::is_equal(m->symbol_table, &s, "is_ctx_yielding")) {
    return true;
  }
  if (compiler::symbol::is_equal(m->symbol_table, &s, "is_yielding_to_handler")) {
    return true;
  }
  return false;
}

ast::Variable_Assignment_Node* create_handler_function(Bubbling_Data* data, ast::Manager* m, ast::Literal_Symbol_Node* func, ast::Function_Literal_Node* lit) {

  ast::Literal_Symbol_Node* in_symbol  = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "in"));
  ast::Literal_Symbol_Node* out_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "out"));
  ast::Literal_Symbol_Node* sp_symbol  = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "sp"));
  ast::Literal_Symbol_Node* ctx_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "ctx"));

  ast::Declaration_Variable_Node* in_arg  = ast::create_variable_declaration(m, in_symbol, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
  ast::Declaration_Variable_Node* out_arg = ast::create_variable_declaration(m, out_symbol, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
  ast::Declaration_Variable_Node* sp_arg  = ast::create_variable_declaration(m, sp_symbol, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
  ast::Declaration_Variable_Node* ctx_arg = ast::create_variable_declaration(m, ctx_symbol, ast::create_node_type_pointer(m, ast::create_node_type_evidence_context(m)));

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

  ast::Node* handler_args_type = ast::create_node_type_pointer(m, ast::create_node_type_any(m));
  handler_args_type            = ast::create_node_arithmetic_mul(m, handler_args_type, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
  handler_args_type            = ast::create_node_arithmetic_mul(m, handler_args_type, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
  handler_args_type            = ast::create_node_arithmetic_mul(m, handler_args_type, ast::create_node_type_pointer(m, ast::create_node_type_evidence_context(m)));
  //    ast::create_node_arithmetic_mul(m, ast::create_node_type_pointer(m, ast::create_node_type_any(m)), ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
  // handler_args_type = ast::create_node_arithmetic_mul(m, handler_args_type, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
  // handler_args_type = ast::create_node_arithmetic_mul(m, handler_args_type, ast::create_node_type_pointer(m, ast::deep_copy(m, context_symbol)));

  ast::Type_Arrow_Node* handler_type = ast::create_node_type_arrow(m, handler_args_type, ast::create_node_type_unit(m));

  ast::Declaration_Constant_Node* handler_declaration = ast::create_constant_declaration(m, handler_declaration_symbol, handler_type);
  // TODO(marcos): define tuple type for the arguments
  ast::Variable_Assignment_Node* handler_assignment = ast::create_node_assignment(m, handler_declaration, handler);

  lib::insert(data->continuation_handler, lit, handler_assignment);
  lib::insert(data->continuations_handlers, handler);

  return handler_assignment;
}

ast::ProgramPoint_List_Node* create_bubble_pp(ast::Manager* m, ast::Node* return_size, ast::Node* is_prompt, ast::Node* own_frame, ast::Node* frame, ast::Node* handler) {

  // ast::Literal_Symbol_Node* closure_symbol      = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "k"));
  // ast::Literal_Symbol_Node* closure_type_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "continuation_t"));
  // ast::Type_Pointer_Node*   closure_type        = ast::create_node_type_pointer(m, closure_type_symbol);

  // ast::Literal_Symbol_Node*    sizeof_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "sizeof"));
  // ast::Declarations_List_Node* sizeof_args   = ast::create_node_declarations_list(m, ast::deep_copy(m, closure_type_symbol), NULL);
  // ast::Function_Call_Node*     sizeof_call   = ast::create_node_function_call(m, sizeof_symbol, sizeof_args);

  // ast::Literal_Symbol_Node*       sizeof_var_symbol     = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "s"));
  // ast::Declaration_Constant_Node* sizeof_var_decl       = ast::create_constant_declaration(m, sizeof_var_symbol, ast::create_node_type_i32(m));
  // ast::Variable_Assignment_Node*  sizeof_var_assignment = ast::create_node_assignment(m, sizeof_var_decl, ast::deep_copy(m, return_size));

  // ast::Literal_Symbol_Node*    closure_alloc_func = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "malloc"));
  // ast::Declarations_List_Node* closure_alloc_args = ast::create_node_declarations_list(m, ast::deep_copy(m, sizeof_var_symbol), NULL);
  // ast::Function_Call_Node*     closure_alloc_call = ast::create_node_function_call(m, closure_alloc_func, closure_alloc_args);

  // TODO(marcos): create a type for closures
  // ast::Type_Any_Node*             closure_type   = ast::create_node_type_any(m);
  // ast::Declaration_Variable_Node* closure_decl            = ast::create_variable_declaration(m, closure_symbol, closure_type);
  // ast::Cast_Type_Node*            closure_alloc_call_cast = ast::create_node_cast_type(m, ast::deep_copy(m, closure_type), closure_alloc_call);
  // ast::Variable_Assignment_Node*  closure_assignment      = ast::create_node_assignment(m, closure_decl, closure_alloc_call_cast);

  ast::Literal_Symbol_Node* bubble_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "bubble"));

  ast::Declarations_List_Node* bubble_args = ast::create_node_declarations_list(m, handler, NULL);
  bubble_args                              = ast::create_node_declarations_list(m, frame, bubble_args);
  bubble_args                              = ast::create_node_declarations_list(m, own_frame, bubble_args);
  bubble_args                              = ast::create_node_declarations_list(m, is_prompt, bubble_args);
  bubble_args                              = ast::create_node_declarations_list(m, return_size, bubble_args);

  ast::Function_Call_Node* call = ast::create_node_function_call(m, bubble_symbol, bubble_args);

  ast::ProgramPoint_List_Node* pp = ast::create_node_program_point(m, call, NULL);

  // pp = ast::create_node_program_point(m, closure_assignment, pp);
  // pp = ast::create_node_program_point(m, sizeof_var_assignment, pp);

  return pp;
}

void insert_is_yielding_check(Bubbling_Data* data, ast::Manager* m, ast::Node* func, ast::ProgramPoint_List_Node* pp, ast::ProgramPoint_List_Node* parent, context::Context* ctx) {
  ast::Node*                     type       = 0;
  ast::Variable_Assignment_Node* stackframe = 0;

  ast::Variable_Assignment_Node* stackframe_assignment = NULL;
  // ast::Declaration_Variable_Node* stack_frame_argument  = NULL;

  assert(func);

  if (ast::Function_Literal_Node* lit = ast::is_instance< ast::Function_Literal_Node* >(func)) {
    type = lit->get_return_type(m);

    stackframe_assignment = stackframe::stack_frame_get_function_local_stack_frame_allocation(data->stack_frame_data, lit);

    // stack_frame_argument  = closures::cps_closure_data_get_stackframe_argument(data->stack_frame_data, lit);
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
          ast::Function_Literal_Node** fun = lib::search(data->symbol_to_literal, sym->get_symbol_id());

          if (fun) {
            ast::Function_Literal_Node* lit = *fun;

            ast::Variable_Assignment_Node* handler_assignment = get_handler_assignment(data, lit);

            ast::Declaration_Constant_Node* handler_decl = ast::as< ast::Declaration_Constant_Node* >(handler_assignment->get_left_operand(m));

            b8 owner = false;

            ast::Node* stack_frame_allocation = NULL;

            if (stackframe_assignment) {
              ast::Declaration_Variable_Node* sf_decl = ast::is_instance< ast::Declaration_Variable_Node* >(stackframe_assignment->get_left_operand(m));

              assert(sf_decl);

              ast::Literal_Symbol_Node*    alloc_func = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "escape_frame"));
              ast::Declarations_List_Node* alloc_args = ast::create_node_declarations_list(m, ast::deep_copy(m, sf_decl->get_symbol(m)), NULL);

              stack_frame_allocation = ast::create_node_function_call(m, alloc_func, alloc_args);

              owner = true;

            } else {
              stack_frame_allocation = ast::deep_copy(m, stackframe::build_sp_symbol(m, stackframe::stack_frame_get_function_depth(data->stack_frame_data, lit)));
            }

            ast::Literal_Symbol_Node*    sizeof_symbol      = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "tsize"));
            ast::Literal_Symbol_Node*    sizeof_type_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "sizeof"));
            ast::Declarations_List_Node* sizeof_type_args   = ast::create_node_declarations_list(m, ast::deep_copy(m, lit->get_return_type(m)), NULL);
            ast::Function_Call_Node*     sizeof_type_call   = ast::create_node_function_call(m, sizeof_type_symbol, sizeof_type_args);

            ast::Declaration_Constant_Node* sizeof_type_decl   = ast::create_constant_declaration(m, sizeof_symbol, ast::create_node_type_i32(m));
            ast::Variable_Assignment_Node*  sizeof_type_assign = ast::create_node_assignment(m, sizeof_type_decl, sizeof_type_call);

            ast::Node* is_owner = NULL;

            if (owner) {
              is_owner = ast::create_node_literal_true(m);
            } else {
              is_owner = ast::create_node_literal_false(m);
            }

            ast::Literal_Symbol_Node*       closure_frame_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "frame"));
            ast::Declaration_Variable_Node* closure_frame_decl =
                ast::create_variable_declaration(m, closure_frame_symbol, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
            ast::Variable_Assignment_Node* closure_frame_assignment = ast::create_node_assignment(m, closure_frame_decl, stack_frame_allocation);

            // ast::Variable_Assignment_Node*  closure_assign = ast::create_node_assignment(m, closure_decl, closure_alloc_call);

            ast::ProgramPoint_List_Node* bubble_pp =
                create_bubble_pp(m, sizeof_symbol, ast::create_node_literal_false(m), is_owner, closure_frame_symbol, handler_decl->get_symbol(m));

            bubble_pp->concat(m, if_body);

            if_body = bubble_pp;

            if_body = ast::create_node_program_point(m, sizeof_type_assign, if_body);
            if_body = ast::create_node_program_point(m, closure_frame_assignment, if_body);

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

  ast::Function_Call_Node*        is_yielding            = ast::create_node_function_call(m, is_yielding_symbol, arguments);
  ast::Literal_Symbol_Node*       yield_symbol           = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "yielding"));
  ast::Declaration_Constant_Node* is_yielding_decl       = ast::create_constant_declaration(m, yield_symbol, ast::create_node_type_i32(m));
  ast::Variable_Assignment_Node*  is_yielding_assignment = ast::create_node_assignment(m, is_yielding_decl, is_yielding);

  pp = pp->insert(m, is_yielding_assignment);
  pp = pp->insert(m, ast::create_node_if_statement(m, ast::deep_copy(m, yield_symbol), if_body));
}

b8 insert_handlers(Bubbling_Data* data, ast::Manager* m, ast::Node* root, ast::ProgramPoint_List_Node* point) {
  if (!ast::is_semantic_node(root) || !ast::is_semantic_node(point)) {
    return false;
  }

  if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(root)) {
    if (ast::Function_Literal_Node* literal = ast::is_instance< ast::Function_Literal_Node* >(assignment->get_right_operand(m))) {
      ast::Node* left = assignment->get_left_operand(m);

      ast::Literal_Symbol_Node* symbol = NULL;

      if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
        symbol = var->get_symbol(m);
      }

      if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
        symbol = var->get_symbol(m);
      }

      assert(symbol);

      insert_handlers(data, m, literal->get_body(m), literal->get_body(m));

      if (cps::is_continuation_closure(stackframe::stack_frame_data_get_cps_data(data->stack_frame_data), m, symbol)) {
        ast::Variable_Assignment_Node* assignment = create_handler_function(data, m, symbol, literal);

        point->insert(m, assignment);

        return true;
      }

      return false;
    }
  }

  if (ast::ProgramPoint_List_Node* pp = ast::is_instance< ast::ProgramPoint_List_Node* >(root)) {
    if (insert_handlers(data, m, pp->get_statement(m), pp)) {
      pp = pp->get_next_program_point(m);
    }

    ast::ProgramPoint_List_Node* next = pp->get_next_program_point(m);

    insert_handlers(data, m, next, next);

    return false;
  }

  insert_handlers(data, m, ast::left_of(m, root), point);
  insert_handlers(data, m, ast::right_of(m, root), point);

  return false;
}

b8 is_handler_function(Bubbling_Data* data, ast::Function_Literal_Node* lit) {
  return lib::search(data->continuations_handlers, lit) != NULL;
}

void bubbling_yields_rec(
    Bubbling_Data* data, ast::Manager* m, ast::Node* root, ast::Node* func, ast::ProgramPoint_List_Node* pp, ast::ProgramPoint_List_Node* parent, context::Context* ctx) {
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

    if (is_handler_function(data, lit) == false) {
      context::Context* local_context = context::context_create(NULL);
      bubbling_yields_rec(data, m, lit->get_body(m), lit, pp, parent, local_context);
      context::context_destroy(local_context);
    }

    return;
  }

  if (ast::Effect_Declaration_Node* lit = ast::is_instance< ast::Effect_Declaration_Node* >(root)) {
    return bubbling_yields_rec(data, m, lit->get_return_type(m), lit, pp, parent, ctx);
  }

  if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(root)) {
    if (ast::Literal_Symbol_Node* symbol = ast::is_instance< ast::Literal_Symbol_Node* >(call->get_function(m))) {
      if (is_internal_function(m, symbol)) {
        return;
      }
    }

    insert_is_yielding_check(data, m, func, pp, parent, ctx);
    return;
  }

  if (ast::Effect_Call_Node* call = ast::is_instance< ast::Effect_Call_Node* >(root)) {
    insert_is_yielding_check(data, m, func, pp, parent, ctx);
    return;
  }

  if (ast::ProgramPoint_List_Node* pp = ast::is_instance< ast::ProgramPoint_List_Node* >(root)) {
    bubbling_yields_rec(data, m, pp->get_statement(m), func, pp, parent, ctx);

    ast::ProgramPoint_List_Node* next = pp->get_next_program_point(m);

    bubbling_yields_rec(data, m, next, func, next, pp, ctx);

    return;
  }

  bubbling_yields_rec(data, m, ast::left_of(m, root), func, pp, parent, ctx);
  bubbling_yields_rec(data, m, ast::right_of(m, root), func, pp, parent, ctx);
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

void build_functions_map(Bubbling_Data* data, ast::Manager* m, ast::Node* root) {
  if (!ast::is_semantic_node(root)) {
    return;
  }

  if (ast::Variable_Assignment_Node* var = ast::is_instance< ast::Variable_Assignment_Node* >(root)) {
    if (ast::Function_Literal_Node* lit = ast::is_instance< ast::Function_Literal_Node* >(var->get_right_operand(m))) {
      if (ast::Declaration_Variable_Node* decl = ast::is_instance< ast::Declaration_Variable_Node* >(var->get_left_operand(m))) {
        lib::insert(data->symbol_to_literal, decl->get_symbol(m)->get_symbol_id(), lit);
      }

      if (ast::Declaration_Constant_Node* decl = ast::is_instance< ast::Declaration_Constant_Node* >(var->get_left_operand(m))) {
        lib::insert(data->symbol_to_literal, decl->get_symbol(m)->get_symbol_id(), lit);
      }
    }
  }

  build_functions_map(data, m, ast::left_of(m, root));
  build_functions_map(data, m, ast::right_of(m, root));
}

struct Move_Global_Data {
  lib::Table< ast::Node*, compiler::symbol::Id >* symbol_map;
};

Move_Global_Data* move_global_data_create() {
  Move_Global_Data* m = new Move_Global_Data();
  m->symbol_map       = lib::table_create< ast::Node*, compiler::symbol::Id >();
  return m;
}

void move_global_data_destroy(Move_Global_Data* m) {
  lib::table_delete(m->symbol_map);
  delete m;
}

void rename_local_functions(ast::Manager* m, ast::Node* node, context::Context* ctx, compiler::symbol::Symbol prefix) {

  if (!ast::is_semantic_node(node)) {
    return;
  }

  if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(node)) {

    ast::Node* left = assignment->get_left_operand(m);

    ast::Literal_Symbol_Node* symbol = NULL;

    if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
      context::context_declare(ctx, m, var);

      symbol = var->get_symbol(m);
    }

    if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
      context::context_declare(ctx, m, var);

      symbol = var->get_symbol(m);
    }

    if (ast::Function_Literal_Node* literal = ast::is_instance< ast::Function_Literal_Node* >(assignment->get_right_operand(m))) {
      if (symbol) {
        compiler::symbol::Symbol s;

        if (prefix.id) {
          compiler::symbol::Symbol u = compiler::symbol::set_entry(m->symbol_table, "_");
          compiler::symbol::Symbol p = compiler::symbol::symbol_with_prefix(m->symbol_table, u, prefix);

          s = compiler::symbol::symbol_with_prefix(m->symbol_table, symbol->get_symbol(m), p);
        } else {
          s = symbol->get_symbol(m);
        }

        symbol->set_symbol_id(s.id);

        context::Context* closure_ctx = context::context_create(ctx);

        rename_local_functions(m, literal->get_arguments(m), closure_ctx, prefix);
        rename_local_functions(m, literal->get_return_type(m), closure_ctx, prefix);
        rename_local_functions(m, literal->get_body(m), closure_ctx, s);

        context::context_destroy(closure_ctx);

        return;
      }
    }

    rename_local_functions(m, ast::left_of(m, node), ctx, prefix);
    rename_local_functions(m, ast::right_of(m, node), ctx, prefix);

    return;
  }

  if (ast::Elif_List_Node* elif = ast::is_instance< ast::Elif_List_Node* >(node)) {
    while (ast::is_semantic_node(elif)) {
      ast::If_Node_Statement* if_stmt = elif->get_if(m);

      rename_local_functions(m, if_stmt->get_condition(m), ctx, prefix);

      context::context_push_scope(ctx);

      rename_local_functions(m, if_stmt->get_body(m), ctx, prefix);

      context::context_pop_scope(ctx);

      elif = elif->get_elif(m);
    }

    return;
  }

  if (ast::Literal_Symbol_Node* symbol = ast::is_instance< ast::Literal_Symbol_Node* >(node)) {
    ast::Node* decl = context::context_is_defined(ctx, symbol);

    if (decl) {
      if (ast::Literal_Symbol_Node* s = ast::is_instance< ast::Literal_Symbol_Node* >(ast::left_of(m, decl))) {
        symbol->set_symbol_id(s->get_symbol_id());
      }
    }

    return;
  }

  rename_local_functions(m, ast::left_of(m, node), ctx, prefix);
  rename_local_functions(m, ast::right_of(m, node), ctx, prefix);
}

b8 move_declarations_to_global_context(
    ast::Manager* m, ast::Node* node, ast::ProgramPoint_List_Node* root, ast::ProgramPoint_List_Node* point, ast::ProgramPoint_List_Node* parent, u64 depth = 0) {
  if (!ast::is_semantic_node(node)) {
    return false;
  }

  if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(node)) {
    assert(point->left == assignment->id);

    if (ast::Function_Literal_Node* lit = ast::is_instance< ast::Function_Literal_Node* >(assignment->get_right_operand(m))) {

      move_declarations_to_global_context(m, lit->get_body(m), root, lit->get_body(m), point, depth + 1);

      if (depth > 0) {

        ast::ProgramPoint_List_Node* cont0 = parent->split(m);

        ast::ProgramPoint_List_Node* cont1 = point->split(m);

        if (ast::is_semantic_node(cont1)) {
          parent->emplace(m, cont1);

          move_declarations_to_global_context(m, cont1, root, cont1, parent, depth);
        }

        if (ast::is_semantic_node(point)) {
          root->insert(m, assignment);
        }

        return true;
      }
    }

    return false;
  }

  if (ast::ProgramPoint_List_Node* pp = ast::is_instance< ast::ProgramPoint_List_Node* >(node)) {
    if (move_declarations_to_global_context(m, pp->get_statement(m), root, pp, parent, depth)) {
      return true;
    }

    ast::ProgramPoint_List_Node* next = pp->get_next_program_point(m);

    move_declarations_to_global_context(m, next, root, next, pp, depth);

    return false;
  }

  if (ast::If_Node_Statement* if_stmt = ast::is_instance< ast::If_Node_Statement* >(node)) {
    move_declarations_to_global_context(m, if_stmt->get_body(m), root, if_stmt->get_body(m), parent, depth);
    return false;
  }

  if (ast::Elif_List_Node* elif = ast::is_instance< ast::Elif_List_Node* >(node)) {
    move_declarations_to_global_context(m, elif->get_if(m), root, point, parent, depth);
    move_declarations_to_global_context(m, elif->get_elif(m), root, point, parent, depth);
    return false;
  }

  return false;
}

void add_bubbling_yields(Bubbling_Data* data, ast::Manager* m, ast::Node* root) {
  build_functions_map(data, m, root);

  ast::ProgramPoint_List_Node* pp = ast::is_instance< ast::ProgramPoint_List_Node* >(root);

  insert_handlers(data, m, root, pp);

  context::Context* ctx0 = context::context_create(NULL);

  bubbling_yields_rec(data, m, root, NULL, pp, pp, ctx0);

  context::context_destroy(ctx0);

  context::Context* ctx1 = context::context_create(NULL);

  rename_local_functions(m, root, ctx1, compiler::symbol::empty(m->symbol_table));

  context::context_destroy(ctx1);
  // parser::print_ast_ir(m, root);
  // printf("--------");
  move_declarations_to_global_context(m, root, pp, pp, pp);
}

} // namespace bubbling
