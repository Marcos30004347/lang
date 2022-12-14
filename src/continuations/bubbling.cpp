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
#include "ast/utils.hpp"
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
#include <cstdio>
#include <stdio.h>

namespace bubbling {

struct Bubbling_Data {
  cps::CPS_Data* cps_data;
  u64 yieldings;
  lib::Set< ast::Function_Call_Node* >* bubblint_functions_blacklist;
  lib::Set< ast::Effect_Call_Node* >* bubblint_effects_blacklist;
  // lib::Set< ast::Function_Literal_Node* >* continuations_handlers;
  lib::Table< compiler::symbol::Id, ast::Function_Literal_Node* >* symbol_to_literal;
  lib::Table< ast::Function_Literal_Node*, compiler::symbol::Symbol >* literal_to_symbol;
  // lib::Table< ast::Function_Literal_Node*, ast::Variable_Assignment_Node* >* continuation_handler;
};

Bubbling_Data* bubbling_data_create(cps::CPS_Data* cps_data) {
  Bubbling_Data* data = new Bubbling_Data();

  data->yieldings = 0;
  data->bubblint_functions_blacklist = lib::set_create< ast::Function_Call_Node* >();
  data->bubblint_effects_blacklist = lib::set_create< ast::Effect_Call_Node* >();
  data->cps_data = cps_data;
  // data->continuation_handler = lib::table_create< ast::Function_Literal_Node*, ast::Variable_Assignment_Node* >();
  data->symbol_to_literal = lib::table_create< compiler::symbol::Id, ast::Function_Literal_Node* >();
  data->literal_to_symbol = lib::table_create< ast::Function_Literal_Node*, compiler::symbol::Symbol >();
  // data->continuations_handlers = lib::set_create< ast::Function_Literal_Node* >();

  return data;
}

void bubbling_data_delete(Bubbling_Data* data) {
  cps::cps_data_destroy(data->cps_data);

  // lib::table_delete(data->continuation_handler);
  lib::table_delete(data->symbol_to_literal);
  lib::table_delete(data->literal_to_symbol);
  // lib::set_delete(data->continuations_handlers);
  lib::set_delete(data->bubblint_functions_blacklist);
  lib::set_delete(data->bubblint_effects_blacklist);
  delete data;
}

// ast::Variable_Assignment_Node* get_handler_assignment(Bubbling_Data* data, ast::Function_Literal_Node* lit) {

//   ast::Variable_Assignment_Node** d = lib::search(data->continuation_handler, lit);

//   if (d) {
//     return *d;
//   }

//   return NULL;
// }

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
  if (compiler::symbol::is_equal(m->symbol_table, &s, "pop_frame")) {
    return true;
  }
  if (compiler::symbol::is_equal(m->symbol_table, &s, "set_is_yielding_to")) {
    return true;
  }
  if (compiler::symbol::is_equal(m->symbol_table, &s, "ctx_is_returning")) {
    return true;
  }
  if (compiler::symbol::is_equal(m->symbol_table, &s, "ctx_allocate_args")) {
    return true;
  }
  if (compiler::symbol::is_equal(m->symbol_table, &s, "ctx_set_returning")) {
    return true;
  }
  if (compiler::symbol::is_equal(m->symbol_table, &s, "ctx_get_handler_args")) {
    return true;
  }
  if (compiler::symbol::is_equal(m->symbol_table, &s, "ctx_is_yielding_to")) {
    return true;
  }

  return false;
}

// ast::Variable_Assignment_Node* create_handler_function(Bubbling_Data* data, ast::Manager* m, ast::Literal_Symbol_Node* func, ast::Function_Literal_Node* lit) {

//   ast::Literal_Symbol_Node* in_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "in"));
//   ast::Literal_Symbol_Node* out_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "out"));
//   ast::Literal_Symbol_Node* sp_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "sp"));
//   ast::Literal_Symbol_Node* ctx_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "ctx"));

//   ast::Declaration_Variable_Node* in_arg = ast::create_variable_declaration(m, in_symbol, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
//   ast::Declaration_Variable_Node* out_arg = ast::create_variable_declaration(m, out_symbol, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
//   ast::Declaration_Variable_Node* sp_arg = ast::create_variable_declaration(m, sp_symbol, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
//   ast::Declaration_Variable_Node* ctx_arg = ast::create_variable_declaration(m, ctx_symbol, ast::create_node_type_pointer(m, ast::create_node_type_evidence_context(m)));

//   ast::Declaration_Variable_Node* first_arg = ast::as< ast::Declaration_Variable_Node* >(lit->get_arguments(m)->get_declaration(m));

//   assert(first_arg);

//   ast::Cast_Type_Node* cast_out = ast::create_node_cast_type(m, ast::create_node_type_pointer(m, ast::deep_copy(m, lit->get_return_type(m))), ast::deep_copy(m, out_symbol));
//   ast::Cast_Type_Node* cast_in = ast::create_node_cast_type(m, ast::deep_copy(m, first_arg->get_type(m)), ast::deep_copy(m, in_symbol));

//   ast::Declarations_List_Node* args = ast::create_node_declarations_list(m, ast::deep_copy(m, sp_symbol), NULL);
//   args = ast::create_node_declarations_list(m, ast::deep_copy(m, ctx_symbol), args);
//   args = ast::create_node_declarations_list(m, cast_in, args);

//   ast::Function_Call_Node* call = ast::create_node_function_call(m, ast::deep_copy(m, func), args);

//   ast::Variable_Assignment_Node* assignment = ast::create_node_assignment(m, ast::create_node_pointer_value(m, cast_out), call);

//   ast::ProgramPoint* body = ast::create_node_program_point(m, assignment, NULL);

//   ast::Declarations_List_Node* arguments = ast::create_node_declarations_list(m, sp_arg, NULL);
//   arguments = ast::create_node_declarations_list(m, ctx_arg, arguments);
//   arguments = ast::create_node_declarations_list(m, out_arg, arguments);
//   arguments = ast::create_node_declarations_list(m, in_arg, arguments);

//   ast::Function_Literal_Node* handler = ast::create_node_function_literal(m, arguments, ast::create_node_type_unit(m), body);

//   ast::Literal_Symbol_Node* handler_declaration_symbol = ast::create_node_literal_symbol(m, compiler::symbol::symbol_with_prefix(m->symbol_table, func->get_symbol(m),
//   "handler_"));

//   ast::Node* handler_args_type = ast::create_node_type_pointer(m, ast::create_node_type_any(m));
//   handler_args_type = ast::create_node_arithmetic_mul(m, handler_args_type, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
//   handler_args_type = ast::create_node_arithmetic_mul(m, handler_args_type, ast::create_node_type_pointer(m, ast::create_node_type_evidence_context(m)));
//   handler_args_type = ast::create_node_arithmetic_mul(m, handler_args_type, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));

//   ast::Type_Arrow_Node* handler_type = ast::create_node_type_arrow(m, handler_args_type, ast::create_node_type_unit(m));

//   ast::Declaration_Constant_Node* handler_declaration = ast::create_constant_declaration(m, handler_declaration_symbol, handler_type);
//   // TODO(marcos): define tuple type for the arguments
//   ast::Variable_Assignment_Node* handler_assignment = ast::create_node_assignment(m, handler_declaration, handler);

//   lib::insert(data->continuation_handler, lit, handler_assignment);
//   lib::insert(data->continuations_handlers, handler);

//   return handler_assignment;
// }

ast::ProgramPoint* create_bubble_pp(ast::Manager* m, ast::Node* return_size, b8 is_prompt, ast::Node* own_frame, ast::Node* frame, ast::Node* handler) {
  ast::Literal_Symbol_Node* bubble_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "bubble"));
  ast::Literal_Symbol_Node* ctx_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "ctx"));
  ast::Declarations_List_Node* bubble_args = ast::create_node_declarations_list(m, handler, NULL);

  bubble_args = ast::create_node_declarations_list(m, frame, bubble_args);
  bubble_args = ast::create_node_declarations_list(m, ast::utils::natural(m, is_prompt), bubble_args);
  bubble_args = ast::create_node_declarations_list(m, own_frame, bubble_args);
  bubble_args = ast::create_node_declarations_list(m, return_size, bubble_args);
  bubble_args = ast::create_node_declarations_list(m, ctx_symbol, bubble_args);

  ast::Function_Call_Node* call = ast::create_node_function_call(m, bubble_symbol, bubble_args);

  ast::ProgramPoint* pp = ast::create_node_program_point(m, call, NULL);

  return pp;
}

ast::ProgramPoint* insert_bubling_if_stmt(
    Bubbling_Data* data,
    ast::Manager* m,
    ast::Function_Literal_Node* func,
    ast::ProgramPoint* pp,
    ast::ProgramPoint* parent,
    context::Context* ctx,
    ast::Function_Call_Node* prompt_call) {
  ast::Node* type = func->get_return_type(m);
  stackframe::Stack_Frame_Data* sf_data = cps::cps_data_get_stack_frame_data(data->cps_data);

  ast::Variable_Assignment_Node* stackframe_assignment = stackframe::stack_frame_get_function_local_stack_frame_allocation(sf_data, func);

  ast::Return_Node_Statement* return_devault_value = ast::create_node_return_statement(m, ast::utils::default_initialization(m, type));

  ast::ProgramPoint* if_body = ast::create_node_program_point(m, return_devault_value, NULL);

  ast::ProgramPoint* point = pp;

  while (ast::is_semantic_node(point)) {
    ast::Function_Call_Node* call = NULL;

    if (ast::Variable_Assignment_Node* a = ast::is_instance< ast::Variable_Assignment_Node* >(point->get_statement(m))) {
      b8 is_ret = false;

      if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(a->get_left_operand(m))) {
        if (compiler::symbol::is_equal(m->symbol_table, var->get_symbol(m)->get_symbol(m), "ret")) {
          is_ret = true;
        }
      }

      if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(a->get_left_operand(m))) {
        if (compiler::symbol::is_equal(m->symbol_table, var->get_symbol(m)->get_symbol(m), "ret")) {
          is_ret = true;
        }
      }
			
      if (is_ret) {
        if (ast::Function_Call_Node* c = ast::is_instance< ast::Function_Call_Node* >(a->get_right_operand(m))) {
          call = c;
        }
      }
    }

    if (ast::Return_Node_Statement* ret = ast::is_instance< ast::Return_Node_Statement* >(point->get_statement(m))) {
      if (ast::Function_Call_Node* c = ast::is_instance< ast::Function_Call_Node* >(ret->get_expression(m))) {
        call = c;
      }
    }

		if(ast::Function_Call_Node* c = ast::is_instance<ast::Function_Call_Node*>(point->get_statement(m))) {
			if(ast::Literal_Symbol_Node* s = ast::is_instance<ast::Literal_Symbol_Node*>(c->get_function(m))) {
				if(cps::is_continuation_closure(data->cps_data, m, s)) {
					printf("AAAAAAAAAAAAAAA ");
					parser::print_ast_ir(m, c);
					printf("\n");
					call = c;
				}
			}
		}
		
    if (call) {
			printf("CALL: ");
			parser::print_ast_ir(m, call);
			printf("\n");
			if (ast::Literal_Symbol_Node* sym = ast::is_instance< ast::Literal_Symbol_Node* >(call->get_function(m))) {
        ast::Function_Literal_Node** fun = lib::search(data->symbol_to_literal, sym->get_symbol_id());
        compiler::symbol::Symbol* symbol = lib::search(data->literal_to_symbol, func);

        if (fun) {
					printf("aaaa\n");
          ast::Function_Literal_Node* lit = *fun;

          // ast::Variable_Assignment_Node* handler_assignment = get_handler_assignment(data, lit);

          // if (handler_assignment) {
          //   ast::Declaration_Constant_Node* handler_decl = ast::as< ast::Declaration_Constant_Node* >(handler_assignment->get_left_operand(m));

          b8 owner = false;

          ast::Node* stack_frame_allocation = NULL;

          if (stackframe_assignment) {
            ast::Declaration_Variable_Node* sf_decl = ast::is_instance< ast::Declaration_Variable_Node* >(stackframe_assignment->get_left_operand(m));

            assert(sf_decl);

            ast::Literal_Symbol_Node* alloc_func = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "escape_frame"));
            ast::Declarations_List_Node* alloc_args = ast::create_node_declarations_list(m, ast::deep_copy(m, sf_decl->get_symbol(m)), NULL);

            stack_frame_allocation = ast::create_node_function_call(m, alloc_func, alloc_args);

            owner = true;

          } else {
            stackframe::Stack_Frame_Data* sf_data = cps::cps_data_get_stack_frame_data(data->cps_data);
            stack_frame_allocation = ast::deep_copy(m, stackframe::build_sp_symbol(m, stackframe::stack_frame_get_function_depth(sf_data, lit)));
          }

          ast::Function_Call_Node* sizeof_type_call = ast::utils::call(m, "sizeof", func->get_return_type(m));
          ast::Declaration_Constant_Node* sizeof_type_decl = ast::create_constant_declaration(m, ast::utils::symbol(m, "tsize"), ast::create_node_type_i32(m));
          ast::Variable_Assignment_Node* sizeof_type_assign = ast::create_node_assignment(m, sizeof_type_decl, sizeof_type_call);

          ast::Node* is_owner = NULL;

          if (owner) {
            is_owner = ast::create_node_literal_true(m);

          } else {
            is_owner = ast::create_node_literal_false(m);
          }

          ast::Declaration_Variable_Node* closure_frame_declaration = ast::utils::variable(m, "frame", ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
          ast::Variable_Assignment_Node* closure_frame_assignment = ast::utils::assignment(m, closure_frame_declaration, stack_frame_allocation);

          ast::Literal_Symbol_Node* frame_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "frame_cast"));
          ast::Declaration_Variable_Node* frame_cast_left = ast::create_variable_declaration(m, frame_symbol, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
          ast::Cast_Type_Node* frame_cast_right = ast::create_node_cast_type(m, ast::create_node_type_pointer(m, ast::create_node_type_any(m)), ast::utils::symbol(m, "frame"));
          ast::Variable_Assignment_Node* frame_cast_assign = ast::create_node_assignment(m, frame_cast_left, frame_cast_right);

          ast::Node* handler = NULL;

          if (point == pp) {
            handler = ast::utils::natural(m, 0);
          } else {
            handler = sym;
          }

          handler::Handler_Pass_Data* hd = stackframe::stack_frame_data_get_handler_pass_data(cps::cps_data_get_stack_frame_data(data->cps_data));

          b8 is_prompt = false;

          if (handler::is_prompt_function(hd, func) || prompt_call) {
            if (prompt_call) {
              handler = ast::utils::get_symbol(m, prompt_call->get_function(m));
            } else {
              handler = ast::create_node_literal_symbol(m, *symbol);
            }
            is_prompt = true;
          }

          ast::ProgramPoint* bubble_pp = create_bubble_pp(m, ast::utils::symbol(m, "tsize"), is_prompt, is_owner, frame_symbol, handler);

          bubble_pp->concat(m, if_body);

          if_body = bubble_pp;

          if_body = ast::create_node_program_point(m, sizeof_type_assign, if_body);
          if_body = ast::create_node_program_point(m, frame_cast_assign, if_body);
          if_body = ast::create_node_program_point(m, closure_frame_assignment, if_body);

          break;
          //  }
        }
      }
    }

    point = point->get_next_program_point(m);
  }

  assert(if_body);

  ast::Literal_Symbol_Node* ctx_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "ctx"));
  ast::Literal_Symbol_Node* is_yielding_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "is_yielding"));
  ast::Declarations_List_Node* arguments = ast::create_node_declarations_list(m, ctx_symbol, NULL);

  // if (insert_check) {
  ast::Function_Call_Node* is_yielding = ast::create_node_function_call(m, is_yielding_symbol, arguments);
  ast::Literal_Symbol_Node* yield_symbol = ast::utils::prefix(m, "yielding", data->yieldings++);
  ast::Declaration_Constant_Node* is_yielding_decl = ast::create_constant_declaration(m, yield_symbol, ast::create_node_type_i32(m));
  ast::Variable_Assignment_Node* is_yielding_assignment = ast::create_node_assignment(m, is_yielding_decl, is_yielding);
  pp = pp->insert(m, is_yielding_assignment);
  pp = pp->insert(m, ast::create_node_if_statement(m, ast::deep_copy(m, yield_symbol), if_body));
  // } else {
  //   while (ast::is_semantic_node(if_body)) {
  //     pp = pp->insert(m, if_body->get_statement(m));
  //     if_body = if_body->get_next_program_point(m);
  //   }
  // }

  return pp;
}

void insert_bubble_handler(
    Bubbling_Data* data,
    ast::Manager* m,
    ast::Function_Literal_Node* fun,
    ast::Literal_Symbol_Node* handler,
    ast::ProgramPoint* pp,
    ast::ProgramPoint* parent,
    context::Context* ctx) {
  ast::Variable_Assignment_Node* stackframe = 0;
  ast::Variable_Assignment_Node* stackframe_assignment = NULL;

  // ast::ProgramPoint_List_Node* if_body = ast::create_node_program_point(m, return_devault_value, NULL);

  ast::ProgramPoint* point = pp;

  b8 owner = false;

  stackframe::Stack_Frame_Data* sf_data = cps::cps_data_get_stack_frame_data(data->cps_data);

  ast::Node* stack_frame_allocation = stack_frame_allocation = ast::deep_copy(m, stackframe::build_sp_symbol(m, stackframe::stack_frame_get_function_depth(sf_data, fun)));

  ast::Literal_Symbol_Node* sizeof_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "tsize"));
  ast::Literal_Symbol_Node* sizeof_type_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "sizeof"));
  ast::Declarations_List_Node* sizeof_type_args = ast::create_node_declarations_list(m, ast::deep_copy(m, fun->get_return_type(m)), NULL);
  ast::Function_Call_Node* sizeof_type_call = ast::create_node_function_call(m, sizeof_type_symbol, sizeof_type_args);

  ast::Declaration_Constant_Node* sizeof_type_decl = ast::create_constant_declaration(m, sizeof_symbol, ast::create_node_type_i32(m));
  ast::Variable_Assignment_Node* sizeof_type_assign = ast::create_node_assignment(m, sizeof_type_decl, sizeof_type_call);

  ast::Node* is_owner = NULL;

  if (owner) {
    is_owner = ast::create_node_literal_true(m);
  } else {
    is_owner = ast::create_node_literal_false(m);
  }

  ast::Literal_Symbol_Node* closure_frame_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "frame"));
  ast::Declaration_Variable_Node* closure_frame_decl = ast::create_variable_declaration(m, closure_frame_symbol, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
  ast::Variable_Assignment_Node* closure_frame_assignment = ast::create_node_assignment(m, closure_frame_decl, stack_frame_allocation);

  ast::ProgramPoint* bubble_pp = create_bubble_pp(m, sizeof_symbol, false, is_owner, closure_frame_symbol, ast::deep_copy(m, handler));

  pp->emplace(m, bubble_pp);
  pp->insert(m, sizeof_type_assign);
  pp->insert(m, closure_frame_assignment);
}

// b8 insert_handlers(Bubbling_Data* data, ast::Manager* m, ast::Node* root, ast::ProgramPoint* point) {
//   if (!ast::is_semantic_node(root) || !ast::is_semantic_node(point)) {
//     return false;
//   }

//   if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(root)) {
//     if (ast::Function_Literal_Node* literal = ast::is_instance< ast::Function_Literal_Node* >(assignment->get_right_operand(m))) {
//       ast::Node* left = assignment->get_left_operand(m);

//       ast::Literal_Symbol_Node* symbol = NULL;

//       if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
//         symbol = var->get_symbol(m);
//       }

//       if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
//         symbol = var->get_symbol(m);
//       }

//       assert(symbol);

//       insert_handlers(data, m, literal->get_body(m), literal->get_body(m));

//       stackframe::Stack_Frame_Data* sf_data = cps::cps_data_get_stack_frame_data(data->cps_data);
//       handler::Handler_Pass_Data* hdata = stackframe::stack_frame_data_get_handler_pass_data(sf_data);

//       if (cps::is_continuation_closure(data->cps_data, m, symbol)) {
//         ast::Variable_Assignment_Node* assignment = create_handler_function(data, m, symbol, literal);
//         point->insert(m, assignment);

//         return true;
//       }

//       return false;
//     }
//   }

//   if (ast::ProgramPoint* pp = ast::is_instance< ast::ProgramPoint* >(root)) {
//     if (insert_handlers(data, m, pp->get_statement(m), pp)) {
//       pp = pp->get_next_program_point(m);
//     }

//     ast::ProgramPoint* next = pp->get_next_program_point(m);

//     insert_handlers(data, m, next, next);

//     return false;
//   }

//   insert_handlers(data, m, ast::left_of(m, root), point);
//   insert_handlers(data, m, ast::right_of(m, root), point);

//   return false;
// }

// b8 is_handler_function(Bubbling_Data* data, ast::Function_Literal_Node* lit) {
//   return lib::search(data->continuations_handlers, lit) != NULL;
// }

ast::ProgramPoint* bubbling_yields_rec(
    Bubbling_Data* data, ast::Manager* m, ast::Node* root, ast::Function_Literal_Node* func, ast::ProgramPoint* pp, ast::ProgramPoint* parent, context::Context* ctx) {
  if (!ast::is_semantic_node(root) || !ast::is_semantic_node(pp)) {
    return pp;
  }

  if (ast::is_instance< ast::Return_Node_Statement* >(root)) {
    return pp;
  }

  if (ast::If_Node_Statement* if_stmt = ast::is_instance< ast::If_Node_Statement* >(root)) {
    if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(if_stmt->get_condition(m))) {
      if (ast::Literal_Symbol_Node* sym = ast::is_instance< ast::Literal_Symbol_Node* >(call->get_function(m))) {
        compiler::symbol::Symbol id = sym->get_symbol(m);

        if (compiler::symbol::is_equal(m->symbol_table, &id, "is_yielding")) {
          return pp;
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

    context::Context* local_context = context::context_create(NULL);
    bubbling_yields_rec(data, m, lit->get_body(m), lit, pp, parent, local_context);
    context::context_destroy(local_context);

    return pp;
  }

  if (ast::Bubble_Handler_Node* lit = ast::is_instance< ast::Bubble_Handler_Node* >(root)) {
    insert_bubble_handler(data, m, func, lit->get_handler(m), pp, parent, ctx);
    return pp;
  }

  if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(root)) {
    if (ast::Literal_Symbol_Node* symbol = ast::is_instance< ast::Literal_Symbol_Node* >(call->get_function(m))) {
      if (is_internal_function(m, symbol)) {
        return pp;
      }
    }
		printf("call: ");
		parser::print_ast_ir(m, call);
		printf("\n");
    if (lib::search(data->bubblint_functions_blacklist, call)) {
			printf("a\n");
      return pp;
    }

    handler::Handler_Pass_Data* hd = stackframe::stack_frame_data_get_handler_pass_data(cps::cps_data_get_stack_frame_data(data->cps_data));

    if (handler::is_prompt_site(hd, call)) {
			printf("b\n");
      return pp;
    }

		printf("-----\n");
		
    if (handler::is_prompt_call(hd, call)) {
      pp = insert_bubling_if_stmt(data, m, func, pp, parent, ctx, call);
    } else {
      pp = insert_bubling_if_stmt(data, m, func, pp, parent, ctx, NULL);
    }

    return pp;
  }

  if (ast::ProgramPoint* pp = ast::is_instance< ast::ProgramPoint* >(root)) {
    pp = bubbling_yields_rec(data, m, pp->get_statement(m), func, pp, parent, ctx);

    ast::ProgramPoint* next = pp->get_next_program_point(m);

    next = bubbling_yields_rec(data, m, next, func, next, pp, ctx);

    return next;
  }

  bubbling_yields_rec(data, m, ast::left_of(m, root), func, pp, parent, ctx);
  bubbling_yields_rec(data, m, ast::right_of(m, root), func, pp, parent, ctx);

  return pp;
}

void add_context_argument(Bubbling_Data* data, ast::Manager* m, ast::Node* root) {
  if (!ast::is_semantic_node(root)) {
    return;
  }

  if (ast::Function_Literal_Node* fun = ast::is_instance< ast::Function_Literal_Node* >(root)) {
    ast::Literal_Symbol_Node* symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "ctx"));
    ast::Type_Pointer_Node* type = ast::create_node_type_pointer(m, ast::create_node_type_evidence_context(m));

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
        lib::insert(data->literal_to_symbol, lit, decl->get_symbol(m)->get_symbol(m));
      }

      if (ast::Declaration_Constant_Node* decl = ast::is_instance< ast::Declaration_Constant_Node* >(var->get_left_operand(m))) {
        lib::insert(data->symbol_to_literal, decl->get_symbol(m)->get_symbol_id(), lit);
        lib::insert(data->literal_to_symbol, lit, decl->get_symbol(m)->get_symbol(m));
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
  m->symbol_map = lib::table_create< ast::Node*, compiler::symbol::Id >();
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
    ast::Node* type = NULL;

    if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
      context::context_declare(ctx, m, var);

      type = var->get_type(m);
      symbol = var->get_symbol(m);
    }

    if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
      context::context_declare(ctx, m, var);

      type = var->get_type(m);
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

        rename_local_functions(m, type, closure_ctx, prefix);
        rename_local_functions(m, literal->get_arguments(m), closure_ctx, prefix);
        rename_local_functions(m, literal->get_return_type(m), closure_ctx, prefix);
        rename_local_functions(m, literal->get_body(m), closure_ctx, s);

        context::context_destroy(closure_ctx);

        return;
      }
    }

    if (ast::Literal_Struct_Node* literal = ast::is_instance< ast::Literal_Struct_Node* >(assignment->get_right_operand(m))) {
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
      }

      return rename_local_functions(m, literal->get_members(m), ctx, prefix);
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

b8 move_declarations_to_global_context(ast::Manager* m, ast::Node* node, ast::ProgramPoint* root, ast::ProgramPoint* point, ast::Node* parent, u64 depth = 0) {
  if (!ast::is_semantic_node(node)) {
    return false;
  }

  if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(node)) {
    assert(point->left == assignment->id);

    if (ast::Function_Literal_Node* lit = ast::is_instance< ast::Function_Literal_Node* >(assignment->get_right_operand(m))) {

      if (depth > 0) {
        if (ast::ProgramPoint* p = ast::is_instance< ast::ProgramPoint* >(parent)) {
          p->split(m);
        }

        ast::ProgramPoint* cont1 = point->split(m);

        if (ast::is_semantic_node(cont1)) {

          if (ast::ProgramPoint* p = ast::is_instance< ast::ProgramPoint* >(parent)) {
            p->emplace(m, cont1);
          }

          if (ast::Function_Literal_Node* f = ast::is_instance< ast::Function_Literal_Node* >(parent)) {
            f->set_body(m, cont1);
          }

          move_declarations_to_global_context(m, cont1, root, cont1, parent, depth);
        }

        if (ast::is_semantic_node(point)) {
          root->insert(m, assignment);
        }

        return move_declarations_to_global_context(m, lit->get_body(m), root, lit->get_body(m), lit, depth + 1);
      }

      move_declarations_to_global_context(m, lit->get_body(m), root, lit->get_body(m), lit, depth + 1);
    }

    if (ast::Literal_Struct_Node* structure = ast::is_instance< ast::Literal_Struct_Node* >(assignment->get_right_operand(m))) {

      if (depth > 0) {
        if (ast::ProgramPoint* p = ast::is_instance< ast::ProgramPoint* >(parent)) {
          p->split(m);
        }

        ast::ProgramPoint* cont1 = point->split(m);

        if (ast::is_semantic_node(cont1)) {

          if (ast::ProgramPoint* p = ast::is_instance< ast::ProgramPoint* >(parent)) {
            p->emplace(m, cont1);
          }

          if (ast::Function_Literal_Node* f = ast::is_instance< ast::Function_Literal_Node* >(parent)) {
            f->set_body(m, cont1);
          }

          move_declarations_to_global_context(m, cont1, root, cont1, parent, depth);
        }

        if (ast::is_semantic_node(point)) {
          root->insert(m, assignment);
        }

        return move_declarations_to_global_context(m, structure->get_members(m), root, NULL, structure, depth + 1);
      }

      move_declarations_to_global_context(m, structure->get_members(m), root, NULL, structure, depth + 1);
    }

    return false;
  }

  if (ast::ProgramPoint* pp = ast::is_instance< ast::ProgramPoint* >(node)) {
    if (move_declarations_to_global_context(m, pp->get_statement(m), root, pp, parent, depth)) {
      return true;
    }

    ast::ProgramPoint* next = pp->get_next_program_point(m);

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

// void add_effect_yields(ast::Manager* m, ast::Node* root) {
// 	if(ast::ProgramPoint_List_Node* point = ast::is_instance<ast::ProgramPoint_List_Node*>(root)) {
// 		while(ast::is_semantic_node(point)) {
// 			if(ast::Variable_Assignment_Node* assignment = ast::is_instance<ast::Variable_Assignment_Node*>(root)) {
// 				ast::Node* right = assignment->get_right_operand(m);
// 				if(ast::Effect_Call_Node* call = ast::is_instance<ast::Effect_Call_Node*>(right)) {

// 				}
// 			}
// 		}
// 	}

// }

void add_bubbling_yields(Bubbling_Data* data, ast::Manager* m, ast::Node* root) {
  build_functions_map(data, m, root);

  ast::ProgramPoint* pp = ast::is_instance< ast::ProgramPoint* >(root);

  // insert_handlers(data, m, root, pp);

  context::Context* ctx0 = context::context_create(NULL);

  bubbling_yields_rec(data, m, root, NULL, pp, pp, ctx0);
  printf("=========================**********\n");
  printf("=========================**********\n");
  parser::print_ast_ir(m, root);
  printf("=========================\n");
  printf("=========================\n");

  context::context_destroy(ctx0);

  context::Context* ctx1 = context::context_create(NULL);

  rename_local_functions(m, root, ctx1, compiler::symbol::empty(m->symbol_table));

  context::context_destroy(ctx1);
  move_declarations_to_global_context(m, root, pp, pp, pp);
}

} // namespace bubbling
