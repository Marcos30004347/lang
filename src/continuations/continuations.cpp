#include "continuations.hpp"
#include "ast/ast_control_flow.hpp"
#include "ast/ast_declaration.hpp"
#include "ast/ast_function.hpp"
#include "ast/ast_kind.hpp"
#include "ast/ast_literals.hpp"
#include "ast/ast_manager.hpp"
#include "ast/ast_operations.hpp"
#include "ast/ast_pointer.hpp"
#include "ast/ast_program_point.hpp"
#include "ast/ast_types.hpp"
#include "ast/utils.hpp"
#include "compiler/compiler.hpp"
#include "compiler/symbol_table.hpp"
#include "context/context.hpp"
#include "continuations/handler.hpp"
#include "lib/set.hpp"
#include "parser/parser.hpp"
#include "stackframe/stackframe.hpp"

#include <assert.h>
#include <cstdio>
#include <stdio.h>

using namespace compiler;
namespace cps {

struct CPS_Data {
  u64 temporaries;

  stackframe::Stack_Frame_Data* stackframe_pass_data;

  lib::Set< ast::Declaration_Variable_Node* >* temporary_variable_declarations;
  lib::Set< ast::Declaration_Constant_Node* >* temporary_constant_declarations;

  lib::Table< compiler::symbol::Id, ast::Declaration_Constant_Node* >* continuation_literals;
  lib::Table< ast::Function_Literal_Node*, ast::Declaration_Constant_Node* >* continuation_declaration;
};

CPS_Data* cps_data_create(stackframe::Stack_Frame_Data* data) {
  CPS_Data* info = new CPS_Data();

  info->temporaries = 0;
  info->stackframe_pass_data = data;

  // info->continuation_arguments = lib::table_create< compiler::symbol::Id, ast::Id >();
  info->continuation_literals = lib::table_create< compiler::symbol::Id, ast::Declaration_Constant_Node* >();
  info->continuation_declaration = lib::table_create< ast::Function_Literal_Node*, ast::Declaration_Constant_Node* >();

  info->temporary_variable_declarations = lib::set_create< ast::Declaration_Variable_Node* >();
  info->temporary_constant_declarations = lib::set_create< ast::Declaration_Constant_Node* >();

  return info;
}

void cps_data_destroy(CPS_Data* info) {
  lib::table_delete(info->continuation_literals);
  lib::table_delete(info->continuation_declaration);
  lib::set_delete(info->temporary_variable_declarations);
  lib::set_delete(info->temporary_constant_declarations);

  stackframe::destroy_stack_frame_data(info->stackframe_pass_data);

  delete info;
}

stackframe::Stack_Frame_Data* cps_data_get_stack_frame_data(CPS_Data* data) {
  return data->stackframe_pass_data;
}

b8 is_continuation_closure(CPS_Data* info, ast::Manager* m, ast::Literal_Symbol_Node* decl) {
  return lib::search(info->continuation_literals, decl->get_symbol_id()) != NULL;
}

b8 is_continuation_closure(CPS_Data* info, ast::Manager* m, ast::Function_Literal_Node* decl) {
  return lib::search(info->continuation_declaration, decl) != NULL;
}

b8 is_temporary_variable(CPS_Data* info, ast::Manager* m, ast::Declaration_Variable_Node* decl) {
  return lib::search(info->temporary_variable_declarations, decl) != NULL;
}

b8 is_temporary_variable(CPS_Data* info, ast::Manager* m, ast::Declaration_Constant_Node* decl) {
  return lib::search(info->temporary_constant_declarations, decl) != NULL;
}

// b8 is_temporary_variable(CPS_Data* info, ast::Manager* m, ast::Declaration_Constant_Node* decl) {
//   return lib::search(info->temporary_declarations, decl) != NULL;
// }

void call_cps_conversion(
    CPS_Data*, ast::Function_Literal_Node*, parser::Parser*, context::Context*, ast::ProgramPoint*, ast::Node*, ast::Node*, ast::Literal_Symbol_Node*, ast::Node*, u64);

void function_literal_cps_conversion(CPS_Data*, parser::Parser*, context::Context*, ast::Function_Literal_Node*, ast::Literal_Symbol_Node*);

void push_zero_argument(ast::Manager* m, ast::Function_Call_Node* call) {
  call->push_argument(m, ast::create_node_literal_natural(m, symbol::number_to_symbol(m->symbol_table, 0)));
}

b8 is_blacklisted(ast::Manager* m, ast::Node* call) {
  ast::Node* node = NULL;

  if (ast::Function_Call_Node* c = ast::is_instance< ast::Function_Call_Node* >(call)) {
    node = c->get_function(m);
  }

  if (ast::Effect_Call_Node* c = ast::is_instance< ast::Effect_Call_Node* >(call)) {
    node = c->get_effect(m);
  }

  if (ast::Literal_Symbol_Node* symbol = ast::is_instance< ast::Literal_Symbol_Node* >(node)) {
    compiler::symbol::Symbol sym = symbol->get_symbol(m);
    if (symbol::is_equal(m->symbol_table, &sym, "push_frame")) {
      return true;
    }
    if (symbol::is_equal(m->symbol_table, &sym, "is_yielding_to_handler")) {
      return true;
    }
    if (symbol::is_equal(m->symbol_table, &sym, "set_is_yielding_to")) {
      return true;
    }
    if (symbol::is_equal(m->symbol_table, &sym, "pop_frame")) {
      return true;
    }
    if (symbol::is_equal(m->symbol_table, &sym, "is_yielding")) {
      return true;
    }
    if (compiler::symbol::is_equal(m->symbol_table, &sym, "set_is_yielding_to")) {
      return true;
    }
    if (compiler::symbol::is_equal(m->symbol_table, &sym, "ctx_is_returning")) {
      return true;
    }
    if (compiler::symbol::is_equal(m->symbol_table, &sym, "ctx_allocate_args")) {
      return true;
    }
    if (compiler::symbol::is_equal(m->symbol_table, &sym, "ctx_set_returning")) {
      return true;
    }
    if (compiler::symbol::is_equal(m->symbol_table, &sym, "ctx_get_handler_args")) {
      return true;
    }
    if (compiler::symbol::is_equal(m->symbol_table, &sym, "ctx_is_yielding_to")) {
      return true;
    }
  }

  return false;
}

void replace_return_call(CPS_Data* info, parser::Parser* parser, ast::Node* node, ast::Literal_Symbol_Node* func) {

  if (node == 0 || ast::is_instance< ast::Literal_Nothing_Node* >(node)) {
    return;
  }

  ast::Manager* manager = parser->ast_manager;

  if (ast::Return_Node_Statement* ret = ast::is_instance< ast::Return_Node_Statement* >(node)) {
    ast::Node* expression = ret->get_expression(manager);
    ast::Node* symbol = ast::deep_copy(manager, func);

    ast::Declarations_List_Node* args = ast::create_node_declarations_list(manager, expression, 0);
    ast::Function_Call_Node* call = ast::create_node_function_call(manager, symbol, args);

    ast::replace(ret, call);

    ast::manager_pop(manager, call);
  }

  replace_return_call(info, parser, ast::left_of(manager, node), func);
  replace_return_call(info, parser, ast::right_of(manager, node), func);
}
	
ast::ProgramPoint* insert_ret_statement(CPS_Data* data, ast::Manager* m, ast::Function_Call_Node* call, ast::Node* return_type, ast::ProgramPoint* pp, u64 depth) {
	pp = pp->insert(m, call);
  if (depth == 0) {
    return pp->insert(m, ast::create_node_return_statement(m, ast::utils::symbol(m, "cont_ret")));
  }

  return pp->insert(m, ast::create_node_return_statement(m, ast::utils::natural(m, 0)));
}

ast::ProgramPoint* insert_cont_ret_assignment(CPS_Data* data, ast::Manager* m, ast::Function_Call_Node* call, ast::Node* return_type, ast::ProgramPoint* pp, u64 depth) {
  if (depth == 0) {
    return pp->insert(m, ast::utils::assignment(m, ast::utils::variable(m, "cont_ret", return_type), ast::utils::default_initialization(m, return_type)));
  }

  return pp;
}

ast::ProgramPoint* rewrite_return(ast::Manager* m, ast::Node* root, ast::Node* return_type, ast::ProgramPoint* pp) {
  if (ast::is_semantic_node(root) == false) {
    return pp;
  }

  if (ast::ProgramPoint* point = ast::is_instance< ast::ProgramPoint* >(root)) {
    point = rewrite_return(m, point->get_statement(m), return_type, point);
    return rewrite_return(m, point->get_next_program_point(m), return_type, point);
  }

  if (ast::Elif_List_Node* elif = ast::is_instance< ast::Elif_List_Node* >(root)) {
    rewrite_return(m, elif->get_if(m), return_type, pp);
    rewrite_return(m, elif->get_elif(m), return_type, pp);
    return pp;
  }

  if (ast::If_Node_Statement* if_stmt = ast::is_instance< ast::If_Node_Statement* >(root)) {
    rewrite_return(m, if_stmt->get_body(m), return_type, if_stmt->get_body(m));
    return pp;
  }

  if (ast::Return_Node_Statement* ret = ast::is_instance< ast::Return_Node_Statement* >(root)) {
    ast::Cast_Type_Node* cast0 = ast::create_node_cast_type(m, ast::create_node_type_pointer(m, return_type), ast::utils::symbol(m, "cont_ret"));
    ast::Cast_Type_Node* cast1 = ast::create_node_cast_type(m, return_type, ret->get_expression(m));
    ast::Variable_Assignment_Node* assignment = ast::utils::assignment(m, ast::create_node_pointer_value(m, cast0), cast1);

    pp->set_statement(m, assignment);

    return pp->insert(m, ast::create_node_return_statement(m, ast::utils::natural(m, 0)));
  }

	if(ast::Function_Literal_Node* lit = ast::is_instance<ast::Function_Literal_Node*>(root)) {
		return pp;
	}
	
  pp = rewrite_return(m, ast::left_of(m, root), return_type, pp);
  pp = rewrite_return(m, ast::right_of(m, root), return_type, pp);

  return pp;
}

ast::Variable_Assignment_Node* create_continuation_function(
    CPS_Data* info, parser::Parser* parser, ast::Node* argument, ast::Node* return_type, ast::ProgramPoint* body, ast::Node* joint, ast::Function_Literal_Node* parent) {

  ast::Manager* m = parser->ast_manager;

  ast::Declaration_Variable_Node* cont_ret = ast::utils::variable(m, "cont_ret", ast::create_node_type_pointer(m, ast::create_node_type_any(m)));

  ast::Declarations_List_Node* arguments = ast::create_node_declarations_list(m, cont_ret, NULL);

  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(argument)) {
    arguments = ast::create_node_declarations_list(m, var, arguments);
  } else if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(argument)) {
    arguments = ast::create_node_declarations_list(m, var, arguments);
  } else {
    ast::Literal_Symbol_Node* s = ast::create_node_literal_symbol(m, symbol::set_entry(m->symbol_table, "_"));
    ast::Type_Pointer_Node* t = ast::create_node_type_pointer(m, ast::create_node_type_any(m));
    ast::Declaration_Variable_Node* v = ast::create_variable_declaration(m, s, t);

    lib::insert(info->temporary_variable_declarations, v);

    arguments = ast::create_node_declarations_list(m, v, arguments);
  }

  u64 stack_depth = stackframe::stack_frame_get_function_depth(info->stackframe_pass_data, parent);

  for (u64 i = stack_depth - 1; i > 0; i--) {
    ast::Literal_Symbol_Node* stack_frame_sym = ast::create_node_literal_symbol(m, symbol::number_to_symbol(m->symbol_table, i, "sp"));
    ast::Type_Pointer_Node* stack_frame_type = ast::create_node_type_pointer(m, ast::create_node_type_any(m));
    ast::Declaration_Variable_Node* stack_frame_decl = ast::create_variable_declaration(m, stack_frame_sym, stack_frame_type);

    ast::Literal_Symbol_Node* stack_ptr_parent = ast::create_node_literal_symbol(m, compiler::symbol::number_to_symbol(m->symbol_table, i + 1, "sp"));

    ast::Type_Pointer_Node* cast_type = ast::create_node_type_pointer(m, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
    ast::Cast_Type_Node* cast_parent = ast::create_node_cast_type(m, cast_type, stack_ptr_parent);

    ast::Variable_Assignment_Node* stack_frame_assign = ast::create_node_assignment(m, stack_frame_decl, ast::create_node_pointer_value(m, cast_parent));

    body = ast::create_node_program_point(m, stack_frame_assign, body);
    // stack_frame_allocations->insert(m, stack_frame_assign);
  }

  rewrite_return(m, body, return_type, body);

  ast::Function_Literal_Node* function = ast::create_node_function_literal(m, arguments, ast::create_node_type_pointer(m, ast::create_node_type_any(m)), body);

  stackframe::stack_frame_set_function_depth(info->stackframe_pass_data, function, stack_depth);

  // ast::Variable_Assignment_Node* frame_allocation = stackframe::stack_frame_get_function_local_stack_frame_allocation(info->stackframe_pass_data, parent);
  // stackframe::stack_frame_set_function_local_stack_frame_allocation(info->stackframe_pass_data, function, frame_allocation);

  symbol::Symbol sym = symbol::number_to_symbol(m->symbol_table, lib::size(info->continuation_literals), "c");

  ast::Literal_Symbol_Node* symbol = ast::create_node_literal_symbol(m, sym);

  ast::Node* argument_type = NULL;

  ast::Declarations_List_Node* tmp = ast::is_instance< ast::Declarations_List_Node* >(arguments);

  while (ast::is_semantic_node(tmp)) {
    ast::Node* t = tmp->get_declaration(m);

    if (ast::Declaration_Variable_Node* arg = ast::is_instance< ast::Declaration_Variable_Node* >(t)) {
      if (argument_type) {
        argument_type = ast::create_node_arithmetic_mul(m, argument_type, ast::deep_copy(m, arg->get_type(m)));
      } else {
        argument_type = ast::deep_copy(m, arg->get_type(m));
      }
    }

    if (ast::Declaration_Constant_Node* arg = ast::is_instance< ast::Declaration_Constant_Node* >(t)) {
      if (argument_type) {
        argument_type = ast::create_node_arithmetic_mul(m, argument_type, ast::deep_copy(m, arg->get_type(m)));
      } else {
        argument_type = ast::deep_copy(m, arg->get_type(m));
      }
    }

    tmp = tmp->get_next_declaration(m);
  }

  ast::Node* type = ast::create_node_type_arrow(m, argument_type, ast::deep_copy(m, return_type));

  ast::Declaration_Constant_Node* declaration = ast::create_constant_declaration(m, symbol, type);

  ast::Variable_Assignment_Node* assignment = ast::create_node_assignment(m, declaration, function);

  handler::Handler_Pass_Data* hdata = stackframe::stack_frame_data_get_handler_pass_data(info->stackframe_pass_data);

  handler::add_context_argument(hdata, m, function, declaration);

  stackframe::stackframe_push_frame_pointer_argument(info->stackframe_pass_data, m, function, declaration, stack_depth);

  lib::insert(info->continuation_literals, symbol->get_symbol_id(), declaration);

  lib::insert(info->continuation_declaration, function, declaration);
	printf("ASIGNMENT:\n");
	parser::print_ast_ir(m, assignment);
  return assignment;
}

void function_literal_assignment_to_constant_declaration(
    CPS_Data* info, parser::Parser* parser, ast::Node* declaration, ast::Node* literal, ast::Node* assignment, ast::ProgramPoint* point) {
  ast::Manager* m = parser->ast_manager;

  // Promote function declaration to constant
  ast::Literal_Symbol_Node* symbol = ast::create_node_literal_symbol(m, symbol::number_to_symbol(m->symbol_table, info->temporaries, "t"));

  info->temporaries += 1;

  ast::Node* type = NULL;

  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(declaration)) {
    type = ast::deep_copy(m, var->get_type(m));
  }

  if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(declaration)) {
    type = ast::deep_copy(m, var->get_type(m));
  }

  assert(type);

  ast::Declaration_Constant_Node* decl = ast::create_constant_declaration(m, symbol, type);
  ast::Variable_Assignment_Node* func = ast::create_node_assignment(m, decl, literal);

  ast::Variable_Assignment_Node* statement = ast::is_instance< ast::Variable_Assignment_Node* >(assignment);

  lib::insert(info->temporary_constant_declarations, decl);

  assert(statement);

  statement->set_right_operand(m, ast::deep_copy(m, symbol));

  point->set_statement(m, func);

  point->insert(m, statement);
}

void program_point_cps_conversion(
    CPS_Data* info,
    ast::Function_Literal_Node* function,
    parser::Parser* parser,
    context::Context* ctx,
    ast::Literal_Symbol_Node* cont_symbol,
    ast::ProgramPoint* statements,
    ast::Literal_Symbol_Node* joint_continuation,
    ast::Node* return_type,
    u64 depth) {

  ast::Manager* m = parser->ast_manager;

  ast::ProgramPoint* previous = NULL;

  while (ast::is_semantic_node(statements)) {
    ast::Node* statement = statements->get_statement(m);

    ast::ProgramPoint* continuation = statements->get_next_program_point(m);

    if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(statement)) {
      context::context_declare(ctx, m, var);
    }

    if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(statement)) {
      context::context_declare(ctx, m, var);
    }

    if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(statement)) {
      ast::Node* right = assignment->get_right_operand(m);
      ast::Node* left = assignment->get_left_operand(m);

      b8 mut = false;

      if (ast::Declaration_Constant_Node* v = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
        context::context_declare(ctx, m, v);
      }

      if (ast::Declaration_Variable_Node* v = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
        mut = true;
        context::context_declare(ctx, m, v);
      }

      if (ast::Function_Literal_Node* lit = ast::is_instance< ast::Function_Literal_Node* >(right)) {
        // printf("parsing this\n");
        // parser::print_ast_ir(m, assignment);

        function_literal_cps_conversion(info, parser, ctx, lit, NULL);

        if (mut) {
          function_literal_assignment_to_constant_declaration(info, parser, left, right, statement, statements);
        }
      }

      if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(right)) {
        if (!is_blacklisted(m, call)) {
          ast::Literal_Symbol_Node* temp_symbol = ast::create_node_literal_symbol(m, symbol::number_to_symbol(m->symbol_table, info->temporaries, "t"));

          ast::Node* temp_left = ast::create_node_literal_symbol(m, symbol::number_to_symbol(m->symbol_table, info->temporaries, "t"));
          info->temporaries = info->temporaries + 1;

          ast::Node* temp_type = NULL;

          if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
            temp_type = var->get_type(m);
          }

          if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
            temp_type = var->get_type(m);
          }

          if (ast::Literal_Symbol_Node* symbol = ast::is_instance< ast::Literal_Symbol_Node* >(left)) {
            temp_type = context::context_type_of(ctx, m, symbol->get_symbol_id());
          }

          if (ast::Cast_Type_Node* cast = ast::is_instance< ast::Cast_Type_Node* >(left)) {
            temp_type = cast->get_to_type(m);
          }

          if (ast::Pointer_Value_Node* val = ast::is_instance< ast::Pointer_Value_Node* >(left)) {
            if (ast::Cast_Type_Node* cast = ast::is_instance< ast::Cast_Type_Node* >(val->get_variable(m))) {
              temp_type = ast::is_instance< ast::Type_Pointer_Node* >(cast->get_to_type(m))->get_pointer_type(m);
              // temp_left = ast::create_node_pointer_value(m, temp_left);
            }
          }

          ast::Cast_Type_Node* cast = ast::create_node_cast_type(m, ast::create_node_type_pointer(m, temp_type), temp_left);

          statements->insert(m, ast::create_node_assignment(m, left, ast::create_node_pointer_value(m, cast)));

          ast::Type_Pointer_Node* p = ast::create_node_type_pointer(m, ast::create_node_type_any(m));
          ast::Declaration_Variable_Node* temp_arg = ast::create_variable_declaration(m, temp_symbol, p);

          lib::insert(info->temporary_variable_declarations, temp_arg);
					printf("TYPE: ");
					parser::print_ast_ir(m, temp_type);
					printf("\n");
					printf("Var: ");
					parser::print_ast_ir(m, temp_symbol);
					printf("\n");
          call_cps_conversion(info, function, parser, ctx, statements, call, temp_arg, joint_continuation, return_type, depth);
          depth = depth + 1;
        }
        // printf("CONTINUATION\n");
        // parser::print_ast_ir(m, continuation);
        // return function_literal_cps_conversion(info, parser, ctx, ast::as<ast::Function_Literal_Node*>(assignment->get_right_operand(m)), NULL);
        return program_point_cps_conversion(info, function, parser, ctx, cont_symbol, continuation, joint_continuation, return_type, depth);
      }
    }

    // if (ast::Effect_Call_Node* call = ast::is_instance< ast::Effect_Call_Node* >(statement)) {
    //   program_point_cps_conversion(info, function, parser, ctx, cont_symbol, continuation, joint_continuation, return_type);

    //   if (is_blacklisted(m, call)) {
    //     return;
    //   }

    //   return call_cps_conversion(info, function, parser, ctx, statements, call, NULL, joint_continuation, return_type);
    // }

    if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(statement)) {
      // printf("CONTINUATION\n");
      // parser::print_ast_ir(m, continuation);
      program_point_cps_conversion(info, function, parser, ctx, cont_symbol, continuation, joint_continuation, return_type, depth + 1);

      // function_literal_cps_conversion(info, parser, ctx, ast::as<ast::Function_Literal_Node*>(assignment->get_right_operand(m)), NULL);

      if (is_blacklisted(m, call)) {
        return;
      }

      return call_cps_conversion(info, function, parser, ctx, statements, call, NULL, joint_continuation, return_type, depth);
    }

    if (ast::Elif_List_Node* elif = ast::is_instance< ast::Elif_List_Node* >(statement)) {
      ast::Literal_Symbol_Node* old_joint = joint_continuation;

      ast::Variable_Assignment_Node* joint_continuation =
          create_continuation_function(info, parser, ast::create_node_literal_nothing(m), return_type, statements->split(m), joint_continuation, function);

      ast::Declaration_Constant_Node* joint_declaration = ast::as< ast::Declaration_Constant_Node* >(joint_continuation->get_left_operand(m));

      ast::Literal_Symbol_Node* joint_symbol = joint_declaration->get_symbol(m);

      ast::Function_Literal_Node* joint_literal = ast::is_instance< ast::Function_Literal_Node* >(joint_continuation->get_right_operand(m));

      ast::ProgramPoint* joint_body = joint_literal->get_body(m);

      program_point_cps_conversion(info, function, parser, ctx, cont_symbol, joint_body, old_joint, return_type, depth);

      statements->set_statement(m, joint_continuation);

      ast::Elif_List_Node* branch = elif;

      while (ast::is_semantic_node(elif)) {
        ast::If_Node_Statement* if_statement = elif->get_if(m);
        ast::ProgramPoint* body = if_statement->get_body(m);

        program_point_cps_conversion(info, function, parser, ctx, cont_symbol, body, joint_symbol, return_type, depth);

        elif = elif->get_elif(m);
      }

      statements = statements->insert(m, branch);

      ast::Function_Call_Node* call = ast::create_node_function_call(m, joint_symbol, ast::create_node_literal_nothing(m));

      push_zero_argument(m, call);
			
      if (depth == 0) {
				ast::Node* addr = ast::create_node_value_address(m, ast::utils::symbol(m, "cont_ret"));
        call->push_argument(m, ast::create_node_cast_type(m, ast::create_node_type_pointer(m, ast::create_node_type_any(m)), addr));
      } else {
        call->push_argument(m, ast::utils::symbol(m, "cont_ret"));
      }

      handler::Handler_Pass_Data* hdata = stackframe::stack_frame_data_get_handler_pass_data(info->stackframe_pass_data);

      handler::pass_context_argument(hdata, m, call);

      u64 stack_depth = stackframe::stack_frame_get_function_depth(info->stackframe_pass_data, function);

      stackframe::stackframe_pass_stack_frame_paramenter(m, &stack_depth, call);

      statements = insert_cont_ret_assignment(info, m, call, return_type, statements, depth);
      statements = insert_ret_statement(info, m, call, return_type, statements, depth);
    }

    previous = statements;
    statements = statements->get_next_program_point(m);
  }

  if (previous && ast::is_semantic_node(joint_continuation)) {
    ast::Node* last_statement = previous->get_statement(m);

    if (ast::is_instance< ast::Function_Call_Node* >(last_statement)) {
      return;
    }

    if (ast::is_instance< ast::Return_Node_Statement* >(last_statement)) {
      return;
    }

    ast::Node* symbol = ast::deep_copy(m, joint_continuation);

    ast::Function_Call_Node* call = ast::create_node_function_call(m, symbol, ast::create_node_literal_nothing(m));

    push_zero_argument(m, call);
    if (depth == 0) {
				ast::Node* addr = ast::create_node_value_address(m, ast::utils::symbol(m, "cont_ret"));
        call->push_argument(m, ast::create_node_cast_type(m, ast::create_node_type_pointer(m, ast::create_node_type_any(m)), addr));
    } else {
      call->push_argument(m, ast::utils::symbol(m, "cont_ret"));
    }

    handler::Handler_Pass_Data* hdata = stackframe::stack_frame_data_get_handler_pass_data(info->stackframe_pass_data);

    handler::pass_context_argument(hdata, m, call);

    u64 stack_depth = stackframe::stack_frame_get_function_depth(info->stackframe_pass_data, function);

    stackframe::stackframe_pass_stack_frame_paramenter(m, &stack_depth, call);

    previous = insert_cont_ret_assignment(info, m, call, return_type, previous, depth);
    previous = insert_ret_statement(info, m, call, return_type, previous, depth);
  }
}

void call_cps_conversion(
    CPS_Data* info,
    ast::Function_Literal_Node* function,
    parser::Parser* parser,
    context::Context* ctx,
    ast::ProgramPoint* program_point,
    ast::Node* call,
    ast::Node* bind,
    ast::Literal_Symbol_Node* joint,
    ast::Node* return_type,
    u64 count) {
  ast::Manager* m = parser->ast_manager;

  ast::ProgramPoint* continuation = program_point->split(m);

  if (ast::is_semantic_node(continuation)) {
    ast::Node* type = ast::deep_copy(m, return_type);

    ast::Variable_Assignment_Node* assignment = create_continuation_function(info, parser, bind, type, continuation, joint, function);

    // printf("INSERTING %lu\n", count);
    // parser::print_ast_ir(m, assignment);

    ast::Declaration_Constant_Node* declaration = ast::is_instance< ast::Declaration_Constant_Node* >(assignment->get_left_operand(m));

    program_point->set_statement(m, assignment);

    if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(bind)) {
      ast::Node* type = var->get_type(m);

      if (ast::Type_Pointer_Node* ptr = ast::is_instance< ast::Type_Pointer_Node* >(type)) {
				if(ast::is_instance<ast::Type_Any_Node*>(ptr->get_pointer_type(m)) == false) {
					type = ptr->get_pointer_type(m);
				}
      }
			
      ast::Literal_Symbol_Node* symbol = ast::is_instance< ast::Literal_Symbol_Node* >(ast::deep_copy(m, var->get_symbol(m)));

      assert(symbol);

      ast::Declaration_Variable_Node* ass_decl = ast::create_variable_declaration(m, symbol, ast::deep_copy(m, type));

      lib::insert(info->temporary_variable_declarations, ass_decl);

      ast::Variable_Assignment_Node* assignment = ast::create_node_assignment(m, ass_decl, call);

      program_point = program_point->insert(m, assignment);

      ast::Literal_Symbol_Node* cont_symb = ast::as< ast::Literal_Symbol_Node* >(ast::deep_copy(m, declaration->get_symbol(m)));

      ast::Node* cont_ret = NULL;

      if (count == 0) {
        cont_ret = ast::create_node_cast_type(m, ast::create_node_type_pointer(m, ast::create_node_type_any(m)), ast::create_node_value_address(m, ast::utils::symbol(m, "cont_ret")));
      } else {
        cont_ret = ast::utils::symbol(m, "cont_ret");
      }

      ast::Declarations_List_Node* arguments = ast::create_node_declarations_list(m, cont_ret, NULL);

      if (ast::Declaration_Variable_Node* decl = ast::is_instance< ast::Declaration_Variable_Node* >(ass_decl)) {
        ast::Literal_Symbol_Node* assi_symb = ast::as< ast::Literal_Symbol_Node* >(ast::deep_copy(m, decl->get_symbol(m)));

        arguments = ast::create_node_declarations_list(m, ast::create_node_value_address(m, assi_symb), arguments);
      }

      ast::Function_Call_Node* cont_call = ast::create_node_function_call(m, cont_symb, arguments);

      handler::Handler_Pass_Data* hdata = stackframe::stack_frame_data_get_handler_pass_data(info->stackframe_pass_data);

      handler::pass_context_argument(hdata, m, cont_call);

      u64 stack_depth = stackframe::stack_frame_get_function_depth(info->stackframe_pass_data, function);

      stackframe::stackframe_pass_stack_frame_paramenter(m, &stack_depth, cont_call);

      program_point = insert_cont_ret_assignment(info, m, cont_call, return_type, program_point, count);
      program_point = insert_ret_statement(info, m, cont_call, return_type, program_point, count);

      return;
    }

    program_point = program_point->insert(m, call);

    ast::Literal_Symbol_Node* cont_symb = ast::as< ast::Literal_Symbol_Node* >(ast::deep_copy(m, declaration->get_symbol(m)));

    ast::Function_Call_Node* cont_call = ast::create_node_function_call(m, cont_symb, ast::create_node_literal_nothing(m));

    push_zero_argument(m, cont_call);
    if (count == 0) {
			
      cont_call->push_argument(m, ast::create_node_cast_type(m, ast::create_node_type_pointer(m, ast::create_node_type_any(m)), ast::create_node_value_address(m, ast::utils::symbol(m, "cont_ret"))));
    } else {
      cont_call->push_argument(m, ast::utils::symbol(m, "cont_ret"));
    }

    handler::Handler_Pass_Data* hdata = stackframe::stack_frame_data_get_handler_pass_data(info->stackframe_pass_data);

    handler::pass_context_argument(hdata, m, cont_call);

    u64 stack_depth = stackframe::stack_frame_get_function_depth(info->stackframe_pass_data, function);

    stackframe::stackframe_pass_stack_frame_paramenter(m, &stack_depth, cont_call);

    program_point = insert_cont_ret_assignment(info, m, cont_call, return_type, program_point, count);
    program_point = insert_ret_statement(info, m, cont_call, return_type, program_point, count);

  } else {
    ast::Node* type = ast::create_node_type_any(m); // TODO: use a global context to find function and the type

    ast::Literal_Symbol_Node* cont_symb = ast::as< ast::Literal_Symbol_Node* >(ast::deep_copy(m, joint));

    ast::Function_Call_Node* cont_call = ast::create_node_function_call(m, cont_symb, ast::create_node_literal_nothing(m));

    push_zero_argument(m, cont_call);
    if (count == 0) {
      cont_call->push_argument(m, ast::create_node_cast_type(m, ast::create_node_type_pointer(m, ast::create_node_type_any(m)), ast::create_node_value_address(m, ast::utils::symbol(m, "cont_ret"))));
    } else {
      cont_call->push_argument(m, ast::utils::symbol(m, "cont_ret"));
    }

    handler::Handler_Pass_Data* hdata = stackframe::stack_frame_data_get_handler_pass_data(info->stackframe_pass_data);

    handler::pass_context_argument(hdata, m, cont_call);

    u64 stack_depth = stackframe::stack_frame_get_function_depth(info->stackframe_pass_data, function);

    stackframe::stackframe_pass_stack_frame_paramenter(m, &stack_depth, cont_call);

    program_point = insert_cont_ret_assignment(info, m, cont_call, return_type, program_point, count);
    program_point = insert_ret_statement(info, m, cont_call, return_type, program_point, count);
  }
}

void function_literal_cps_conversion(
    CPS_Data* info, parser::Parser* parser, context::Context* ctx, ast::Function_Literal_Node* function, ast::Literal_Symbol_Node* continuation_symbol) {

  ast::Manager* m = parser->ast_manager;
  ast::ProgramPoint* statements = function->get_body(m);
  ast::Type_Arrow_Node* type = ast::create_node_type_arrow(m, ast::create_node_type_any(m), ast::create_node_type_any(m));

  u64 index = lib::size(info->continuation_literals);

  symbol::Symbol cont_id = symbol::number_to_symbol(m->symbol_table, index, "c");

  if (continuation_symbol == NULL) {
    continuation_symbol = ast::create_node_literal_symbol(m, cont_id);
  }

  program_point_cps_conversion(info, function, parser, ctx, continuation_symbol, statements, NULL, function->get_return_type(m), is_continuation_closure(info, m, function));
}

ast::ProgramPoint*
shallow_insert_pops(ast::Manager* m, ast::Node* root, ast::Node* type, ast::Literal_Symbol_Node* sym, ast::Literal_Natural_Node* size, ast::ProgramPoint* point) {
  if (ast::is_semantic_node(root) == false) {
    return point;
  }

  if (ast::is_instance< ast::Function_Literal_Node* >(root)) {
    return point;
  }

  if (ast::If_Node_Statement* if_stmt = ast::is_instance< ast::If_Node_Statement* >(root)) {
    shallow_insert_pops(m, if_stmt->get_body(m), type, sym, size, if_stmt->get_body(m));
    return point;
  }

  if (ast::Elif_List_Node* elif = ast::is_instance< ast::Elif_List_Node* >(root)) {
    while (ast::is_semantic_node(elif)) {
      shallow_insert_pops(m, elif->get_if(m)->get_body(m), type, sym, size, elif->get_if(m)->get_body(m));

      elif = elif->get_elif(m);
    }

    return point;
  }

  if (ast::ProgramPoint* pp = ast::is_instance< ast::ProgramPoint* >(root)) {
    while (ast::is_semantic_node(pp)) {

      pp = shallow_insert_pops(m, pp->get_statement(m), type, sym, size, pp);

      if (ast::is_semantic_node(pp)) {
        pp = pp->get_next_program_point(m);
      }
    }

    return pp;
  }

  if (ast::Return_Node_Statement* ret = ast::is_instance< ast::Return_Node_Statement* >(root)) {
    point->set_statement(m, ast::utils::assignment(m, ast::utils::variable(m, "ret", type), ret->get_expression(m)));
    point = point->insert(m, ast::utils::call(m, "pop_frame", sym, size));
    point = point->insert(m, ast::create_node_return_statement(m, ast::utils::symbol(m, "ret")));

    return point;
  }

  point = shallow_insert_pops(m, ast::left_of(m, root), type, sym, size, point);
  point = shallow_insert_pops(m, ast::right_of(m, root), type, sym, size, point);

  return point;
}

ast::ProgramPoint* shallow_insert_rets(ast::Manager* m, ast::Node* root, ast::Node* type, ast::ProgramPoint* point) {
  if (ast::is_semantic_node(root) == false) {
    return point;
  }

  if (ast::is_instance< ast::Function_Literal_Node* >(root)) {
    return point;
  }

  if (ast::If_Node_Statement* if_stmt = ast::is_instance< ast::If_Node_Statement* >(root)) {
    shallow_insert_rets(m, if_stmt->get_body(m), type, if_stmt->get_body(m));
    return point;
  }

  if (ast::Elif_List_Node* elif = ast::is_instance< ast::Elif_List_Node* >(root)) {

    while (ast::is_semantic_node(elif)) {
      shallow_insert_rets(m, elif->get_if(m)->get_body(m), type, elif->get_if(m)->get_body(m));
      elif = elif->get_elif(m);
    }

    return point;
  }

  if (ast::ProgramPoint* pp = ast::is_instance< ast::ProgramPoint* >(root)) {
    while (ast::is_semantic_node(pp)) {

      pp = shallow_insert_rets(m, pp->get_statement(m), type, pp);

      if (ast::is_semantic_node(pp)) {
        pp = pp->get_next_program_point(m);
      }
    }

    return pp;
  }

  if (ast::Return_Node_Statement* ret = ast::is_instance< ast::Return_Node_Statement* >(root)) {
    point->set_statement(m, ast::utils::assignment(m, ast::utils::variable(m, "ret", type), ret->get_expression(m)));
    point = point->insert(m, ast::create_node_return_statement(m, ast::utils::symbol(m, "ret")));

    return point;
  }

  point = shallow_insert_rets(m, ast::left_of(m, root), type, point);
  point = shallow_insert_rets(m, ast::right_of(m, root), type, point);

  return point;
}

void insert_stack_pops(CPS_Data* data, ast::Manager* m, ast::Node* root) {
  if (ast::is_semantic_node(root) == false) {
    return;
  }

  if (ast::Function_Literal_Node* literal = ast::is_instance< ast::Function_Literal_Node* >(root)) {
    ast::Variable_Assignment_Node* stack_assignment = stackframe::stack_frame_get_function_local_stack_frame_allocation(data->stackframe_pass_data, literal);
    ast::ProgramPoint* body = literal->get_body(m);

    if (ast::is_semantic_node(stack_assignment)) {
      ast::Literal_Symbol_Node* stack_symbol = ast::utils::get_symbol(m, ast::left_of(m, stack_assignment));
      ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(ast::right_of(m, stack_assignment));
      ast::Literal_Natural_Node* size = ast::is_instance< ast::Literal_Natural_Node* >(ast::utils::get_nth_argument(m, call, 0));

      shallow_insert_pops(m, body, literal->get_return_type(m), stack_symbol, size, body);
    } else {
      shallow_insert_rets(m, body, literal->get_return_type(m), body);
    }

    return insert_stack_pops(data, m, body);
  }

  insert_stack_pops(data, m, ast::left_of(m, root));
  insert_stack_pops(data, m, ast::right_of(m, root));
}

void remove_stack_pops(CPS_Data* data, ast::Manager* m, ast::Node* root, ast::ProgramPoint* point) {
  if (ast::is_semantic_node(root) == false) {
    return;
  }

  if (ast::Function_Literal_Node* literal = ast::is_instance< ast::Function_Literal_Node* >(root)) {
    return remove_stack_pops(data, m, literal->get_body(m), point);
  }

  if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(root)) {
    if (ast::Literal_Symbol_Node* symbol = ast::is_instance< ast::Literal_Symbol_Node* >(call->get_function(m))) {
      if (symbol::is_equal(m->symbol_table, symbol->get_symbol(m), "pop_frame")) {
        point->set_statement(m, point->get_next_program_point(m)->get_statement(m));
        return ast::set_right(m, point, ast::right_of(m, point->get_next_program_point(m)));
      }
    }
  }

  if (ast::ProgramPoint* pp = ast::is_instance< ast::ProgramPoint* >(root)) {
    remove_stack_pops(data, m, pp->get_statement(m), pp);
    remove_stack_pops(data, m, pp->get_next_program_point(m), pp->get_next_program_point(m));
    return;
  }

  if (ast::Function_Literal_Node* lit = ast::is_instance< ast::Function_Literal_Node* >(root)) {
    remove_stack_pops(data, m, lit->get_body(m), lit->get_body(m));
    return;
  }

  remove_stack_pops(data, m, ast::left_of(m, root), point);
  remove_stack_pops(data, m, ast::right_of(m, root), point);
}

void convert_to_cps_style_pass(CPS_Data* info, parser::Parser* parser, ast::Node* root) {
  if (!ast::is_semantic_node(root)) {
    return;
  }

  ast::Manager* m = parser->ast_manager;

  if (ast::Function_Literal_Node* lit = ast::is_instance< ast::Function_Literal_Node* >(root)) {
    context::Context* ctx = context::context_create(NULL);
    function_literal_cps_conversion(info, parser, ctx, ast::as< ast::Function_Literal_Node* >(root), NULL);
    context::context_destroy(ctx);
  } else {
    convert_to_cps_style_pass(info, parser, ast::left_of(m, root));
    convert_to_cps_style_pass(info, parser, ast::right_of(m, root));
  }
}

void convert_to_cps_style(CPS_Data* info, parser::Parser* parser, ast::Node* root) {
  convert_to_cps_style_pass(info, parser, root);

  ast::ProgramPoint* pp = ast::is_instance< ast::ProgramPoint* >(root);

  parser::print_ast_ir(parser->ast_manager, root);

  remove_stack_pops(info, parser->ast_manager, root, pp);
  insert_stack_pops(info, parser->ast_manager, root);
}

} // namespace cps
