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
#include "compiler/compiler.hpp"
#include "compiler/symbol_table.hpp"
#include "context/context.hpp"
#include "continuations/handler.hpp"
#include "lib/set.hpp"
#include "parser/parser.hpp"

#include <assert.h>
#include <stdio.h>

using namespace compiler;
namespace cps {

struct CPS_Data {
  u64 temporaries;

  handler::Handler_Pass_Data* handler_pass_data;

  lib::Set< ast::Declaration_Variable_Node* >* temporary_variable_declarations;
  lib::Set< ast::Declaration_Constant_Node* >* temporary_constant_declarations;

  lib::Table< compiler::symbol::Id, ast::Declaration_Constant_Node* >*        continuation_literals;
  lib::Table< ast::Function_Literal_Node*, ast::Declaration_Constant_Node* >* continuation_declaration;
};

CPS_Data* cps_data_create(handler::Handler_Pass_Data* data) {
  CPS_Data* info = new CPS_Data();

  info->temporaries       = 0;
  info->handler_pass_data = data;

  // info->continuation_arguments = lib::table_create< compiler::symbol::Id, ast::Id >();
  info->continuation_literals    = lib::table_create< compiler::symbol::Id, ast::Declaration_Constant_Node* >();
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

  handler::handler_pass_data_destroy(info->handler_pass_data);

  delete info;
}

handler::Handler_Pass_Data* cps_data_get_handler_data(CPS_Data* data) {
  return data->handler_pass_data;
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

void call_cps_conversion(CPS_Data*, parser::Parser*, context::Context*, ast::ProgramPoint_List_Node*, ast::Node*, ast::Node*, ast::Literal_Symbol_Node*, ast::Node*);

void function_literal_cps_conversion(CPS_Data*, parser::Parser*, context::Context*, ast::Function_Literal_Node*, ast::Literal_Symbol_Node*);

void push_zero_argument(ast::Manager* m, ast::Function_Call_Node* call) {
  call->push_argument(m, ast::create_node_literal_natural(m, symbol::number_to_symbol(m->symbol_table, 0)));
}

void replace_return_call(CPS_Data* info, parser::Parser* parser, ast::Node* node, ast::Literal_Symbol_Node* func) {

  if (node == 0 || ast::is_instance< ast::Literal_Nothing_Node* >(node)) {
    return;
  }

  ast::Manager* manager = parser->ast_manager;

  if (ast::Return_Node_Statement* ret = ast::is_instance< ast::Return_Node_Statement* >(node)) {
    ast::Node* expression = ret->get_expression(manager);
    ast::Node* symbol     = ast::deep_copy(manager, func);

    ast::Declarations_List_Node* args = ast::create_node_declarations_list(manager, expression, 0);
    ast::Function_Call_Node*     call = ast::create_node_function_call(manager, symbol, args);

    ast::replace(ret, call);

    ast::manager_pop(manager, call);
  }

  replace_return_call(info, parser, ast::left_of(manager, node), func);
  replace_return_call(info, parser, ast::right_of(manager, node), func);
}

ast::Variable_Assignment_Node*
create_continuation_function(CPS_Data* info, parser::Parser* parser, ast::Node* argument, ast::Node* return_type, ast::ProgramPoint_List_Node* body, ast::Node* joint) {

  ast::Manager* m = parser->ast_manager;

  ast::Node* arguments = ast::create_node_literal_nothing(m);

  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(argument)) {
    arguments = ast::create_node_declarations_list(m, var, NULL);
  } else if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(argument)) {
    arguments = ast::create_node_declarations_list(m, var, NULL);
  } else {
    ast::Literal_Symbol_Node*       s = ast::create_node_literal_symbol(m, symbol::set_entry(m->symbol_table, "_"));
    ast::Type_Pointer_Node*         t = ast::create_node_type_pointer(m, ast::create_node_type_any(m));
    ast::Declaration_Variable_Node* v = ast::create_variable_declaration(m, s, t);

    lib::insert(info->temporary_variable_declarations, v);

    arguments = ast::create_node_declarations_list(m, v, NULL);
  }

  ast::Function_Literal_Node* function = ast::create_node_function_literal(m, arguments, return_type, body);

  symbol::Symbol sym = symbol::number_to_symbol(m->symbol_table, lib::size(info->continuation_literals), "c");

  ast::Literal_Symbol_Node* symbol = ast::create_node_literal_symbol(m, sym);

  // TODO: infer type from 'arguments' and 'return_type'

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

  handler::add_context_argument(info->handler_pass_data, m, function, declaration);

  lib::insert(info->continuation_literals, symbol->get_symbol_id(), declaration);

  lib::insert(info->continuation_declaration, function, declaration);

  return assignment;
}

void function_literal_assignment_to_constant_declaration(
    CPS_Data* info, parser::Parser* parser, ast::Node* declaration, ast::Node* literal, ast::Node* assignment, ast::ProgramPoint_List_Node* point) {
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
  ast::Variable_Assignment_Node*  func = ast::create_node_assignment(m, decl, literal);

  ast::Variable_Assignment_Node* statement = ast::is_instance< ast::Variable_Assignment_Node* >(assignment);

  lib::insert(info->temporary_constant_declarations, decl);

  assert(statement);

  statement->set_right_operand(m, ast::deep_copy(m, symbol));

  point->set_statement(m, func);

  point->insert(m, statement);
}

void program_point_cps_conversion(
    CPS_Data*                    info,
    parser::Parser*              parser,
    context::Context*            ctx,
    ast::Literal_Symbol_Node*    cont_symbol,
    ast::ProgramPoint_List_Node* statements,
    ast::Literal_Symbol_Node*    joint_continuation,
    ast::Node*                   return_type) {

  ast::Manager* m = parser->ast_manager;

  ast::ProgramPoint_List_Node* previous = NULL;

  while (ast::is_semantic_node(statements)) {
    ast::Node* statement = statements->get_statement(m);

    ast::ProgramPoint_List_Node* continuation = statements->get_next_program_point(m);

    if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(statement)) {
      context::context_declare(ctx, m, var);
    }

    if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(statement)) {
      context::context_declare(ctx, m, var);
    }

    if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(statement)) {
      ast::Node* right = assignment->get_right_operand(m);
      ast::Node* left  = assignment->get_left_operand(m);

      b8 mut = false;

      if (ast::Declaration_Constant_Node* v = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
        context::context_declare(ctx, m, v);
      }

      if (ast::Declaration_Variable_Node* v = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
        mut = true;
        context::context_declare(ctx, m, v);
      }

      if (ast::Function_Literal_Node* lit = ast::is_instance< ast::Function_Literal_Node* >(right)) {
        function_literal_cps_conversion(info, parser, ctx, lit, NULL);

        if (mut) {
          function_literal_assignment_to_constant_declaration(info, parser, left, right, statement, statements);
        }
      }

      if (ast::Effect_Call_Node* call = ast::is_instance< ast::Effect_Call_Node* >(right)) {
        ast::Literal_Symbol_Node* temp_symbol = ast::create_node_literal_symbol(m, symbol::number_to_symbol(m->symbol_table, info->temporaries, "t"));

        info->temporaries = info->temporaries + 1;

        ast::Node* temp_type = NULL;

        if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
          temp_type = var->get_type(m);
          // bind = ast::create_variable_declaration(m, var->get_symbol(m), var->get_type(m));
        }

        if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
          temp_type = var->get_type(m);
          // bind = ast::create_variable_declaration(m, var->get_symbol(m), var->get_type(m));
        }

        if (ast::Literal_Symbol_Node* symbol = ast::is_instance< ast::Literal_Symbol_Node* >(left)) {
          temp_type = context::context_type_of(ctx, m, symbol->get_symbol_id());
        }

        assert(temp_type);

        statements->insert(m, ast::create_node_assignment(m, left, ast::create_node_pointer_value(m, ast::deep_copy(m, temp_symbol))));

        ast::Literal_Symbol_Node*       s        = ast::as< ast::Literal_Symbol_Node* >(ast::deep_copy(m, temp_symbol));
        ast::Type_Pointer_Node*         p        = ast::create_node_type_pointer(m, ast::deep_copy(m, temp_type));
        ast::Declaration_Variable_Node* temp_arg = ast::create_variable_declaration(m, s, p);

        lib::insert(info->temporary_variable_declarations, temp_arg);

        call_cps_conversion(info, parser, ctx, statements, call, temp_arg, joint_continuation, return_type);

        return program_point_cps_conversion(info, parser, ctx, cont_symbol, continuation, joint_continuation, return_type);
      }

      if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(right)) {
        ast::Literal_Symbol_Node* temp_symbol = ast::create_node_literal_symbol(m, symbol::number_to_symbol(m->symbol_table, info->temporaries, "t"));

        info->temporaries = info->temporaries + 1;

        ast::Node* temp_type = NULL;

        if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
          temp_type = var->get_type(m);
          // bind = ast::create_variable_declaration(m, var->get_symbol(m), var->get_type(m));
        }

        if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
          temp_type = var->get_type(m);
          // bind = ast::create_variable_declaration(m, var->get_symbol(m), var->get_type(m));
        }

        if (ast::Literal_Symbol_Node* symbol = ast::is_instance< ast::Literal_Symbol_Node* >(left)) {
          temp_type = context::context_type_of(ctx, m, symbol->get_symbol_id());
        }

        assert(temp_type);

        statements->insert(m, ast::create_node_assignment(m, left, ast::create_node_pointer_value(m, ast::deep_copy(m, temp_symbol))));

        ast::Literal_Symbol_Node*       s        = ast::as< ast::Literal_Symbol_Node* >(ast::deep_copy(m, temp_symbol));
        ast::Type_Pointer_Node*         p        = ast::create_node_type_pointer(m, ast::deep_copy(m, temp_type));
        ast::Declaration_Variable_Node* temp_arg = ast::create_variable_declaration(m, s, p);

        lib::insert(info->temporary_variable_declarations, temp_arg);

        call_cps_conversion(info, parser, ctx, statements, call, temp_arg, joint_continuation, return_type);

        return program_point_cps_conversion(info, parser, ctx, cont_symbol, continuation, joint_continuation, return_type);
      }
    }

    if (ast::Effect_Call_Node* call = ast::is_instance< ast::Effect_Call_Node* >(statement)) {
      program_point_cps_conversion(info, parser, ctx, cont_symbol, continuation, joint_continuation, return_type);
      return call_cps_conversion(info, parser, ctx, statements, call, NULL, joint_continuation, return_type);
    }

    if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(statement)) {
      program_point_cps_conversion(info, parser, ctx, cont_symbol, continuation, joint_continuation, return_type);
      return call_cps_conversion(info, parser, ctx, statements, call, NULL, joint_continuation, return_type);
    }

    if (ast::Elif_List_Node* elif = ast::is_instance< ast::Elif_List_Node* >(statement)) {

      ast::Literal_Symbol_Node* old_joint = joint_continuation;

      ast::Variable_Assignment_Node* joint_continuation =
          create_continuation_function(info, parser, ast::create_node_literal_nothing(m), return_type, statements->split(m), joint_continuation);

      ast::Declaration_Constant_Node* joint_declaration = ast::as< ast::Declaration_Constant_Node* >(joint_continuation->get_left_operand(m));

      ast::Literal_Symbol_Node* joint_symbol = joint_declaration->get_symbol(m);

      ast::Function_Literal_Node* joint_literal = ast::is_instance< ast::Function_Literal_Node* >(joint_continuation->get_right_operand(m));

      ast::ProgramPoint_List_Node* joint_body = joint_literal->get_body(m);

      program_point_cps_conversion(info, parser, ctx, cont_symbol, joint_body, old_joint, return_type);

      statements->set_statement(m, joint_continuation);

      ast::Elif_List_Node* branch = elif;

      while (ast::is_semantic_node(elif)) {
        ast::If_Node_Statement*      if_statement = elif->get_if(m);
        ast::ProgramPoint_List_Node* body         = if_statement->get_body(m);

        program_point_cps_conversion(info, parser, ctx, cont_symbol, body, joint_symbol, return_type);

        elif = elif->get_elif(m);
      }

      statements = statements->insert(m, branch);

      ast::Function_Call_Node* call = ast::create_node_function_call(m, joint_symbol, ast::create_node_literal_nothing(m));

      push_zero_argument(m, call);

      handler::pass_context_argument(info->handler_pass_data, m, call);

      statements = statements->insert(m, ast::create_node_return_statement(m, call));
    }

    previous   = statements;
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

    handler::pass_context_argument(info->handler_pass_data, m, call);

    previous->insert(m, ast::create_node_return_statement(m, call));
  }
}

void call_cps_conversion(
    CPS_Data*                    info,
    parser::Parser*              parser,
    context::Context*            ctx,
    ast::ProgramPoint_List_Node* program_point,
    ast::Node*                   call,
    ast::Node*                   bind,
    ast::Literal_Symbol_Node*    joint,
    ast::Node*                   return_type) {
  ast::Manager* m = parser->ast_manager;

  ast::ProgramPoint_List_Node* continuation = program_point->split(m);

  if (ast::is_semantic_node(continuation)) {
    ast::Node* type = ast::deep_copy(m, return_type);

    ast::Variable_Assignment_Node* assignment = NULL;

    assignment = create_continuation_function(info, parser, bind, type, continuation, joint);

    ast::Declaration_Constant_Node* declaration = ast::is_instance< ast::Declaration_Constant_Node* >(assignment->get_left_operand(m));

    program_point->set_statement(m, assignment);

    if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(bind)) {
      ast::Node* type = var->get_type(m);

      if (ast::Type_Pointer_Node* ptr = ast::is_instance< ast::Type_Pointer_Node* >(type)) {
        type = ptr->get_pointer_type(m);
      }

      ast::Literal_Symbol_Node* symbol = ast::is_instance< ast::Literal_Symbol_Node* >(ast::deep_copy(m, var->get_symbol(m)));

      assert(symbol);

      ast::Declaration_Variable_Node* ass_decl = ast::create_variable_declaration(m, symbol, ast::deep_copy(m, type));

      lib::insert(info->temporary_variable_declarations, ass_decl);

      ast::Variable_Assignment_Node* assignment = ast::create_node_assignment(m, ass_decl, call);

      program_point = program_point->insert(m, assignment);

      ast::Literal_Symbol_Node* cont_symb = ast::as< ast::Literal_Symbol_Node* >(ast::deep_copy(m, declaration->get_symbol(m)));

      ast::Node* arguments = ast::create_node_literal_nothing(m);

      if (ast::Declaration_Variable_Node* decl = ast::is_instance< ast::Declaration_Variable_Node* >(ass_decl)) {
        ast::Literal_Symbol_Node* assi_symb = ast::as< ast::Literal_Symbol_Node* >(ast::deep_copy(m, decl->get_symbol(m)));

        arguments = ast::create_node_declarations_list(m, ast::create_node_value_address(m, assi_symb), NULL);
      }

      ast::Function_Call_Node* cont_call = ast::create_node_function_call(m, cont_symb, arguments);

      handler::pass_context_argument(info->handler_pass_data, m, cont_call);

      program_point->insert(m, ast::create_node_return_statement(m, cont_call));

      return;
    }

    program_point = program_point->insert(m, call);

    ast::Literal_Symbol_Node* cont_symb = ast::as< ast::Literal_Symbol_Node* >(ast::deep_copy(m, declaration->get_symbol(m)));

    ast::Function_Call_Node* cont_call = ast::create_node_function_call(m, cont_symb, ast::create_node_literal_nothing(m));

    push_zero_argument(m, cont_call);

    handler::pass_context_argument(info->handler_pass_data, m, cont_call);

    program_point = program_point->insert(m, ast::create_node_return_statement(m, cont_call));

  } else {
    ast::Node* type = ast::create_node_type_any(m); // TODO: use a global context to find function and the type

    ast::Literal_Symbol_Node* cont_symb = ast::as< ast::Literal_Symbol_Node* >(ast::deep_copy(m, joint));

    ast::Function_Call_Node* cont_call = ast::create_node_function_call(m, cont_symb, ast::create_node_literal_nothing(m));

    push_zero_argument(m, cont_call);

    handler::pass_context_argument(info->handler_pass_data, m, cont_call);

    program_point = program_point->insert(m, ast::create_node_return_statement(m, cont_call));
  }
}

void function_literal_cps_conversion(
    CPS_Data* info, parser::Parser* parser, context::Context* ctx, ast::Function_Literal_Node* function, ast::Literal_Symbol_Node* continuation_symbol) {

  ast::Manager*                m          = parser->ast_manager;
  ast::ProgramPoint_List_Node* statements = function->get_body(m);
  ast::Type_Arrow_Node*        type       = ast::create_node_type_arrow(m, ast::create_node_type_any(m), ast::create_node_type_any(m));

  u64 index = lib::size(info->continuation_literals);

  symbol::Symbol cont_id = symbol::number_to_symbol(m->symbol_table, index, "c");

  if (continuation_symbol == NULL) {
    continuation_symbol = ast::create_node_literal_symbol(m, cont_id);
  }

  program_point_cps_conversion(info, parser, ctx, continuation_symbol, statements, NULL, function->get_return_type(m));
}

void convert_to_cps_style(CPS_Data* info, parser::Parser* parser, ast::Node* root) {
  if (!ast::is_semantic_node(root)) {
    return;
  }

  ast::Manager* m = parser->ast_manager;

  if (ast::is_instance< ast::Function_Literal_Node* >(root)) {
    context::Context* ctx = context::context_create(NULL);
    function_literal_cps_conversion(info, parser, ctx, ast::as< ast::Function_Literal_Node* >(root), NULL);
    context::context_destroy(ctx);
  } else {
    convert_to_cps_style(info, parser, ast::left_of(m, root));
    convert_to_cps_style(info, parser, ast::right_of(m, root));
  }
}

} // namespace cps
