#include "continuations.hpp"
#include "ast/ast_control_flow.hpp"
#include "ast/ast_declaration.hpp"
#include "ast/ast_function.hpp"
#include "ast/ast_kind.hpp"
#include "ast/ast_literals.hpp"
#include "ast/ast_manager.hpp"
#include "ast/ast_operations.hpp"
#include "ast/ast_program_point.hpp"
#include "compiler/compiler.hpp"
#include "compiler/symbol_table.hpp"
#include "lib/set.hpp"
#include "parser/parser.hpp"

#include <assert.h>
#include <stdio.h>

using namespace compiler;
namespace cps {

struct CPS_Data {
  lib::Table< compiler::symbol::Id, ast::Declaration_Constant_Node* >* continuation_literals;
  // lib::Table< compiler::symbol::Id, ast::Id >* continuation_arguments;
};

void call_cps_conversion(CPS_Data* info, compiler::Compiler* compiler, ast::ProgramPoint_List_Node* program_point, ast::Node* call, ast::Node* bind, ast::Node* joint);

void function_literal_cps_conversion(CPS_Data* info, compiler::Compiler* compiler, ast::Function_Literal_Node* function, ast::Literal_Symbol_Node* continuation);

void replace_return_call(CPS_Data* info, Compiler* compiler, ast::Node* node, ast::Literal_Symbol_Node* func) {

  if (node == 0 || ast::is_instance< ast::Literal_Nothing_Node* >(node)) {
    return;
  }

  ast::Manager* manager = compiler->parser->ast_manager;

  if (ast::Return_Node_Statement* ret = ast::is_instance< ast::Return_Node_Statement* >(node)) {
    ast::Node* expression = ret->get_expression(manager);
    ast::Node* symbol     = ast::deep_copy(manager, func);

    ast::Declarations_List_Node* args = ast::create_node_declarations_list(manager, expression, 0);
    ast::Function_Call_Node*     call = ast::create_node_function_call(manager, symbol, args);

    ast::replace(ret, call);

    ast::manager_pop(manager, call);
  }

  replace_return_call(info, compiler, ast::left_of(manager, node), func);
  replace_return_call(info, compiler, ast::right_of(manager, node), func);
}

ast::Variable_Assignment_Node*
create_continuation_function(CPS_Data* info, Compiler* compiler, ast::Node* argument, ast::Node* return_type, ast::ProgramPoint_List_Node* body, ast::Node* joint) {

  ast::Manager* m = compiler->parser->ast_manager;

  // NOTE(marcos): If the binding dosent have a type set it to be 'any',
  // this is just a quick patch since we dont have type check and
  // inference yet.

  if (ast::is_semantic_node(argument)) {
    if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(argument)) {

      ast::Node* type = var->get_type(m);

      assert(ast::is_semantic_node(type));
    }
  }

  ast::Node* arguments = ast::create_node_literal_nothing(m);

  if (ast::is_semantic_node(argument)) {
    arguments = ast::create_node_declarations_list(m, argument, NULL);
  }

  ast::Node* ty = ast::create_node_type_any(m);

  ast::Node* function = ast::create_node_function_literal(m, arguments, ty, body);

  symbol::Symbol sym = symbol::number_to_symbol(m->symbol_table, lib::size(info->continuation_literals), "c");

  ast::Literal_Symbol_Node* symbol = ast::create_node_literal_symbol(m, sym);

  // TODO: infer type from 'arguments' and 'return_type'
  ast::Node* type = ast::create_node_type_arrow(m, ast::create_node_type_any(m), ast::create_node_type_any(m));

  ast::Declaration_Constant_Node* declaration = ast::create_constant_declaration(m, symbol, type);

  ast::Variable_Assignment_Node* assignment = ast::create_node_assignment(m, declaration, function);

  lib::insert(info->continuation_literals, symbol->get_symbol_id(), declaration);

  return assignment;
}

void function_literal_assignment_to_constant_declaration(CPS_Data* info, Compiler* compiler, ast::Node* literal, ast::Node* assignment, ast::ProgramPoint_List_Node* point) {
  ast::Manager* m = compiler->parser->ast_manager;

  // Promote function declaration to constant
  ast::Literal_Symbol_Node* symbol = ast::create_node_literal_symbol(m, symbol::number_to_symbol(m->symbol_table, lib::size(info->continuation_literals), "c"));

  ast::Node* type = ast::create_node_type_any(m); // TODO(marcos): infer function type

  ast::Declaration_Variable_Node* decl = ast::create_variable_declaration(m, symbol, type);
  ast::Variable_Assignment_Node*  func = ast::create_node_assignment(m, decl, literal);

  ast::Variable_Assignment_Node* statement = ast::is_instance< ast::Variable_Assignment_Node* >(assignment);

  assert(statement);

  statement->set_right_operand(m, ast::deep_copy(m, symbol));

  point->set_statement(m, func);

  point->insert(m, statement);
}

void program_point_cps_conversion(
    CPS_Data* info, Compiler* compiler, ast::Literal_Symbol_Node* cont_symbol, ast::ProgramPoint_List_Node* statements, ast::Literal_Symbol_Node* joint_continuation) {

  ast::Manager* m = compiler->parser->ast_manager;

  ast::ProgramPoint_List_Node* previous = NULL;
  while (ast::is_semantic_node(statements)) {
    ast::Node* statement = statements->get_statement(m);

    ast::ProgramPoint_List_Node* continuation = statements->get_next_program_point(m);

    if (ast::Variable_Assignment_Node* var = ast::is_instance< ast::Variable_Assignment_Node* >(statement)) {
      ast::Node* right = var->get_right_operand(m);

      if (ast::Function_Literal_Node* lit = ast::is_instance< ast::Function_Literal_Node* >(right)) {
        function_literal_cps_conversion(info, compiler, lit, NULL);
        function_literal_assignment_to_constant_declaration(info, compiler, right, statement, statements);
      }

      if (ast::Effect_Call_Node* call = ast::is_instance< ast::Effect_Call_Node* >(right)) {
        ast::Node* left = var->get_left_operand(m);

        if (!ast::is_instance< ast::Declaration_Variable_Node* >(left)) {

          if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
            left = ast::create_variable_declaration(m, var->get_symbol(m), var->get_type(m));
          }
        }

        program_point_cps_conversion(info, compiler, cont_symbol, continuation, joint_continuation);

        return call_cps_conversion(info, compiler, statements, call, left, joint_continuation);
      }

      if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(right)) {
        ast::Node* left = var->get_left_operand(m);

        if (!ast::is_instance< ast::Declaration_Variable_Node* >(left)) {

          if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
            left = ast::create_variable_declaration(m, var->get_symbol(m), var->get_type(m));
          }
        }

        program_point_cps_conversion(info, compiler, cont_symbol, continuation, joint_continuation);

        return call_cps_conversion(info, compiler, statements, call, left, joint_continuation);
      }
    }

    if (ast::Effect_Call_Node* call = ast::is_instance< ast::Effect_Call_Node* >(statement)) {
      program_point_cps_conversion(info, compiler, cont_symbol, continuation, joint_continuation);
      return call_cps_conversion(info, compiler, statements, call, NULL, joint_continuation);
    }

    if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(statement)) {
      program_point_cps_conversion(info, compiler, cont_symbol, continuation, joint_continuation);
      return call_cps_conversion(info, compiler, statements, call, NULL, joint_continuation);
    }

    if (ast::Elif_List_Node* elif = ast::is_instance< ast::Elif_List_Node* >(statement)) {
      ast::Literal_Symbol_Node* old_joint = joint_continuation;

      ast::Variable_Assignment_Node* joint_continuation =
          create_continuation_function(info, compiler, ast::create_node_literal_nothing(m), ast::create_node_type_any(m), statements->split(m), joint_continuation);

      ast::Declaration_Constant_Node* joint_declaration = ast::as< ast::Declaration_Constant_Node* >(joint_continuation->get_left_operand(m));

      ast::Literal_Symbol_Node* joint_symbol = joint_declaration->get_symbol(m);

      ast::Function_Literal_Node* joint_literal = ast::is_instance< ast::Function_Literal_Node* >(joint_continuation->get_right_operand(m));

      ast::ProgramPoint_List_Node* joint_body = joint_literal->get_body(m);

      program_point_cps_conversion(info, compiler, cont_symbol, joint_body, old_joint);

      statements->set_statement(m, joint_continuation);

      ast::Elif_List_Node* branch = elif;

      while (ast::is_semantic_node(elif)) {
        ast::If_Node_Statement* if_statement = elif->get_if(m);

        ast::ProgramPoint_List_Node* body = if_statement->get_body(m);

        program_point_cps_conversion(info, compiler, cont_symbol, body, joint_symbol);

        elif = elif->get_elif(m);
      }

      statements = statements->insert(m, branch);

      ast::Function_Call_Node* call = ast::create_node_function_call(m, joint_symbol, ast::create_node_literal_nothing(m));

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

    ast::Node*               symbol = ast::deep_copy(m, joint_continuation);
    ast::Function_Call_Node* call   = ast::create_node_function_call(m, symbol, ast::create_node_literal_nothing(m));

    previous->insert(m, ast::create_node_return_statement(m, call));
  }
}

void call_cps_conversion(CPS_Data* info, Compiler* compiler, ast::ProgramPoint_List_Node* program_point, ast::Node* call, ast::Node* bind, ast::Node* joint) {

  ast::Manager* m = compiler->parser->ast_manager;

  ast::ProgramPoint_List_Node* continuation = program_point->split(m);

  if (ast::is_semantic_node(continuation)) {
    ast::Node* type = ast::create_node_type_any(m); // TODO: use a global context to find function and the type

    ast::Variable_Assignment_Node* assignment = NULL;

    if (ast::is_declaration_node(bind)) {
      assignment = create_continuation_function(info, compiler, bind, type, continuation, joint);
    } else {
      assignment = create_continuation_function(info, compiler, NULL, type, continuation, joint);
    }

    ast::Declaration_Constant_Node* declaration = ast::is_instance< ast::Declaration_Constant_Node* >(assignment->get_left_operand(m));

    program_point->set_statement(m, assignment);

    if (ast::is_semantic_node(bind)) {
      ast::Variable_Assignment_Node* assignment = ast::create_node_assignment(m, ast::deep_copy(m, bind), call);

      program_point = program_point->insert(m, assignment);

      ast::Literal_Symbol_Node* cont_symb = ast::as< ast::Literal_Symbol_Node* >(ast::deep_copy(m, declaration->get_symbol(m)));

      ast::Node* arguments = ast::create_node_literal_nothing(m);

      if (ast::Declaration_Variable_Node* decl = ast::is_instance< ast::Declaration_Variable_Node* >(bind)) {
        ast::Literal_Symbol_Node* assi_symb = ast::as< ast::Literal_Symbol_Node* >(ast::deep_copy(m, decl->get_symbol(m)));

        arguments = ast::create_node_declarations_list(m, assi_symb, NULL);
      }

      if (ast::Declaration_Constant_Node* decl = ast::is_instance< ast::Declaration_Constant_Node* >(bind)) {
        ast::Literal_Symbol_Node* assi_symb = ast::as< ast::Literal_Symbol_Node* >(ast::deep_copy(m, decl->get_symbol(m)));

        arguments = ast::create_node_declarations_list(m, assi_symb, NULL);
      }

      ast::Function_Call_Node* cont_call = ast::create_node_function_call(m, cont_symb, arguments);

      program_point = program_point->insert(m, ast::create_node_return_statement(m, cont_call));
    } else {
      program_point = program_point->insert(m, call);

      ast::Literal_Symbol_Node* cont_symb = ast::as< ast::Literal_Symbol_Node* >(ast::deep_copy(m, declaration->get_symbol(m)));

      ast::Function_Call_Node* cont_call = ast::create_node_function_call(m, cont_symb, ast::create_node_literal_nothing(m));

      program_point = program_point->insert(m, ast::create_node_return_statement(m, cont_call));
    }
  }
}

void function_literal_cps_conversion(CPS_Data* info, Compiler* compiler, ast::Function_Literal_Node* function, ast::Literal_Symbol_Node* continuation_symbol) {

  ast::Manager*                m          = compiler->parser->ast_manager;
  ast::ProgramPoint_List_Node* statements = function->get_body(m);
  ast::Type_Arrow_Node*        type       = ast::create_node_type_arrow(m, ast::create_node_type_any(m), ast::create_node_type_any(m));

  u64 index = lib::size(info->continuation_literals);

  symbol::Symbol cont_id = symbol::number_to_symbol(m->symbol_table, index, "c");

  if (continuation_symbol == NULL) {
    continuation_symbol = ast::create_node_literal_symbol(m, cont_id);
  }

  // ast::Declaration_Variable_Node* bind = ast::create_variable_declaration(m, continuation_symbol, type);

  // lib::insert(info->continuation_arguments, continuation_symbol->get_symbol_id(), bind->id);

  // function->push_argument(m, bind);

  // replace_return_call(info, compiler, statements, continuation_symbol);

  program_point_cps_conversion(info, compiler, continuation_symbol, statements, NULL);
}

void convert_to_cps_style(CPS_Data* info, Compiler* compiler, ast::Node* root) {
  if (!ast::is_semantic_node(root)) {
    return;
  }

  ast::Manager* m = compiler->parser->ast_manager;

  if (ast::is_instance< ast::Function_Literal_Node* >(root)) {
    return function_literal_cps_conversion(info, compiler, ast::as< ast::Function_Literal_Node* >(root), NULL);
  } else {
    convert_to_cps_style(info, compiler, ast::left_of(m, root));
    convert_to_cps_style(info, compiler, ast::right_of(m, root));
  }
}

// void cps_restore_call_assignments(Conversion_Result* info, Compiler* compiler, ast::Node* root) {
//   if (!ast::is_semantic_node(root)) {
//     return;
//   }

//   ast::Manager* m = compiler->parser->ast_manager;

//   if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(root)) {
// 		if(ast::Literal_Symbol_Node* func =
// ast::is_instance<ast::Literal_Symbol_Node*>(call->get_function(m))) {
// 			if(lib::search(info->continuation_arguments, func->get_symbol_id()) != NULL) {
// 				call->
// 			}
// 		}
//   } else {
//     cps_restore_call_assignments(info, compiler, ast::left_of(m, root));
//     cps_restore_call_assignments(info, compiler, ast::right_of(m, root));
//   }
// }

CPS_Data* cps_result_create() {
  CPS_Data* info = new CPS_Data();

  // info->continuation_arguments = lib::table_create< compiler::symbol::Id, ast::Id >();
  info->continuation_literals = lib::table_create< compiler::symbol::Id, ast::Declaration_Constant_Node* >();

  return info;
}

void cps_result_destroy(CPS_Data* info) {
  // lib::table_delete(info->continuation_arguments);
  lib::table_delete(info->continuation_literals);

  delete info;
}

b8 is_continuation_closure(CPS_Data* info, ast::Manager* m, ast::Literal_Symbol_Node* decl) {
  return lib::search(info->continuation_literals, decl->get_symbol_id()) != NULL;
}

} // namespace cps
