#include "handler.hpp"
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

#include "compiler/symbol_table.hpp"
#include "context/context.hpp"
#include "lexer.hpp"
#include "lib/set.hpp"
#include "lib/table.hpp"
#include "parser/parser.hpp"
#include <cassert>
#include <cstdio>
#include <cstdlib>

using namespace compiler;
using namespace symbol;

namespace handler {

struct Handler_Pass_Data {
  u64 yielding_calls;
  u64 temporaries;
  lib::Set< ast::Function_Literal_Node* >* prompt_functions;
  lib::Set< ast::Function_Call_Node* >* prompt_site;
  lib::Set< ast::Function_Call_Node* >* prompt_calls;
  lib::Table< symbol::Id, u64 >* hashes;
  lib::Set< ast::Declaration_Variable_Node* >* context_declarations;
  lib::Table< ast::Function_Literal_Node*, ast::Literal_Symbol_Node* >* ret_var_map;
  lib::Table< symbol::Id, ast::Literal_Handler_Node* >* symbol_to_handler;
  lib::Table< symbol::Id, ast::Variable_Assignment_Node* >* symbol_to_struct_arguments;
};

b8 is_prompt_site(Handler_Pass_Data* data, ast::Function_Call_Node* call) {
  return lib::search(data->prompt_site, call);
}
	
b8 is_prompt_call(Handler_Pass_Data* data, ast::Function_Call_Node* call) {
  return lib::search(data->prompt_calls, call);
}

b8 handler_pass_data_is_context_argument(Handler_Pass_Data* data, ast::Declaration_Variable_Node* decl) {
  return lib::search(data->context_declarations, decl) != NULL;
}

b8 handler_pass_data_is_context_argument(Handler_Pass_Data* data, ast::Declaration_Constant_Node* decl) {
  return false;
}

Handler_Pass_Data* handler_pass_data_create() {
  Handler_Pass_Data* h = new Handler_Pass_Data();

  h->prompt_functions = lib::set_create< ast::Function_Literal_Node* >();
  h->yielding_calls = 0;
  h->temporaries = 0;
  h->hashes = lib::table_create< symbol::Id, u64 >();
  h->ret_var_map = lib::table_create< ast::Function_Literal_Node*, ast::Literal_Symbol_Node* >();
  h->context_declarations = lib::set_create< ast::Declaration_Variable_Node* >();
  h->symbol_to_handler = lib::table_create< symbol::Id, ast::Literal_Handler_Node* >();
  h->symbol_to_struct_arguments = lib::table_create< symbol::Id, ast::Variable_Assignment_Node* >();
  h->prompt_site = lib::set_create< ast::Function_Call_Node* >();
  h->prompt_calls = lib::set_create< ast::Function_Call_Node* >();

  return h;
}

void handler_pass_data_destroy(Handler_Pass_Data* d) {
	
  lib::table_delete(d->hashes);
  lib::table_delete(d->ret_var_map);
  lib::set_delete(d->context_declarations);
  lib::table_delete(d->symbol_to_handler);
  lib::table_delete(d->symbol_to_struct_arguments);
  lib::set_delete(d->prompt_functions);
  lib::set_delete(d->prompt_site);
  lib::set_delete(d->prompt_calls);

  delete d;
}

b8 is_prompt_function(Handler_Pass_Data* data, ast::Function_Literal_Node* lit) {
  if (lib::search(data->prompt_functions, lit)) {
    return true;
  }

  return false;
}

void add_context_argument(Handler_Pass_Data* data, ast::Manager* m, ast::Function_Literal_Node* f, ast::Node* decl) {
  ast::Type_Evidence_Context_Node* ctx_type = ast::create_node_type_evidence_context(m);
  ast::Literal_Symbol_Node* ctx_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "ctx"));

  ast::Declaration_Variable_Node* ctx_arg = ast::create_variable_declaration(m, ctx_symbol, ast::create_node_type_pointer(m, ctx_type));

  lib::insert(data->context_declarations, ctx_arg);

  f->push_argument(m, ctx_arg);

  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(decl)) {
    ast::Type_Arrow_Node* arrow_from = ast::is_instance< ast::Type_Arrow_Node* >(var->get_type(m));

    assert(arrow_from);

    ast::Node* from = arrow_from->get_from_type(m);

    if (ast::is_instance< ast::Type_Unit_Node* >(from)) {
      from = ast::create_node_type_pointer(m, ast::create_node_type_evidence_context(m));
    } else {
      from = ast::create_node_arithmetic_mul(m, from, ast::create_node_type_pointer(m, ast::create_node_type_evidence_context(m)));
    }

    arrow_from->set_from_type(m, from);
  }

  if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(decl)) {
    ast::Type_Arrow_Node* arrow_from = ast::is_instance< ast::Type_Arrow_Node* >(var->get_type(m));

    assert(arrow_from);

    ast::Node* from = arrow_from->get_from_type(m);

    if (ast::is_instance< ast::Type_Unit_Node* >(from)) {
      from = ast::create_node_type_pointer(m, ast::create_node_type_evidence_context(m));
    } else {
      from = ast::create_node_arithmetic_mul(m, from, ast::create_node_type_pointer(m, ast::create_node_type_evidence_context(m)));
    }

    arrow_from->set_from_type(m, from);
  }
}

void pass_context_argument(Handler_Pass_Data* data, ast::Manager* m, ast::Function_Call_Node* f) {
  if (ast::is_instance< ast::Type_Int32_Node* >(f->get_function(m))) {
    return;
  }

  if (ast::is_instance< ast::Type_Pointer_Node* >(f->get_function(m))) {
    return;
  }

  if (ast::is_instance< ast::Type_Unit_Node* >(f->get_function(m))) {
    return;
  }

  if (ast::is_instance< ast::Type_Arrow_Node* >(f->get_function(m))) {
    return;
  }

  if (ast::is_instance< ast::Type_Any_Node* >(f->get_function(m))) {
    return;
  }

  ast::Literal_Symbol_Node* ctx_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "ctx"));
  f->push_argument(m, ctx_symbol);
}

void pass_context_argument(Handler_Pass_Data* data, ast::Manager* m, ast::Effect_Call_Node* f) {

  ast::Literal_Symbol_Node* ctx_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "ctx"));

  f->push_argument(m, ctx_symbol);
}

void add_evidence_context_to_functions(Handler_Pass_Data* data, ast::Manager* m, ast::Node* root) {
  if (!ast::is_semantic_node(root)) {
    return;
  }

  if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(root)) {
    if (ast::Function_Literal_Node* lit = ast::is_instance< ast::Function_Literal_Node* >(assignment->get_right_operand(m))) {
      add_context_argument(data, m, lit, assignment->get_left_operand(m));
    }
  }

  add_evidence_context_to_functions(data, m, ast::left_of(m, root));
  add_evidence_context_to_functions(data, m, ast::right_of(m, root));
}

void add_evidence_context_to_calls(Handler_Pass_Data* data, ast::Manager* m, ast::Node* root) {
  if (!ast::is_semantic_node(root)) {
    return;
  }

  if (ast::Function_Call_Node* lit = ast::is_instance< ast::Function_Call_Node* >(root)) {
    if (ast::Literal_Symbol_Node* symbol = ast::is_instance< ast::Literal_Symbol_Node* >(lit->get_function(m))) {
      Symbol s = symbol->get_symbol(m);
      if (is_equal(m->symbol_table, &s, "sizeof") == false) {
        pass_context_argument(data, m, lit);
      }
    } else {
      pass_context_argument(data, m, lit);
    }
  }

  add_evidence_context_to_calls(data, m, ast::left_of(m, root));
  add_evidence_context_to_calls(data, m, ast::right_of(m, root));
}

// ast::ProgramPoint* add_free_allocations(ast::Manager* m, ast::Node* root, ast::ProgramPoint* pp) {
// 	if(ast::is_semantic_node(root) == false) {
// 		return 0;
// 	}

// 	if(ast::Return_Node_Statement* ret = ast::is_instance<ast::Return_Node_Statement*>(root)) {
// 		pp->set_statement(m, ast::utils::call(m, ast::utils::symbol(m, "free_allocations")));
// 		pp->insert(m, ret);

// 		return pp->get_next_program_point(m);
// 	}

// 	if(ast::ProgramPoint* pp = ast::is_instance<ast::ProgramPoint*>(root)) {
// 		pp = add_free_allocations(m, pp->get_statement(m), pp);
// 		pp = add_free_allocations(m, pp->get_next_program_point(m), pp);
// 		return pp;
// 	}

// 	if(ast::Elif_List_Node* elif = ast::is_instance<ast::Elif_List_Node*>(root)) {
// 		while(ast::is_semantic_node(elif)) {
// 			add_free_allocations(m, elif->get_if(m)->get_body(m), elif->get_if(m)->get_body(m));
// 			elif = elif->get_elif(m);
// 		}

// 		return pp;
// 	}

// 	if(ast::If_Node_Statement* if_stmt = ast::is_instance<ast::If_Node_Statement*>(root)) {
// 		add_free_allocations(m, if_stmt->get_body(m), if_stmt->get_body(m));
// 		return pp;
// 	}

// 	add_free_allocations(m, ast::left_of(m, root), pp);
// 	add_free_allocations(m, ast::right_of(m, root), pp);

// 	return pp;
// }

ast::Function_Call_Node* create_yielding_to(ast::Manager* m, u64 hash) {
  ast::Literal_Symbol_Node* symbol = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "yielding_to_handler"));
  // ast::Literal_Symbol_Node*  ctx    = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "ctx"));
  ast::Literal_Natural_Node* id = ast::create_node_literal_natural(m, number_to_symbol(m->symbol_table, hash));

  ast::Declarations_List_Node* args = ast::create_node_declarations_list(m, id, NULL);
  // args                              = ast::create_node_declarations_list(m, ctx, args);

  return ast::create_node_function_call(m, symbol, args);
}

ast::Function_Call_Node* create_is_yielding(ast::Manager* m) {
  ast::Literal_Symbol_Node* symbol = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "is_yielding"));
  return ast::create_node_function_call(m, symbol, NULL);
}

void handler_rewrite(
    lib::Set< ast::Return_Node_Statement* >* ignore, Handler_Pass_Data* data, ast::Manager* m, ast::Node* root, ast::ProgramPoint* pp, ast::Literal_Symbol_Node* var) {
  if (ast::is_semantic_node(root) == false) {
    return;
  }

  if (ast::Function_Literal_Node* lit = ast::is_instance< ast::Function_Literal_Node* >(root)) {
    return;
  }

  if (ast::Return_Node_Statement* ret = ast::is_instance< ast::Return_Node_Statement* >(root)) {
    if (lib::search(ignore, ret)) {
      return;
    }

    ast::Literal_Natural_Node* zero = ast::create_node_literal_natural(m, number_to_symbol(m->symbol_table, 0));

    ast::Return_Node_Statement* new_return = ast::create_node_return_statement(m, zero);

    lib::insert(ignore, new_return);

    pp->insert(m, new_return);

    ast::Literal_Symbol_Node* set_returning_symbol = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "ctx_set_returning"));
    ast::Declarations_List_Node* set_returning_arguments = ast::create_node_declarations_list(m, ast::create_node_literal_true(m), NULL);
    ast::Function_Call_Node* set_returning_call = ast::create_node_function_call(m, set_returning_symbol, set_returning_arguments);

    pp->insert(m, set_returning_call);

    ast::Variable_Assignment_Node* assignment = ast::create_node_assignment(m, ast::create_node_pointer_value(m, ast::deep_copy(m, var)), ret->get_expression(m));

    pp->set_statement(m, assignment);

    return;
  }

  if (ast::ProgramPoint* point = ast::is_instance< ast::ProgramPoint* >(root)) {
    handler_rewrite(ignore, data, m, point->get_statement(m), point, var);
    handler_rewrite(ignore, data, m, point->get_next_program_point(m), point, var);
    return;
  }

  handler_rewrite(ignore, data, m, ast::left_of(m, root), pp, var);
  handler_rewrite(ignore, data, m, ast::right_of(m, root), pp, var);
}

void populate_handler_pass_data(Handler_Pass_Data* data, ast::Manager* m, ast::Node* node) {
  if (!ast::is_semantic_node(node)) {
    return;
  }

  if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(node)) {
    if (ast::Literal_Handler_Node* handler = ast::is_instance< ast::Literal_Handler_Node* >(assignment->get_right_operand(m))) {
      if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(assignment->get_left_operand(m))) {
        lib::insert(data->symbol_to_handler, var->get_symbol(m)->get_symbol_id(), handler);
      }
      if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(assignment->get_left_operand(m))) {
        lib::insert(data->symbol_to_handler, var->get_symbol(m)->get_symbol_id(), handler);
      }

      ast::ProgramPoint* statements = handler->get_body(m);

      while (ast::is_semantic_node(statements)) {
        if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(statements->get_statement(m))) {
          if (ast::Function_Literal_Node* literal = ast::is_instance< ast::Function_Literal_Node* >(assignment->get_right_operand(m))) {
            if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(assignment->get_left_operand(m))) {
              lib::insert(data->hashes, var->get_symbol(m)->get_symbol_id(), lib::size(data->hashes));
            }
            if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(assignment->get_left_operand(m))) {
              lib::insert(data->hashes, var->get_symbol(m)->get_symbol_id(), lib::size(data->hashes));
            }
          }
        }

        statements = statements->get_next_program_point(m);
      }
    }
  }

  populate_handler_pass_data(data, m, ast::left_of(m, node));
  populate_handler_pass_data(data, m, ast::right_of(m, node));
}

void replace_var_with_access(
    Handler_Pass_Data* data, ast::Manager* m, ast::Literal_Symbol_Node* structure, ast::Node* root, ast::Node* parent, context::Context* members, context::Context* ctx) {
  if (!ast::is_semantic_node(root)) {
    return;
  }

  if (ast::Literal_Symbol_Node* symbol = ast::is_instance< ast::Literal_Symbol_Node* >(root)) {
    if (context::context_is_defined(ctx, symbol) == false && context::context_is_defined(members, symbol)) {
      ast::Member_Access_Node* access = ast::create_node_member_access(m, structure, symbol);
      if (parent->left == root->id) {
        parent->left = access->id;
      }

      if (parent->right == root->id) {
        parent->right = access->id;
      }
    }
  }

  if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(root)) {
    context::context_declare(ctx, m, var);
  }

  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(root)) {
    context::context_declare(ctx, m, var);
  }

  if (ast::Function_Literal_Node* lit = ast::is_instance< ast::Function_Literal_Node* >(root)) {
    context::Context* _ctx = context::context_create(ctx);

    ast::Declarations_List_Node* arguments = lit->get_arguments(m);

    while (ast::is_semantic_node(arguments)) {
      ast::Node* argument = arguments->get_declaration(m);

      if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(argument)) {
        context::context_declare(_ctx, m, var);
      }

      if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(argument)) {
        context::context_declare(_ctx, m, var);
      }

      arguments = arguments->get_next_declaration(m);
    }

    replace_var_with_access(data, m, structure, lit->get_body(m), lit, members, _ctx);

    context::context_destroy(_ctx);

    return;
  }

  if (ast::Elif_List_Node* elif = ast::is_instance< ast::Elif_List_Node* >(root)) {

    while (ast::is_semantic_node(elif)) {
      ast::If_Node_Statement* if_stmt = elif->get_if(m);

      context::context_push_scope(ctx);
      replace_var_with_access(data, m, structure, if_stmt, elif, members, ctx);
      context::context_pop_scope(ctx);

      elif = elif->get_elif(m);
    }

    return;
  }

  replace_var_with_access(data, m, structure, ast::left_of(m, root), root, members, ctx);
  replace_var_with_access(data, m, structure, ast::right_of(m, root), root, members, ctx);
}

ast::Literal_Struct_Node* create_effect_agruments_struct(Handler_Pass_Data* data, ast::Manager* m, ast::Declarations_List_Node* arguments) {
  ast::ProgramPoint* members = NULL;

  while (ast::is_semantic_node(arguments)) {
    ast::Node* argument = arguments->get_declaration(m);

    if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(argument)) {
      members = ast::create_node_program_point(m, var, members);
    }

    if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(argument)) {
      members = ast::create_node_program_point(m, var, members);
    }

    arguments = arguments->get_next_declaration(m);
  }

  return ast::create_node_literal_struct(m, members);
}

void handler_rewrite_pass(Handler_Pass_Data* data, ast::Manager* m, ast::Node* node, ast::Node* return_type) {
  if (ast::is_semantic_node(node) == false) {
    return;
  }

  if (ast::Function_Literal_Node* literal = ast::is_instance< ast::Function_Literal_Node* >(node)) {
    return handler_rewrite_pass(data, m, literal->get_body(m), literal->get_return_type(m));
  }

  if (ast::Literal_Handler_Node* handler = ast::is_instance< ast::Literal_Handler_Node* >(node)) {
    ast::ProgramPoint* body = handler->get_body(m);
    ast::ProgramPoint* head = body;

    while (ast::is_semantic_node(body)) {
      ast::Node* statement = body->get_statement(m);

      if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(statement)) {
        if (ast::Function_Literal_Node* literal = ast::is_instance< ast::Function_Literal_Node* >(assignment->get_right_operand(m))) {
          ast::Literal_Symbol_Node* literal_name = NULL;

          if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(assignment->get_left_operand(m))) {
            literal_name = var->get_symbol(m);
          }

          if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(assignment->get_left_operand(m))) {
            literal_name = var->get_symbol(m);
          }

          context::Context* ctx = context::context_create(NULL);
          context::Context* mem = context::context_create(NULL);

          ast::Declarations_List_Node* arguments = literal->get_arguments(m);

          while (ast::is_semantic_node(arguments)) {
            ast::Node* argument = arguments->get_declaration(m);
            if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(argument)) {
              context::context_declare(mem, m, var);
            }

            if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(argument)) {
              context::context_declare(mem, m, var);
            }
            arguments = arguments->get_next_declaration(m);
          }

          ast::Variable_Assignment_Node** structure_assign_ref = lib::search(data->symbol_to_struct_arguments, literal_name->get_symbol_id());

          assert(structure_assign_ref);

          ast::Variable_Assignment_Node* structure_assignment = *structure_assign_ref;

          ast::Literal_Symbol_Node* structure_symbol = ast::utils::get_symbol(m, structure_assignment->get_left_operand(m));

          lib::insert(data->symbol_to_struct_arguments, literal_name->get_symbol_id(), structure_assignment);

          ast::Literal_Symbol_Node* argument_symbol = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "args"));
          ast::Declaration_Variable_Node* argument = ast::create_variable_declaration(m, argument_symbol, ast::create_node_type_pointer(m, ast::deep_copy(m, structure_symbol)));

          literal->set_arguments(m, ast::create_node_declarations_list(m, argument, NULL));

          if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(assignment->get_left_operand(m))) {
            ast::Type_Arrow_Node* arrow = ast::is_instance< ast::Type_Arrow_Node* >(var->get_type(m));

            assert(arrow);

            var->set_type(m, ast::create_node_type_arrow(m, ast::create_node_type_pointer(m, ast::deep_copy(m, structure_symbol)), arrow->get_to_type(m)));
          }

          if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(assignment->get_left_operand(m))) {
            ast::Type_Arrow_Node* arrow = ast::is_instance< ast::Type_Arrow_Node* >(var->get_type(m));

            assert(arrow);

            var->set_type(m, ast::create_node_type_arrow(m, ast::create_node_type_pointer(m, ast::deep_copy(m, structure_symbol)), arrow->get_to_type(m)));
          }

          replace_var_with_access(data, m, argument_symbol, literal->get_body(m), literal, mem, ctx);

          context::context_destroy(ctx);
          context::context_destroy(mem);

          ast::Literal_Symbol_Node* var_symbol = ast::utils::symbol(m, "prompt_ret");
          ast::Declaration_Variable_Node* var = ast::create_variable_declaration(m, var_symbol, ast::create_node_type_pointer(m, return_type));

          literal->push_argument(m, var);

          ast::utils::add_argument_to_function_declaration(m, literal, assignment, ast::create_node_type_pointer(m, return_type));

          lib::Set< ast::Return_Node_Statement* >* ignore = lib::set_create< ast::Return_Node_Statement* >();

          handler_rewrite(ignore, data, m, literal->get_body(m), literal->get_body(m), var_symbol);

          lib::set_delete(ignore);

          handler_rewrite_pass(data, m, literal->get_body(m), NULL);
        }
      }

      body = body->get_next_program_point(m);
    }

    return;
  }

  handler_rewrite_pass(data, m, ast::left_of(m, node), return_type);
  handler_rewrite_pass(data, m, ast::right_of(m, node), return_type);
}

ast::ProgramPoint*
handle_effect_check(Handler_Pass_Data* data, ast::Manager* m, ast::Literal_Symbol_Node* literal_symbol, ast::Function_Literal_Node* literal, ast::Node* ret_type) {
  u64* hash = lib::search(data->hashes, literal_symbol->get_symbol_id());

  assert(hash);

  ast::Literal_Natural_Node* nat = ast::create_node_literal_natural(m, number_to_symbol(m->symbol_table, *hash));

  // u64 n = data->yielding_calls++;

  ast::Literal_Symbol_Node* ctx_is_yielding_to_var_symbol = ast::utils::prefix(m, "yielding_to_", literal_symbol);
  ast::Literal_Symbol_Node* ctx_get_handler_args_symbol = ast::utils::symbol(m, "ctx_get_handler_args");
  ast::Literal_Symbol_Node* ctx_get_handler_args_var_symbol = ast::utils::prefix(m, "args_for_", literal_symbol);
  ast::Literal_Symbol_Node* ctx_is_returning_symbol = ast::utils::symbol(m, "ctx_is_returning");
  ast::Literal_Symbol_Node* ctx_is_returning_var_symbol = ast::utils::prefix(m, "ctx_is_returning_", literal_symbol);

  ast::Function_Call_Node* ctx_is_yielding_to_call = ast::utils::call(m, "ctx_is_yielding_to", nat);

  ast::Declaration_Constant_Node* ctx_is_yielding_declaration = ast::create_constant_declaration(m, ctx_is_yielding_to_var_symbol, ast::create_node_type_i32(m));
  ast::Variable_Assignment_Node* ctx_is_yileding_assignment = ast::create_node_assignment(m, ctx_is_yielding_declaration, ctx_is_yielding_to_call);

  ast::Node* argument = literal->get_arguments(m)->get_declaration(m);

  ast::Node* args_type = NULL;

  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(argument)) {
    args_type = var->get_type(m);
  }

  if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(argument)) {
    args_type = var->get_type(m);
  }

  assert(args_type);

  ast::Function_Call_Node* ctx_get_handler_args_call = ast::create_node_function_call(m, ctx_get_handler_args_symbol, NULL);
  ast::Declaration_Variable_Node* ctx_get_handler_args_declaration = ast::create_variable_declaration(m, ctx_get_handler_args_var_symbol, ast::deep_copy(m, args_type));

  ast::Cast_Type_Node* ctx_get_handler_args_declaration_cast = ast::create_node_cast_type(m, ast::deep_copy(m, args_type), ctx_get_handler_args_call);
  ast::Variable_Assignment_Node* ctx_get_handler_args_assignment = ast::create_node_assignment(m, ctx_get_handler_args_declaration, ctx_get_handler_args_declaration_cast);

  ast::Declarations_List_Node* handler_args = NULL;

  // ast::Variable_Assignment_Node* ret_assignment = ast::utils::assignment(m, ast::utils::variable(m, "ret", ret_type), ast::utils::default_initialization(m, ret_type));

  handler_args = ast::create_node_declarations_list(m, ast::create_node_cast_type(m, ast::create_node_type_pointer(m, ret_type), ast::utils::symbol(m, "out")), handler_args);
  handler_args = ast::create_node_declarations_list(m, ctx_get_handler_args_var_symbol, handler_args);

  ast::Function_Call_Node* handler_call = ast::create_node_function_call(m, literal_symbol, handler_args);

  ast::ProgramPoint* if_stmt_body = NULL;

  //ast::Function_Call_Node* ctx_is_returning_call = ast::create_node_function_call(m, ctx_is_returning_symbol, NULL);
  //ast::Declaration_Constant_Node* ctx_is_returning_decl = ast::create_constant_declaration(m, ctx_is_returning_var_symbol, ast::create_node_type_i32(m));
  //ast::Variable_Assignment_Node* ctx_is_returning_assignment = ast::create_node_assignment(m, ctx_is_returning_decl, ctx_is_returning_call);

  //ast::Function_Call_Node* is_yielding = ast::utils::call(m, ast::utils::symbol(m, "is_yielding"));
  //ast::Variable_Assignment_Node* is_yielding_assignment = ast::utils::assignment(m, ast::utils::variable(m, "yielding__", ast::create_node_type_i32(m)), is_yielding);
  // ast::ProgramPoint* free_allocations_body = ast::create_node_program_point(m, ast::utils::call(m, ast::utils::symbol(m, "free_allocations")), NULL);
  // ast::If_Node_Statement* free_allocations_if_stmt =
  //     ast::create_node_if_statement(m, ast::create_node_logical_equal_to(m, ast::utils::symbol(m, "yielding__"), ast::utils::natural(m, 0)), free_allocations_body);

  if_stmt_body = ast::create_node_program_point(m, ast::create_node_return_statement(m, ast::utils::natural(m, 1)), if_stmt_body);
  if_stmt_body = ast::create_node_program_point(m,ast::utils::call(m, "ctx_effect_handled", ast::utils::natural(m, *hash)), if_stmt_body);
	
  // if_stmt_body = ast::create_node_program_point(m, ast::create_node_elif_list(m, free_allocations_if_stmt, NULL), if_stmt_body);
  //if_stmt_body = ast::create_node_program_point(m, is_yielding_assignment, if_stmt_body);
  if_stmt_body = ast::create_node_program_point(m, handler_call, if_stmt_body);
  if_stmt_body = ast::create_node_program_point(m, ctx_get_handler_args_assignment, if_stmt_body);

  ast::If_Node_Statement* if_stmt = ast::create_node_if_statement(m, ctx_is_yielding_to_var_symbol, if_stmt_body);

  ast::ProgramPoint* point = NULL;

  // point = ast::create_node_program_point(m, ast::create_node_return_statement(m, ast::utils::natural(m, 1)), point);
  point = ast::create_node_program_point(m, ast::create_node_elif_list(m, if_stmt, NULL), point);
  point = ast::create_node_program_point(m, ctx_is_yileding_assignment, point);

  return point;
}

ast::Variable_Assignment_Node* create_prompt_continuation(
    ast::Manager* m, ast::Literal_Symbol_Node* symbol, ast::Node* return_type, ast::ProgramPoint* body, ast::Declarations_List_Node* arguments, ast::Node* argument_type) {
  ast::Literal_Symbol_Node* s = ast::create_node_literal_symbol(m, symbol::set_entry(m->symbol_table, "_"));
  ast::Declaration_Variable_Node* v =
      ast::create_variable_declaration(m, s, ast::create_node_type_arrow(m, argument_type, ast::create_node_type_pointer(m, ast::create_node_type_any(m))));

  ast::Function_Literal_Node* function = ast::create_node_function_literal(m, arguments, return_type, body);

  ast::Node* type = ast::create_node_type_arrow(m, argument_type, ast::deep_copy(m, return_type));

  ast::Declaration_Constant_Node* declaration = ast::create_constant_declaration(m, symbol, type);

  ast::Variable_Assignment_Node* assignment = ast::create_node_assignment(m, declaration, function);

  return assignment;
}

void create_prompt_argument(ast::Manager* m, ast::Declarations_List_Node** args, ast::Node** type) {
  ast::Literal_Symbol_Node* in_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "in"));
  ast::Literal_Symbol_Node* out_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "out"));

  ast::Declaration_Variable_Node* in_arg = ast::create_variable_declaration(m, in_symbol, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
  ast::Declaration_Variable_Node* out_arg = ast::create_variable_declaration(m, out_symbol, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
  ast::Declarations_List_Node* arguments = NULL;

  arguments = ast::create_node_declarations_list(m, out_arg, arguments);
  arguments = ast::create_node_declarations_list(m, in_arg, arguments);

  ast::Node* handler_args_type = ast::create_node_type_pointer(m, ast::create_node_type_any(m));
  handler_args_type = ast::create_node_arithmetic_mul(m, handler_args_type, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));

  *args = arguments;
  *type = handler_args_type;
}

void convert_with_statements(Handler_Pass_Data* data, ast::Manager* m, ast::Node* root, ast::Function_Literal_Node* fun, ast::ProgramPoint* point) {
  if (!ast::is_semantic_node(root)) {
    return;
  }

  if (ast::ProgramPoint* pp = ast::is_instance< ast::ProgramPoint* >(root)) {
    convert_with_statements(data, m, pp->get_statement(m), fun, pp);
    convert_with_statements(data, m, pp->get_next_program_point(m), fun, pp);
    return;
  }

  if (ast::Function_Literal_Node* func = ast::is_instance< ast::Function_Literal_Node* >(root)) {
    return convert_with_statements(data, m, func->get_body(m), func, func->get_body(m));
  }

  if (ast::With_Node_Statement* with_stmt = ast::is_instance< ast::With_Node_Statement* >(root)) {

    if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(point->get_statement(m))) {
      assignment->set_right_operand(m, with_stmt->get_call(m));
    }

    if (ast::is_instance< ast::With_Node_Statement* >(point->get_statement(m))) {
      point->set_statement(m, with_stmt->get_call(m));
    }

    if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(with_stmt->get_call(m))) {
      lib::insert(data->prompt_site, call);
    }

    ast::Literal_Symbol_Node** ret_var_symbol_ref = lib::search(data->ret_var_map, fun);
    ast::Literal_Symbol_Node* ret_var_symbol = NULL;

    // if (ret_var_symbol_ref) {
    //   ret_var_symbol = *ret_var_symbol_ref;
    // } else {
    //   ret_var_symbol = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "ret"));

    //   ast::Declaration_Variable_Node* decl = ast::create_variable_declaration(m, ret_var_symbol, ast::deep_copy(m, fun->get_return_type(m)));
    //   ast::Node* value = ast::utils::default_initialization(m, fun->get_return_type(m));

    //   point = point->insert(m, ast::utils::assignment(m, decl, value));

    //   lib::insert(data->ret_var_map, fun, ret_var_symbol);
    // }

    ast::Declarations_List_Node* handlers = with_stmt->get_list(m);

    while (ast::is_semantic_node(handlers)) {
      if (ast::Literal_Symbol_Node* symbol = ast::is_instance< ast::Literal_Symbol_Node* >(handlers->get_declaration(m))) {

        ast::Literal_Handler_Node* handler = *lib::search(data->symbol_to_handler, symbol->get_symbol_id());

        ast::ProgramPoint* handler_statements = handler->get_body(m);

        ast::ProgramPoint* check = NULL;

        ast::Type_Pointer_Node* any_ptr = ast::create_node_type_pointer(m, ast::create_node_type_any(m));
        ast::Type_Evidence_Context_Node* ctx = ast::create_node_type_evidence_context(m);
        ast::Arithmetic_Operation_Mul_Node* from = ast::create_node_arithmetic_mul(m, any_ptr, ast::create_node_arithmetic_mul(m, ctx, any_ptr));

        // ast::Declarations_List_Node* arguments = ast::create_node_declarations_list(m, ast::utils::variable(m, "_", any_ptr), NULL);
        // ast::Variable_Assignment_Node* continuation =
        //     create_prompt_continuation(m, ast::utils::symbol(m, "cp"), fun->get_return_type(m), point->split(m), arguments, any_ptr);

        // point = point->insert(m, continuation);

        while (ast::is_semantic_node(handler_statements)) {
          if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(handler_statements->get_statement(m))) {
            if (ast::Function_Literal_Node* literal = ast::is_instance< ast::Function_Literal_Node* >(assignment->get_right_operand(m))) {
              ast::Literal_Symbol_Node* literal_symbol = NULL;
              if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(assignment->get_left_operand(m))) {
                literal_symbol = var->get_symbol(m);
              }
              if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(assignment->get_left_operand(m))) {
                literal_symbol = var->get_symbol(m);
              }

              ast::ProgramPoint* pp = handle_effect_check(data, m, literal_symbol, literal, fun->get_return_type(m));

              if (check) {
                check->concat(m, pp);
              } else {
                check = pp;
              }

              convert_with_statements(data, m, literal, literal, NULL);
            }
          }

          handler_statements = handler_statements->get_next_program_point(m);
        }

        ast::Cast_Type_Node* ret_cast = ast::create_node_cast_type(m, ast::create_node_type_pointer(m, fun->get_return_type(m)), ast::utils::symbol(m, "out"));
        ast::Pointer_Value_Node* ret_val = ast::create_node_pointer_value(m, ret_cast);
        ast::Node* type =
            ast::create_node_arithmetic_mul(m, any_ptr, ast::create_node_arithmetic_mul(m, ast::create_node_type_pointer(m, ast::create_node_type_evidence_context(m)), any_ptr));

        // ast::Cast_Type_Node* fun_cast = ast::create_node_cast_type(m, ast::create_node_type_arrow(m, type, fun->get_return_type(m)), ast::utils::symbol(m, "in"));
        // ast::Variable_Assignment_Node* fun_assi = ast::utils::assignment(m, ast::utils::variable(m, "k", ast::create_node_type_arrow(m, type, fun->get_return_type(m))),
        // fun_cast); ast::Function_Call_Node* call = ast::utils::call(m, ast::utils::symbol(m, "k"), ast::utils::natural(m, 0)); ast::Variable_Assignment_Node* ret_ass =
        // ast::utils::assignment(m, ret_val, call); ast::Function_Call_Node* call =  ast::utils::call(m, ast::utils::get_symbol(m, continuation->get_left_operand(m)),
        // ast::utils::natural(m, 0));
        // ast::Variable_Assignment_Node* ret_ass = ast::utils::assignment(m, ret_val, call);

        ast::Declaration_Variable_Node* is_yielding_decl = ast::utils::variable(m, "is_yielding__", ast::create_node_type_i32(m));
        ast::Variable_Assignment_Node* is_yielding_ass = ast::utils::assignment(m, is_yielding_decl, ast::utils::call(m, ast::utils::symbol(m, "is_yielding")));

        // ast::ProgramPoint* is_yielding_pp = ast::create_node_program_point(m, ret_ass, NULL);

        ast::Node* is_yielding_cond = ast::create_node_logical_equal_to(m, ast::utils::symbol(m, "is_yielding__"), ast::utils::natural(m, 0));
        // ast::If_Node_Statement* is_yielding_if_stmt = ast::create_node_if_statement(m, is_yielding_cond, is_yielding_pp);

        ast::Return_Node_Statement* ret_stmt = ast::create_node_return_statement(m, ast::utils::natural(m, 0));

        // check->push(m, fun_assi);
        check->push(m, is_yielding_ass);
        // check->push(m, is_yielding_if_stmt);
        check->push(m, ret_stmt);

        ast::Declarations_List_Node* prompt_argument;
        ast::Node* prompt_argument_type;

        create_prompt_argument(m, &prompt_argument, &prompt_argument_type);

        ast::Variable_Assignment_Node* prompt =
            create_prompt_continuation(m, ast::utils::prefix(m, "prompt_", symbol), ast::create_node_type_unit(m), check, prompt_argument, prompt_argument_type);
        ast::Function_Literal_Node* prompt_lit = ast::is_instance< ast::Function_Literal_Node* >(prompt->get_right_operand(m));

        // add_free_allocations(m, prompt_lit->get_body(m), prompt_lit->get_body(m));

        lib::insert(data->prompt_functions, ast::is_instance< ast::Function_Literal_Node* >(prompt->get_right_operand(m)));

        point = point->insert(m, prompt);

        ast::Declaration_Variable_Node* decl = ast::utils::variable(m, "prompt_ret", fun->get_return_type(m));
        ast::Variable_Assignment_Node* assignment = ast::utils::assignment(m, decl, ast::utils::default_initialization(m, fun->get_return_type(m)));

        ast::Cast_Type_Node* ret_addr = ast::create_node_cast_type(m, any_ptr, ast::create_node_value_address(m, ast::utils::symbol(m, "prompt_ret")));
        // ast::Cast_Type_Node* arg_cast = ast::create_node_cast_type(m, any_ptr, ast::utils::get_symbol(m, ast::left_of(m, continuation)));
        ast::Function_Call_Node* prompt_call = ast::utils::call(m, ast::utils::get_symbol(m, ast::left_of(m, prompt)), ast::utils::natural(m, 0), ret_addr);

        point = point->insert(m, assignment);
        point = point->insert(m, prompt_call);

				lib::insert(data->prompt_calls, prompt_call);
				
        ast::Literal_Symbol_Node* set_returning_symbol = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "ctx_set_returning"));
        ast::Declarations_List_Node* set_returning_arguments = ast::create_node_declarations_list(m, ast::create_node_literal_false(m), NULL);
        ast::Function_Call_Node* set_returning_call = ast::create_node_function_call(m, set_returning_symbol, set_returning_arguments);
        ast::ProgramPoint* if_stmt_return_body = NULL;

        if_stmt_return_body = ast::create_node_program_point(m, ast::create_node_return_statement(m, ast::utils::symbol(m, "prompt_ret")), if_stmt_return_body);
        if_stmt_return_body = ast::create_node_program_point(m, set_returning_call, if_stmt_return_body);

        ast::Literal_Symbol_Node* ctx_is_returning_var_symbol = ast::utils::symbol(m, "ctx_is_returning_");
				ast::Function_Call_Node* ctx_is_returning_call = ast::create_node_function_call(m,ast::utils::symbol(m, "ctx_is_returning"), NULL);
				ast::Declaration_Constant_Node* ctx_is_returning_decl = ast::create_constant_declaration(m, ctx_is_returning_var_symbol, ast::create_node_type_i32(m));
				ast::Variable_Assignment_Node* ctx_is_returning_assignment = ast::create_node_assignment(m, ctx_is_returning_decl, ctx_is_returning_call);

				ast::If_Node_Statement* if_stmt_return = ast::create_node_if_statement(m, ctx_is_returning_var_symbol, if_stmt_return_body);

        point = point->insert(m, ctx_is_returning_assignment);
        point = point->insert(m, ast::create_node_elif_list(m, if_stmt_return, NULL));
        // point = point->insert(m, ast::create_node_return_statement(m, ast::utils::symbol(m, "prompt_ret")));
      }

      handlers = handlers->get_next_declaration(m);
    }

    return;
  }

  convert_with_statements(data, m, ast::left_of(m, root), fun, point);
  convert_with_statements(data, m, ast::right_of(m, root), fun, point);
}

void inline_handlers(Handler_Pass_Data* data, ast::Manager* m, ast::Node* root, ast::Node* parent) {
  if (!ast::is_semantic_node(root)) {
    return;
  }

  if (ast::Function_Literal_Node* literal = ast::is_instance< ast::Function_Literal_Node* >(root)) {
    return inline_handlers(data, m, literal->get_body(m), literal);
  }

  if (ast::ProgramPoint* pp = ast::is_instance< ast::ProgramPoint* >(root)) {
    ast::Node* old_parent = parent;
    ast::ProgramPoint* parent = pp;

    while (ast::is_semantic_node(pp)) {
      if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(pp->get_statement(m))) {
        if (ast::Literal_Handler_Node* handler = ast::is_instance< ast::Literal_Handler_Node* >(assignment->get_right_operand(m))) {
          ast::ProgramPoint* body = handler->get_body(m);

          ast::ProgramPoint* p = pp;

          while (ast::is_semantic_node(body)) {
            pp = pp->insert(m, body->get_statement(m));
            body = body->get_next_program_point(m);
          }

          if (parent == pp) {
            if (old_parent->left == p->id) {
              old_parent->left = p->get_next_program_point(m)->id;
            }

            if (old_parent->right == p->id) {
              old_parent->right = p->get_next_program_point(m)->id;
            }
          } else {
            if (parent->left == p->id) {
              parent->left = p->get_next_program_point(m)->id;
            }

            if (parent->right == p->id) {
              parent->right = p->get_next_program_point(m)->id;
            }
          }
        } else {
          inline_handlers(data, m, pp->get_statement(m), pp);
        }

      } else {
        inline_handlers(data, m, pp->get_statement(m), pp);
      }

      parent = pp;
      pp = pp->get_next_program_point(m);
    }

    return;
  }

  inline_handlers(data, m, ast::left_of(m, root), root);
  inline_handlers(data, m, ast::right_of(m, root), root);
}

ast::Node* get_typeof(ast::Manager* m, context::Context* ctx, ast::Node* node) {
  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(node)) {
    return var->get_type(m);
  }
  if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(node)) {
    return var->get_type(m);
  }

  if (ast::Cast_Type_Node* cast = ast::is_instance< ast::Cast_Type_Node* >(node)) {
    return cast->get_to_type(m);
  }

  if (ast::Literal_Symbol_Node* symbol = ast::is_instance< ast::Literal_Symbol_Node* >(node)) {
    return context::context_type_of(ctx, m, symbol->get_symbol_id());
  }

  assert(false && "Unknow node type");
}

void resume_convert_pass(Handler_Pass_Data* data, ast::Manager* m, ast::Node* root, context::Context* ctx, ast::ProgramPoint* parent, ast::ProgramPoint* current) {
  if (ast::is_semantic_node(root) == false) {
    return;
  }

  if (ast::Function_Literal_Node* lit = ast::is_instance< ast::Function_Literal_Node* >(root)) {
    context::Context* _ctx = context::context_from_declarations_list(m, lit->get_arguments(m), ctx);

    resume_convert_pass(data, m, lit->get_body(m), _ctx, parent, lit->get_body(m));

    context::context_destroy(_ctx);

    return;
  }

  if (ast::Elif_List_Node* elif = ast::is_instance< ast::Elif_List_Node* >(root)) {
    while (ast::is_semantic_node(elif)) {
      context::context_push_scope(ctx);

      resume_convert_pass(data, m, elif->get_if(m)->get_body(m), ctx, parent, elif->get_if(m)->get_body(m));

      context::context_pop_scope(ctx);

      elif = elif->get_elif(m);
    }

    return;
  }

  if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(root)) {
    if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(assignment->get_right_operand(m))) {
      if (ast::Literal_Symbol_Node* symbol = ast::is_instance< ast::Literal_Symbol_Node* >(call->get_function(m))) {
        Symbol s = symbol->get_symbol(m);

        if (symbol::is_equal(m->symbol_table, &s, "resume")) {
          ast::Node* left = assignment->get_left_operand(m);
					
          ast::Literal_Symbol_Node* var_sym = ast::create_node_literal_symbol(m, number_to_symbol(m->symbol_table, data->temporaries++, "t"));
          ast::Type_Pointer_Node* var_type = ast::create_node_type_pointer(m, ast::create_node_type_any(m));
          ast::Declaration_Variable_Node* var_decl = ast::create_variable_declaration(m, var_sym, var_type);

          assignment->set_left_operand(m, var_decl);

          ast::Cast_Type_Node* var_cast = ast::create_node_cast_type(m, ast::create_node_type_pointer(m, get_typeof(m, ctx, left)), var_sym);
          ast::Variable_Assignment_Node* assign = ast::create_node_assignment(m, left, ast::create_node_pointer_value(m, var_cast));

          // ast::Literal_Symbol_Node* deallocate = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "deallocate"));
          // current->insert(m, ast::create_node_function_call(m, deallocate, ast::create_node_declarations_list(m, var_sym, NULL)));
          current->insert(m, assign);
        }
      }
    }
  }

  if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(root)) {
    if (ast::Literal_Symbol_Node* symbol = ast::is_instance< ast::Literal_Symbol_Node* >(call->get_function(m))) {
      Symbol s = symbol->get_symbol(m);

      if (symbol::is_equal(m->symbol_table, &s, "resume")) {
        ast::Declarations_List_Node* args = call->get_arguments(m);

        ast::Node* arg = args->get_declaration(m);

        if (ast::is_semantic_node(arg) == false) {
          return;
        }

        if (ast::Literal_Symbol_Node* arg_sym = ast::is_instance< ast::Literal_Symbol_Node* >(arg)) {
          Symbol s = arg_sym->get_symbol(m);

          if (!symbol::is_equal(m->symbol_table, &s, "ctx")) {
            ast::Literal_Symbol_Node* var_sym = ast::create_node_literal_symbol(m, number_to_symbol(m->symbol_table, data->temporaries++, "t"));
            ast::Type_Pointer_Node* var_type = ast::create_node_type_pointer(m, ast::create_node_type_any(m));
            ast::Cast_Type_Node* var_cast = ast::create_node_cast_type(m, var_type, ast::create_node_value_address(m, arg_sym));
            ast::Declaration_Variable_Node* var_decl = ast::create_variable_declaration(m, var_sym, var_type);
            ast::Variable_Assignment_Node* var_assign = ast::create_node_assignment(m, var_decl, var_cast);

            parent->insert(m, var_assign);

            args->set_declaration(m, var_sym);
          }

        } else {
          assert(false && "TODO: implement type inference for value");
        }
      }
    }

    return;
  }

  if (ast::ProgramPoint* pp = ast::is_instance< ast::ProgramPoint* >(root)) {
    resume_convert_pass(data, m, pp->get_statement(m), ctx, parent, pp);
    resume_convert_pass(data, m, pp->get_next_program_point(m), ctx, pp, pp);
    return;
  }

  resume_convert_pass(data, m, ast::left_of(m, root), ctx, parent, current);
  resume_convert_pass(data, m, ast::right_of(m, root), ctx, parent, current);
}

u64 get_hash(Handler_Pass_Data* data, ast::Manager* m, ast::Node* n) {
  if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(n)) {
    if (u64* hash = lib::search(data->hashes, var->get_symbol(m)->get_symbol_id())) {
      return *hash;
    }
  }

  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(n)) {
    if (u64* hash = lib::search(data->hashes, var->get_symbol(m)->get_symbol_id())) {
      return *hash;
    }
  }

  if (ast::Literal_Symbol_Node* symbol = ast::is_instance< ast::Literal_Symbol_Node* >(n)) {
    if (u64* hash = lib::search(data->hashes, symbol->get_symbol_id())) {
      return *hash;
    }
  }

  assert(false && "unknow type");
  return 0;
}

void handler_declaration_conversion_pass(Handler_Pass_Data* data, ast::Manager* m, ast::Node* root, ast::ProgramPoint* pp) {
  if (ast::is_semantic_node(root) == false) {
    return;
  }

  if (ast::ProgramPoint* pp = ast::is_instance< ast::ProgramPoint* >(root)) {
    handler_declaration_conversion_pass(data, m, pp->get_statement(m), pp);
    handler_declaration_conversion_pass(data, m, pp->get_next_program_point(m), pp->get_next_program_point(m));
  }

  if (ast::Effect_Call_Node* call = ast::is_instance< ast::Effect_Call_Node* >(root)) {
    ast::change_kind(call, ast::AST_FUNCTION_CALL);
  }

  if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(root)) {

    if (ast::Effect_Declaration_Node* effect = ast::is_instance< ast::Effect_Declaration_Node* >(assignment->get_right_operand(m))) {
      ast::Literal_Natural_Node* hash = ast::create_node_literal_natural(m, number_to_symbol(m->symbol_table, get_hash(data, m, assignment->get_left_operand(m))));

      ast::Function_Call_Node* set_is_yielding_to_call = ast::utils::call(m, "set_is_yielding_to", hash);

      ast::Literal_Struct_Node* structure = create_effect_agruments_struct(data, m, effect->get_arguments(m));

      ast::Literal_Symbol_Node* structure_symbol =
          ast::create_node_literal_symbol(m, symbol_with_prefix(m->symbol_table, ast::utils::get_symbol(m, assignment->get_left_operand(m))->get_symbol(m), "args_"));
      ast::Declaration_Constant_Node* strucuture_decl = ast::create_constant_declaration(m, structure_symbol, ast::create_node_type_struct(m));
      ast::Variable_Assignment_Node* structure_assignment = ast::create_node_assignment(m, strucuture_decl, structure);

      lib::insert(data->symbol_to_struct_arguments, ast::utils::get_symbol(m, assignment->get_left_operand(m))->get_symbol_id(), structure_assignment);

      // TODO: define argument structure here and not on handler rewrite pass

      // ast::Variable_Assignment_Node* assignment = structure_assignment;

      assert(structure);

      ast::ProgramPoint* members = structure->get_members(m);
      ast::Declarations_List_Node* arguments = effect->get_arguments(m);
      ast::Function_Call_Node* allocate_args_call =
          ast::utils::call(m, "ctx_allocate_args", ast::utils::call(m, "sizeof", ast::utils::get_symbol(m, structure_assignment->get_left_operand(m))));
      ast::Literal_Symbol_Node* allocate_args_sym = ast::create_node_literal_symbol(m, number_to_symbol(m->symbol_table, data->temporaries++, "t"));
      ast::Declaration_Constant_Node* allocate_args_decl = ast::create_constant_declaration(m, allocate_args_sym, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
      ast::Variable_Assignment_Node* allocate_args_assign = ast::create_node_assignment(m, allocate_args_decl, allocate_args_call);

      ast::Type_Pointer_Node* type = ast::create_node_type_pointer(m, ast::utils::get_symbol(m, structure_assignment->get_left_operand(m)));
      ast::Cast_Type_Node* allocate_args_cast = ast::create_node_cast_type(m, type, allocate_args_sym);
      ast::Literal_Symbol_Node* args_sym = ast::create_node_literal_symbol(m, number_to_symbol(m->symbol_table, data->temporaries++, "t"));
      ast::Declaration_Constant_Node* args_decl = ast::create_constant_declaration(m, args_sym, type);
      ast::Variable_Assignment_Node* args_assignment = ast::create_node_assignment(m, args_decl, allocate_args_cast);

      ast::ProgramPoint* body = NULL;

      body = ast::create_node_program_point(m, ast::create_node_return_statement(m, ast::utils::default_initialization(m, effect->get_return_type(m))), body);

      while (ast::is_semantic_node(members)) {
        assert(members);
        assert(arguments);

        ast::Literal_Symbol_Node* member_symbol = ast::utils::get_symbol(m, members->get_statement(m));

        ast::Member_Access_Node* access = ast::create_node_member_access(m, args_sym, member_symbol);

        body = ast::create_node_program_point(m, ast::create_node_assignment(m, access, ast::utils::get_symbol(m, arguments->get_declaration(m))), body);

        arguments->get_next_declaration(m);
        members = members->get_next_program_point(m);
      }

      body = ast::create_node_program_point(m, args_assignment, body);
      body = ast::create_node_program_point(m, allocate_args_assign, body);
      body = ast::create_node_program_point(m, set_is_yielding_to_call, body);
      // body = ast::create_node_program_point(m, structure_assignment, body);

      ast::Node* statement = pp->get_statement(m);

      pp->set_statement(m, structure_assignment);

      pp = pp->insert(m, statement);
      root = pp->get_statement(m);

      ast::Function_Literal_Node* literal = ast::create_node_function_literal(m, effect->get_arguments(m), effect->get_return_type(m), body);
      assignment->set_right_operand(m, literal);
    }
  }

  handler_declaration_conversion_pass(data, m, ast::left_of(m, root), pp);
  handler_declaration_conversion_pass(data, m, ast::right_of(m, root), pp);
}

// void handler_add_free_allocations(Handler_Pass_Data* data, ast::Manager* m, ast::Node* root) {
// 	if(ast::is_semantic_node(root) == false) {
// 		return;
// 	}

// 	if(ast::Variable_Assignment_Node* assignment = ast::is_instance<ast::Variable_Assignment_Node*>(root)) {
// 		if(ast::Literal_Handler_Node* literal = ast::is_instance<ast::Literal_Handler_Node*>(assignment->get_right_operand(m))) {
// 			ast::ProgramPoint* body = literal->get_body(m);
// 				while(ast::is_semantic_node(body)) {
// 				if(ast::Variable_Assignment_Node* a = ast::is_instance<ast::Variable_Assignment_Node*>(body->get_statement(m))) {
// 					if(ast::Function_Literal_Node* lit = ast::is_instance<ast::Function_Literal_Node*>(a->get_right_operand(m))) {
// 						add_free_allocations(m, lit->get_body(m), lit->get_body(m));
// 					}
// 				}

// 				body = body->get_next_program_point(m);
// 			}
// 		}
// 	}

// 	handler_add_free_allocations(data, m, ast::left_of(m, root));
// 	handler_add_free_allocations(data, m, ast::right_of(m, root));
// }

void handler_conversion_pass(Handler_Pass_Data* data, ast::Manager* m, ast::Node* root) {
  // handeler_conversion_pass_recursive(data, m, root, ast::is_instance< ast::ProgramPoint_List_Node* >(root));
  // convert_effect_declarations(data, m, root);
  populate_handler_pass_data(data, m, root);

  // handler_add_free_allocations(data, m, root);

  handler_declaration_conversion_pass(data, m, root, NULL);

  handler_rewrite_pass(data, m, root, NULL);

  convert_with_statements(data, m, root, NULL, NULL);

  inline_handlers(data, m, root, NULL);

  context::Context* ctx = context::context_create(NULL);

  resume_convert_pass(data, m, root, ctx, NULL, NULL);

  context::context_destroy(ctx);
  add_evidence_context_to_functions(data, m, root);
  add_evidence_context_to_calls(data, m, root);
}

} // namespace handler
