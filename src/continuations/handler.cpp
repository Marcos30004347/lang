#include "handler.hpp"
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
#include "lib/set.hpp"
#include "lib/table.hpp"
#include "parser/parser.hpp"
#include <cassert>
#include <cstdio>

using namespace compiler;
using namespace symbol;

namespace handler {

const i8* HANDLE_EFFECT_MACRO  = "#HANDLE_EFFECT";
const i8* GET_EFFECT_ARG_MACRO = "#GET_EFFECT_ARG";
const i8* PROMPT_HANDLER_MACRO = "#PROMPT_HANDLER";

struct Handler_Pass_Data {
  lib::Table< symbol::Id, u64 >* hashes;

  // lib::Set< ast::Variable_Assignment_Node* >* effect_handlers_assignments;
  //  lib::Set< ast::Variable_Assignment_Node* >* prompt_handlers_assignments;

  // lib::Table< symbol::Id, ast::Variable_Assignment_Node* >* handler_prompt;
  lib::Set< ast::Declaration_Variable_Node* >* context_declarations;
  lib::Set< ast::Function_Literal_Node* >*     effect_functions;
  lib::Set< ast::Function_Literal_Node* >*     prompt_functions;
};

b8 handler_pass_data_is_effect_function(Handler_Pass_Data* data, ast::Function_Literal_Node* lit) {
  ast::Function_Literal_Node** f = lib::search(data->effect_functions, lit);
  if (f)
    return true;
  return false;
}

b8 handler_pass_data_is_prompt_function(Handler_Pass_Data* data, ast::Function_Literal_Node* lit) {
  ast::Function_Literal_Node** f = lib::search(data->prompt_functions, lit);
  if (f)
    return true;
  return false;
}

b8 handler_pass_data_is_context_argument(Handler_Pass_Data* data, ast::Declaration_Variable_Node* decl) {
  return lib::search(data->context_declarations, decl) != NULL;
}

b8 handler_pass_data_is_context_argument(Handler_Pass_Data* data, ast::Declaration_Constant_Node* decl) {
  return false;
}

Handler_Pass_Data* handler_pass_data_create() {
  Handler_Pass_Data* h = new Handler_Pass_Data();

  h->hashes = lib::table_create< symbol::Id, u64 >();
  // h->handler_prompt = lib::table_create< symbol::Id, ast::Variable_Assignment_Node* >();
  // h->effect_handlers_assignments = lib::set_create< ast::Variable_Assignment_Node* >();
  // h->prompt_handlers_assignments = lib::set_create< ast::Variable_Assignment_Node* >();
  h->effect_functions     = lib::set_create< ast::Function_Literal_Node* >();
  h->prompt_functions     = lib::set_create< ast::Function_Literal_Node* >();
  h->context_declarations = lib::set_create< ast::Declaration_Variable_Node* >();
  return h;
}

void handler_pass_data_destroy(Handler_Pass_Data* d) {
  lib::table_delete(d->hashes);
  // lib::table_delete(d->handler_prompt);

  lib::set_delete(d->context_declarations);
  lib::set_delete(d->prompt_functions);
  lib::set_delete(d->effect_functions);

  delete d;
}

void add_context_argument(Handler_Pass_Data* data, ast::Manager* m, ast::Function_Literal_Node* f, ast::Node* decl) {
  ast::Type_Evidence_Context_Node* ctx_type   = ast::create_node_type_evidence_context(m);
  ast::Literal_Symbol_Node*        ctx_symbol = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "ctx"));

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
    pass_context_argument(data, m, lit);
  }

  // if (ast::Effect_Call_Node* lit = ast::is_instance< ast::Effect_Call_Node* >(root)) {
  //   pass_context_argument(data, m, lit);
  // }

  add_evidence_context_to_calls(data, m, ast::left_of(m, root));
  add_evidence_context_to_calls(data, m, ast::right_of(m, root));
}

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
  // ast::Literal_Symbol_Node* ctx    = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "ctx"));

  // ast::Declarations_List_Node* args = ast::create_node_declarations_list(m, ctx, NULL);

  return ast::create_node_function_call(m, symbol, NULL);
}

ast::ProgramPoint_List_Node* create_handler_effect_if_stmt(ast::Manager* m, u64 hash, ast::Literal_Symbol_Node* effect, ast::Function_Literal_Node* literal) {

  ast::Declarations_List_Node* args      = literal->get_arguments(m);
  ast::Declarations_List_Node* call_args = NULL;

  u64 i = 0;

  ast::Node* size_acc = ast::create_node_literal_natural(m, number_to_symbol(m->symbol_table, 0));

  ast::ProgramPoint_List_Node* get_sizes_pp  = NULL;
  ast::ProgramPoint_List_Node* get_stride_pp = NULL;

  while (ast::is_semantic_node(args)) {
    ast::Declaration_Variable_Node* arg = ast::is_instance< ast::Declaration_Variable_Node* >(args->get_declaration(m));

    assert(arg);

    ast::Literal_Symbol_Node* ctx = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "ctx"));

    ast::Literal_Symbol_Node*       sizeof_symbol        = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "sizeof"));
    ast::Declarations_List_Node*    sizeof_arg           = ast::create_node_declarations_list(m, ast::deep_copy(m, arg->get_type(m)), NULL);
    ast::Function_Call_Node*        sizeof_call          = ast::create_node_function_call(m, sizeof_symbol, sizeof_arg);
    ast::Literal_Symbol_Node*       type_size_symbol     = ast::create_node_literal_symbol(m, number_to_symbol(m->symbol_table, i, "type_size"));
    ast::Declaration_Constant_Node* type_size_decl       = ast::create_constant_declaration(m, type_size_symbol, ast::create_node_type_i32(m));
    ast::Variable_Assignment_Node*  type_size_assignment = ast::create_node_assignment(m, type_size_decl, sizeof_call);

    get_sizes_pp = ast::create_node_program_point(m, type_size_assignment, get_sizes_pp);

    ast::Literal_Symbol_Node*       arg_stride_symbol = ast::create_node_literal_symbol(m, number_to_symbol(m->symbol_table, i, "arg_stride"));
    ast::Declaration_Constant_Node* arg_stride_decl   = ast::create_constant_declaration(m, arg_stride_symbol, ast::create_node_type_i32(m));
    ast::Variable_Assignment_Node*  arg_stride_assignment =
        ast::create_node_assignment(m, arg_stride_decl, ast::create_node_arithmetic_add(m, size_acc, ast::deep_copy(m, type_size_symbol)));

    get_stride_pp = ast::create_node_program_point(m, arg_stride_assignment, get_stride_pp);

    size_acc = ast::deep_copy(m, arg_stride_symbol);

    ast::Literal_Symbol_Node*    yielding_args_symbol = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "yielding_arguments_buffer"));
    ast::Declarations_List_Node* yielding_args_args   = ast::create_node_declarations_list(m, ctx, NULL);
    ast::Function_Call_Node*     yielding_args_call   = ast::create_node_function_call(m, yielding_args_symbol, yielding_args_args);

    ast::Arithmetic_Operation_Add_Node* arg_ptr    = ast::create_node_arithmetic_add(m, yielding_args_call, ast::deep_copy(m, arg_stride_symbol));
    ast::Cast_Type_Node*                arg_cast   = ast::create_node_cast_type(m, ast::create_node_type_pointer(m, ast::deep_copy(m, arg->get_type(m))), arg_ptr);
    ast::Pointer_Value_Node*            effect_arg = ast::create_node_pointer_value(m, arg_cast);

    call_args = ast::create_node_declarations_list(m, effect_arg, call_args);

    i = i + 1;

    args = args->get_next_declaration(m);
  }

  ast::Function_Call_Node* effect_call = ast::create_node_function_call(m, ast::deep_copy(m, effect), call_args);

  ast::ProgramPoint_List_Node* body = NULL;

  if (get_sizes_pp) {
    body = get_sizes_pp->concat(m, get_stride_pp);

    body->push(m, ast::create_node_return_statement(m, effect_call));
  } else {
    body = ast::create_node_program_point(m, ast::create_node_return_statement(m, effect_call), NULL);
  }

  ast::Literal_Symbol_Node*       is_yielding_symbol     = ast::create_node_literal_symbol(m, number_to_symbol(m->symbol_table, hash, "is_yielding_to_handler"));
  ast::Declaration_Variable_Node* is_yielding_decl       = ast::create_variable_declaration(m, is_yielding_symbol, ast::create_node_type_i32(m));
  ast::Variable_Assignment_Node*  is_yielding_assignment = ast::create_node_assignment(m, is_yielding_decl, create_yielding_to(m, hash));

  ast::Elif_List_Node*         elif = ast::create_node_elif_list(m, ast::create_node_if_statement(m, ast::deep_copy(m, is_yielding_symbol), body), NULL);
  ast::ProgramPoint_List_Node* pp   = ast::create_node_program_point(m, elif, NULL);

  return ast::create_node_program_point(m, is_yielding_assignment, pp);
}

ast::Function_Literal_Node* create_prompt_function(Handler_Pass_Data* data, ast::Manager* m, ast::Literal_Symbol_Node* name, ast::Literal_Handler_Node* handler) {
  ast::Declarations_List_Node* prompt_arguments = ast::create_node_declarations_list(m, 0, 0);

  ast::ProgramPoint_List_Node* body = handler->get_body(m);
  ast::ProgramPoint_List_Node* iter = body;

  while (ast::is_semantic_node(body)) {
    ast::Node* statement = body->get_statement(m);

    if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(statement)) {
      if (ast::Function_Literal_Node* literal = ast::is_instance< ast::Function_Literal_Node* >(assignment->get_right_operand(m))) {

        lib::insert(data->effect_functions, literal);

        ast::Literal_Symbol_Node* symbol = NULL;

        ast::Node* left = assignment->get_left_operand(m);

        if (ast::Declaration_Constant_Node* decl = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
          symbol = decl->get_symbol(m);
        }

        if (ast::Declaration_Variable_Node* decl = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
          symbol = decl->get_symbol(m);
        }

        assert(symbol);

        u64* hash = lib::search(data->hashes, symbol->get_symbol_id());

        assert(hash);

        body->emplace(m, create_handler_effect_if_stmt(m, *hash, symbol, literal));
      }
    }

    body = body->get_next_program_point(m);
  }

  while (ast::is_semantic_node(iter->get_next_program_point(m))) {
    iter = iter->get_next_program_point(m);
  }

  ast::Literal_Symbol_Node*    prompt      = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, PROMPT_HANDLER_MACRO));
  ast::Declarations_List_Node* prompt_arg  = ast::create_node_declarations_list(m, ast::deep_copy(m, name), NULL);
  ast::Function_Call_Node*     prompt_call = ast::create_node_function_call(m, prompt, prompt_arg);

  ast::ProgramPoint_List_Node* tmp = iter;

  iter = iter->insert(m, prompt_call);
  iter = iter->insert(m, ast::create_node_return_statement(m, ast::create_node_literal_nothing(m)));

  ast::Literal_Symbol_Node*       is_yielding_symbol     = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "is_ctx_yielding"));
  ast::Declaration_Variable_Node* is_yielding_decl       = ast::create_variable_declaration(m, is_yielding_symbol, ast::create_node_type_i32(m));
  ast::Variable_Assignment_Node*  is_yielding_assignment = ast::create_node_assignment(m, is_yielding_decl, create_is_yielding(m));

  ast::If_Node_Statement* if_is_yielding = ast::create_node_if_statement(m, ast::deep_copy(m, is_yielding_symbol), handler->get_body(m));

  ast::Elif_List_Node* elif = ast::create_node_elif_list(m, if_is_yielding, NULL);

  ast::ProgramPoint_List_Node* prompt_return = ast::create_node_program_point(m, ast::create_node_return_statement(m, ast::create_node_literal_nothing(m)), NULL);
  ast::ProgramPoint_List_Node* prompt_body   = ast::create_node_program_point(m, elif, prompt_return);
  prompt_body                                = ast::create_node_program_point(m, is_yielding_assignment, prompt_body);

  ast::Function_Literal_Node* prompt_function = ast::create_node_function_literal(m, 0, ast::create_node_type_unit(m), prompt_body);

  lib::insert(data->prompt_functions, prompt_function);

  return prompt_function;
}

void handeler_conversion_pass_recursive(Handler_Pass_Data* data, ast::Manager* m, ast::Node* root, ast::ProgramPoint_List_Node* point) {
  if (!ast::is_semantic_node(root) || !ast::is_semantic_node(point)) {
    return;
  }

  if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(root)) {
    ast::Node* right = assignment->get_right_operand(m);
    ast::Node* left  = assignment->get_left_operand(m);

    if (ast::Effect_Declaration_Node* effect = ast::is_instance< ast::Effect_Declaration_Node* >(right)) {
      symbol::Id id = 0;

      if (ast::Declaration_Constant_Node* decl = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
        id = decl->get_symbol(m)->get_symbol_id();
      }

      if (ast::Declaration_Variable_Node* decl = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
        id = decl->get_symbol(m)->get_symbol_id();
      }

      assert(id);

      lib::insert(data->hashes, id, lib::size(data->hashes) + 1);
    }

    if (ast::Literal_Handler_Node* handler = ast::is_instance< ast::Literal_Handler_Node* >(right)) {
      ast::Node* type = ast::create_node_type_any(m); // TODO(marcos): get return type

      ast::Node* left = assignment->get_left_operand(m);

      ast::Literal_Symbol_Node* name = NULL;

      if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
        ast::Type_Unit_Node*  unit  = ast::create_node_type_unit(m);
        ast::Type_Arrow_Node* arrow = ast::create_node_type_arrow(m, unit, ast::deep_copy(m, type));

        name = var->get_symbol(m);
        var->set_type(m, arrow);
      }

      if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
        ast::Type_Unit_Node*  unit  = ast::create_node_type_unit(m);
        ast::Type_Arrow_Node* arrow = ast::create_node_type_arrow(m, unit, ast::deep_copy(m, type));

        name = var->get_symbol(m);
        var->set_type(m, arrow);
      }

      assert(name);

      assignment->set_right_operand(m, create_prompt_function(data, m, name, handler));

      return;
    }
  }

  if (ast::ProgramPoint_List_Node* pp = ast::is_instance< ast::ProgramPoint_List_Node* >(root)) {
    ast::Node*                   left  = pp->get_statement(m);
    ast::ProgramPoint_List_Node* right = pp->get_next_program_point(m);

    handeler_conversion_pass_recursive(data, m, left, pp);
    handeler_conversion_pass_recursive(data, m, right, right);

    return;
  }

  handeler_conversion_pass_recursive(data, m, ast::left_of(m, root), point);
  handeler_conversion_pass_recursive(data, m, ast::right_of(m, root), point);
}

void handeler_conversion_pass(Handler_Pass_Data* data, ast::Manager* m, ast::Node* root) {
  handeler_conversion_pass_recursive(data, m, root, ast::is_instance< ast::ProgramPoint_List_Node* >(root));

  add_evidence_context_to_functions(data, m, root);
  add_evidence_context_to_calls(data, m, root);
}

} // namespace handler
