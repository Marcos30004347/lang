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
#include "context/context.hpp"
#include "lexer.hpp"
#include "lib/set.hpp"
#include "lib/table.hpp"
#include "parser/parser.hpp"
#include <cassert>
#include <cstdio>

using namespace compiler;
using namespace symbol;

namespace handler {

struct Handler_Pass_Data {
  u64 yielding_calls;
  lib::Table< symbol::Id, u64 >* hashes;
  lib::Set< ast::Declaration_Variable_Node* >* context_declarations;
  lib::Table< ast::Function_Literal_Node*, ast::Literal_Symbol_Node* >* ret_var_map;
  // lib::Set< ast::Function_Literal_Node* >* effect_functions;
  // lib::Set< ast::Function_Literal_Node* >* prompt_functions;
  // lib::Set< ast::Node* >* prompt_declarations;
  lib::Table< symbol::Id, ast::Literal_Handler_Node* >* symbol_to_handler;
};

// b8 handler_pass_data_is_effect_function(Handler_Pass_Data* data, ast::Function_Literal_Node* lit) {
//   ast::Function_Literal_Node** f = lib::search(data->effect_functions, lit);
//   if (f)
//     return true;
//   return false;
// }

// b8 handler_pass_data_is_prompt_function(Handler_Pass_Data* data, ast::Function_Literal_Node* lit) {
//   if (lib::search(data->prompt_functions, lit)) {
//     return true;
//   }
//   return false;
// }

// b8 handler_pass_data_is_prompt_declaration(Handler_Pass_Data* data, ast::Node* decl) {
//   if (lib::search(data->prompt_declarations, decl)) {
//     return true;
//   }
//   return false;
// }

b8 handler_pass_data_is_context_argument(Handler_Pass_Data* data, ast::Declaration_Variable_Node* decl) {
  return lib::search(data->context_declarations, decl) != NULL;
}

b8 handler_pass_data_is_context_argument(Handler_Pass_Data* data, ast::Declaration_Constant_Node* decl) {
  return false;
}

Handler_Pass_Data* handler_pass_data_create() {
  Handler_Pass_Data* h = new Handler_Pass_Data();
  h->yielding_calls = 0;
  h->hashes = lib::table_create< symbol::Id, u64 >();
  h->ret_var_map = lib::table_create< ast::Function_Literal_Node*, ast::Literal_Symbol_Node* >();
  // h->handler_prompt = lib::table_create< symbol::Id, ast::Variable_Assignment_Node* >();
  // h->effect_handlers_assignments = lib::set_create< ast::Variable_Assignment_Node* >();
  // h->prompt_handlers_assignments = lib::set_create< ast::Variable_Assignment_Node* >();
  // h->effect_functions = lib::set_create< ast::Function_Literal_Node* >();
  // h->prompt_functions = lib::set_create< ast::Function_Literal_Node* >();
  h->context_declarations = lib::set_create< ast::Declaration_Variable_Node* >();
  // h->prompt_declarations = lib::set_create< ast::Node* >();
  h->symbol_to_handler = lib::table_create< symbol::Id, ast::Literal_Handler_Node* >();
  return h;
}

void handler_pass_data_destroy(Handler_Pass_Data* d) {
  lib::table_delete(d->hashes);
  lib::table_delete(d->ret_var_map);
  lib::set_delete(d->context_declarations);
  // lib::set_delete(d->prompt_functions);
  // lib::set_delete(d->effect_functions);
  // lib::set_delete(d->prompt_declarations);
  lib::table_delete(d->symbol_to_handler);

  delete d;
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

// ast::ProgramPoint_List_Node* create_handler_effect_if_stmt(ast::Manager* m, u64 hash, ast::Literal_Symbol_Node* effect, ast::Function_Literal_Node* literal) {

//   ast::Declarations_List_Node* args      = literal->get_arguments(m);
//   ast::Declarations_List_Node* call_args = NULL;

//   u64 i = 0;

//   ast::Node* size_acc = ast::create_node_literal_natural(m, number_to_symbol(m->symbol_table, 0));

//   ast::ProgramPoint_List_Node* get_sizes_pp  = NULL;
//   ast::ProgramPoint_List_Node* get_stride_pp = NULL;

//   while (ast::is_semantic_node(args)) {
//     ast::Declaration_Variable_Node* arg = ast::is_instance< ast::Declaration_Variable_Node* >(args->get_declaration(m));

//     assert(arg);

//     ast::Literal_Symbol_Node* ctx = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "ctx"));

//     ast::Literal_Symbol_Node*       sizeof_symbol        = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "sizeof"));
//     ast::Declarations_List_Node*    sizeof_arg           = ast::create_node_declarations_list(m, ast::deep_copy(m, arg->get_type(m)), NULL);
//     ast::Function_Call_Node*        sizeof_call          = ast::create_node_function_call(m, sizeof_symbol, sizeof_arg);
//     ast::Literal_Symbol_Node*       type_size_symbol     = ast::create_node_literal_symbol(m, number_to_symbol(m->symbol_table, i, "type_size"));
//     ast::Declaration_Constant_Node* type_size_decl       = ast::create_constant_declaration(m, type_size_symbol, ast::create_node_type_i32(m));
//     ast::Variable_Assignment_Node*  type_size_assignment = ast::create_node_assignment(m, type_size_decl, sizeof_call);

//     get_sizes_pp = ast::create_node_program_point(m, type_size_assignment, get_sizes_pp);

//     ast::Literal_Symbol_Node*       arg_stride_symbol = ast::create_node_literal_symbol(m, number_to_symbol(m->symbol_table, i, "arg_stride"));
//     ast::Declaration_Constant_Node* arg_stride_decl   = ast::create_constant_declaration(m, arg_stride_symbol, ast::create_node_type_i32(m));
//     ast::Variable_Assignment_Node*  arg_stride_assignment =
//         ast::create_node_assignment(m, arg_stride_decl, ast::create_node_arithmetic_add(m, size_acc, ast::deep_copy(m, type_size_symbol)));

//     get_stride_pp = ast::create_node_program_point(m, arg_stride_assignment, get_stride_pp);

//     size_acc = ast::deep_copy(m, arg_stride_symbol);

//     ast::Literal_Symbol_Node*    yielding_args_symbol = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "yielding_arguments_buffer"));
//     ast::Declarations_List_Node* yielding_args_args   = ast::create_node_declarations_list(m, ctx, NULL);
//     ast::Function_Call_Node*     yielding_args_call   = ast::create_node_function_call(m, yielding_args_symbol, yielding_args_args);

//     ast::Arithmetic_Operation_Add_Node* arg_ptr    = ast::create_node_arithmetic_add(m, yielding_args_call, ast::deep_copy(m, arg_stride_symbol));
//     ast::Cast_Type_Node*                arg_cast   = ast::create_node_cast_type(m, ast::create_node_type_pointer(m, ast::deep_copy(m, arg->get_type(m))), arg_ptr);
//     ast::Pointer_Value_Node*            effect_arg = ast::create_node_pointer_value(m, arg_cast);

//     call_args = ast::create_node_declarations_list(m, effect_arg, call_args);

//     i = i + 1;

//     args = args->get_next_declaration(m);
//   }

//   ast::Function_Call_Node* effect_call = ast::create_node_function_call(m, ast::deep_copy(m, effect), call_args);

//   ast::ProgramPoint_List_Node* body = NULL;

//   if (get_sizes_pp) {
//     body = get_sizes_pp->concat(m, get_stride_pp);

//     body->push(m, ast::create_node_return_statement(m, effect_call));
//   } else {
//     body = ast::create_node_program_point(m, ast::create_node_return_statement(m, effect_call), NULL);
//   }

//   ast::Literal_Symbol_Node*       is_yielding_symbol     = ast::create_node_literal_symbol(m, number_to_symbol(m->symbol_table, hash, "is_yielding_to_handler"));
//   ast::Declaration_Variable_Node* is_yielding_decl       = ast::create_variable_declaration(m, is_yielding_symbol, ast::create_node_type_i32(m));
//   ast::Variable_Assignment_Node*  is_yielding_assignment = ast::create_node_assignment(m, is_yielding_decl, create_yielding_to(m, hash));

//   ast::Elif_List_Node*         elif = ast::create_node_elif_list(m, ast::create_node_if_statement(m, ast::deep_copy(m, is_yielding_symbol), body), NULL);
//   ast::ProgramPoint_List_Node* pp   = ast::create_node_program_point(m, elif, NULL);

//   return ast::create_node_program_point(m, is_yielding_assignment, pp);
// }

// ast::Function_Literal_Node* create_prompt_function(Handler_Pass_Data* data, ast::Manager* m, ast::Literal_Symbol_Node* name, ast::Literal_Handler_Node* handler) {
//   ast::Declarations_List_Node* prompt_arguments = ast::create_node_declarations_list(m, 0, 0);

//   ast::ProgramPoint_List_Node* body = handler->get_body(m);
//   ast::ProgramPoint_List_Node* iter = body;

//   while (ast::is_semantic_node(body)) {
//     ast::Node* statement = body->get_statement(m);

//     if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(statement)) {
//       if (ast::Function_Literal_Node* literal = ast::is_instance< ast::Function_Literal_Node* >(assignment->get_right_operand(m))) {

//         lib::insert(data->effect_functions, literal);

//         ast::Literal_Symbol_Node* symbol = NULL;

//         ast::Node* left = assignment->get_left_operand(m);

//         if (ast::Declaration_Constant_Node* decl = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
//           symbol = decl->get_symbol(m);
//         }

//         if (ast::Declaration_Variable_Node* decl = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
//           symbol = decl->get_symbol(m);
//         }

//         assert(symbol);

//         u64* hash = lib::search(data->hashes, symbol->get_symbol_id());

//         assert(hash);

//         body->emplace(m, create_handler_effect_if_stmt(m, *hash, symbol, literal));
//       }
//     }

//     body = body->get_next_program_point(m);
//   }

//   while (ast::is_semantic_node(iter->get_next_program_point(m))) {
//     iter = iter->get_next_program_point(m);
//   }

//   ast::Bubble_Handler_Node* bubble = ast::create_bubble_handler(m, ast::deep_copy(m, name));

//   iter = iter->insert(m, bubble);
//   iter = iter->insert(m, ast::create_node_return_statement(m, ast::create_node_literal_nothing(m)));

//   ast::Literal_Symbol_Node*       is_yielding_symbol     = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "is_ctx_yielding"));
//   ast::Declaration_Variable_Node* is_yielding_decl       = ast::create_variable_declaration(m, is_yielding_symbol, ast::create_node_type_i32(m));
//   ast::Variable_Assignment_Node*  is_yielding_assignment = ast::create_node_assignment(m, is_yielding_decl, create_is_yielding(m));

//   ast::If_Node_Statement* if_is_yielding = ast::create_node_if_statement(m, ast::deep_copy(m, is_yielding_symbol), handler->get_body(m));

//   ast::Elif_List_Node* elif = ast::create_node_elif_list(m, if_is_yielding, NULL);

//   ast::ProgramPoint_List_Node* prompt_return = ast::create_node_program_point(m, ast::create_node_return_statement(m, ast::create_node_literal_nothing(m)), NULL);
//   ast::ProgramPoint_List_Node* prompt_body   = ast::create_node_program_point(m, elif, prompt_return);
//   prompt_body                                = ast::create_node_program_point(m, is_yielding_assignment, prompt_body);

//   ast::Literal_Symbol_Node* ignore0 = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "_"));
//   ast::Literal_Symbol_Node* ignore1 = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "__"));

//   ast::Declarations_List_Node* args =
//       ast::create_node_declarations_list(m, ast::create_variable_declaration(m, ignore0, ast::create_node_type_pointer(m, ast::create_node_type_any(m))), NULL);
//   args = ast::create_node_declarations_list(m, ast::create_variable_declaration(m, ignore1, ast::create_node_type_pointer(m, ast::create_node_type_any(m))), args);

//   ast::Function_Literal_Node* prompt_function = ast::create_node_function_literal(m, args, ast::create_node_type_unit(m), prompt_body);

//   lib::insert(data->prompt_functions, prompt_function);

//   return prompt_function;
// }

// void handeler_conversion_pass_recursive(Handler_Pass_Data* data, ast::Manager* m, ast::Node* root, ast::ProgramPoint_List_Node* pp_cur) {
//   if (!ast::is_semantic_node(root) || !ast::is_semantic_node(pp_cur)) {
//     return;
//   }

//   // if (ast::With_Node_Statement* with = ast::is_instance< ast::With_Node_Statement* >(root)) {
//   //   ast::Declarations_List_Node* list = with->get_list(m);
//   //   pp_cur->set_statement(m, ast::create_prompt_statement(m, list->get_declaration(m)));
//   //   list = list->get_next_declaration(m);

//   //   while (ast::is_semantic_node(list)) {
//   //     pp_cur = pp_cur->insert(m, ast::create_prompt_statement(m, list->get_declaration(m)));
//   //     list   = list->get_next_declaration(m);
//   //   }

//   //   pp_cur = pp_cur->insert(m, with->get_call(m));

//   //   list = with->get_list(m);

//   //   while (ast::is_semantic_node(list)) {
//   //     ast::Literal_Natural_Node*   zero = ast::create_node_literal_natural(m, number_to_symbol(m->symbol_table, 0));
//   //     ast::Declarations_List_Node* args = ast::create_node_declarations_list(m, zero, NULL);
//   //     ast::Function_Call_Node*     call = ast::create_node_function_call(m, list->get_declaration(m), args);

//   //     pp_cur = pp_cur->insert(m, call);

//   //     list = list->get_next_declaration(m);
//   //   }
//   // }

//   if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(root)) {
//     ast::Node* right = assignment->get_right_operand(m);
//     ast::Node* left  = assignment->get_left_operand(m);

//     if (ast::Effect_Declaration_Node* effect = ast::is_instance< ast::Effect_Declaration_Node* >(right)) {
//       symbol::Id id = 0;

//       if (ast::Declaration_Constant_Node* decl = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
//         id = decl->get_symbol(m)->get_symbol_id();
//       }

//       if (ast::Declaration_Variable_Node* decl = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
//         id = decl->get_symbol(m)->get_symbol_id();
//       }

//       assert(id);

//       lib::insert(data->hashes, id, lib::size(data->hashes) + 1);
//     }

//     if (ast::Literal_Handler_Node* handler = ast::is_instance< ast::Literal_Handler_Node* >(right)) {
//       ast::Node* type = ast::create_node_type_any(m); // TODO(marcos): get return type

//       ast::Node* left = assignment->get_left_operand(m);

//       ast::Literal_Symbol_Node* name = NULL;

//       if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
//         ast::Type_Pointer_Node* from  = ast::create_node_type_pointer(m, ast::create_node_type_any(m));
//         ast::Type_Arrow_Node*   arrow = ast::create_node_type_arrow(m, from, ast::deep_copy(m, type));

//         name = var->get_symbol(m);
//         var->set_type(m, arrow);
//       }

//       if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
//         ast::Type_Pointer_Node* from  = ast::create_node_type_pointer(m, ast::create_node_type_any(m));
//         ast::Type_Arrow_Node*   arrow = ast::create_node_type_arrow(m, from, ast::deep_copy(m, type));

//         name = var->get_symbol(m);
//         var->set_type(m, arrow);
//       }

//       assert(name);

//       assignment->set_right_operand(m, create_prompt_function(data, m, name, handler));

//       lib::insert(data->prompt_declarations, left);

//       return;
//     }
//   }

//   if (ast::ProgramPoint_List_Node* pp = ast::is_instance< ast::ProgramPoint_List_Node* >(root)) {
//     ast::Node*                   left  = pp->get_statement(m);
//     ast::ProgramPoint_List_Node* right = pp->get_next_program_point(m);

//     handeler_conversion_pass_recursive(data, m, left, pp);
//     handeler_conversion_pass_recursive(data, m, right, right);

//     return;
//   }

//   handeler_conversion_pass_recursive(data, m, ast::left_of(m, root), pp_cur);
//   handeler_conversion_pass_recursive(data, m, ast::right_of(m, root), pp_cur);
// }

// void convert_effect_declarations(Handler_Pass_Data* data, ast::Manager* m, ast::Node* root) {
//   if (!ast::is_semantic_node(root)) {
//     return;
//   }

//   if (ast::ProgramPoint_List_Node* point = ast::is_instance< ast::ProgramPoint_List_Node* >(root)) {
//     while (ast::is_semantic_node(point)) {
//       if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(point->get_statement(m))) {

//         if (ast::Effect_Call_Node* call = ast::is_instance< ast::Effect_Call_Node* >(assignment->get_right_operand(m))) {
//           ast::Declarations_List_Node* arguments = call->get_arguments(m);

//           assignment->set_right_operand(m, ast::create_node_function_call(m, call->get_effect(m), arguments));
//         }

//         if (ast::Effect_Declaration_Node* decl = ast::is_instance< ast::Effect_Declaration_Node* >(assignment->get_right_operand(m))) {

//           ast::Literal_Symbol_Node* symbol = NULL;
//           if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(assignment->get_left_operand(m))) {
//             symbol = var->get_symbol(m);
//           }

//           if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(assignment->get_left_operand(m))) {
//             symbol = var->get_symbol(m);
//           }

//           ast::Node* arguments = decl->get_arguments(m);

//           ast::Literal_Symbol_Node* set_yielding_to = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "set_is_yielding_to"));

//           u64* hash = lib::search(data->hashes, symbol->get_symbol_id());

//           assert(hash);

//           ast::Declarations_List_Node* args = ast::create_node_declarations_list(m, ast::create_node_literal_natural(m, number_to_symbol(m->symbol_table, *hash)), NULL);

//           ast::Function_Call_Node* call = ast::create_node_function_call(m, set_yielding_to, args);

//           ast::Return_Node_Statement* return_devault_value = NULL;

//           if (ast::is_instance< ast::Type_Pointer_Node* >(decl->get_return_type(m))) {
//             return_devault_value = ast::create_node_return_statement(m, ast::create_node_literal_natural(m, compiler::symbol::number_to_symbol(m->symbol_table, 0)));
//           } else {
//             return_devault_value = ast::create_node_return_statement(m, ast::create_node_function_call(m, ast::deep_copy(m, decl->get_return_type(m)), NULL));
//           }

//           ast::ProgramPoint_List_Node* body = ast::create_node_program_point(m, return_devault_value, NULL);
//           body                              = ast::create_node_program_point(m, call, body);

//           ast::Function_Literal_Node* literal = ast::create_node_function_literal(m, arguments, decl->get_return_type(m), body);
//           assignment->set_right_operand(m, literal);
//         }
//       }
//       if (ast::Effect_Call_Node* call = ast::is_instance< ast::Effect_Call_Node* >(point->get_statement(m))) {
//         ast::Declarations_List_Node* arguments = call->get_arguments(m);

//         point->set_statement(m, ast::create_node_function_call(m, call->get_effect(m), arguments));
//       }

//       point = point->get_next_program_point(m);
//     }
//   }

//   convert_effect_declarations(data, m, ast::left_of(m, root));
//   convert_effect_declarations(data, m, ast::right_of(m, root));
// }

void handler_rewrite(
    lib::Set< ast::Return_Node_Statement* >* ignore, Handler_Pass_Data* data, ast::Manager* m, ast::Node* root, ast::ProgramPoint_List_Node* pp, ast::Literal_Symbol_Node* var) {
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

    parser::print_ast(m, new_return);
    parser::print_ast(m, pp);

    pp->insert(m, new_return);

    ast::Literal_Symbol_Node* set_returning_symbol = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "ctx_set_returning"));
    ast::Declarations_List_Node* set_returning_arguments = ast::create_node_declarations_list(m, ast::create_node_literal_true(m), NULL);
    ast::Function_Call_Node* set_returning_call = ast::create_node_function_call(m, set_returning_symbol, set_returning_arguments);

    pp->insert(m, set_returning_call);

    ast::Variable_Assignment_Node* assignment = ast::create_node_assignment(m, ast::create_node_pointer_value(m, ast::deep_copy(m, var)), ret->get_expression(m));

    pp->set_statement(m, assignment);

    return;
  }

  if (ast::ProgramPoint_List_Node* point = ast::is_instance< ast::ProgramPoint_List_Node* >(root)) {
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

      ast::ProgramPoint_List_Node* statements = handler->get_body(m);

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

ast::Literal_Struct_Node* create_effect_agruments_struct(Handler_Pass_Data* data, ast::Manager* m, ast::Function_Literal_Node* lit) {
  ast::Declarations_List_Node* arguments = lit->get_arguments(m);

  ast::ProgramPoint_List_Node* members = NULL;

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
  // parser::print_ast(m, node);

  if (ast::is_semantic_node(node) == false) {
    return;
  }

  if (ast::Function_Literal_Node* literal = ast::is_instance< ast::Function_Literal_Node* >(node)) {
    return handler_rewrite_pass(data, m, literal->get_body(m), literal->get_return_type(m));
  }

  if (ast::Literal_Handler_Node* handler = ast::is_instance< ast::Literal_Handler_Node* >(node)) {
    ast::ProgramPoint_List_Node* body = handler->get_body(m);
    ast::ProgramPoint_List_Node* head = body;

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

          ast::Literal_Struct_Node* structure_literal = create_effect_agruments_struct(data, m, literal);

          ast::Literal_Symbol_Node* structure_symbol = ast::create_node_literal_symbol(m, symbol_with_prefix(m->symbol_table, literal_name->get_symbol(m), "args_"));
          ast::Declaration_Constant_Node* strucuture_decl = ast::create_constant_declaration(m, structure_symbol, ast::create_node_type_struct(m));
          ast::Variable_Assignment_Node* structure_assignment = ast::create_node_assignment(m, strucuture_decl, structure_literal);

          body->set_statement(m, structure_assignment);
          body->insert(m, statement);
          body = body->get_next_program_point(m);

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

          ast::Literal_Symbol_Node* var_symbol = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "ret"));
          ast::Declaration_Variable_Node* var = ast::create_variable_declaration(m, var_symbol, ast::create_node_type_pointer(m, ast::deep_copy(m, return_type)));

          literal->push_argument(m, var);

          if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(assignment->get_left_operand(m))) {
            ast::Type_Arrow_Node* arrow_from = ast::is_instance< ast::Type_Arrow_Node* >(var->get_type(m));

            assert(arrow_from);

            ast::Node* from = arrow_from->get_from_type(m);
            ast::Node* type = ast::create_node_type_pointer(m, ast::deep_copy(m, return_type));

            if (ast::is_instance< ast::Type_Unit_Node* >(from)) {
              from = ast::create_node_type_pointer(m, type);
            } else {
              from = ast::create_node_arithmetic_mul(m, from, type);
            }

            arrow_from->set_from_type(m, from);
            arrow_from->set_to_type(m, ast::create_node_type_i32(m));
            literal->set_return_type(m, ast::create_node_type_i32(m));
          }

          if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(assignment->get_left_operand(m))) {
            ast::Type_Arrow_Node* arrow_from = ast::is_instance< ast::Type_Arrow_Node* >(var->get_type(m));

            assert(arrow_from);

            ast::Node* from = arrow_from->get_from_type(m);
            ast::Node* type = ast::create_node_type_pointer(m, ast::deep_copy(m, return_type));

            if (ast::is_instance< ast::Type_Unit_Node* >(from)) {
              from = ast::create_node_type_pointer(m, type);
            } else {
              from = ast::create_node_arithmetic_mul(m, from, type);
            }

            arrow_from->set_from_type(m, from);
            arrow_from->set_to_type(m, ast::create_node_type_i32(m));
            literal->set_return_type(m, ast::create_node_type_i32(m));
          }

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

void add_handle_effect_check(
    Handler_Pass_Data* data,
    ast::Manager* m,
    ast::Literal_Symbol_Node* literal_symbol,
    ast::Function_Literal_Node* literal,
    ast::Literal_Symbol_Node* ret_var,
    ast::ProgramPoint_List_Node* point) {
  u64* hash = lib::search(data->hashes, literal_symbol->get_symbol_id());

  assert(hash);

  ast::Literal_Natural_Node* nat = ast::create_node_literal_natural(m, number_to_symbol(m->symbol_table, *hash));

  u64 n = data->yielding_calls++;

  Symbol sym = symbol_with_prefix(m->symbol_table, symbol_with_prefix(m->symbol_table, number_to_symbol(m->symbol_table, n), literal_symbol->get_symbol(m)), "yielding_to_");

  ast::Literal_Symbol_Node* ctx_is_yielding_to_symbol = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "ctx_is_yielding_to"));
  ast::Literal_Symbol_Node* ctx_is_yielding_to_var_symbol = ast::create_node_literal_symbol(m, sym);
  ast::Literal_Symbol_Node* ctx_get_handler_args_symbol = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "ctx_get_handler_args"));
  ast::Literal_Symbol_Node* ctx_get_handler_args_var_symbol = ast::create_node_literal_symbol(m, symbol_with_prefix(m->symbol_table, literal_symbol->get_symbol(m), "args_for_"));
  ast::Literal_Symbol_Node* ctx_is_returning_symbol = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "ctx_is_returning"));
  ast::Literal_Symbol_Node* ctx_is_returning_var_symbol =
      ast::create_node_literal_symbol(m, symbol_with_prefix(m->symbol_table, literal_symbol->get_symbol(m), "ctx_is_returning_"));

  ast::Declarations_List_Node* ctx_is_yielding_to_args = ast::create_node_declarations_list(m, nat, NULL);
  ast::Function_Call_Node* ctx_is_yielding_to_call = ast::create_node_function_call(m, ctx_is_yielding_to_symbol, ctx_is_yielding_to_args);

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

  handler_args = ast::create_node_declarations_list(m, ast::create_node_value_address(m, ret_var), handler_args);
  handler_args = ast::create_node_declarations_list(m, ctx_get_handler_args_var_symbol, handler_args);

  ast::Function_Call_Node* handler_call = ast::create_node_function_call(m, literal_symbol, handler_args);

  ast::ProgramPoint_List_Node* if_stmt_body = NULL;

  ast::Function_Call_Node* ctx_is_returning_call = ast::create_node_function_call(m, ctx_is_returning_symbol, NULL);
  ast::Declaration_Constant_Node* ctx_is_returning_decl = ast::create_constant_declaration(m, ctx_is_returning_var_symbol, ast::create_node_type_i32(m));
  ast::Variable_Assignment_Node* ctx_is_returning_assignment = ast::create_node_assignment(m, ctx_is_returning_decl, ctx_is_returning_call);

  ast::Literal_Symbol_Node* set_returning_symbol = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "ctx_set_returning"));
  ast::Declarations_List_Node* set_returning_arguments = ast::create_node_declarations_list(m, ast::create_node_literal_false(m), NULL);
  ast::Function_Call_Node* set_returning_call = ast::create_node_function_call(m, set_returning_symbol, set_returning_arguments);

  ast::ProgramPoint_List_Node* if_stmt_return_body = NULL;

  if_stmt_return_body = ast::create_node_program_point(m, ast::create_node_return_statement(m, ast::deep_copy(m, ret_var)), if_stmt_return_body);
  if_stmt_return_body = ast::create_node_program_point(m, set_returning_call, if_stmt_return_body);

  ast::If_Node_Statement* if_stmt_return = ast::create_node_if_statement(m, ctx_is_returning_var_symbol, if_stmt_return_body);

  if_stmt_body = ast::create_node_program_point(m, if_stmt_return, if_stmt_body);
  if_stmt_body = ast::create_node_program_point(m, ctx_is_returning_assignment, if_stmt_body);
  if_stmt_body = ast::create_node_program_point(m, handler_call, if_stmt_body);
  if_stmt_body = ast::create_node_program_point(m, ctx_get_handler_args_assignment, if_stmt_body);

  ast::If_Node_Statement* if_stmt = ast::create_node_if_statement(m, ctx_is_yielding_to_var_symbol, if_stmt_body);

  point->insert(m, if_stmt);
  point->insert(m, ctx_is_yileding_assignment);
}

void convert_with_statements(Handler_Pass_Data* data, ast::Manager* m, ast::Node* root, ast::Function_Literal_Node* fun, ast::ProgramPoint_List_Node* point) {
  if (!ast::is_semantic_node(root)) {
    return;
  }

  if (ast::ProgramPoint_List_Node* pp = ast::is_instance< ast::ProgramPoint_List_Node* >(root)) {
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

    ast::Literal_Symbol_Node** ret_var_symbol_ref = lib::search(data->ret_var_map, fun);
    ast::Literal_Symbol_Node* ret_var_symbol = NULL;

    if (!ret_var_symbol_ref) {
      ret_var_symbol = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, "ret"));
      ast::Declaration_Variable_Node* decl = ast::create_variable_declaration(m, ret_var_symbol, ast::deep_copy(m, fun->get_return_type(m)));
      point = point->insert(m, decl);

      lib::insert(data->ret_var_map, fun, ret_var_symbol);
    } else {
      ret_var_symbol = *ret_var_symbol_ref;
    }

    ast::Declarations_List_Node* handlers = with_stmt->get_list(m);

    while (ast::is_semantic_node(handlers)) {
      if (ast::Literal_Symbol_Node* symbol = ast::is_instance< ast::Literal_Symbol_Node* >(handlers->get_declaration(m))) {
        ast::Literal_Handler_Node** h = lib::search(data->symbol_to_handler, symbol->get_symbol_id());

        assert(h);

        ast::Literal_Handler_Node* handler = *h;

        ast::ProgramPoint_List_Node* handler_statements = handler->get_body(m);

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

              add_handle_effect_check(data, m, literal_symbol, literal, ret_var_symbol, point);
              convert_with_statements(data, m, literal, literal, NULL);
            }
          }

          handler_statements = handler_statements->get_next_program_point(m);
        }
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

  if (ast::ProgramPoint_List_Node* pp = ast::is_instance< ast::ProgramPoint_List_Node* >(root)) {
    ast::Node* old_parent = parent;
    ast::ProgramPoint_List_Node* parent = pp;

    while (ast::is_semantic_node(pp)) {
      if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(pp->get_statement(m))) {
        if (ast::Literal_Handler_Node* handler = ast::is_instance< ast::Literal_Handler_Node* >(assignment->get_right_operand(m))) {
          ast::ProgramPoint_List_Node* body = handler->get_body(m);

          ast::ProgramPoint_List_Node* p = pp;

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

void handeler_conversion_pass(Handler_Pass_Data* data, ast::Manager* m, ast::Node* root) {
  // handeler_conversion_pass_recursive(data, m, root, ast::is_instance< ast::ProgramPoint_List_Node* >(root));
  // convert_effect_declarations(data, m, root);
  populate_handler_pass_data(data, m, root);

  handler_rewrite_pass(data, m, root, NULL);
  convert_with_statements(data, m, root, NULL, NULL);
  inline_handlers(data, m, root, NULL);

  add_evidence_context_to_functions(data, m, root);
  add_evidence_context_to_calls(data, m, root);
}

} // namespace handler
