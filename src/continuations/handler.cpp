#include "handler.hpp"
#include "ast/ast_control_flow.hpp"
#include "ast/ast_declaration.hpp"
#include "ast/ast_function.hpp"
#include "ast/ast_literals.hpp"
#include "ast/ast_manager.hpp"
#include "ast/ast_operations.hpp"
#include "ast/ast_program_point.hpp"
#include "ast/ast_types.hpp"
#include "compiler/symbol_table.hpp"
#include "lib/set.hpp"
#include "lib/table.hpp"
#include <cassert>

using namespace compiler;
using namespace symbol;

namespace handler {

struct Handler_Pass_Data {
  lib::Table< symbol::Id, u64 >* hashes;

  // lib::Set< ast::Variable_Assignment_Node* >* effect_handlers_assignments;
  //  lib::Set< ast::Variable_Assignment_Node* >* prompt_handlers_assignments;

  // lib::Table< symbol::Id, ast::Variable_Assignment_Node* >* handler_prompt;

  lib::Set< ast::Function_Literal_Node* >* effect_functions;
  lib::Set< ast::Function_Literal_Node* >* prompt_functions;
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

Handler_Pass_Data* handler_pass_data_create() {
  Handler_Pass_Data* h = new Handler_Pass_Data();

  h->hashes = lib::table_create< symbol::Id, u64 >();
  // h->handler_prompt = lib::table_create< symbol::Id, ast::Variable_Assignment_Node* >();
  // h->effect_handlers_assignments = lib::set_create< ast::Variable_Assignment_Node* >();
  // h->prompt_handlers_assignments = lib::set_create< ast::Variable_Assignment_Node* >();
  h->effect_functions = lib::set_create< ast::Function_Literal_Node* >();
  h->prompt_functions = lib::set_create< ast::Function_Literal_Node* >();
  return h;
}

void handler_pass_data_destroy(Handler_Pass_Data* d) {
  lib::table_delete(d->hashes);
  // lib::table_delete(d->handler_prompt);

  lib::set_delete(d->prompt_functions);
  lib::set_delete(d->effect_functions);

  delete d;
}

ast::Function_Literal_Node* create_prompt_function(Handler_Pass_Data* data, ast::Manager* m, ast::Literal_Handler_Node* handler, ast::Node* type) {
  ast::Declarations_List_Node* prompt_arguments = ast::create_node_declarations_list(m, 0, 0);

  ast::ProgramPoint_List_Node* body = handler->get_body(m);
  // ast::ProgramPoint_List_Node* iter = body;

  while (ast::is_semantic_node(body->get_next_program_point(m))) {
    ast::Node* statement = body->get_statement(m);

    if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(statement)) {
      if (ast::Function_Literal_Node* literal = ast::is_instance< ast::Function_Literal_Node* >(assignment->get_right_operand(m))) {
        lib::insert(data->effect_functions, literal);
      }
    }

    body = body->get_next_program_point(m);
  }

  ast::Function_Literal_Node* prompt_function = ast::create_node_function_literal(m, 0, ast::deep_copy(m, type), handler->get_body(m));

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

      if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
        ast::Type_Unit_Node*  unit  = ast::create_node_type_unit(m);
        ast::Type_Arrow_Node* arrow = ast::create_node_type_arrow(m, unit, ast::deep_copy(m, type));

        var->set_type(m, arrow);
      }

      if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
        ast::Type_Unit_Node*  unit  = ast::create_node_type_unit(m);
        ast::Type_Arrow_Node* arrow = ast::create_node_type_arrow(m, unit, ast::deep_copy(m, type));

        var->set_type(m, arrow);
      }

      assignment->set_right_operand(m, create_prompt_function(data, m, handler, ast::deep_copy(m, type)));

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
}

} // namespace handler
