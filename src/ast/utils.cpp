#include "utils.hpp"
#include "ast/ast_declaration.hpp"
#include "ast/ast_function.hpp"
#include "ast/ast_literals.hpp"
#include "ast/ast_manager.hpp"
#include "ast/ast_operations.hpp"
#include "ast/ast_pointer.hpp"
#include "ast/ast_program_point.hpp"
#include "ast/ast_types.hpp"
#include "compiler/symbol_table.hpp"
#include <cassert>

namespace ast {
namespace utils {
using ast::Declaration_Variable_Node;

ast::Literal_Symbol_Node* symbol(ast::Manager* m, const i8* name) {
  compiler::symbol::Symbol s = compiler::symbol::set_entry(m->symbol_table, name);
  return ast::create_node_literal_symbol(m, s);
}

ast::Declaration_Variable_Node* variable(ast::Manager* m, const i8* name, ast::Node* type) {
  return ast::create_variable_declaration(m, symbol(m, name), type);
}

Declaration_Constant_Node* constant(ast::Manager* m, const i8* name, ast::Node* type) {
  return ast::create_constant_declaration(m, symbol(m, name), type);
}

ast::Variable_Assignment_Node* assignment(ast::Manager* m, ast::Node* left, ast::Node* right) {
  return ast::create_node_assignment(m, left, right);
}

ast::Literal_Symbol_Node* natural(ast::Manager* m, u64 nat) {
  compiler::symbol::Symbol s = compiler::symbol::number_to_symbol(m->symbol_table, nat);
  return ast::create_node_literal_symbol(m, s);
}

ast::Literal_Symbol_Node* prefix(ast::Manager* m, const i8* prefix, u64 nat) {
  compiler::symbol::Symbol a = compiler::symbol::set_entry(m->symbol_table, prefix);
  compiler::symbol::Symbol b = compiler::symbol::number_to_symbol(m->symbol_table, nat);
  compiler::symbol::Symbol p = compiler::symbol::symbol_with_prefix(m->symbol_table, b, a);

  return ast::create_node_literal_symbol(m, p);
}

ast::Literal_Symbol_Node* prefix(ast::Manager* m, const i8* prefix, const i8* name) {

  compiler::symbol::Symbol a = compiler::symbol::set_entry(m->symbol_table, prefix);
  compiler::symbol::Symbol b = compiler::symbol::set_entry(m->symbol_table, name);
  compiler::symbol::Symbol p = compiler::symbol::symbol_with_prefix(m->symbol_table, b, a);

  return ast::create_node_literal_symbol(m, p);
}

ast::Literal_Symbol_Node* prefix(ast::Manager* m, const i8* prefix, ast::Literal_Symbol_Node* name) {

  compiler::symbol::Symbol a = compiler::symbol::set_entry(m->symbol_table, prefix);
  compiler::symbol::Symbol p = compiler::symbol::symbol_with_prefix(m->symbol_table, name->get_symbol(m), a);

  return ast::create_node_literal_symbol(m, p);
}

ast::Literal_Symbol_Node* prefix(ast::Manager* m, ast::Literal_Symbol_Node* prefix, ast::Literal_Symbol_Node* name) {

  compiler::symbol::Symbol p = compiler::symbol::symbol_with_prefix(m->symbol_table, name->get_symbol(m), prefix->get_symbol(m));

  return ast::create_node_literal_symbol(m, p);
}

ast::Literal_Symbol_Node* get_symbol(ast::Manager* m, ast::Node* n) {
  if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(n)) {
    return var->get_symbol(m);
  }

  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(n)) {
    return var->get_symbol(m);
  }

  if (ast::Literal_Symbol_Node* symbol = ast::is_instance< ast::Literal_Symbol_Node* >(n)) {
    return symbol;
  }

  assert(false && "unknow type");

  return 0;
}
ast::Function_Call_Node* call(ast::Manager* m, ast::Node* name) {
  return ast::create_node_function_call(m, name, 0);
}
ast::Function_Call_Node* call(ast::Manager* m, ast::Node* name, ast::Node* argA) {
  ast::Declarations_List_Node* args = ast::create_node_declarations_list(m, argA, NULL);
  return ast::create_node_function_call(m, name, args);
}
ast::Function_Call_Node* call(ast::Manager* m, ast::Node* name, ast::Node* argA, ast::Node* argB) {
  ast::Declarations_List_Node* args = NULL;

  args = ast::create_node_declarations_list(m, argB, args);
  args = ast::create_node_declarations_list(m, argA, args);

  return ast::create_node_function_call(m, name, args);
}

ast::Function_Call_Node* call(ast::Manager* m, const char* name, ast::Node* argA) {
  ast::Literal_Symbol_Node* s = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, name));
  ast::Declarations_List_Node* args = ast::create_node_declarations_list(m, argA, NULL);
  return ast::create_node_function_call(m, s, args);
}

ast::Function_Call_Node* call(ast::Manager* m, const char* name, ast::Node* argA, ast::Node* argB) {
  ast::Literal_Symbol_Node* s = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, name));
  ast::Declarations_List_Node* args = NULL;

  args = ast::create_node_declarations_list(m, argB, args);
  args = ast::create_node_declarations_list(m, argA, args);

  return ast::create_node_function_call(m, s, args);
}

ast::Function_Call_Node* call(ast::Manager* m, const char* name, ast::Node* argA, ast::Node* argB, ast::Node* argC) {
  ast::Literal_Symbol_Node* s = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, name));
  ast::Declarations_List_Node* args = NULL;

  args = ast::create_node_declarations_list(m, argC, args);
  args = ast::create_node_declarations_list(m, argB, args);
  args = ast::create_node_declarations_list(m, argA, args);

  return ast::create_node_function_call(m, s, args);
}

ast::Function_Call_Node* call(ast::Manager* m, const char* name, ast::Node* argA, ast::Node* argB, ast::Node* argC, ast::Node* argD) {
  ast::Literal_Symbol_Node* s = ast::create_node_literal_symbol(m, set_entry(m->symbol_table, name));
  ast::Declarations_List_Node* args = NULL;

  args = ast::create_node_declarations_list(m, argD, args);
  args = ast::create_node_declarations_list(m, argC, args);
  args = ast::create_node_declarations_list(m, argB, args);
  args = ast::create_node_declarations_list(m, argA, args);

  return ast::create_node_function_call(m, s, args);
}

ast::Node* get_nth_argument(ast::Manager* m, ast::Function_Call_Node* call, u64 n) {
  ast::Declarations_List_Node* arguments = call->get_arguments(m);
  while (arguments && n > 0) {
    n -= 1;
    arguments = arguments->get_next_declaration(m);
  }

  return arguments->get_declaration(m);
}

ast::Node* default_initialization(ast::Manager* m, ast::Node* type) {
  if (ast::is_instance< ast::Type_Int32_Node* >(type)) {
    return natural(m, 0);
  }

  if (ast::Type_Pointer_Node* ptr = ast::is_instance< ast::Type_Pointer_Node* >(type)) {
		if(ast::is_instance<ast::Type_Any_Node*>(ptr->get_pointer_type(m))) {
			return natural(m, 0);
		}
		
		if(ast::is_instance<ast::Type_Unit_Node*>(ptr->get_pointer_type(m))) {
			return natural(m, 0);
		}
		
    return natural(m, 0);
  }
	
	if(ast::is_instance<ast::Type_Unit_Node*>(type)) {
		return natural(m, 0);
	}

  if (ast::is_instance< ast::Type_Any_Node* >(type)) {
    return natural(m, 0);
  }

  return call(m, type);
}

void add_argument_to_function_declaration(ast::Manager* m, ast::Function_Literal_Node* literal, ast::Variable_Assignment_Node* assignment, ast::Node* type) {
  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(assignment->get_left_operand(m))) {
    ast::Type_Arrow_Node* arrow_from = ast::is_instance< ast::Type_Arrow_Node* >(var->get_type(m));

    assert(arrow_from);

    ast::Node* from = arrow_from->get_from_type(m);

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

    if (ast::is_instance< ast::Type_Unit_Node* >(from)) {
      from = ast::create_node_type_pointer(m, type);
    } else {
      from = ast::create_node_arithmetic_mul(m, from, type);
    }

    arrow_from->set_from_type(m, from);
    arrow_from->set_to_type(m, ast::create_node_type_i32(m));
    literal->set_return_type(m, ast::create_node_type_i32(m));
  }
}

} // namespace utils
} // namespace ast
