#include "transpiler.hpp"
#include "ast/ast_control_flow.hpp"
#include "ast/ast_declaration.hpp"
#include "ast/ast_function.hpp"
#include "ast/ast_literals.hpp"
#include "ast/ast_manager.hpp"

#include "ast/ast_operations.hpp"
#include "ast/ast_pointer.hpp"
#include "ast/ast_program_point.hpp"
#include "ast/ast_types.hpp"
#include "stdio.h"
#include <cstdio>

namespace compiler {
namespace transpiler {

struct C_Transpiler_Data {
  b8 emit_semicolon_after_program_point;
};

void output_tabs(u64 tabs) {

  for (i32 i = 0; i < tabs; i++) {
    printf(" ");
  }
}

void output_c_code_rec(C_Transpiler_Data* data, ast::Manager* m, ast::Node* root, u64 tabs = 0) {
  if (ast::Literal_Natural_Node* nat = ast::is_instance< ast::Literal_Natural_Node* >(root)) {

    symbol::Symbol s = nat->get_symbol(m);

    for (u64 i = 0; i < s.size; i++) {
      printf("%c", symbol::char_at(&s, i));
    }

    return;
  }

  if (ast::Literal_Symbol_Node* sym = ast::is_instance< ast::Literal_Symbol_Node* >(root)) {

    symbol::Symbol s = sym->get_symbol(m);

    for (u64 i = 0; i < s.size; i++) {
      printf("%c", symbol::char_at(&s, i));
    }

    return;
  }

  if (ast::Type_Unit_Node* unit = ast::is_instance< ast::Type_Unit_Node* >(root)) {
    printf("unsigned char*");
    return;
  }

  if (ast::Type_Int32_Node* unit = ast::is_instance< ast::Type_Int32_Node* >(root)) {
    printf("int");
    return;
  }

  if (ast::Type_Any_Node* any = ast::is_instance< ast::Type_Any_Node* >(root)) {
    printf("unsigned char");
    return;
  }

  if (ast::Literal_True_Node* any = ast::is_instance< ast::Literal_True_Node* >(root)) {
    printf("1");
    return;
  }

  if (ast::Literal_False_Node* any = ast::is_instance< ast::Literal_False_Node* >(root)) {
    printf("0");
    return;
  }

  if (ast::Type_Evidence_Context_Node* any = ast::is_instance< ast::Type_Evidence_Context_Node* >(root)) {
    printf("context_t");
    return;
  }

  if (ast::Type_Pointer_Node* ptr = ast::is_instance< ast::Type_Pointer_Node* >(root)) {
    output_c_code_rec(data, m, ptr->get_pointer_type(m), tabs);
    printf("*");
    return;
  }

  if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(root)) {
    output_c_code_rec(data, m, var->get_type(m), tabs);
    printf(" ");
    output_c_code_rec(data, m, var->get_symbol(m), tabs);
    return;
  }

  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(root)) {
    printf("const ");
    output_c_code_rec(data, m, var->get_type(m), tabs);
    printf(" ");
    output_c_code_rec(data, m, var->get_symbol(m), tabs);
    return;
  }

  if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(root)) {
    ast::Node* left = assignment->get_left_operand(m);
    ast::Node* right = assignment->get_right_operand(m);

    if (ast::Function_Literal_Node* lit = ast::is_instance< ast::Function_Literal_Node* >(right)) {
      output_c_code_rec(data, m, lit->get_return_type(m), tabs);
      printf(" ");

      if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
        output_c_code_rec(data, m, var->get_symbol(m), tabs);
      }

      if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
        output_c_code_rec(data, m, var->get_symbol(m), tabs);
      }

      printf("(");

      ast::Declarations_List_Node* arguments = lit->get_arguments(m);

      while (ast::is_semantic_node(arguments)) {
        output_c_code_rec(data, m, arguments->get_declaration(m), tabs);
        arguments = arguments->get_next_declaration(m);

        if (ast::is_semantic_node(arguments)) {
          printf(", ");
        }
      }

      printf(")");

      printf("{\n");

      b8 old_emit_value = data->emit_semicolon_after_program_point;

      data->emit_semicolon_after_program_point = true;

      output_c_code_rec(data, m, lit->get_body(m), tabs + 2);

      data->emit_semicolon_after_program_point = old_emit_value;

      printf("}\n");

      return;
    }

    if (ast::Literal_Struct_Node* structure = ast::is_instance< ast::Literal_Struct_Node* >(right)) {
      printf("struct ");

      if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
        output_c_code_rec(data, m, var->get_symbol(m), tabs);
      }

      if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
        output_c_code_rec(data, m, var->get_symbol(m), tabs);
      }
      printf(" {\n");

      b8 old_emit_value = data->emit_semicolon_after_program_point;

      data->emit_semicolon_after_program_point = true;

      output_c_code_rec(data, m, structure->get_members(m), tabs + 2);

      data->emit_semicolon_after_program_point = old_emit_value;

      printf("};");

      return;
    }

    output_c_code_rec(data, m, left, tabs);
    printf(" = ");
    output_c_code_rec(data, m, right, tabs);
    return;
  }

  if (ast::Value_Address_Node* addr = ast::is_instance< ast::Value_Address_Node* >(root)) {
    printf("&(");
    output_c_code_rec(data, m, addr->get_variable(m), tabs);
    printf(")");
    return;
  }

  if (ast::Pointer_Value_Node* val = ast::is_instance< ast::Pointer_Value_Node* >(root)) {
    printf("*(");
    output_c_code_rec(data, m, val->get_variable(m), tabs);
    printf(")");
    return;
  }

  if (ast::Arithmetic_Operation_Add_Node* op = ast::is_instance< ast::Arithmetic_Operation_Add_Node* >(root)) {
    output_c_code_rec(data, m, op->get_left_operand(m), tabs);
    printf(" + ");
    output_c_code_rec(data, m, op->get_right_operand(m), tabs);
    return;
  }

  if (ast::Arithmetic_Operation_Sub_Node* op = ast::is_instance< ast::Arithmetic_Operation_Sub_Node* >(root)) {
    output_c_code_rec(data, m, op->get_left_operand(m), tabs);
    printf(" - ");
    output_c_code_rec(data, m, op->get_right_operand(m), tabs);
    return;
  }

  if (ast::Arithmetic_Operation_Mul_Node* op = ast::is_instance< ast::Arithmetic_Operation_Mul_Node* >(root)) {
    output_c_code_rec(data, m, op->get_left_operand(m), tabs);
    printf(" * ");
    output_c_code_rec(data, m, op->get_right_operand(m), tabs);
    return;
  }

  if (ast::Arithmetic_Operation_Div_Node* op = ast::is_instance< ast::Arithmetic_Operation_Div_Node* >(root)) {
    output_c_code_rec(data, m, op->get_left_operand(m), tabs);
    printf(" / ");
    output_c_code_rec(data, m, op->get_right_operand(m), tabs);
    return;
  }

  if (ast::Return_Node_Statement* ret = ast::is_instance< ast::Return_Node_Statement* >(root)) {
    printf("return ");

    if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(ret->get_expression(m))) {
      if (ast::is_instance< ast::Type_Unit_Node* >(call->get_function(m))) {
        printf("0");
        return;
      }
    }

    output_c_code_rec(data, m, ret->get_expression(m), tabs);
    return;
  }

  if (ast::If_Node_Statement* if_stmt = ast::is_instance< ast::If_Node_Statement* >(root)) {
    printf("if (");

    output_c_code_rec(data, m, if_stmt->get_condition(m), tabs);

    printf(") {\n");

    b8 old_emit_value = data->emit_semicolon_after_program_point;

    data->emit_semicolon_after_program_point = true;

    output_c_code_rec(data, m, if_stmt->get_body(m), tabs + 2);

    data->emit_semicolon_after_program_point = old_emit_value;

    output_tabs(tabs);
    printf("}\n");

    return;
  }

  if (ast::Elif_List_Node* elif = ast::is_instance< ast::Elif_List_Node* >(root)) {
    output_c_code_rec(data, m, elif->get_if(m), tabs);

    elif = elif->get_elif(m);

    while (ast::is_semantic_node(elif)) {
      output_tabs(tabs);
      printf(" else ");
      output_c_code_rec(data, m, elif->get_if(m), tabs);
      elif->get_elif(m);
    }

    return;
  }

  if (ast::Cast_Type_Node* cast = ast::is_instance< ast::Cast_Type_Node* >(root)) {
    printf("(");
    output_c_code_rec(data, m, cast->get_to_type(m), tabs);
    printf(")");
    printf("(");
    output_c_code_rec(data, m, cast->get_expr(m), tabs);
    printf(")");
    return;
  }

  if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(root)) {
    output_c_code_rec(data, m, call->get_function(m));

    printf("(");

    ast::Declarations_List_Node* arguments = call->get_arguments(m);

    while (ast::is_semantic_node(arguments)) {
      output_c_code_rec(data, m, arguments->get_declaration(m), tabs);
      arguments = arguments->get_next_declaration(m);
      if (ast::is_semantic_node(arguments)) {
        printf(", ");
      }
    }

    printf(")");

    return;
  }

  if (ast::Member_Access_Node* access = ast::is_instance< ast::Member_Access_Node* >(root)) {
    ast::Node* object = access->get_object(m);

    ast::Node* tmp = object;

    u64 dereferences = 0;

    while (ast::Pointer_Value_Node* ptr_val = ast::is_instance< ast::Pointer_Value_Node* >(tmp)) {
      dereferences += 1;
      tmp = ptr_val->get_variable(m);
    }

    u64 ptrs = 0;

    if (ast::Cast_Type_Node* cast = ast::is_instance< ast::Cast_Type_Node* >(tmp)) {
      ast::Node* type = cast->get_to_type(m);

      while (ast::Type_Pointer_Node* ptr = ast::is_instance< ast::Type_Pointer_Node* >(type)) {
        ptrs += 1;
        type = ptr->get_pointer_type(m);
      }
    }
		printf("(");
    output_c_code_rec(data, m, object, tabs);
		printf(")");

		if (dereferences < ptrs) {
      printf("->");
    } else {
      printf(".");
    }

    output_c_code_rec(data, m, ast::right_of(m, access), tabs);
    return;
  }

  if (ast::ProgramPoint_List_Node* pp = ast::is_instance< ast::ProgramPoint_List_Node* >(root)) {
    while (ast::is_semantic_node(pp)) {
      output_tabs(tabs);

      ast::Node* statement = pp->get_statement(m);

      output_c_code_rec(data, m, statement, tabs);

      if (data->emit_semicolon_after_program_point) {
        b8 blacklisted = ast::is_instance< ast::Elif_List_Node* >(statement) || ast::is_instance< ast::If_Node_Statement* >(statement);

        if (!blacklisted) {
          printf(";");
        }
      }

      printf("\n");

      pp = pp->get_next_program_point(m);
    }

    return;
  }

  if (ast::is_instance< ast::Literal_Nothing_Node* >(root)) {
    printf("0");
    return;
  }

  printf("/*TODO*/");
}

void output_c_function_declarations_code_rec(C_Transpiler_Data* data, ast::Manager* m, ast::Node* root, u64 tabs = 0) {
  if (ast::Literal_Symbol_Node* sym = ast::is_instance< ast::Literal_Symbol_Node* >(root)) {

    symbol::Symbol s = sym->get_symbol(m);

    for (u64 i = 0; i < s.size; i++) {
      printf("%c", symbol::char_at(&s, i));
    }

    return;
  }

  if (ast::Type_Unit_Node* unit = ast::is_instance< ast::Type_Unit_Node* >(root)) {
    printf("unsigned char*");
    return;
  }

  if (ast::Type_Int32_Node* unit = ast::is_instance< ast::Type_Int32_Node* >(root)) {
    printf("int");
    return;
  }

  if (ast::Type_Any_Node* any = ast::is_instance< ast::Type_Any_Node* >(root)) {
    printf("unsigned char");
    return;
  }

  if (ast::Type_Evidence_Context_Node* any = ast::is_instance< ast::Type_Evidence_Context_Node* >(root)) {
    printf("context_t");
    return;
  }

  if (ast::Type_Pointer_Node* ptr = ast::is_instance< ast::Type_Pointer_Node* >(root)) {
    output_c_function_declarations_code_rec(data, m, ptr->get_pointer_type(m), tabs);
    printf("*");
    return;
  }

  if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(root)) {
    output_c_function_declarations_code_rec(data, m, var->get_type(m), tabs);
    printf(" ");
    output_c_function_declarations_code_rec(data, m, var->get_symbol(m), tabs);
    return;
  }

  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(root)) {
    printf("const ");
    output_c_function_declarations_code_rec(data, m, var->get_type(m), tabs);
    printf(" ");
    output_c_function_declarations_code_rec(data, m, var->get_symbol(m), tabs);
    return;
  }

  if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(root)) {
    ast::Node* left = assignment->get_left_operand(m);
    ast::Node* right = assignment->get_right_operand(m);

    if (ast::Function_Literal_Node* lit = ast::is_instance< ast::Function_Literal_Node* >(right)) {
      output_c_function_declarations_code_rec(data, m, lit->get_return_type(m), tabs);
      printf(" ");

      if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
        output_c_function_declarations_code_rec(data, m, var->get_symbol(m), tabs);
      }

      if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
        output_c_function_declarations_code_rec(data, m, var->get_symbol(m), tabs);
      }

      printf("(");

      ast::Declarations_List_Node* arguments = lit->get_arguments(m);

      while (ast::is_semantic_node(arguments)) {
        output_c_function_declarations_code_rec(data, m, arguments->get_declaration(m), tabs);
        arguments = arguments->get_next_declaration(m);

        if (ast::is_semantic_node(arguments)) {
          printf(", ");
        }
      }

      printf(")");

      return;
    }

    if (ast::Literal_Struct_Node* structure = ast::is_instance< ast::Literal_Struct_Node* >(right)) {
      printf("typedef struct ");

      if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
        output_c_function_declarations_code_rec(data, m, var->get_symbol(m), tabs);
      }

      if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
        output_c_function_declarations_code_rec(data, m, var->get_symbol(m), tabs);
      }

      printf(" ");

      if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
        output_c_function_declarations_code_rec(data, m, var->get_symbol(m), tabs);
      }

      if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
        output_c_function_declarations_code_rec(data, m, var->get_symbol(m), tabs);
      }

      return;
    }

    return;
  }

  if (ast::ProgramPoint_List_Node* pp = ast::is_instance< ast::ProgramPoint_List_Node* >(root)) {
    while (ast::is_semantic_node(pp)) {
      output_tabs(tabs);

      ast::Node* statement = pp->get_statement(m);

      output_c_function_declarations_code_rec(data, m, statement, tabs);

      if (data->emit_semicolon_after_program_point) {
        printf(";");
      }

      printf("\n");

      pp = pp->get_next_program_point(m);
    }

    return;
  }

  printf("/*TODO*/");
}

void output_c_code(ast::Manager* m, ast::Node* root) {
  C_Transpiler_Data* data = new C_Transpiler_Data();

  printf("struct context_t {};\n");
  printf("void pop_frame(int);\n");
  printf("unsigned char* push_frame(int);\n");
  printf("int is_yielding(context_t*);\n");
  printf("int yielding_to_handler(int, context_t*);\n");
  printf("unsigned char* escape_frame(unsigned char*);\n");
  printf("void bubble(context_t*, const int, int, unsigned char*, unsigned char*(*)(unsigned char*, unsigned char*, context_t*, unsigned char*));\n");
  printf("void set_is_yielding_to(int, context_t*);\n");
  printf("void ctx_set_returning(int, context_t*);\n");
  printf("int ctx_is_yielding_to(int, context_t*);\n");
  printf("char* ctx_get_handler_args(context_t*);\n");
  printf("unsigned char*resume(unsigned char*, context_t*);\n");
  printf("int ctx_is_returning(context_t*);\n");
	printf("void deallocate(unsigned char*, context_t*);\n");
	printf("unsigned char* ctx_allocate_args(unsigned int, context_t*);\n");
  data->emit_semicolon_after_program_point = true;
  output_c_function_declarations_code_rec(data, m, root, 0);
  data->emit_semicolon_after_program_point = false;
  output_c_code_rec(data, m, root, 0);
  delete data;
}

} // namespace transpiler

} // namespace compiler
