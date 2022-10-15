#include "stackframe.hpp"
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
#include "continuations/continuations.hpp"
#include "lib/table.hpp"

#include <assert.h>

#include "context/context.hpp"
#include "lib/utils.hpp"
#include "parser/parser.hpp"

#include <stdio.h>
#include <sys/resource.h>

namespace stackframe {

struct Stack_Frame_Data {
  cps::CPS_Data* cps_data;

  u64 stack_size;

  lib::Table< ast::Node*, u64 >* stack_address;
  lib::Table< u64, u64 >*        stack_depth;

  lib::Table< ast::Function_Literal_Node*, u64 >* function_depth;

  lib::Table< ast::Function_Literal_Node*, ast::Variable_Assignment_Node* >* function_stackframe_allocation;

  lib::Table< ast::Function_Literal_Node*, ast::ProgramPoint_List_Node* >* setup_end;
};

Stack_Frame_Data* create_stack_frame_data(cps::CPS_Data* data) {
  Stack_Frame_Data* d = new Stack_Frame_Data();

  d->cps_data = data;

  d->stack_address                  = lib::table_create< ast::Node*, u64 >();
  d->stack_depth                    = lib::table_create< u64, u64 >();
  d->setup_end                      = lib::table_create< ast::Function_Literal_Node*, ast::ProgramPoint_List_Node* >();
  d->function_depth                 = lib::table_create< ast::Function_Literal_Node*, u64 >();
  d->function_stackframe_allocation = lib::table_create< ast::Function_Literal_Node*, ast::Variable_Assignment_Node* >();
  return d;
}

void destroy_stack_frame_data(Stack_Frame_Data* d) {
  lib::table_delete(d->stack_address);
  lib::table_delete(d->stack_depth);
  lib::table_delete(d->setup_end);
  lib::table_delete(d->function_depth);
  lib::table_delete(d->function_stackframe_allocation);

  cps::cps_data_destroy(d->cps_data);

  delete d;
}

cps::CPS_Data* stack_frame_data_get_cps_data(Stack_Frame_Data* data) {
  return data->cps_data;
}

u64 stack_frame_get_function_depth(Stack_Frame_Data* data, ast::Function_Literal_Node* lit) {
  if (u64* depth = lib::search(data->function_depth, lit)) {
    return *depth;
  }

  assert(false);

  return 0;
}

ast::ProgramPoint_List_Node* stack_frame_get_function_literal_setup_end(Stack_Frame_Data* data, ast::Manager*, ast::Function_Literal_Node* lit) {
  ast::ProgramPoint_List_Node** pp = lib::search< ast::Function_Literal_Node*, ast::ProgramPoint_List_Node* >(data->setup_end, lit);

  if (pp) {
    return *pp;
  }

  return NULL;
}

ast::Variable_Assignment_Node* stack_frame_get_function_local_stack_frame_allocation(Stack_Frame_Data* data, ast::Function_Literal_Node* f) {
  ast::Variable_Assignment_Node** a = lib::search(data->function_stackframe_allocation, f);

  if (a) {
    return *a;
  }

  return NULL;
}

u64 compute_sizeof_type(ast::Manager* m, ast::Node* root, context::Context* ctx) {
  assert(ast::is_semantic_node(root));

  if (ast::Literal_Struct_Node* ty = ast::is_instance< ast::Literal_Struct_Node* >(root)) {
    ast::ProgramPoint_List_Node* members = ty->get_members(m);

    u64 size = 0;

    while (ast::is_semantic_node(members)) {
      if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(members->get_statement(m))) {
        size += compute_sizeof_type(m, var->get_type(m), ctx);
      }

      if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(members->get_statement(m))) {
        size += compute_sizeof_type(m, var->get_type(m), ctx);
      }

      members = members->get_next_program_point(m);
    }

    return size;
  }

  if (ast::Type_Int32_Node* ty = ast::is_instance< ast::Type_Int32_Node* >(root)) {
    return sizeof(i32);
  }

  if (ast::Type_Pointer_Node* ty = ast::is_instance< ast::Type_Pointer_Node* >(root)) {
    return sizeof(void*);
  }

  if (ast::Type_Arrow_Node* ty = ast::is_instance< ast::Type_Arrow_Node* >(root)) {
    return sizeof(void*);
  }

  if (ast::Type_Any_Node* ty = ast::is_instance< ast::Type_Any_Node* >(root)) {
    return sizeof(void*);
  }

  if (ast::Literal_Symbol_Node* ty = ast::is_instance< ast::Literal_Symbol_Node* >(root)) {
    return compute_sizeof_type(m, context::context_get_struct_definition(ctx, m, ty), ctx);
  }

  assert(false && "Uncaught type");
}

u64 stack_allocate(Stack_Frame_Data* data, ast::Manager* m, ast::Node* root, context::Context* ctx, context::Context* args_ctx, u64 stack_size, u64 depth, ast::Node* parent) {
  if (!ast::is_semantic_node(root)) {
    return stack_size;
  }

  if (ast::Function_Literal_Node* f = ast::is_instance< ast::Function_Literal_Node* >(root)) {
    if (cps::is_continuation_closure(data->cps_data, m, f) == false) {
      stack_size       = 0;
      data->stack_size = 0;
      depth            = depth + 1;
    }

    lib::insert(data->function_depth, f, depth);

    ast::Literal_Symbol_Node* stack_ptr = ast::create_node_literal_symbol(m, compiler::symbol::number_to_symbol(m->symbol_table, depth, "sp"));

    ast::Declarations_List_Node* arguments = f->get_arguments(m);

    context::Context* context       = context::context_create(ctx);
    context::Context* arguments_ctx = context::context_create(NULL);

    ast::ProgramPoint_List_Node* assign_arguments = NULL;
    ast::Type_Pointer_Node*      stack_type       = NULL;

    u64 increment = 0;

    while (arguments) {
      ast::Literal_Symbol_Node* symbol = NULL;

      if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(arguments->get_declaration(m))) {
        context::context_declare(arguments_ctx, m, var);
        symbol = var->get_symbol(m);

        stack_type = ast::create_node_type_pointer(m, ast::deep_copy(m, var->get_type(m)));

        if (cps::is_temporary_variable(data->cps_data, m, var) == false) {
          lib::insert< ast::Node*, u64 >(data->stack_address, var, stack_size);

          increment = compute_sizeof_type(m, stack_type, context);
        }
      }

      if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(arguments->get_declaration(m))) {
        context::context_declare(arguments_ctx, m, var);

        symbol = var->get_symbol(m);

        stack_type = ast::create_node_type_pointer(m, ast::deep_copy(m, var->get_type(m)));

        if (cps::is_temporary_variable(data->cps_data, m, var) == false) {
          lib::insert< ast::Node*, u64 >(data->stack_address, var, stack_size);

          increment = compute_sizeof_type(m, stack_type, context);
        }
      }

      ast::Node* displacement = ast::create_node_literal_natural(m, compiler::symbol::number_to_symbol(m->symbol_table, stack_size));

      stack_size += increment;

      ast::Node* address = ast::create_node_arithmetic_add(m, ast::deep_copy(m, stack_ptr), displacement);

      ast::Cast_Type_Node*           cast       = ast::create_node_cast_type(m, ast::create_node_type_pointer(m, ast::deep_copy(m, stack_type)), address);
      ast::Pointer_Value_Node*       value      = ast::create_node_pointer_value(m, cast);
      ast::Variable_Assignment_Node* assignment = ast::create_node_assignment(m, value, ast::create_node_value_address(m, ast::deep_copy(m, symbol)));

      assign_arguments = ast::create_node_program_point(m, assignment, assign_arguments);

      arguments = arguments->get_next_declaration(m);
    }

    if (cps::is_continuation_closure(data->cps_data, m, f)) {
      stack_size = stack_allocate(data, m, f->get_body(m), context, arguments_ctx, stack_size, depth, f);
    } else {

      stack_size       = stack_allocate(data, m, f->get_body(m), context, arguments_ctx, stack_size, depth, f);
      data->stack_size = lib::max(data->stack_size, stack_size);

      ast::Literal_Natural_Node*   stack_ptr_size          = ast::create_node_literal_natural(m, compiler::symbol::number_to_symbol(m->symbol_table, data->stack_size));
      ast::Literal_Symbol_Node*    stack_ptr_allocate_func = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "alloca"));
      ast::Declarations_List_Node* stack_ptr_allocate_args = ast::create_node_declarations_list(m, stack_ptr_size, NULL);
      ast::Function_Call_Node*     stack_ptr_allocate_call = ast::create_node_function_call(m, stack_ptr_allocate_func, stack_ptr_allocate_args);

      ast::Declaration_Variable_Node* stack_ptr_decl = ast::create_variable_declaration(m, stack_ptr, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));

      ast::Variable_Assignment_Node* stack_ptr_assignment = ast::create_node_assignment(m, stack_ptr_decl, stack_ptr_allocate_call);

      lib::insert(data->function_stackframe_allocation, f, stack_ptr_assignment);

      if (assign_arguments) {
        assign_arguments->concat(m, f->get_body(m));
      } else {
        assign_arguments = f->get_body(m);
      }

      lib::insert(data->setup_end, f, f->get_body(m));

      ast::ProgramPoint_List_Node* stack_ptr_assigment_pp = ast::create_node_program_point(m, stack_ptr_assignment, assign_arguments);

      f->set_body(m, stack_ptr_assigment_pp);

      context::context_destroy(context);
      context::context_destroy(arguments_ctx);
    }

    data->stack_size = lib::max(data->stack_size, stack_size);

    return stack_size;
  }

  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(root)) {
    context::context_declare(ctx, m, var);

    if (cps::is_temporary_variable(data->cps_data, m, var) == false) {
      u64 sizeof_var = compute_sizeof_type(m, var->get_type(m), ctx);

      lib::insert< ast::Node*, u64 >(data->stack_address, var, stack_size);

      data->stack_size = lib::max(data->stack_size, stack_size + sizeof_var);

      return stack_size + sizeof_var;
    }

    return stack_size;
  }

  if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(root)) {
    context::context_declare(ctx, m, var);

    if (cps::is_temporary_variable(data->cps_data, m, var) == false) {
      u64 sizeof_var = compute_sizeof_type(m, var->get_type(m), ctx);

      lib::insert< ast::Node*, u64 >(data->stack_address, var, stack_size);

      data->stack_size = lib::max(data->stack_size, stack_size + sizeof_var);

      return stack_size + sizeof_var;
    }

    return stack_size;
  }

  if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(root)) {
    ast::Node* left  = assignment->get_left_operand(m);
    ast::Node* right = assignment->get_right_operand(m);

    ast::Literal_Symbol_Node* stack_ptr = ast::create_node_literal_symbol(m, compiler::symbol::number_to_symbol(m->symbol_table, depth, "sp"));

    if (ast::is_declaration_node(left)) {
      if (ast::Function_Literal_Node* lit = ast::is_instance< ast::Function_Literal_Node* >(right)) {
        return stack_allocate(data, m, lit, ctx, args_ctx, stack_size, depth, assignment);
      }

      if (ast::Effect_Declaration_Node* lit = ast::is_instance< ast::Effect_Declaration_Node* >(right)) {
        return stack_allocate(data, m, lit, ctx, args_ctx, stack_size, depth, assignment);
      }

      ast::Literal_Symbol_Node* symbol = NULL;
      ast::Node*                type   = NULL;

      if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
        symbol = var->get_symbol(m);
        type   = var->get_type(m);

        if (cps::is_temporary_variable(data->cps_data, m, var)) {
          return stack_size;
        }
      }

      if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
        symbol = var->get_symbol(m);
        type   = var->get_type(m);

        if (cps::is_temporary_variable(data->cps_data, m, var)) {
          return stack_size;
        }
      }

      if (ast::Literal_Struct_Node* structure = ast::is_instance< ast::Literal_Struct_Node* >(assignment->get_right_operand(m))) {
        context::context_define_struct(ctx, m, symbol, structure);
        return stack_size;
      }

      u64 _stack_size = stack_allocate(data, m, left, ctx, args_ctx, stack_size, depth, assignment);

      data->stack_size = lib::max(data->stack_size, _stack_size);

      ast::Node* displacement = ast::create_node_literal_natural(m, compiler::symbol::number_to_symbol(m->symbol_table, stack_size));
      ast::Node* address      = ast::create_node_arithmetic_add(m, ast::deep_copy(m, stack_ptr), displacement);

      stack_size = _stack_size;

      data->stack_size = lib::max(data->stack_size, stack_size);

      ast::Cast_Type_Node* cast = ast::create_node_cast_type(m, ast::create_node_type_pointer(m, ast::deep_copy(m, type)), address);

      assignment->set_left_operand(m, ast::create_node_pointer_value(m, cast));

      stack_size = stack_allocate(data, m, right, ctx, args_ctx, stack_size, depth, assignment);

      data->stack_size = lib::max(data->stack_size, stack_size);

      return stack_size;
    }

    data->stack_size = lib::max(data->stack_size, stack_size);

    stack_size = stack_allocate(data, m, left, ctx, args_ctx, stack_size, depth, assignment);

    data->stack_size = lib::max(data->stack_size, stack_size);

    stack_size = stack_allocate(data, m, right, ctx, args_ctx, stack_size, depth, assignment);

    data->stack_size = lib::max(data->stack_size, stack_size);

    return stack_size;
  }

  if (ast::If_Node_Statement* if_stmt = ast::is_instance< ast::If_Node_Statement* >(root)) {

    stack_size = stack_allocate(data, m, if_stmt->get_condition(m), ctx, args_ctx, stack_size, depth, if_stmt);

    data->stack_size = lib::max(data->stack_size, stack_size);

    context::context_push_scope(ctx);

    stack_size = stack_allocate(data, m, if_stmt->get_body(m), ctx, args_ctx, stack_size, depth, if_stmt);

    context::context_pop_scope(ctx);

    data->stack_size = lib::max(data->stack_size, stack_size);

    return stack_size;
  }

  if (ast::Elif_List_Node* elif = ast::is_instance< ast::Elif_List_Node* >(root)) {

    while (ast::is_semantic_node(elif)) {
      u64 stack = stack_allocate(data, m, elif->get_if(m), ctx, args_ctx, stack_size, depth, elif);

      data->stack_size = lib::max(data->stack_size, stack);

      elif = elif->get_elif(m);
    }

    return stack_size;
  }

  if (ast::Literal_Symbol_Node* symbol = ast::is_instance< ast::Literal_Symbol_Node* >(root)) {
    ast::Node* type = context::context_type_of(ctx, m, symbol);

    ast::Literal_Symbol_Node* stack_ptr = ast::create_node_literal_symbol(m, compiler::symbol::number_to_symbol(m->symbol_table, depth, "sp"));

    if (type) {
      ast::Node* decl = context::context_is_defined(ctx, symbol);

      u64* stack_address = lib::search(data->stack_address, decl);

      if (stack_address) {
        ast::Node* displacement = ast::create_node_literal_natural(m, compiler::symbol::number_to_symbol(m->symbol_table, *stack_address));
        ast::Node* address      = ast::create_node_arithmetic_add(m, stack_ptr, displacement);

        ast::Cast_Type_Node*     cast  = ast::create_node_cast_type(m, ast::create_node_type_pointer(m, ast::deep_copy(m, type)), address);
        ast::Pointer_Value_Node* value = ast::create_node_pointer_value(m, cast);

        if (parent->left == symbol->id) {
          parent->left = value->id;
        }

        if (parent->right == symbol->id) {
          parent->right = value->id;
        }
      }

      return stack_size;
    }

    type = context::context_type_of(args_ctx, m, symbol);

    if (type) {
      ast::Node* decl = context::context_is_defined(args_ctx, symbol);

      u64* stack_address = lib::search(data->stack_address, decl);

      if (stack_address) {
        ast::Node* displacement = ast::create_node_literal_natural(m, compiler::symbol::number_to_symbol(m->symbol_table, *stack_address));
        ast::Node* address      = ast::create_node_arithmetic_add(m, ast::deep_copy(m, stack_ptr), displacement);

        ast::Type_Pointer_Node*  ptr_type = ast::create_node_type_pointer(m, ast::create_node_type_pointer(m, ast::deep_copy(m, type)));
        ast::Cast_Type_Node*     cast     = ast::create_node_cast_type(m, ptr_type, address);
        ast::Pointer_Value_Node* value    = ast::create_node_pointer_value(m, ast::create_node_pointer_value(m, cast));

        if (parent->left == symbol->id) {
          parent->left = value->id;
        }

        if (parent->right == symbol->id) {
          parent->right = value->id;
        }
      }

      return stack_size;
    }

    return stack_size;
  }

  stack_size = stack_allocate(data, m, ast::left_of(m, root), ctx, args_ctx, stack_size, depth, root);

  data->stack_size = lib::max(data->stack_size, stack_size);

  stack_allocate(data, m, ast::right_of(m, root), ctx, args_ctx, stack_size, depth, root);

  data->stack_size = lib::max(data->stack_size, stack_size);

  return stack_size;
}

void allocate_stack_frame(Stack_Frame_Data* data, ast::Manager* m, ast::Node* root) {
  context::Context* ctx = context::context_create(NULL);
  stack_allocate(data, m, root, ctx, NULL, 0, 0, NULL);
  context::context_destroy(ctx);
}

} // namespace stackframe
