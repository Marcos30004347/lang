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
#include "ast/utils.hpp"

#include "compiler/symbol_table.hpp"
#include "continuations/handler.hpp"
#include "lib/set.hpp"
#include "lib/table.hpp"

#include <assert.h>

#include "context/context.hpp"
#include "lib/utils.hpp"
#include "parser/parser.hpp"

#include <climits>
#include <stdio.h>
#include <sys/resource.h>

namespace stackframe {

struct Stack_Frame_Data {
  handler::Handler_Pass_Data* handler_data;

  u64 stack_size;

  lib::Set< ast::Declaration_Variable_Node* >* stack_arguments;

  lib::Table< ast::Node*, u64 >* stack_address;
  lib::Table< ast::Node*, u64 >* stack_depth;

  lib::Table< ast::Function_Literal_Node*, u64 >* function_depth;

  lib::Table< ast::Function_Literal_Node*, ast::Variable_Assignment_Node* >* function_stackframe_allocation;
  lib::Table< ast::Function_Literal_Node*, ast::Declaration_Variable_Node* >* function_stackframe_argument;

  lib::Table< ast::Function_Literal_Node*, ast::ProgramPoint* >* setup_end;
};

Stack_Frame_Data* create_stack_frame_data(handler::Handler_Pass_Data* data) {
  Stack_Frame_Data* d = new Stack_Frame_Data();

  d->handler_data = data;

  d->stack_address = lib::table_create< ast::Node*, u64 >();
  d->stack_depth = lib::table_create< ast::Node*, u64 >();
  d->setup_end = lib::table_create< ast::Function_Literal_Node*, ast::ProgramPoint* >();
  d->function_depth = lib::table_create< ast::Function_Literal_Node*, u64 >();
  d->function_stackframe_allocation = lib::table_create< ast::Function_Literal_Node*, ast::Variable_Assignment_Node* >();
  d->stack_arguments = lib::set_create< ast::Declaration_Variable_Node* >();
  d->function_stackframe_argument = lib::table_create< ast::Function_Literal_Node*, ast::Declaration_Variable_Node* >();

  return d;
}

void destroy_stack_frame_data(Stack_Frame_Data* d) {
  lib::table_delete(d->stack_address);
  lib::table_delete(d->stack_depth);
  lib::table_delete(d->setup_end);
  lib::table_delete(d->function_depth);
  lib::table_delete(d->function_stackframe_allocation);
  lib::set_delete(d->stack_arguments);
  lib::table_delete(d->function_stackframe_argument);

  handler::handler_pass_data_destroy(d->handler_data);

  delete d;
}

b8 stack_frame_data_is_parent_stack_argument(Stack_Frame_Data* data, ast::Declaration_Variable_Node* var) {
  return lib::search(data->stack_arguments, var) != NULL;
}

b8 stack_frame_data_is_parent_stack_argument(Stack_Frame_Data* data, ast::Declaration_Constant_Node* var) {
  return false;
}

handler::Handler_Pass_Data* stack_frame_data_get_handler_pass_data(Stack_Frame_Data* data) {
  return data->handler_data;
}

ast::Literal_Symbol_Node* build_sp_symbol(ast::Manager* m, u64 depth) {
  return ast::create_node_literal_symbol(m, compiler::symbol::number_to_symbol(m->symbol_table, depth, "sp"));
}

u64 stack_frame_get_function_depth(Stack_Frame_Data* data, ast::Function_Literal_Node* lit) {
  if (u64* depth = lib::search(data->function_depth, lit)) {
    return *depth;
  }

  assert(false);

  return 0;
}
void stack_frame_set_function_depth(Stack_Frame_Data* data, ast::Function_Literal_Node* lit, u64 depth) {
  lib::insert(data->function_depth, lit, depth);
}

void stackframe_pass_stack_frame_paramenter(ast::Manager* m, u64* depth, ast::Function_Call_Node* call) {
  if (depth) {
    ast::Literal_Symbol_Node* sp = build_sp_symbol(m, *depth);
    call->push_argument(m, sp);
  } else {
    ast::Literal_Natural_Node* zr = ast::create_node_literal_natural(m, compiler::symbol::number_to_symbol(m->symbol_table, 0));
    call->push_argument(m, zr);
  }
}

ast::ProgramPoint* stack_frame_get_function_literal_setup_end(Stack_Frame_Data* data, ast::Manager*, ast::Function_Literal_Node* lit) {
  ast::ProgramPoint** pp = lib::search< ast::Function_Literal_Node*, ast::ProgramPoint* >(data->setup_end, lit);

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

void stack_frame_set_function_local_stack_frame_allocation(Stack_Frame_Data* data, ast::Function_Literal_Node* f, ast::Variable_Assignment_Node* assignment) {
  lib::insert(data->function_stackframe_allocation, f, assignment);
}

u64 compute_sizeof_type(ast::Manager* m, ast::Node* root, context::Context* ctx) {
  assert(ast::is_semantic_node(root));

  if (ast::Literal_Struct_Node* ty = ast::is_instance< ast::Literal_Struct_Node* >(root)) {
    ast::ProgramPoint* members = ty->get_members(m);

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

  if (ast::Type_Unit_Node* ty = ast::is_instance< ast::Type_Unit_Node* >(root)) {
    return sizeof(char);
  }

  if (ast::Literal_Symbol_Node* ty = ast::is_instance< ast::Literal_Symbol_Node* >(root)) {
    return compute_sizeof_type(m, context::context_get_struct_definition(ctx, m, ty), ctx);
  }

  parser::print_ast(m, root);
  assert(false && "Uncaught type");
}
u64 set_min_rec(lib::SetNode< u64 >* b) {
  if (b == NULL) {
    return UINT_MAX;
  }

  u64 m = lib::min(b->key, set_min_rec(b->left));
  u64 n = lib::min(b->key, set_min_rec(b->right));

  return lib::min(m, n);
}

u64 set_min(lib::Set< u64 >* b) {
  return set_min_rec(b->root);
}

u64 set_max_rec(lib::SetNode< u64 >* b) {
  if (b == NULL) {
    return 0;
  }

  u64 m = lib::max(b->key, set_max_rec(b->left));
  u64 n = lib::max(b->key, set_max_rec(b->right));

  return lib::max(m, n);
}

u64 set_max(lib::Set< u64 >* b) {
  return set_max_rec(b->root);
}

ast::ProgramPoint* create_used_stacks_accesses(ast::Manager* m, u64 depth, ast::ProgramPoint* head, lib::Set< u64 >* stack) {
  u64 min = lib::min(depth, set_min(stack));

  if (depth <= 1) {
    return head;
  }

  for (i64 i = depth - 1; i >= min + 1; i--) {
    ast::Literal_Symbol_Node* parent_stack_ptr = ast::create_node_literal_symbol(m, compiler::symbol::number_to_symbol(m->symbol_table, i, "sp"));
    ast::Type_Pointer_Node* type = ast::create_node_type_pointer(m, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
    ast::Cast_Type_Node* cast = ast::create_node_cast_type(m, type, parent_stack_ptr);
    ast::Pointer_Value_Node* value = ast::create_node_pointer_value(m, cast);

    ast::Literal_Symbol_Node* used_stack_ptr = ast::create_node_literal_symbol(m, compiler::symbol::number_to_symbol(m->symbol_table, i - 1, "sp"));
    ast::Declaration_Constant_Node* declaration = ast::create_constant_declaration(m, used_stack_ptr, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));

    ast::Variable_Assignment_Node* assignment = ast::create_node_assignment(m, declaration, value);

    head = ast::create_node_program_point(m, assignment, head);
  }

  return head;
}

ast::ProgramPoint* create_used_stacks_assignments(ast::Manager* m, u64 depth, ast::ProgramPoint* head, lib::Set< u64 >* stack) {
  if (stack == NULL) {
    return head;
  }

  if (depth <= 1) {
    return head;
  }

  // if (depth > stack->key) {
  ast::Literal_Symbol_Node* stack_ptr = ast::create_node_literal_symbol(m, compiler::symbol::number_to_symbol(m->symbol_table, depth, "sp"));

  // ast::Literal_Symbol_Node*           increase = ast::create_node_literal_symbol(m, compiler::symbol::number_to_symbol(m->symbol_table, sizeof(void*) * (stack->key - 1)));
  // ast::Arithmetic_Operation_Add_Node* addition = ast::create_node_arithmetic_add(m, stack_ptr, increase);

  ast::Type_Pointer_Node* type = ast::create_node_type_pointer(m, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
  ast::Cast_Type_Node* cast = ast::create_node_cast_type(m, type, stack_ptr);
  ast::Pointer_Value_Node* access = ast::create_node_pointer_value(m, cast);

  // ast::Declaration_Constant_Node* declaration = ast::create_constant_declaration(m, stack_ptr, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));

  ast::Literal_Symbol_Node* stack_ptr_parent = ast::create_node_literal_symbol(m, compiler::symbol::number_to_symbol(m->symbol_table, depth - 1, "sp"));
  // ast::Literal_Symbol_Node*           increase_parent = ast::create_node_literal_symbol(m, compiler::symbol::number_to_symbol(m->symbol_table, sizeof(void*) * (stack->key -
  // 1))); ast::Arithmetic_Operation_Add_Node* addition_parent = ast::create_node_arithmetic_add(m, stack_ptr_parent, increase);
  ast::Cast_Type_Node* cast_parent = ast::create_node_cast_type(m, ast::create_node_type_pointer(m, ast::create_node_type_any(m)), stack_ptr_parent);
  ast::Variable_Assignment_Node* assignment = ast::create_node_assignment(m, access, cast_parent);

  head = ast::create_node_program_point(m, assignment, head);
  //}

  // head = create_used_stacks_assignments_rec(m, depth, head, stack->left);
  // return create_used_stacks_assignments_rec(m, depth, head, stack->right);
  return head;
}
// ast::ProgramPoint_List_Node* create_used_stacks_assignments(ast::Manager* m, u64 depth, ast::ProgramPoint_List_Node* head, lib::Set< u64 >* stack) {
//   return create_used_stacks_assignments_rec(m, depth, head, stack->root);
// }

void set_union_rec(lib::Set< u64 >* a, lib::SetNode< u64 >* b) {
  if (b == NULL) {
    return;
  }

  lib::insert(a, b->key);
  set_union_rec(a, b->left);
  set_union_rec(a, b->right);
}

void set_union(lib::Set< u64 >* a, lib::Set< u64 >* b) {
  return set_union_rec(a, b->root);
}

void stackframe_push_frame_pointer_argument(Stack_Frame_Data* data, ast::Manager* m, ast::Function_Literal_Node* f, ast::Node* f_decl, u64 depth) {
  ast::Literal_Symbol_Node* stack_ptr = ast::create_node_literal_symbol(m, compiler::symbol::number_to_symbol(m->symbol_table, depth, "sp"));

  ast::Declaration_Variable_Node* stack_ptr_decl = ast::create_variable_declaration(m, stack_ptr, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));

  lib::insert(data->stack_arguments, stack_ptr_decl);

  f->push_argument(m, stack_ptr_decl);

  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(f_decl)) {
    ast::Type_Arrow_Node* arrow_from = ast::is_instance< ast::Type_Arrow_Node* >(var->get_type(m));

    assert(arrow_from);

    ast::Node* from = arrow_from->get_from_type(m);

    if (ast::is_instance< ast::Type_Unit_Node* >(from)) {
      from = ast::create_node_type_pointer(m, ast::create_node_type_any(m));
    } else {
      from = ast::create_node_arithmetic_mul(m, from, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
    }

    arrow_from->set_from_type(m, from);
  }

  if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(f_decl)) {
    ast::Type_Arrow_Node* arrow_from = ast::is_instance< ast::Type_Arrow_Node* >(var->get_type(m));

    assert(arrow_from);

    ast::Node* from = arrow_from->get_from_type(m);

    if (ast::is_instance< ast::Type_Unit_Node* >(from)) {
      from = ast::create_node_type_pointer(m, ast::create_node_type_any(m));
    } else {
      from = ast::create_node_arithmetic_mul(m, from, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));
    }

    arrow_from->set_from_type(m, from);
  }
}

void add_pop_frame_call(ast::Manager* m, ast::ProgramPoint* point, ast::Literal_Symbol_Node* stack_sym, ast::Node* frame_size) {
  ast::Function_Call_Node* stack_ptr_pop_call = ast::utils::call(m, "pop_frame", stack_sym, frame_size);

  while (ast::is_semantic_node(point)) {
    ast::ProgramPoint* next = point->get_next_program_point(m);

    if (ast::is_semantic_node(next)) {
      if (ast::Return_Node_Statement* ret = ast::is_instance< ast::Return_Node_Statement* >(next->get_statement(m))) {
        point->insert(m, stack_ptr_pop_call);
      }
    }

    if (ast::Elif_List_Node* elif = ast::is_instance< ast::Elif_List_Node* >(point->get_statement(m))) {
      while (ast::is_semantic_node(elif)) {
        ast::If_Node_Statement* if_stmt = elif->get_if(m);

        add_pop_frame_call(m, if_stmt->get_body(m), stack_sym, frame_size);

        elif = elif->get_elif(m);
      }
    }

    point = next;
  }
}

// TODO(marcos): refactor this function
u64 stack_allocate(
    Stack_Frame_Data* data,
    ast::Manager* m,
    ast::Node* root,
    context::Context* ctx,
    context::Context* args_ctx,
    u64 stack_size,
    u64 depth,
    ast::Node* parent,
    lib::Set< u64 >* used_stacks,
    lib::Set< u64 >* childs_used_stacks) {
  if (!ast::is_semantic_node(root)) {
    return stack_size;
  }

  if (ast::Function_Literal_Node* f = ast::is_instance< ast::Function_Literal_Node* >(root)) {
    used_stacks = lib::set_create< u64 >();
    lib::Set< u64 >* function_childs_used_stacks = lib::set_create< u64 >();

    set_union(function_childs_used_stacks, childs_used_stacks);

    u64 previous_frame_ptr_size = sizeof(void*);

    //u64 return_size = compute_sizeof_type(m, f->get_return_type(m), ctx);
    u64 frame_header_size = previous_frame_ptr_size; // + return_size;

    stack_size = frame_header_size;
    data->stack_size = frame_header_size;

    depth = depth + 1;

    lib::insert(data->function_depth, f, depth);

    ast::Literal_Symbol_Node* stack_ptr = ast::create_node_literal_symbol(m, compiler::symbol::number_to_symbol(m->symbol_table, depth, "sp"));

    ast::Declarations_List_Node* arguments = f->get_arguments(m);

    context::Context* context = context::context_create(ctx);
    context::Context* arguments_ctx = context::context_create(NULL);

    ast::ProgramPoint* assign_arguments = NULL;
    ast::Type_Pointer_Node* stack_type = NULL;

    while (arguments) {
      ast::Literal_Symbol_Node* symbol = NULL;

      u64 increment = 0;

      b8 should_save_on_stack = true;

      ast::Node* decl = arguments->get_declaration(m);

      if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(decl)) {
        context::context_declare(arguments_ctx, m, var);

        lib::insert(data->stack_depth, decl, depth);

        symbol = var->get_symbol(m);
        stack_type = ast::create_node_type_pointer(m, ast::deep_copy(m, var->get_type(m)));

        b8 is_stack_frame = stack_frame_data_is_parent_stack_argument(data, var);
        b8 is_context = handler::handler_pass_data_is_context_argument(data->handler_data, var);

        if (/*is_temporary == false && */ is_context == false && is_stack_frame == false) {
          should_save_on_stack = false;

          lib::insert< ast::Node*, u64 >(data->stack_address, var, stack_size);

          increment = compute_sizeof_type(m, stack_type, context);
        }
      }

      if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(decl)) {

        context::context_declare(arguments_ctx, m, var);

        lib::insert(data->stack_depth, decl, depth);

        symbol = var->get_symbol(m);
        stack_type = ast::create_node_type_pointer(m, ast::deep_copy(m, var->get_type(m)));

        b8 is_stack_frame = stack_frame_data_is_parent_stack_argument(data, var);
        b8 is_context = handler::handler_pass_data_is_context_argument(data->handler_data, var);

        if (is_context == false && is_stack_frame == false) {
          should_save_on_stack = false;

          lib::insert< ast::Node*, u64 >(data->stack_address, var, stack_size);

          increment = compute_sizeof_type(m, stack_type, context);
        }
      }

      if (increment) {

        ast::Node* displacement = ast::create_node_literal_natural(m, compiler::symbol::number_to_symbol(m->symbol_table, stack_size));

        stack_size += increment;

        ast::Node* address = ast::create_node_arithmetic_add(m, ast::deep_copy(m, stack_ptr), displacement);

        ast::Cast_Type_Node* cast = ast::create_node_cast_type(m, ast::create_node_type_pointer(m, ast::deep_copy(m, stack_type)), address);
        ast::Pointer_Value_Node* value = ast::create_node_pointer_value(m, cast);
        ast::Variable_Assignment_Node* assignment = ast::create_node_assignment(m, value, ast::create_node_value_address(m, ast::deep_copy(m, symbol)));

        assign_arguments = ast::create_node_program_point(m, assignment, assign_arguments);
      }

      arguments = arguments->get_next_declaration(m);
    }

    u64 tmp = data->stack_size;

    data->stack_size = 0;

    stack_size = stack_allocate(data, m, f->get_body(m), context, arguments_ctx, stack_size, depth, f, used_stacks, function_childs_used_stacks);

    // data->stack_size = lib::max(data->stack_size, stack_size);

    // TODO(marcos): use sizeof(any*) instead of sizeof(void*)
    ast::Literal_Natural_Node* stack_ptr_size = ast::create_node_literal_natural(m, compiler::symbol::number_to_symbol(m->symbol_table, data->stack_size));
    ast::Literal_Symbol_Node* stack_ptr_allocate_func = ast::create_node_literal_symbol(m, compiler::symbol::set_entry(m->symbol_table, "push_frame"));
    ast::Declarations_List_Node* stack_ptr_allocate_args = ast::create_node_declarations_list(m, stack_ptr_size, NULL);
    ast::Function_Call_Node* stack_ptr_allocate_call = ast::create_node_function_call(m, stack_ptr_allocate_func, stack_ptr_allocate_args);

    ast::Declaration_Variable_Node* stack_ptr_decl = ast::create_variable_declaration(m, stack_ptr, ast::create_node_type_pointer(m, ast::create_node_type_any(m)));

    ast::Variable_Assignment_Node* stack_ptr_assignment = ast::create_node_assignment(m, stack_ptr_decl, stack_ptr_allocate_call);

    lib::insert(data->function_stackframe_allocation, f, stack_ptr_assignment);

    assign_arguments = create_used_stacks_accesses(m, depth, assign_arguments, used_stacks);
    assign_arguments = create_used_stacks_assignments(m, depth, assign_arguments, function_childs_used_stacks);

    if (assign_arguments) {
      assign_arguments->concat(m, f->get_body(m));
    } else {
      assign_arguments = f->get_body(m);
    }

    lib::insert(data->setup_end, f, f->get_body(m));

    ast::ProgramPoint* stack_ptr_assigment_pp = ast::create_node_program_point(m, stack_ptr_assignment, assign_arguments);

    f->set_body(m, stack_ptr_assigment_pp);

    add_pop_frame_call(m, f->get_body(m), stack_ptr, stack_ptr_size);

    context::context_destroy(context);
    context::context_destroy(arguments_ctx);

    data->stack_size = tmp;
    // }

    data->stack_size = lib::max(data->stack_size, stack_size);

    lib::set_delete(used_stacks);

    return stack_size;
  }

  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(root)) {
    if (!context::context_is_defined(ctx, var->get_symbol(m))) {
      context::context_declare(ctx, m, var);
    }

    if (compiler::symbol::is_equal(m->symbol_table, var->get_symbol(m)->get_symbol(m), "ret")) {
      return stack_size;
    }

    // if (compiler::symbol::is_equal(m->symbol_table, var->get_symbol(m)->get_symbol(m), "prompt_ret")) {
    //   return stack_size;
    // }

    u64 sizeof_var = compute_sizeof_type(m, var->get_type(m), ctx);

    lib::insert< ast::Node*, u64 >(data->stack_depth, var, depth);

    data->stack_size = lib::max(data->stack_size, stack_size + sizeof_var);

    lib::insert< ast::Node*, u64 >(data->stack_address, var, stack_size);

    // stack_allocate(data, m, var->get_symbol(m), ctx, args_ctx, stack_size, depth, var, used_stacks, childs_used_stacks);

    return stack_size + sizeof_var;
  }

  if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(root)) {
    if (!context::context_is_defined(ctx, var->get_symbol(m))) {
      context::context_declare(ctx, m, var);
    }

    if (compiler::symbol::is_equal(m->symbol_table, var->get_symbol(m)->get_symbol(m), "ret")) {
      return stack_size;
    }

    // if (compiler::symbol::is_equal(m->symbol_table, var->get_symbol(m)->get_symbol(m), "prompt_ret")) {
    //   return stack_size;
    // }
    u64 sizeof_var = compute_sizeof_type(m, var->get_type(m), ctx);

    lib::insert< ast::Node*, u64 >(data->stack_depth, var, depth);

    data->stack_size = lib::max(data->stack_size, stack_size + sizeof_var);

    lib::insert< ast::Node*, u64 >(data->stack_address, var, stack_size);

    // stack_allocate(data, m, var->get_symbol(m), ctx, args_ctx, stack_size, depth, var, used_stacks, childs_used_stacks);

    return stack_size + sizeof_var;
  }

  if (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(root)) {
    ast::Node* left = assignment->get_left_operand(m);
    ast::Node* right = assignment->get_right_operand(m);

    data->stack_size = lib::max(data->stack_size, stack_size);

    if (ast::Literal_Handler_Node* handler = ast::is_instance< ast::Literal_Handler_Node* >(right)) {
      ast::ProgramPoint* body = handler->get_body(m);

      // TODO(marcos): this fail is handler have member variables
      while (ast::is_semantic_node(body)) {
        stack_allocate(data, m, body->get_statement(m), ctx, args_ctx, stack_size, depth, NULL, used_stacks, childs_used_stacks);
        body = body->get_next_program_point(m);
      }

      return stack_size;
    }

    if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
      if (compiler::symbol::is_equal(m->symbol_table, var->get_symbol(m)->get_symbol(m), "ret")) {
        return stack_size;
      }

      // if (compiler::symbol::is_equal(m->symbol_table, var->get_symbol(m)->get_symbol(m), "prompt_ret")) {
      //   return stack_size;
      // }
      context::context_declare(ctx, m, var);
      lib::insert< ast::Node*, u64 >(data->stack_depth, var, depth);
    }

    if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
      if (compiler::symbol::is_equal(m->symbol_table, var->get_symbol(m)->get_symbol(m), "ret")) {
        return stack_size;
      }

      // if (compiler::symbol::is_equal(m->symbol_table, var->get_symbol(m)->get_symbol(m), "prompt_ret")) {
      //   return stack_size;
      // }
      context::context_declare(ctx, m, var);
      lib::insert< ast::Node*, u64 >(data->stack_depth, var, depth);
    }

    if (ast::is_declaration_node(left)) {
      if (ast::Function_Literal_Node* lit = ast::is_instance< ast::Function_Literal_Node* >(right)) {

        if (depth > 0 /*|| is_prompt*/) {
          stackframe_push_frame_pointer_argument(data, m, lit, left, depth);
        }

        // if (handler::handler_pass_data_is_prompt_declaration(data->handler_data, left)) {
        //   stackframe_push_frame_pointer_argument(data, m, lit, left, depth);
        // }

        u64 tmp = data->stack_size;

        data->stack_size = 0;

        stack_allocate(data, m, lit, ctx, args_ctx, stack_size, depth, assignment, used_stacks, childs_used_stacks);

        data->stack_size = tmp;

        return stack_size;
      }

      if (ast::Effect_Declaration_Node* lit = ast::is_instance< ast::Effect_Declaration_Node* >(right)) {
        return stack_size;
      }

      ast::Literal_Symbol_Node* symbol = NULL;
      ast::Node* type = NULL;

      if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(left)) {
        symbol = var->get_symbol(m);
        type = var->get_type(m);

        lib::insert(data->stack_depth, left, depth);
      }

      if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(left)) {
        symbol = var->get_symbol(m);
        type = var->get_type(m);

        lib::insert(data->stack_depth, left, depth);
      }

      if (ast::Literal_Struct_Node* structure = ast::is_instance< ast::Literal_Struct_Node* >(assignment->get_right_operand(m))) {
        context::context_define_struct(ctx, m, symbol, structure);
        return stack_size;
      }

      u64 _stack_size = stack_allocate(data, m, left, ctx, args_ctx, stack_size, depth, assignment, used_stacks, childs_used_stacks);

      data->stack_size = lib::max(data->stack_size, _stack_size);

      lib::insert(used_stacks, depth);
      lib::insert(childs_used_stacks, depth);

      ast::Literal_Symbol_Node* stack_ptr = ast::create_node_literal_symbol(m, compiler::symbol::number_to_symbol(m->symbol_table, depth, "sp"));

      ast::Node* displacement = ast::create_node_literal_natural(m, compiler::symbol::number_to_symbol(m->symbol_table, stack_size));
      ast::Node* address = ast::create_node_arithmetic_add(m, ast::deep_copy(m, stack_ptr), displacement);

      stack_size = _stack_size;

      data->stack_size = lib::max(data->stack_size, stack_size);

      ast::Cast_Type_Node* cast = ast::create_node_cast_type(m, ast::create_node_type_pointer(m, ast::deep_copy(m, type)), address);

      assignment->set_left_operand(m, ast::create_node_pointer_value(m, cast));

      stack_size = stack_allocate(data, m, right, ctx, args_ctx, stack_size, depth, assignment, used_stacks, childs_used_stacks);

      data->stack_size = lib::max(data->stack_size, stack_size);

      return stack_size;
    }

    stack_size = stack_allocate(data, m, left, ctx, args_ctx, stack_size, depth, assignment, used_stacks, childs_used_stacks);
    stack_size = stack_allocate(data, m, right, ctx, args_ctx, stack_size, depth, assignment, used_stacks, childs_used_stacks);

    data->stack_size = lib::max(data->stack_size, stack_size);

    return stack_size;
  }

  if (ast::If_Node_Statement* if_stmt = ast::is_instance< ast::If_Node_Statement* >(root)) {

    stack_size = stack_allocate(data, m, if_stmt->get_condition(m), ctx, args_ctx, stack_size, depth, if_stmt, used_stacks, childs_used_stacks);

    data->stack_size = lib::max(data->stack_size, stack_size);

    context::context_push_scope(ctx);

    stack_size = stack_allocate(data, m, if_stmt->get_body(m), ctx, args_ctx, stack_size, depth, if_stmt, used_stacks, childs_used_stacks);

    context::context_pop_scope(ctx);

    data->stack_size = lib::max(data->stack_size, stack_size);

    return stack_size;
  }

  if (ast::Elif_List_Node* elif = ast::is_instance< ast::Elif_List_Node* >(root)) {

    while (ast::is_semantic_node(elif)) {
      u64 stack = stack_allocate(data, m, elif->get_if(m), ctx, args_ctx, stack_size, depth, elif, used_stacks, childs_used_stacks);

      data->stack_size = lib::max(data->stack_size, stack);

      elif = elif->get_elif(m);
    }

    return stack_size;
  }

  if (ast::Function_Call_Node* call = ast::is_instance< ast::Function_Call_Node* >(root)) {
    if (ast::Literal_Symbol_Node* sym = ast::is_instance< ast::Literal_Symbol_Node* >(call->get_function(m))) {
      if (ast::Node* decl = context::context_is_defined(ctx, sym)) {
        u64* decl_depth = lib::search(data->stack_depth, decl);
        //  b8 is_prompt = handler::handler_pass_data_is_prompt_declaration(cps::cps_data_get_handler_data(data->cps_data), decl);

        if ((decl_depth && *decl_depth > 0)) {
          stack_allocate(data, m, sym, ctx, args_ctx, stack_size, depth, parent, used_stacks, childs_used_stacks);
          stackframe_pass_stack_frame_paramenter(m, decl_depth, call);
        }
      }
    }
  }

  if (ast::Member_Access_Node* access = ast::is_instance< ast::Member_Access_Node* >(root)) {
    stack_size = stack_allocate(data, m, ast::left_of(m, root), ctx, args_ctx, stack_size, depth, root, used_stacks, childs_used_stacks);
    data->stack_size = lib::max(data->stack_size, stack_size);
    return stack_size;
  }

  if (ast::Literal_Symbol_Node* symbol = ast::is_instance< ast::Literal_Symbol_Node* >(root)) {
    ast::Node* type = context::context_type_of(ctx, m, symbol);

    if (compiler::symbol::is_equal(m->symbol_table, symbol->get_symbol(m), "ret")) {
      return stack_size;
    }

    // if (compiler::symbol::is_equal(m->symbol_table, symbol->get_symbol(m), "prompt_ret")) {
    //   return stack_size;
    // }
    if (type) {
      ast::Node* decl = context::context_is_defined(ctx, symbol);
      u64* stack_address = lib::search(data->stack_address, decl);

      if (stack_address) {
        u64* var_depth = lib::search(data->stack_depth, decl);
        if (ast::is_instance< ast::Type_Arrow_Node* >(type) && *var_depth <= 1) {
          return stack_size;
        }

        assert(var_depth);

        lib::insert(used_stacks, *var_depth);
        lib::insert(childs_used_stacks, *var_depth);

        ast::Literal_Symbol_Node* stack_ptr = ast::create_node_literal_symbol(m, compiler::symbol::number_to_symbol(m->symbol_table, *var_depth, "sp"));
        ast::Node* displacement = ast::create_node_literal_natural(m, compiler::symbol::number_to_symbol(m->symbol_table, *stack_address));
        ast::Node* address = ast::create_node_arithmetic_add(m, stack_ptr, displacement);

        ast::Cast_Type_Node* cast = ast::create_node_cast_type(m, ast::create_node_type_pointer(m, ast::deep_copy(m, type)), address);
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
        u64* var_depth = lib::search(data->stack_depth, decl);

        if (ast::is_instance< ast::Type_Arrow_Node* >(type) && *var_depth <= 1) {
          return stack_size;
        }
        assert(var_depth);

        lib::insert(used_stacks, *var_depth);
        lib::insert(childs_used_stacks, *var_depth);

        ast::Literal_Symbol_Node* stack_ptr = ast::create_node_literal_symbol(m, compiler::symbol::number_to_symbol(m->symbol_table, *var_depth, "sp"));
        ast::Node* displacement = ast::create_node_literal_natural(m, compiler::symbol::number_to_symbol(m->symbol_table, *stack_address));
        ast::Node* address = ast::create_node_arithmetic_add(m, ast::deep_copy(m, stack_ptr), displacement);

        ast::Type_Pointer_Node* ptr_type = ast::create_node_type_pointer(m, ast::create_node_type_pointer(m, ast::deep_copy(m, type)));
        ast::Cast_Type_Node* cast = ast::create_node_cast_type(m, ptr_type, address);
        ast::Pointer_Value_Node* value = ast::create_node_pointer_value(m, ast::create_node_pointer_value(m, cast));

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

  stack_size = stack_allocate(data, m, ast::left_of(m, root), ctx, args_ctx, stack_size, depth, root, used_stacks, childs_used_stacks);

  data->stack_size = lib::max(data->stack_size, stack_size);

  stack_size = stack_allocate(data, m, ast::right_of(m, root), ctx, args_ctx, stack_size, depth, root, used_stacks, childs_used_stacks);

  data->stack_size = lib::max(data->stack_size, stack_size);

  return stack_size;
}

void allocate_stack_frame(Stack_Frame_Data* data, ast::Manager* m, ast::Node* root) {
  context::Context* ctx = context::context_create(NULL);

  lib::Set< u64 >* stacks = lib::set_create< u64 >();
  lib::Set< u64 >* childs_stacks = lib::set_create< u64 >();

  stack_allocate(data, m, root, ctx, NULL, 0, 0, NULL, stacks, childs_stacks);

  lib::set_delete(stacks);
  lib::set_delete(childs_stacks);

  context::context_destroy(ctx);
}

} // namespace stackframe
