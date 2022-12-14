#pragma once

#include "lexer.hpp"

enum AST_Kind {
  // Compounds
  AST_NULL_NODE = 0,
  AST_UNDEFINED_NODE,
  AST_UNITIALIZED_NODE,

  __AST_KIND_BEGIN,

  AST_PROGRAM_POINT,

  AST_CALL_ARGS_LIST,
  AST_DECL_ARGS_LIST,

  AST_WITH_HANDLER,

  // Control Flow
  __AST_CTRL_FLOW_START,
  AST_CTRL_FLOW_IF,
  AST_CTRL_FLOW_IF_ELSE,
  AST_CTRL_FLOW_RETURN,
  AST_CTRL_FLOW_MATCH,
  AST_CTRL_FLOW_CASE,
  __AST_CTRL_FLOW_END,

  // Bindings
  AST_BIND_CONSTANT,
  AST_BIND_VARIABLE,
  AST_BIND_TYPE,

  // Declarations
  __AST_LITERAL_START,
  AST_SYMBOL_LITERAL,
  AST_NATURAL_LITERAL,
  AST_FUNCTION_LITERAL,
  AST_HANDLER_LITERAL,
  __AST_LITERAL_END,
  AST_FUN_SIGNATURE,

  // Types
  __AST_TYPE_KIND_START,
  AST_TYPE_I32,
  AST_TYPE_UNIT,
  AST_TYPE_ANY,
  AST_TYPE_ARROW,
  AST_TYPE_EFFECT,
  AST_TYPE_TUPLE,
  AST_TYPE_TYPE,
  AST_TYPE_UNION,
  AST_TYPE_YIELD,
  AST_TYPE_STRUCT,
  AST_TYPE_VARIABLE,
  AST_TYPE_POINTER,
  __AST_TYPE_KIND_END,

  // Binary operators
  __AST_BINARY_OPERATOR_START,
  AST_OP_BIN_ASSIGN,
  AST_OP_BIN_ADD,
  AST_OP_BIN_SUB,
  AST_OP_BIN_MUL,
  AST_OP_BIN_DIV,
  AST_OP_BIN_GT,
  AST_OP_BIN_LT,
  AST_OP_BIN_GE,
  AST_OP_BIN_LE,
  AST_OP_BIN_NE,
  AST_OP_BIN_EQ,
  AST_OP_MEMBER_ACCESS,
  __AST_BINARY_OPERATOR_END,

  // Unary operators
  __AST_UNARY_OPERATOR_START,
  AST_OP_UNA_SUB,
  AST_OP_UNA_ADD,
  AST_OP_POINTER_LOAD,
  AST_OP_ADDRESS_OF,
  __AST_UNARY_OPERATOR_END,

  // Applications
  __AST_CALL_OPERATION_START,
  AST_FUNCTION_CALL,
  AST_EFFECT_CALL,
  __AST_CALL_OPERATION_END,

  __AST_INTERNAL_START,
  _AST_GET_CLOSURE_HANDLER,
  _AST_GET_CLOSURE_ENVIRONMENT_BUFFER,
  _AST_SIZE_OF,
  _AST_TYPE_OF,
  _AST_BITSET,
  _AST_BITSET_SET_BIT_ON,
  _AST_BITSET_SET_BIT_OFF,
  _AST_BITSET_BINARY_UNION,
  _AST_BITSET_BINARY_INTERSECTION,
  _AST_BITSET_IS_BIT_UP,
  _AST_REALLOCATE_HEAP_BUFFER,
  _AST_ALLOCATE_HEAP_BUFFER,
  _AST_CAPTURE_VARIABLE_INTO_ENVIRONMENT,
  _AST_BORROW_VARIABLE_INTO_ENVIRONMENT,
  _AST_SETUP_CLOSURE_ENVIRONMENT_BUFFER_HEADER,
  _AST_UPDATE_CLOSURE_ENVIRONMENT_BUFFER_HEADER,
  _AST_BUILD_CLOSURE_OBJECT,
  _AST_IS_CLOSURE_OBJECT_LOCAL,
  _AST_GET_CLOSURE_ENVIRONMENT_BUFFER_SIZE,
  _AST_GET_CLOSURE_OBJECT_BITSET,
  _AST_SET_CLOSURE_OBJECT_EXTERN,
  __AST_INTERNAL_END,
  __AST_KIND_END,
};

typedef u32 AST_Id;

typedef struct AST_Bucket AST_Bucket;

struct AST_Node {
  AST_Id id;

  // TODO(marcos): this node can use less bytes
  // if we take this token away and just store
  // a reference to it here, like a TokenId ref
  // as we do for the AST_Node childs.
  Token tok;

  AST_Id left;
  AST_Id right;

  // NOTE(marcos): kinds > __AST_KIND_END represents
  // temporary values, that is, values inserted by
  // the compiler for compilation purposes.
  u64 kind;

  AST_Bucket* bucket;
};

#define AST_BUCKET_SIZE 128

struct AST_Bucket {
  u64         id;
  AST_Node    data[AST_BUCKET_SIZE];
  AST_Bucket* prev;
  AST_Bucket* next;
};

struct AST_Manager {
  u64 size;
  u64 temp;

  AST_Id statements_list_root_id;
  AST_Id statements_list_tail_id;

  AST_Node* statement_list_tail_ptr;

  AST_Bucket* root;
  AST_Bucket* tail;
};

AST_Node* ast_manager_push_decl(AST_Manager* m, AST_Node* decl);

void ast_manager_init(AST_Manager* m);
void ast_manager_free(AST_Manager* m);

AST_Node* ast_manager_get(AST_Manager* m, AST_Id id);
AST_Node* ast_manager_get_relative(AST_Manager* m, AST_Node* from, AST_Id id);

AST_Node* ast_node_null(AST_Manager* m);
AST_Node* ast_symbol(AST_Manager* m, Token tok);
AST_Node* ast_i32_lit(AST_Manager* m, Token tok);
AST_Node* ast_type_bind(AST_Manager* m, Token tok, AST_Node* sym, AST_Node* type);
AST_Node* ast_constant_bind(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r);
AST_Node* ast_variable_bind(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r);
AST_Node* ast_bin_gt(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r);
AST_Node* ast_bin_lt(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r);
AST_Node* ast_bin_ge(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r);
AST_Node* ast_bin_le(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r);
AST_Node* ast_bin_ne(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r);
AST_Node* ast_bin_eq(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r);
AST_Node* ast_bin_add(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r);
AST_Node* ast_bin_sub(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r);
AST_Node* ast_bin_mul(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r);
AST_Node* ast_bin_div(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r);
AST_Node* ast_una_add(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r);
AST_Node* ast_una_sub(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r);
AST_Node* ast_assignment(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r);
AST_Node* ast_member_access(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r);
AST_Node* ast_call(AST_Manager* m, Token tok, AST_Node* sym, AST_Node* params, bool effectfull = false);
AST_Node* ast_function_literal(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r);
AST_Node* ast_function_signature(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r);
AST_Node* ast_type_effect(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r);
AST_Node* ast_handler_literal(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r);
AST_Node* ast_decl_list(AST_Manager* m, Token tok);
AST_Node* ast_program_point(AST_Manager* m, Token tok);
AST_Node* ast_call_arg_list(AST_Manager* m, Token tok);
AST_Node* ast_ctrl_flow_if(AST_Manager* m, Token tok, AST_Node* cond, AST_Node* body, AST_Node* elif);
AST_Node* ast_ctrl_flow_ret(AST_Manager* m, Token tok, AST_Node* expr);
AST_Node* ast_with_handler(AST_Manager* m, Token tok, AST_Node* call, AST_Node* hnd);
AST_Node* ast_ctrl_flow_match(AST_Manager* m, Token tok, AST_Node* expr, AST_Node* cases);
AST_Node* ast_ctrl_flow_case(AST_Manager* m, Token tok, AST_Node* expr, AST_Node* body, AST_Node* tail);
AST_Node* ast_type_type(AST_Manager* m, Token tok);
AST_Node* ast_type_i32(AST_Manager* m, Token tok);
AST_Node* ast_type_unit(AST_Manager* m, Token tok);
AST_Node* ast_type_arrow(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r);
AST_Node* ast_type_yield(AST_Manager* m, Token tok, AST_Node* eff);
AST_Node* ast_type_any(AST_Manager* m, Token tok);
AST_Node* ast_type_struct(AST_Manager* m, Token tok, AST_Node* members);
AST_Node* ast_type_pointer(AST_Manager* m, Token tok, AST_Node* type);
AST_Node* ast_unitialized(AST_Manager* m);
AST_Node* ast_type_union(AST_Manager* m, Token tok, AST_Node* l, AST_Node* tail);
AST_Node* ast_teamplate_type_variable(AST_Manager* m, Token tok, AST_Node* l);
AST_Node* ast_decl_args(AST_Manager* m, Token tok, AST_Node* l, AST_Node* tail);
AST_Node* ast_struct_member(AST_Manager* m, Token tok, AST_Node* mem, AST_Node* tail);
AST_Node* ast_temp_node(AST_Manager* m);
AST_Node* ast_pointer_load(AST_Manager* m, Token tok, AST_Node* ptr);
AST_Node* ast_address_of(AST_Manager* m, Token tok, AST_Node* ptr);
AST_Node* ast_undefined(AST_Manager* m);

b8 ast_is_binary_operation(AST_Node* n);
b8 ast_is_unary_operation(AST_Node* n);
b8 ast_is_temporary(AST_Manager* m, AST_Node* n);
b8 ast_is_null_node(AST_Node* m);
b8 ast_is_undefined_node(AST_Node* m);

AST_Node* _internal_ast_size_of(AST_Manager* m, AST_Node* type);
AST_Node* _internal_ast_get_closure_handler(AST_Manager* m, AST_Node* closure);
AST_Node* _internal_ast_get_closure_environment_buffer(AST_Manager* m, AST_Node* closure);
// AST_Node* _internal_ast_build_stack_closure_object(AST_Manager* m, AST_Node* func, AST_Node* env);
// AST_Node* _internal_ast_build_heap_closure_object(AST_Manager* m, AST_Node* func, AST_Node* env);
AST_Node* _internal_ast_bitset(AST_Manager* m, u64 size);
AST_Node* _internal_ast_bitset_set_bit_on(AST_Manager* m, AST_Node* bitset, u64 index);
AST_Node* _internal_ast_bitset_set_bit_off(AST_Manager* m, AST_Node* bitset, u64 index);
AST_Node* _internal_ast_bitset_union(AST_Manager* m, AST_Node* bitset_a, AST_Node* bitset_b);
AST_Node* _internal_ast_bitset_intersection(AST_Manager* m, AST_Node* bitset_a, AST_Node* bitset_b);
AST_Node* _internal_ast_bitset_is_bit_up(AST_Manager* m, AST_Node* bitset_a, u64 index);
AST_Node* _internal_ast_sizeof(AST_Manager* m, AST_Node* n);
AST_Node* _internal_ast_typeof(AST_Manager* m, AST_Node* n);
AST_Node* _internal_ast_reallocate_heap_buffer(AST_Manager* m, AST_Node* buffer, AST_Node* size);
AST_Node* _internal_ast_allocate_heap_buffer(AST_Manager* m, AST_Node* size);
AST_Node* _internal_ast_setup_closure_environment_buffer_header(
    AST_Manager* m,
    AST_Node*    env_type,
    AST_Node*    header_type,
    AST_Node*    buffer,
    AST_Node*    incc,
    AST_Node*    local_env);
AST_Node* _internal_ast_update_closure_environment_buffer_header(
    AST_Manager* m,
    AST_Node*    env_type,
    AST_Node*    header_type,
    AST_Node*    buffer,
    AST_Node*    incc,
    AST_Node*    local_env);
AST_Node* _internal_ast_capture_variable_into_environment(
    AST_Manager* m,
    AST_Node*    env_type,
    AST_Node*    buff_header_type,
    AST_Node*    buffer,
    AST_Node*    var,
    AST_Node*    incc,
    AST_Node*    local_env);
AST_Node* _internal_ast_borrow_variable_into_environment(
    AST_Manager* m,
    AST_Node*    env_type,
    AST_Node*    buff_header_type,
    AST_Node*    buffer,
    AST_Node*    var,
    AST_Node*    incc,
    AST_Node*    local_env);
AST_Node* _internal_ast_set_closure_object_extern(AST_Manager* m, AST_Node* closure);
AST_Node* _internal_ast_is_closure_object_local(AST_Manager* m, AST_Node* closure);
AST_Node* _internal_ast_get_closure_object_bitset(AST_Manager* m, AST_Node* closure);
AST_Node* _internal_ast_get_closure_environment_buffer_size(AST_Manager* m, AST_Node* closure);
// AST_Node* _internal_ast_capture_environment_bitset(AST_Manager* m, AST_Node* type, AST_Node* buffer, AST_Node*
// bitset, AST_Node* variables);

i8*       ast_kind_to_cstr(u64 k, u64 x = 0);
AST_Node* ast_copy(AST_Manager* m, AST_Node* node);
void      ast_change_kind(AST_Node* m, AST_Kind kind);
AST_Node* ast_call_push_argument(AST_Manager* m, Token tok, AST_Node* call, AST_Node* arg);

AST_Node* ast_type_bind_get_type(AST_Manager* m, AST_Node* n);
AST_Node* ast_type_bind_get_symbol(AST_Manager* m, AST_Node* n);
AST_Node* ast_program_point_get_tail(AST_Manager* m, AST_Node* n);
AST_Node* ast_program_point_get_decl(AST_Manager* m, AST_Node* n);
AST_Node* ast_bind_get_type_bind(AST_Manager* m, AST_Node* n);
AST_Node* ast_bind_get_expr(AST_Manager* m, AST_Node* n);
AST_Node* ast_function_literal_get_signature(AST_Manager* m, AST_Node* n);
AST_Node* ast_function_literal_get_body(AST_Manager* m, AST_Node* n);
AST_Node* ast_function_signature_get_args(AST_Manager* m, AST_Node* n);
AST_Node* ast_function_signature_get_return_type(AST_Manager* m, AST_Node* n);
AST_Node* ast_decl_list_get_elem(AST_Manager* m, AST_Node* n);
AST_Node* ast_decl_list_get_tail(AST_Manager* m, AST_Node* n);
AST_Node* ast_fun_call_get_call_sym(AST_Manager* m, AST_Node* n);
AST_Node* ast_fun_call_get_call_args(AST_Manager* m, AST_Node* n);
AST_Node* ast_ctrl_flow_return_get_expression(AST_Manager* m, AST_Node* ret);
AST_Node* ast_function_literal_push_argument(AST_Manager* m, Token tok, AST_Node* fun_decl, AST_Node* arg);
