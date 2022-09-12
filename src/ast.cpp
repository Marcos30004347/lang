#include "ast.hpp"

#include "lexer.hpp"
#include "stdlib.h"
#include "utils.hpp"
#include <cassert>
#include <cstdio>
#include <cstring>
#include <stdlib.h>
#include <string.h>

const char* ast_kind_strs[] = {
    // Compounds
    "AST_NULL_NODE",
    "AST_UNDEFINED_NODE",
    "AST_UNITIALIZED_NODE",
    "__AST_KIND_BEGIN",

    "AST_PROGRAM_POINT",

    "AST_CALL_ARGS_LIST",
    "AST_DECL_ARGS_LIST",

    "AST_WITH_HANDLER",

    // Control Flow
    "__AST_CTRL_FLOW_START",
    "AST_CTRL_FLOW_IF",
    "AST_CTRL_FLOW_IF_ELSE",
    "AST_CTRL_FLOW_RETURN",
    "AST_CTRL_FLOW_MATCH",
    "AST_CTRL_FLOW_CASE",
    "__AST_CTRL_FLOW_END",

    // Bindings
    "AST_BIND_CONSTANT",
    "AST_BIND_VARIABLE",
    "AST_BIND_TYPE",

    // Declarations
    "__AST_LITERAL_START",
    "AST_SYM_LITERAL",
    "AST_I32_LITERAL",
    "AST_FUN_LITERAL",
    "AST_HANDLER_LITERAL",
    "__AST_LITERAL_END",

    "AST_FUN_SIGNATURE",

    // Types
    "__AST_TYPE_KIND_START",
    "AST_TYPE_I32",
    "AST_TYPE_UNIT",
    "AST_TYPE_ANY",
    "AST_TYPE_ARROW",
    "AST_TYPE_EFFECT",
    "AST_TYPE_TUPLE",
    "AST_TYPE_TYPE",
    "AST_TYPE_UNION",
    "AST_TYPE_YIELD",
    "AST_TYPE_STRUCT",
    "AST_TYPE_VARIABLE",
    "AST_TYPE_POINTER",
    "__AST_TYPE_KIND_END",

    // Binary operators
    "__AST_BINARY_OPERATOR_START",
    "AST_OP_BIN_ASSIGN",
    "AST_OP_BIN_ADD",
    "AST_OP_BIN_SUB",
    "AST_OP_BIN_MUL",
    "AST_OP_BIN_DIV",
    "AST_OP_BIN_GT",
    "AST_OP_BIN_LT",
    "AST_OP_BIN_GE",
    "AST_OP_BIN_LE",
    "AST_OP_BIN_NE",
    "AST_OP_BIN_EQ",
    "AST_OP_MEMBER_ACCESS",
    "__AST_BINARY_OPERATOR_END",

    // Unary operators
    "__AST_UNARY_OPERATOR_START",
    "AST_OP_UNA_SUB",
    "AST_OP_UNA_ADD",
    "AST_OP_POINTER_LOAD",
    "AST_OP_ADDRESS_OF",
    "__AST_UNARY_OPERATOR_END",

    // Applications
    "__AST_CALL_OPERATION_START",
    "AST_FUN_CALL",
    "AST_EFF_CALL",
    "__AST_CALL_OPERATION_END",

    "__AST_INTERNAL_START",
    "_AST_GET_CLOSURE_HANDLER",
    "_AST_GET_CLOSURE_ENVIRONMENT",
    "_AST_SIZE_OF",
    "_AST_TYPE_OF",
    "_AST_BITSET",
    "_AST_BITSET_SET_BIT_ON",
    "_AST_BITSET_SET_BIT_OFF",
    "_AST_BITSET_BINARY_UNION",
    "_AST_BITSET_BINARY_INTERSECTION",
    "_AST_BITSET_IS_BIT_UP",
    "_AST_REALLOCATE_HEAP_BUFFER",
    "_AST_ALLOCATE_HEAP_BUFFER",
    "_AST_CAPTURE_VARIABLE_INTO_ENVIRONMENT",
    "_AST_BORROW_VARIABLE_INTO_ENVIRONMENT",
    "_AST_SETUP_CLOSURE_ENVIRONMENT_BUFFER_HEADER",
    "_AST_BUILD_CLOSURE_OBJECT",
    "__AST_INTERNAL_END",
    "__AST_KIND_END",

    "__AST_KIND_END",
};

void ast_manager_init(AST_Manager* m) {
  m->size = 1;
  m->root = (AST_Bucket*)malloc(sizeof(AST_Bucket));

  m->statements_list_root_id = 0;
  m->statements_list_tail_id = 0;
  m->statement_list_tail_ptr = &m->root->data[0];

  m->temp = __AST_KIND_END + 1;

  m->root->id   = 0;
  m->root->next = 0;
  m->root->prev = 0;

  m->tail = m->root;

  m->root->data[0].id     = 0;
  m->root->data[0].kind   = AST_NULL_NODE;
  m->root->data[0].left   = 0;
  m->root->data[0].right  = 0;
  m->root->data[0].bucket = m->root;
  m->root->data[0].tok    = lexer_undef_token();
}

b8 ast_is_null_node(AST_Node* m) { return m->id == 0; }
b8 ast_is_undefined_node(AST_Node* m) { return m->kind == AST_UNDEFINED_NODE; }

void ast_manager_free(AST_Manager* m) {
  while (m->root) {
    AST_Bucket* tmp = m->root;
    m->root         = m->root->next;
    free(tmp);
  }
}

void ast_init(AST_Bucket* b, AST_Node* a, Token tok, u64 kind, AST_Id id, AST_Id l = 0, AST_Id r = 0) {
  a->left   = l;
  a->right  = r;
  a->id     = id;
  a->kind   = kind;
  a->tok    = tok;
  a->bucket = b;
}

AST_Id ast_manager_reserve(AST_Manager* m, Token tok, u64 kind, AST_Id l, AST_Id r) {
  AST_Id id = m->size;

  if (id % AST_BUCKET_SIZE == 0) {
    m->tail->next       = (AST_Bucket*)malloc(sizeof(AST_Bucket));
    m->tail->next->prev = m->tail;
    m->tail->next->id   = m->tail->id + 1;
    m->tail             = m->tail->next;
  }

  ast_init(m->tail, &m->tail->data[id % AST_BUCKET_SIZE], tok, kind, id, l, r);

  m->size = m->size + 1;

  return id;
}

AST_Node* ast_manager_get(AST_Manager* m, AST_Id id) {
  AST_Bucket* b = m->root;

  for (u64 i = 0; i < id / AST_BUCKET_SIZE; i++) {
    b = b->next;
  }

  return &b->data[id % AST_BUCKET_SIZE];
}

AST_Node* ast_manager_get_relative(AST_Manager* m, AST_Node* root, AST_Id child_id) {
  assert(root);
  AST_Bucket* b = root->bucket;

  u32 w = round_down(root->id, AST_BUCKET_SIZE);
  u32 s = round_down(child_id, AST_BUCKET_SIZE);

  while (s > w) {
    w += AST_BUCKET_SIZE;
    b = b->next;
  }

  while (w > s) {
    w -= AST_BUCKET_SIZE;
    b = b->prev;
  }

  return &b->data[child_id % AST_BUCKET_SIZE];
}

AST_Node* ast_manager_alloc(AST_Manager* m, Token tok, u64 kind, AST_Id l, AST_Id r) { return ast_manager_get(m, ast_manager_reserve(m, tok, kind, l, r)); }

void ast_change_kind(AST_Node* m, AST_Kind kind) { m->kind = kind; }

AST_Node* ast_symbol(AST_Manager* m, Token tok) { return ast_manager_alloc(m, tok, AST_SYMBOL_LITERAL, 0, 0); }

AST_Node* ast_i32_lit(AST_Manager* m, Token tok) { return ast_manager_alloc(m, tok, AST_NATURAL_LITERAL, 0, 0); }

AST_Node* ast_bin_add(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r) { return ast_manager_alloc(m, tok, AST_OP_BIN_ADD, l->id, r->id); }

AST_Node* ast_bin_sub(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r) { return ast_manager_alloc(m, tok, AST_OP_BIN_SUB, l->id, r->id); }

AST_Node* ast_bin_mul(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r) { return ast_manager_alloc(m, tok, AST_OP_BIN_MUL, l->id, r->id); }

AST_Node* ast_bin_div(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r) { return ast_manager_alloc(m, tok, AST_OP_BIN_DIV, l->id, r->id); }

AST_Node* ast_una_add(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r) { return ast_manager_alloc(m, tok, AST_OP_UNA_ADD, l->id, r->id); }

AST_Node* ast_una_sub(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r) { return ast_manager_alloc(m, tok, AST_OP_UNA_SUB, l->id, r->id); }

AST_Node* ast_bin_gt(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r) { return ast_manager_alloc(m, tok, AST_OP_BIN_GT, l->id, r->id); }

AST_Node* ast_bin_lt(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r) { return ast_manager_alloc(m, tok, AST_OP_BIN_LT, l->id, r->id); }

AST_Node* ast_bin_ge(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r) { return ast_manager_alloc(m, tok, AST_OP_BIN_GE, l->id, r->id); }

AST_Node* ast_bin_le(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r) { return ast_manager_alloc(m, tok, AST_OP_BIN_LE, l->id, r->id); }
AST_Node* ast_bin_ne(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r) { return ast_manager_alloc(m, tok, AST_OP_BIN_NE, l->id, r->id); }

AST_Node* ast_bin_eq(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r) { return ast_manager_alloc(m, tok, AST_OP_BIN_EQ, l->id, r->id); }

AST_Node* ast_member_access(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r) { return ast_manager_alloc(m, tok, AST_OP_MEMBER_ACCESS, l->id, r->id); }

AST_Node* ast_assignment(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r) { return ast_manager_alloc(m, tok, AST_OP_BIN_ASSIGN, l->id, r->id); }

AST_Node* ast_call(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r, bool effectfull) {
  return ast_manager_alloc(m, tok, effectfull ? AST_EFFECT_CALL : AST_FUNCTION_CALL, l->id, r->id);
}

AST_Node* ast_call_arg_list(AST_Manager* m, Token tok) { return ast_manager_alloc(m, tok, AST_CALL_ARGS_LIST, 0, 0); }

// AST_Node *ast_decl_arg_list(AST_Manager *m, Token tok) {
//   return ast_manager_alloc(m, tok, AST_DECL_ARGS_LIST, 0, 0);
// }

AST_Node* ast_program_point(AST_Manager* m, Token tok) { return ast_manager_alloc(m, tok, AST_PROGRAM_POINT, 0, 0); }

AST_Node* ast_decl_list(AST_Manager* m, Token tok) { return ast_manager_alloc(m, tok, AST_PROGRAM_POINT, 0, 0); }

// AST_Node *ast_handler_eff_list(AST_Manager *m, Token tok) {
//   return ast_manager_alloc(m, tok, AST_HND_EFF_LIST, 0, 0);
// }

AST_Node* ast_constant_bind(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r) { return ast_manager_alloc(m, tok, AST_BIND_CONSTANT, l->id, r->id); }

AST_Node* ast_variable_bind(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r) { return ast_manager_alloc(m, tok, AST_BIND_VARIABLE, l->id, r->id); }

AST_Node* ast_function_literal(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r) { return ast_manager_alloc(m, tok, AST_FUNCTION_LITERAL, l->id, r->id); }
AST_Node* ast_function_signature(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r) { return ast_manager_alloc(m, tok, AST_FUN_SIGNATURE, l->id, r->id); }

AST_Node* ast_type_effect(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r) { return ast_manager_alloc(m, tok, AST_TYPE_EFFECT, l->id, r->id); }

AST_Node* ast_handler_literal(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r) { return ast_manager_alloc(m, tok, AST_HANDLER_LITERAL, l->id, r->id); }

i8* ast_kind_to_cstr(u64 k, u64 x) {
  if (k >= __AST_KIND_END) {
    assert(k >= x);
    i32 n = snprintf(NULL, 0, "%lu", k - x);
    assert(n > 0);
    i8  buf[n + 1];
    i32 c = snprintf(buf, n + 1, "%lu", k - x);
    assert(buf[n] == '\0');
    assert(c == n);

    return copy_str(buf);
  }

  return copy_str(ast_kind_strs[k]);
}

AST_Node* ast_ctrl_flow_if(AST_Manager* m, Token tok, AST_Node* cond, AST_Node* body, AST_Node* elif) {
  AST_Node* a = ast_manager_alloc(m, tok, AST_CTRL_FLOW_IF, cond->id, body->id);

  if (ast_is_null_node(elif)) { return a; }

  return ast_manager_alloc(m, tok, AST_CTRL_FLOW_IF_ELSE, a->id, elif->id);
}

AST_Node* ast_ctrl_flow_match(AST_Manager* m, Token tok, AST_Node* expr, AST_Node* cases) { return ast_manager_alloc(m, tok, AST_CTRL_FLOW_MATCH, expr->id, cases->id); }

AST_Node* ast_ctrl_flow_case(AST_Manager* m, Token tok, AST_Node* expr, AST_Node* body, AST_Node* tail) {
  AST_Node* case_expr = ast_manager_alloc(m, tok, AST_CTRL_FLOW_CASE, expr->id, body->id);

  AST_Node* pp = ast_program_point(m, tok);

  pp->left  = case_expr->id;
  pp->right = tail->id;

  return pp;
}

AST_Node* ast_ctrl_flow_ret(AST_Manager* m, Token tok, AST_Node* expr) { return ast_manager_alloc(m, tok, AST_CTRL_FLOW_RETURN, expr->id, 0); }

AST_Node* ast_with_handler(AST_Manager* m, Token tok, AST_Node* call, AST_Node* hnd) { return ast_manager_alloc(m, tok, AST_WITH_HANDLER, call->id, hnd->id); }

AST_Node* ast_type_type(AST_Manager* m, Token tok) { return ast_manager_alloc(m, tok, AST_TYPE_TYPE, 0, 0); }

AST_Node* ast_type_i32(AST_Manager* m, Token tok) { return ast_manager_alloc(m, tok, AST_TYPE_I32, 0, 0); }

AST_Node* ast_type_unit(AST_Manager* m, Token tok) { return ast_manager_alloc(m, tok, AST_TYPE_UNIT, 0, 0); }

AST_Node* ast_unitialized(AST_Manager* m) { return ast_manager_alloc(m, lexer_undef_token(), AST_UNITIALIZED_NODE, 0, 0); }
AST_Node* ast_type_any(AST_Manager* m, Token tok) { return ast_manager_alloc(m, tok, AST_TYPE_ANY, 0, 0); }

AST_Node* ast_type_pointer(AST_Manager* m, Token tok, AST_Node* type) { return ast_manager_alloc(m, tok, AST_TYPE_POINTER, type->id, 0); }
AST_Node* ast_pointer_load(AST_Manager* m, Token tok, AST_Node* type) { return ast_manager_alloc(m, tok, AST_OP_POINTER_LOAD, type->id, 0); }

AST_Node* ast_address_of(AST_Manager* m, Token tok, AST_Node* value) { return ast_manager_alloc(m, tok, AST_OP_ADDRESS_OF, value->id, 0); }

AST_Node* ast_type_arrow(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r) { return ast_manager_alloc(m, tok, AST_TYPE_ARROW, l->id, r->id); }

AST_Node* ast_node_null(AST_Manager* m) { return &m->root->data[0]; }

AST_Node* ast_type_bind(AST_Manager* m, Token tok, AST_Node* sym, AST_Node* type) { return ast_manager_alloc(m, tok, AST_BIND_TYPE, sym->id, type->id); }

AST_Node* ast_type_union(AST_Manager* m, Token tok, AST_Node* l, AST_Node* tail) { return ast_manager_alloc(m, tok, AST_TYPE_UNION, l->id, tail->id); }

AST_Node* ast_teamplate_type_variable(AST_Manager* m, Token tok, AST_Node* l) { return ast_manager_alloc(m, tok, AST_TYPE_VARIABLE, l->id, ast_node_null(m)->id); }

AST_Node* ast_decl_args(AST_Manager* m, Token tok, AST_Node* l, AST_Node* tail) { return ast_manager_alloc(m, tok, AST_DECL_ARGS_LIST, l->id, tail->id); }
AST_Node* ast_temp_node(AST_Manager* m) {
  m->temp += 1;
  return ast_manager_alloc(m, lexer_undef_token(), m->temp, 0, 0);
}

AST_Node* ast_type_yield(AST_Manager* m, Token tok, AST_Node* eff) { return ast_manager_alloc(m, tok, AST_TYPE_YIELD, eff->id, ast_node_null(m)->id); }

AST_Node* ast_type_struct(AST_Manager* m, Token tok, AST_Node* mems) { return ast_manager_alloc(m, tok, AST_TYPE_STRUCT, mems->id, ast_node_null(m)->id); }

AST_Node* ast_struct_member(AST_Manager* m, Token tok, AST_Node* mem, AST_Node* tail) { return ast_manager_alloc(m, tok, AST_PROGRAM_POINT, mem->id, tail->id); }

AST_Node* ast_type_bind_get_type(AST_Manager* m, AST_Node* n) { return ast_manager_get_relative(m, n, n->right); }

AST_Node* ast_type_bind_get_symbol(AST_Manager* m, AST_Node* n) { return ast_manager_get_relative(m, n, n->left); }

AST_Node* ast_program_point_get_tail(AST_Manager* m, AST_Node* n) { return ast_manager_get_relative(m, n, n->right); }

AST_Node* ast_program_point_get_decl(AST_Manager* m, AST_Node* n) { return ast_manager_get_relative(m, n, n->left); }

AST_Node* ast_bind_get_type_bind(AST_Manager* m, AST_Node* n) { return ast_manager_get_relative(m, n, n->left); }

AST_Node* ast_bind_get_expr(AST_Manager* m, AST_Node* n) { return ast_manager_get_relative(m, n, n->right); }

AST_Node* ast_function_literal_get_signature(AST_Manager* m, AST_Node* n) { return ast_manager_get_relative(m, n, n->left); }

AST_Node* ast_function_literal_get_body(AST_Manager* m, AST_Node* n) { return ast_manager_get_relative(m, n, n->right); }

AST_Node* ast_function_signature_get_args(AST_Manager* m, AST_Node* n) { return ast_manager_get_relative(m, n, n->left); }

AST_Node* ast_function_signature_get_return_type(AST_Manager* m, AST_Node* n) { return ast_manager_get_relative(m, n, n->right); }

AST_Node* ast_decl_list_get_elem(AST_Manager* m, AST_Node* n) { return ast_manager_get_relative(m, n, n->left); }

AST_Node* ast_decl_list_get_tail(AST_Manager* m, AST_Node* n) { return ast_manager_get_relative(m, n, n->right); }

AST_Node* ast_fun_call_get_call_sym(AST_Manager* m, AST_Node* n) { return ast_manager_get_relative(m, n, n->left); }

AST_Node* ast_fun_call_get_call_args(AST_Manager* m, AST_Node* n) { return ast_manager_get_relative(m, n, n->right); }

AST_Node* ast_ctrl_flow_return_get_expression(AST_Manager* m, AST_Node* ret) { return ast_manager_get_relative(m, ret, ret->left); }

AST_Node* ast_manager_push_decl(AST_Manager* m, AST_Node* decl) {
  AST_Node* root = ast_decl_list(m, decl->tok);

  root->left  = decl->id;
  root->right = ast_node_null(m)->id;

  if (m->statements_list_root_id == 0) {
    m->statements_list_root_id = root->id;
    m->statements_list_tail_id = root->id;

    m->statement_list_tail_ptr = root;

    return root;
  }

  m->statement_list_tail_ptr->right = root->id;
  m->statement_list_tail_ptr        = root;

  return root;
}

AST_Node* ast_function_literal_push_argument(AST_Manager* m, Token tok, AST_Node* func, AST_Node* arg) {
  AST_Node* sign = ast_function_literal_get_signature(m, func);
  AST_Node* args = ast_function_signature_get_args(m, sign);

  if (ast_is_null_node(args)) {
    AST_Node* narg = ast_decl_args(m, tok, arg, ast_node_null(m));
    sign->left     = narg->id;
    return narg;
  }

  if (ast_is_null_node(ast_manager_get_relative(m, args, args->left))) {
    args->left = arg->id;
    return args;
  }

  while (args->right != 0) {
    args = ast_manager_get_relative(m, args, args->right);
  }

  AST_Node* narg = ast_decl_args(m, tok, arg, ast_node_null(m));

  args->right = narg->id;

  return narg;
}

AST_Node* ast_call_push_argument(AST_Manager* m, Token tok, AST_Node* call, AST_Node* arg) {
  AST_Node* args = ast_manager_get_relative(m, call, call->right);

  if (ast_is_null_node(args)) {
    AST_Node* narg = ast_decl_args(m, tok, arg, ast_node_null(m));
    call->right    = narg->id;
    return narg;
  }

  if (ast_is_null_node(ast_manager_get_relative(m, args, args->left))) {
    args->left = arg->id;
    return args;
  }

  while (args->right != 0) {
    args = ast_manager_get_relative(m, args, args->right);
  }

  AST_Node* narg = ast_decl_args(m, tok, arg, ast_node_null(m));

  args->right = narg->id;

  return narg;
}

b8 ast_is_temporary(AST_Manager* m, AST_Node* n) { return n->kind >= __AST_KIND_END; }

AST_Node* ast_undefined(AST_Manager* m) { return ast_manager_alloc(m, lexer_undef_token(), AST_UNDEFINED_NODE, ast_node_null(m)->id, ast_node_null(m)->id); }

AST_Node* _internal_ast_size_of(AST_Manager* m, AST_Node* type) { return ast_manager_alloc(m, lexer_undef_token(), _AST_SIZE_OF, type->id, ast_node_null(m)->id); }
// AST_Node* _internal_ast_build_stack_closure_object(AST_Manager* m, AST_Node* func, AST_Node* env) {
//   return ast_manager_alloc(m, lexer_undef_token(), _AST_BUILD_STACK_CLOSURE_OBJECT, func->id, env->id);
// }

// AST_Node* _internal_ast_build_heap_closure_object(AST_Manager* m, AST_Node* func, AST_Node* env) {
//   return ast_manager_alloc(m, lexer_undef_token(), _AST_BUILD_HEAP_CLOSURE_OBJECT, func->id, env->id);
// }
AST_Node* _internal_ast_get_closure_handler(AST_Manager* m, AST_Node* closure) {

  return ast_manager_alloc(m, lexer_undef_token(), _AST_GET_CLOSURE_HANDLER, closure->id, ast_node_null(m)->id);
}

AST_Node* _internal_ast_get_closure_environment_buffer(AST_Manager* m, AST_Node* closure) {
  return ast_manager_alloc(m, lexer_undef_token(), _AST_GET_CLOSURE_ENVIRONMENT_BUFFER, closure->id, ast_node_null(m)->id);
}
AST_Node* _internal_ast_get_closure_environment_buffer_size(AST_Manager* m, AST_Node* closure) {
  return ast_manager_alloc(m, lexer_undef_token(), _AST_GET_CLOSURE_ENVIRONMENT_BUFFER_SIZE, closure->id, ast_node_null(m)->id);
}

AST_Node* ast_copy(AST_Manager* m, AST_Node* node) {
  if (ast_is_null_node(node)) return ast_node_null(m);

  AST_Node* l = ast_copy(m, ast_manager_get_relative(m, node, node->left));
  AST_Node* r = ast_copy(m, ast_manager_get_relative(m, node, node->right));
  AST_Node* c = ast_manager_alloc(m, node->tok, node->kind, l->id, r->id);

  return c;
}

AST_Node* _internal_ast_sizeof(AST_Manager* m, AST_Node* n) { return ast_manager_alloc(m, lexer_undef_token(), _AST_SIZE_OF, n->id, ast_node_null(m)->id); }

AST_Node* _internal_ast_typeof(AST_Manager* m, AST_Node* n) { return ast_manager_alloc(m, lexer_undef_token(), _AST_TYPE_OF, n->id, ast_node_null(m)->id); }

AST_Node* _internal_ast_reallocate_heap_buffer(AST_Manager* m, AST_Node* buffer, AST_Node* size) {
  return ast_manager_alloc(m, lexer_undef_token(), _AST_REALLOCATE_HEAP_BUFFER, size->id, ast_node_null(m)->id);
}

AST_Node* _internal_ast_allocate_heap_buffer(AST_Manager* m, AST_Node* size) {
  return ast_manager_alloc(m, lexer_undef_token(), _AST_ALLOCATE_HEAP_BUFFER, size->id, ast_node_null(m)->id);
}

AST_Node* _internal_ast_capture_variable_into_environment(
    AST_Manager* m, AST_Node* env_type, AST_Node* header_type, AST_Node* buffer, AST_Node* var, AST_Node* incc, AST_Node* local_env) {

  Token undef = lexer_undef_token();

  AST_Node* l5 = ast_decl_args(m, undef, local_env, ast_node_null(m));
  AST_Node* l4 = ast_decl_args(m, undef, incc, l5);
  AST_Node* l3 = ast_decl_args(m, undef, var, l4);
  AST_Node* l2 = ast_decl_args(m, undef, buffer, l3);
  AST_Node* l1 = ast_decl_args(m, undef, header_type, l2);
  AST_Node* l0 = ast_decl_args(m, undef, env_type, l1);

  return ast_manager_alloc(m, lexer_undef_token(), _AST_CAPTURE_VARIABLE_INTO_ENVIRONMENT, l0->id, ast_node_null(m)->id);
}

AST_Node* _internal_ast_borrow_variable_into_environment(
    AST_Manager* m, AST_Node* env_type, AST_Node* header_type, AST_Node* buffer, AST_Node* var, AST_Node* incc, AST_Node* local_env) {
  Token undef = lexer_undef_token();

  AST_Node* l5 = ast_decl_args(m, undef, local_env, ast_node_null(m));
  AST_Node* l4 = ast_decl_args(m, undef, incc, l5);
  AST_Node* l3 = ast_decl_args(m, undef, var, l4);
  AST_Node* l2 = ast_decl_args(m, undef, buffer, l3);
  AST_Node* l1 = ast_decl_args(m, undef, header_type, l2);
  AST_Node* l0 = ast_decl_args(m, undef, env_type, l1);

  return ast_manager_alloc(m, lexer_undef_token(), _AST_BORROW_VARIABLE_INTO_ENVIRONMENT, l0->id, ast_node_null(m)->id);
}

AST_Node*
_internal_ast_setup_closure_environment_buffer_header(AST_Manager* m, AST_Node* env_type, AST_Node* header_type, AST_Node* buffer, AST_Node* incc, AST_Node* local_env) {

  Token undef = lexer_undef_token();

  AST_Node* l5 = ast_decl_args(m, undef, local_env, ast_node_null(m));
  AST_Node* l4 = ast_decl_args(m, undef, incc, l5);
  AST_Node* l2 = ast_decl_args(m, undef, buffer, l4);
  AST_Node* l1 = ast_decl_args(m, undef, header_type, l2);
  AST_Node* l0 = ast_decl_args(m, undef, env_type, l2);

  return ast_manager_alloc(m, lexer_undef_token(), _AST_SETUP_CLOSURE_ENVIRONMENT_BUFFER_HEADER, l0->id, ast_node_null(m)->id);
}

AST_Node*
_internal_ast_update_closure_environment_buffer_header(AST_Manager* m, AST_Node* env_type, AST_Node* header_type, AST_Node* buffer, AST_Node* incc, AST_Node* local_env) {

  Token undef = lexer_undef_token();

  AST_Node* l5 = ast_decl_args(m, undef, local_env, ast_node_null(m));
  AST_Node* l4 = ast_decl_args(m, undef, incc, l5);
  AST_Node* l2 = ast_decl_args(m, undef, buffer, l4);
  AST_Node* l1 = ast_decl_args(m, undef, header_type, l2);
  AST_Node* l0 = ast_decl_args(m, undef, env_type, l2);

  return ast_manager_alloc(m, lexer_undef_token(), _AST_UPDATE_CLOSURE_ENVIRONMENT_BUFFER_HEADER, l0->id, ast_node_null(m)->id);
}

AST_Node* _internal_ast_set_closure_object_extern(AST_Manager* m, AST_Node* closure) {
  Token undef = lexer_undef_token();

  AST_Node* l0 = ast_decl_args(m, undef, closure, ast_node_null(m));

  return ast_manager_alloc(m, lexer_undef_token(), _AST_SET_CLOSURE_OBJECT_EXTERN, l0->id, ast_node_null(m)->id);
}

AST_Node* _internal_ast_get_closure_object_bitset(AST_Manager* m, AST_Node* closure) {
  Token undef = lexer_undef_token();

  AST_Node* l0 = ast_decl_args(m, undef, closure, ast_node_null(m));

  return ast_manager_alloc(m, lexer_undef_token(), _AST_GET_CLOSURE_OBJECT_BITSET, l0->id, ast_node_null(m)->id);
}

AST_Node* _internal_ast_is_closure_object_local(AST_Manager* m, AST_Node* closure) {

  Token undef = lexer_undef_token();

  AST_Node* l0 = ast_decl_args(m, undef, closure, ast_node_null(m));

  return ast_manager_alloc(m, lexer_undef_token(), _AST_IS_CLOSURE_OBJECT_LOCAL, l0->id, ast_node_null(m)->id);
}

AST_Node* _internal_ast_bitset(AST_Manager* m, u64 size) {
  Token bitset_bits;

  bitset_bits.buf  = size;
  bitset_bits.size = -1;
  bitset_bits.col  = -1;
  bitset_bits.row  = -1;
  bitset_bits.pos  = -1;
  bitset_bits.type = TOKEN_I32_LIT;

  return ast_manager_alloc(m, bitset_bits, _AST_BITSET, ast_node_null(m)->id, ast_node_null(m)->id);
}

AST_Node* _internal_ast_bitset_set_bit_on(AST_Manager* m, AST_Node* bitset, u64 index) {
  Token bitset_bits;

  bitset_bits.buf  = index;
  bitset_bits.size = -1;
  bitset_bits.col  = -1;
  bitset_bits.row  = -1;
  bitset_bits.pos  = -1;
  bitset_bits.type = TOKEN_I32_LIT;

  return ast_manager_alloc(m, bitset_bits, _AST_BITSET_SET_BIT_ON, bitset->id, ast_node_null(m)->id);
}

AST_Node* _internal_ast_bitset_set_bit_off(AST_Manager* m, AST_Node* bitset, u64 index) {
  Token bitset_bits;

  bitset_bits.buf  = index;
  bitset_bits.size = -1;
  bitset_bits.col  = -1;
  bitset_bits.row  = -1;
  bitset_bits.pos  = -1;
  bitset_bits.type = TOKEN_I32_LIT;

  return ast_manager_alloc(m, bitset_bits, _AST_BITSET_SET_BIT_OFF, bitset->id, ast_node_null(m)->id);
}

AST_Node* _internal_ast_bitset_union(AST_Manager* m, AST_Node* bitset_a, AST_Node* bitset_b) {
  return ast_manager_alloc(m, lexer_undef_token(), _AST_BITSET_BINARY_UNION, bitset_a->id, bitset_b->id);
}

AST_Node* _internal_ast_binary_set_intersection(AST_Manager* m, AST_Node* bitset_a, AST_Node* bitset_b) {
  return ast_manager_alloc(m, lexer_undef_token(), _AST_BITSET_BINARY_INTERSECTION, bitset_a->id, bitset_b->id);
}

AST_Node* _internal_ast_bitset_is_bit_up(AST_Manager* m, AST_Node* bitset_a, u64 index) {
  Token bitset_index;

  bitset_index.buf  = index;
  bitset_index.size = -1;
  bitset_index.col  = -1;
  bitset_index.row  = -1;
  bitset_index.pos  = -1;
  bitset_index.type = TOKEN_I32_LIT;

  return ast_manager_alloc(m, bitset_index, _AST_BITSET_IS_BIT_UP, bitset_a->id, ast_node_null(m)->id);
}

b8 ast_is_binary_operation(AST_Node* n) { return n->kind >= __AST_BINARY_OPERATOR_START && n->kind <= __AST_BINARY_OPERATOR_END; }

b8 ast_is_unary_operation(AST_Node* n) { return n->kind > __AST_UNARY_OPERATOR_START && n->kind < __AST_UNARY_OPERATOR_END; }
