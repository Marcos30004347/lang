#include "ast.hpp"

#include "lexer.hpp"
#include "stdlib.h"
#include "utils.hpp"
#include <cassert>
#include <cstring>
#include <stdlib.h>
#include <string.h>
#include <sys/cdefs.h>

const char* ast_kind_strs[] = {
    // Compounds
    "AST_NULL_NODE",

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
    //"AST_CTRL_FLOW_MATCH",
    //"AST_CTRL_FLOW_CASE",
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
    "__AST_UNARY_OPERATOR_END",

    // Applications
    "__AST_CALL_OPERATION_START",
    "AST_FUN_CALL",
    "AST_EFF_CALL",
    "__AST_CALL_OPERATION_END",

    "__AST_INTERNAL_START",
    "AST_PHI_NODE",
    "AST_PHI_NODE_ARG",
    "AST_PHI_NODE_ARG_LIST",
    "__AST_INTERNAL_END",

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

  AST_Node* x = &b->data[child_id % AST_BUCKET_SIZE];

  assert(x->id == child_id);

  return x;
}

AST_Node* ast_manager_alloc(AST_Manager* m, Token tok, u64 kind, AST_Id l, AST_Id r) { return ast_manager_get(m, ast_manager_reserve(m, tok, kind, l, r)); }

void      ast_change_kind(AST_Node* m, AST_Kind kind) { m->kind = kind; }
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

AST_Node* ast_phi(AST_Manager* m, AST_Node* args) {
  assert(args->kind == AST_DECL_ARGS_LIST);

  return ast_manager_alloc(m, lexer_undef_token(), AST_PHI_NODE, args->id, ast_node_null(m)->id);
}

AST_Node* ast_phi_arg(AST_Manager* m, AST_Node* arg, AST_Node* branch) { return ast_manager_alloc(m, lexer_undef_token(), AST_PHI_NODE_ARG, arg->id, branch->id); }

// AST_Node* ast_match(AST_Manager* m, Token tok, AST_Node* expr, AST_Node* cases) {
// 	assert(cases->kind == AST_DECL_ARGS_LIST || cases->kind == AST_NULL_NODE);
//   return ast_manager_alloc(m, lexer_undef_token(), AST_CTRL_FLOW_CASE, expr->id, cases->id);
// }

// AST_Node* ast_match_case(AST_Manager* m, Token tok, AST_Node* expr, AST_Node* body) {
//   return ast_manager_alloc(m, lexer_undef_token(), AST_CTRL_FLOW_CASE, expr->id, body->id);
// }

i8* ast_kind_to_cstr(u64 k) {
  if (k >= __AST_KIND_END) {
    i8 buff[256];

    u8 i = 0;

    while (k) {
      buff[i++] = k % 10 + '0';
      k /= 10;
    }

    buff[i++] = '\0';

    return copy_str(buff);
  }

  return copy_str(ast_kind_strs[k]);
}

AST_Node* ast_ctrl_flow_if(AST_Manager* m, Token tok, AST_Node* cond, AST_Node* body, AST_Node* elif) {
  AST_Node* a = ast_manager_alloc(m, tok, AST_CTRL_FLOW_IF, cond->id, body->id);

  if (ast_is_null_node(elif)) { return a; }

  return ast_manager_alloc(m, tok, AST_CTRL_FLOW_IF_ELSE, a->id, elif->id);
}

AST_Node* ast_ctrl_flow_ret(AST_Manager* m, Token tok, AST_Node* expr) { return ast_manager_alloc(m, tok, AST_CTRL_FLOW_RETURN, expr->id, 0); }

AST_Node* ast_with_handler(AST_Manager* m, Token tok, AST_Node* call, AST_Node* hnd) { return ast_manager_alloc(m, tok, AST_WITH_HANDLER, call->id, hnd->id); }

AST_Node* ast_type_type(AST_Manager* m, Token tok) { return ast_manager_alloc(m, tok, AST_TYPE_TYPE, 0, 0); }

AST_Node* ast_type_i32(AST_Manager* m, Token tok) { return ast_manager_alloc(m, tok, AST_TYPE_I32, 0, 0); }

AST_Node* ast_type_unit(AST_Manager* m, Token tok) { return ast_manager_alloc(m, tok, AST_TYPE_UNIT, 0, 0); }

AST_Node* ast_type_any(AST_Manager* m, Token tok) { return ast_manager_alloc(m, tok, AST_TYPE_ANY, 0, 0); }

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
