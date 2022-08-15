#include "ast.hpp"

#include "lexer.hpp"
#include "stdlib.h"
#include "utils.hpp"
#include <cassert>
#include <sys/cdefs.h>

const char *ast_kind_strs[AST_EFF_CALL + 1] = {
    "AST_NULL_NODE",
    "AST_STATEMENT_LIST",
    "AST_CALL_ARGS_LIST",
    "AST_DECL_ARGS_LIST",
    "AST_HANDLER_EFF_LIST",
    "AST_DECL_LIST",
    "AST_TYPE_TUPLE_MEM_LIST",
    "AST_IF_DECL",
    "AST_TUPLE_LIST",
    "AST_HND_EFF_ARGS",
    "AST_WITH_HND",
    "AST_CTRL_FLOW_IF",
    "AST_CTRL_FLOW_ELSE",
    "AST_CTRL_FLOW_RETURN",
    "AST_CTRL_FLOW_CONTINUE",
    "AST_CONST_BIND",
    "AST_MUT_BIND",
    "AST_TYPE_BIND",
    "AST_SYM_DECL",
    "AST_I32_DECL",
    "AST_FUN_DECL",
    "AST_CLOS_DECL",
    "AST_EFF_DECL",
    "AST_HND_DECL",
    "AST_HND_EFF_DECL",
    "AST_TYPE_I32",
    "AST_TYPE_UNIT",
    "AST_TYPE_ARROW",
    "AST_TYPE_TUPLE",
    "AST_TYPE_TYPE",
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
    "AST_OP_UNA_SUB",
    "AST_OP_UNA_ADD",
    "AST_FUN_CALL",
    "AST_EFF_CALL",
};

void ast_manager_init(AST_Manager *m) {
  m->size = 1;
  m->root = (AST_Bucket *)malloc(sizeof(AST_Bucket));

  m->root->id = 0;
  m->root->next = 0;
  m->root->prev = 0;

  m->tail = m->root;

  m->root->data[0].id = 0;
  m->root->data[0].kind = AST_NULL_NODE;
  m->root->data[0].left = 0;
  m->root->data[0].right = 0;
  m->root->data[0].bucket = m->root;
  m->root->data[0].tok = lexer_undef_token();
}

void ast_manager_free(AST_Manager *m) {
  while (m->root) {
    AST_Bucket *tmp = m->root;
    m->root = m->root->next;
    free(tmp);
  }
}

void ast_init(AST_Bucket *b, AST_Node *a, Token tok, AST_Kind kind, AST_Id id,
              AST_Id l, AST_Id r) {
  a->left = l;
  a->right = r;
  a->id = id;
  a->kind = kind;
  a->tok = tok;
  a->bucket = b;
}

AST_Id ast_manager_reserve(AST_Manager *m, Token tok, AST_Kind kind, AST_Id l,
                           AST_Id r) {
  AST_Id id = m->size;

  if (id % AST_BUCKET_SIZE == 0) {
    m->tail->next = (AST_Bucket *)malloc(sizeof(AST_Bucket));
    m->tail->next->prev = m->tail;
    m->tail->next->id = m->tail->id + 1;
    m->tail = m->tail->next;
  }

  ast_init(m->tail, &m->tail->data[id % AST_BUCKET_SIZE], tok, kind, id, l, r);

  m->size = m->size + 1;

  return id;
}

AST_Node *ast_manager_get(AST_Manager *m, AST_Id id) {
  AST_Bucket *b = m->root;

  for (u64 i = 0; i < id / AST_BUCKET_SIZE; i++) {
    b = b->next;
  }

  return &b->data[id % AST_BUCKET_SIZE];
}

AST_Node *ast_manager_get_relative(AST_Manager *m, AST_Node *root,
                                   AST_Id child_id) {
  assert(root);

  AST_Bucket *b = root->bucket;

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

  AST_Node *x = &b->data[child_id % AST_BUCKET_SIZE];

  assert(x->id == child_id);

  return x;
}

AST_Node *ast_manager_alloc(AST_Manager *m, Token tok, AST_Kind kind, AST_Id l,
                            AST_Id r) {
  return ast_manager_get(m, ast_manager_reserve(m, tok, kind, l, r));
}

AST_Node *ast_symbol(AST_Manager *m, Token tok) {
  return ast_manager_alloc(m, tok, AST_SYM_DECL, 0, 0);
}

AST_Node *ast_i32_lit(AST_Manager *m, Token tok) {
  return ast_manager_alloc(m, tok, AST_I32_DECL, 0, 0);
}

AST_Node *ast_bin_add(AST_Manager *m, Token tok, AST_Node *l, AST_Node *r) {
  return ast_manager_alloc(m, tok, AST_OP_BIN_ADD, l->id, r->id);
}

AST_Node *ast_bin_sub(AST_Manager *m, Token tok, AST_Node *l, AST_Node *r) {
  return ast_manager_alloc(m, tok, AST_OP_BIN_SUB, l->id, r->id);
}

AST_Node *ast_bin_mul(AST_Manager *m, Token tok, AST_Node *l, AST_Node *r) {
  return ast_manager_alloc(m, tok, AST_OP_BIN_MUL, l->id, r->id);
}

AST_Node *ast_bin_div(AST_Manager *m, Token tok, AST_Node *l, AST_Node *r) {
  return ast_manager_alloc(m, tok, AST_OP_BIN_DIV, l->id, r->id);
}

AST_Node *ast_una_add(AST_Manager *m, Token tok, AST_Node *l, AST_Node *r) {
  return ast_manager_alloc(m, tok, AST_OP_UNA_ADD, l->id, r->id);
}

AST_Node *ast_una_sub(AST_Manager *m, Token tok, AST_Node *l, AST_Node *r) {
  return ast_manager_alloc(m, tok, AST_OP_UNA_SUB, l->id, r->id);
}

AST_Node *ast_bin_gt(AST_Manager *m, Token tok, AST_Node *l, AST_Node *r) {
  return ast_manager_alloc(m, tok, AST_OP_BIN_GT, l->id, r->id);
}

AST_Node *ast_bin_lt(AST_Manager *m, Token tok, AST_Node *l, AST_Node *r) {
  return ast_manager_alloc(m, tok, AST_OP_BIN_LT, l->id, r->id);
}

AST_Node *ast_bin_ge(AST_Manager *m, Token tok, AST_Node *l, AST_Node *r) {
  return ast_manager_alloc(m, tok, AST_OP_BIN_GE, l->id, r->id);
}

AST_Node *ast_bin_le(AST_Manager *m, Token tok, AST_Node *l, AST_Node *r) {
  return ast_manager_alloc(m, tok, AST_OP_BIN_LE, l->id, r->id);
}
AST_Node *ast_bin_ne(AST_Manager *m, Token tok, AST_Node *l, AST_Node *r) {
  return ast_manager_alloc(m, tok, AST_OP_BIN_NE, l->id, r->id);
}

AST_Node *ast_bin_eq(AST_Manager *m, Token tok, AST_Node *l, AST_Node *r) {
  return ast_manager_alloc(m, tok, AST_OP_BIN_EQ, l->id, r->id);
}

AST_Node *ast_member_access(AST_Manager *m, Token tok, AST_Node *l,
                            AST_Node *r) {
  return ast_manager_alloc(m, tok, AST_OP_MEMBER_ACCESS, l->id, r->id);
}

AST_Node *ast_assignment(AST_Manager *m, Token tok, AST_Node *l, AST_Node *r) {
  return ast_manager_alloc(m, tok, AST_OP_BIN_ASSIGN, l->id, r->id);
}

AST_Node *ast_call(AST_Manager *m, Token tok, AST_Node *l, AST_Node *r,
                   bool effectfull) {
  return ast_manager_alloc(m, tok, effectfull ? AST_EFF_CALL : AST_FUN_CALL,
                           l->id, r->id);
}

AST_Node *ast_call_arg_list(AST_Manager *m, Token tok) {
  return ast_manager_alloc(m, tok, AST_CALL_ARGS_LIST, 0, 0);
}

AST_Node *ast_decl_arg_list(AST_Manager *m, Token tok) {
  return ast_manager_alloc(m, tok, AST_DECL_ARGS_LIST, 0, 0);
}

AST_Node *ast_statement_list(AST_Manager *m, Token tok) {
  return ast_manager_alloc(m, tok, AST_STATEMENT_LIST, 0, 0);
}

AST_Node *ast_decl_list(AST_Manager *m, Token tok) {
  return ast_manager_alloc(m, tok, AST_STATEMENT_LIST, 0, 0);
}

AST_Node *ast_handler_eff_list(AST_Manager *m, Token tok) {
  return ast_manager_alloc(m, tok, AST_HND_EFF_LIST, 0, 0);
}

AST_Node *ast_const_bind(AST_Manager *m, Token tok, AST_Node *l, AST_Node *r) {
  return ast_manager_alloc(m, tok, AST_CONST_BIND, l->id, r->id);
}

AST_Node *ast_mut_bind(AST_Manager *m, Token tok, AST_Node *l, AST_Node *r) {
  return ast_manager_alloc(m, tok, AST_MUT_BIND, l->id, r->id);
}

AST_Node *ast_fun_decl(AST_Manager *m, Token tok, AST_Node *l, AST_Node *r) {
  return ast_manager_alloc(m, tok, AST_FUN_DECL, l->id, r->id);
}

AST_Node *ast_eff_decl(AST_Manager *m, Token tok, AST_Node *l, AST_Node *r) {
  return ast_manager_alloc(m, tok, AST_EFF_DECL, l->id, r->id);
}

AST_Node *ast_handler_eff_decl(AST_Manager *m, Token tok, AST_Node *l,
                               AST_Node *r) {
  return ast_manager_alloc(m, tok, AST_HND_EFF_DECL, l->id, r->id);
}

AST_Node *ast_handler_decl(AST_Manager *m, Token tok, AST_Node *l,
                           AST_Node *r) {
  return ast_manager_alloc(m, tok, AST_HND_DECL, l->id, r->id);
}

const char *ast_kind_to_cstr(AST_Kind k) { return ast_kind_strs[k]; }

AST_Node *ast_ctrl_flow_if(AST_Manager *m, Token tok, AST_Node *cond,
                           AST_Node *body, AST_Node *elif) {
  AST_Node *a = ast_manager_alloc(m, tok, AST_IF_DECL, cond->id, body->id);
  return ast_manager_alloc(m, tok, AST_CTRL_FLOW_IF, a->id, elif->id);
}

AST_Node *ast_ctrl_flow_ret(AST_Manager *m, Token tok, AST_Node *expr) {
  return ast_manager_alloc(m, tok, AST_CTRL_FLOW_RETURN, expr->id, 0);
}

AST_Node *ast_ctrl_flow_continue(AST_Manager *m, Token tok, AST_Node *cont,
                                 AST_Node *expr) {
  return ast_manager_alloc(m, tok, AST_CTRL_FLOW_CONTINUE, cont->id, expr->id);
}

AST_Node *ast_hnd_eff_decl_args(AST_Manager *m, Token tok, AST_Node *args,
                                AST_Node *cont) {
  return ast_manager_alloc(m, tok, AST_HND_EFF_ARGS, args->id, cont->id);
}

AST_Node *ast_with_hnd(AST_Manager *m, Token tok, AST_Node *call,
                       AST_Node *hnd) {
  return ast_manager_alloc(m, tok, AST_WITH_HND, call->id, hnd->id);
}

AST_Node *ast_type_type(AST_Manager *m, Token tok) {
  return ast_manager_alloc(m, tok, AST_TYPE_TYPE, 0, 0);
}

AST_Node *ast_type_i32(AST_Manager *m, Token tok) {
  return ast_manager_alloc(m, tok, AST_TYPE_I32, 0, 0);
}

AST_Node *ast_type_unit(AST_Manager *m, Token tok) {
  return ast_manager_alloc(m, tok, AST_TYPE_UNIT, 0, 0);
}

AST_Node *ast_type_arrow(AST_Manager *m, Token tok, AST_Node *l, AST_Node *r) {
  return ast_manager_alloc(m, tok, AST_TYPE_ARROW, l->id, r->id);
}

AST_Node *ast_type_tuple_arg(AST_Manager *m, Token tok, AST_Node *mem,
                             AST_Node *tail) {
  return ast_manager_alloc(m, tok, AST_TYPE_TUPLE_MEM_LIST, mem->id, tail->id);
}

AST_Node *ast_type_tuple(AST_Manager *m, Token tok, AST_Node *members) {
  assert(members->kind == AST_TYPE_TUPLE_MEM_LIST);
  return ast_manager_alloc(m, tok, AST_TYPE_I32, members->id, 0);
}

AST_Node *ast_node_null(AST_Manager *m) { return &m->root->data[0]; }

AST_Node *ast_tuple_list(AST_Manager *m, Token tok, AST_Node *l,
                         AST_Node *tail) {
  return ast_manager_alloc(m, tok, AST_TUPLE_LIST, l->id, tail->id);
}

AST_Node *ast_type_bind(AST_Manager *m, Token tok, AST_Node *sym,
                        AST_Node *type) {
  return ast_manager_alloc(m, tok, AST_TYPE_BIND, sym->id, type->id);
}


