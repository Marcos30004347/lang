#include "ast.hpp"

#include "stdlib.h"
#include "utils.hpp"
#include <cassert>
#include <sys/cdefs.h>

#define TYPE_UNDEF 0

const char *ast_kind_strs[AST_EFF_CALL + 1] = {
    "AST_STATEMENT_LIST",   "AST_CALL_ARGS_LIST",   "AST_DECL_ARGS_LIST",
    "AST_HANDLER_EFF_LIST", "AST_DECL_LIST",        "AST_IF_DECL",
    "AST_HND_EFF_ARGS",     "AST_WITH_HND",         "AST_CTRL_FLOW_IF",
    "AST_CTRL_FLOW_ELSE",   "AST_CTRL_FLOW_RETURN", "AST_CTRL_FLOW_CONTINUE",
    "AST_CONST_BIND",       "AST_MUT_BIND",         "AST_SYM_DECL",
    "AST_I32_DECL",         "AST_FUN_DECL",         "AST_EFF_DECL",
    "AST_HND_DECL",         "AST_HND_EFF_DECL",     "AST_TYPE_I32",
    "AST_OP_BIN_ASSIGN",    "AST_OP_BIN_ADD",       "AST_OP_BIN_SUB",
    "AST_OP_BIN_MUL",       "AST_OP_BIN_DIV",       "AST_OP_BIN_GT",
    "AST_OP_BIN_LT",        "AST_OP_BIN_GE",        "AST_OP_BIN_LE",
    "AST_OP_BIN_NE",        "AST_OP_BIN_EQ",        "AST_OP_MEMBER_ACCESS",
    "AST_OP_UNA_SUB",       "AST_OP_UNA_ADD",       "AST_FUN_CALL",
    "AST_EFF_CALL",
};

void ast_manager_init(AST_Manager *m) {
  m->size = 1;
  m->root = (AST_Bucket *)malloc(sizeof(AST_Bucket));
	
	m->root->id = 0;
	m->root->next = 0;
	m->root->prev = 0;
	
  m->tail = m->root;
}

void ast_manager_free(AST_Manager *m) {
  while (m->root) {
    AST_Bucket *tmp = m->root;
    m->root = m->root->next;
    free(tmp);
  }
}

void ast_init(AST_Bucket *b, ASTNode *a, Token tok, AST_Kind kind, AST_Id id,
              AST_Id ty, AST_Id l, AST_Id r) {
  a->left = l;
  a->right = r;
  a->type = ty;
  a->id = id;
  a->kind = kind;
  a->tok = tok;
  a->bucket = b;
}

AST_Id ast_manager_reserve(AST_Manager *m, Token tok, AST_Kind kind, AST_Id ty,
                           AST_Id l, AST_Id r) {
  AST_Id id = m->size;

  if (id % AST_BUCKET_SIZE == 0) {
    m->tail->next = (AST_Bucket *)malloc(sizeof(AST_Bucket));
		m->tail->next->prev = m->tail;
		m->tail->next->id = m->tail->id + 1;
    m->tail = m->tail->next;
  }
	
  ast_init(m->tail, &m->tail->data[id % AST_BUCKET_SIZE], tok, kind, id, ty, l, r);
	
  m->size = m->size + 1;

  return id;
}

ASTNode *ast_manager_get(AST_Manager *m, AST_Id id) {
  AST_Bucket *b = m->root;

  for (u64 i = 0; i < id / AST_BUCKET_SIZE; i++) {
    b = b->next;
  }

  return &b->data[id % AST_BUCKET_SIZE];
}


ASTNode *ast_manager_get_relative(AST_Manager *m, ASTNode *root, AST_Id child_id) {
	assert(root);
	
	AST_Bucket *b = root->bucket;

	u32 w = round_down(root->id, AST_BUCKET_SIZE);
	u32 s = round_down(child_id, AST_BUCKET_SIZE);
	
	while(s > w) {
		w += AST_BUCKET_SIZE;
		b = b->next;
	}

	while(w > s) {
		w -= AST_BUCKET_SIZE;
		b = b->prev;
	}
	
	ASTNode* x = &b->data[child_id % AST_BUCKET_SIZE];

	assert(x->id == child_id);
	
	return x;
}

ASTNode *ast_manager_alloc(AST_Manager *m, Token tok, AST_Kind kind, AST_Id ty,
                       AST_Id l, AST_Id r) {
  return ast_manager_get(m, ast_manager_reserve(m, tok, kind, ty, l, r));
}

ASTNode *ast_symbol(AST_Manager *m, Token tok) {
  return ast_manager_alloc(m, tok, AST_SYM_DECL, TYPE_UNDEF, 0, 0);
}

ASTNode *ast_i32_lit(AST_Manager *m, Token tok) {
  return ast_manager_alloc(m, tok, AST_I32_DECL, TYPE_UNDEF, 0, 0);
}

ASTNode *ast_bin_add(AST_Manager *m, Token tok, AST_Id l, AST_Id r) {
  return ast_manager_alloc(m, tok, AST_OP_BIN_ADD, TYPE_UNDEF, l, r);
}

ASTNode *ast_bin_sub(AST_Manager *m, Token tok, AST_Id l, AST_Id r) {
  return ast_manager_alloc(m, tok, AST_OP_BIN_SUB, TYPE_UNDEF, l, r);
}

ASTNode *ast_bin_mul(AST_Manager *m, Token tok, AST_Id l, AST_Id r) {
  return ast_manager_alloc(m, tok, AST_OP_BIN_MUL, TYPE_UNDEF, l, r);
}

ASTNode *ast_bin_div(AST_Manager *m, Token tok, AST_Id l, AST_Id r) {
  return ast_manager_alloc(m, tok, AST_OP_BIN_DIV, TYPE_UNDEF, l, r);
}

ASTNode *ast_una_add(AST_Manager *m, Token tok, AST_Id l, AST_Id r) {
  return ast_manager_alloc(m, tok, AST_OP_UNA_ADD, TYPE_UNDEF, l, r);
}

ASTNode *ast_una_sub(AST_Manager *m, Token tok, AST_Id l, AST_Id r) {
  return ast_manager_alloc(m, tok, AST_OP_UNA_SUB, TYPE_UNDEF, l, r);
}

ASTNode *ast_bin_gt(AST_Manager *m, Token tok, AST_Id l, AST_Id r) {
  return ast_manager_alloc(m, tok, AST_OP_BIN_GT, TYPE_UNDEF, l, r);
}

ASTNode *ast_bin_lt(AST_Manager *m, Token tok, AST_Id l, AST_Id r) {
  return ast_manager_alloc(m, tok, AST_OP_BIN_LT, TYPE_UNDEF, l, r);
}

ASTNode *ast_bin_ge(AST_Manager *m, Token tok, AST_Id l, AST_Id r) {
  return ast_manager_alloc(m, tok, AST_OP_BIN_GE, TYPE_UNDEF, l, r);
}

ASTNode *ast_bin_le(AST_Manager *m, Token tok, AST_Id l, AST_Id r) {
  return ast_manager_alloc(m, tok, AST_OP_BIN_LE, TYPE_UNDEF, l, r);
}
ASTNode *ast_bin_ne(AST_Manager *m, Token tok, AST_Id l, AST_Id r) {
  return ast_manager_alloc(m, tok, AST_OP_BIN_NE, TYPE_UNDEF, l, r);
}

ASTNode *ast_bin_eq(AST_Manager *m, Token tok, AST_Id l, AST_Id r) {
  return ast_manager_alloc(m, tok, AST_OP_BIN_EQ, TYPE_UNDEF, l, r);
}

ASTNode *ast_member_access(AST_Manager *m, Token tok, AST_Id l, AST_Id r) {
  return ast_manager_alloc(m, tok, AST_OP_MEMBER_ACCESS, TYPE_UNDEF, l, r);
}

ASTNode *ast_assignment(AST_Manager *m, Token tok, AST_Id l, AST_Id r) {
  return ast_manager_alloc(m, tok, AST_OP_BIN_ASSIGN, TYPE_UNDEF, l, r);
}

ASTNode *ast_call(AST_Manager *m, Token tok, AST_Id l, AST_Id r, bool effectfull) {
  if (effectfull) {
    return ast_manager_alloc(m, tok, AST_EFF_CALL, TYPE_UNDEF, l, r);
  }

  return ast_manager_alloc(m, tok, AST_FUN_CALL, TYPE_UNDEF, l, r);
}

ASTNode *ast_call_arg_list(AST_Manager *m, Token tok) {
  return ast_manager_alloc(m, tok, AST_CALL_ARGS_LIST, TYPE_UNDEF, 0, 0);
}

ASTNode *ast_decl_arg_list(AST_Manager *m, Token tok) {
  return ast_manager_alloc(m, tok, AST_DECL_ARGS_LIST, TYPE_UNDEF, 0, 0);
}

ASTNode *ast_statement_list(AST_Manager *m, Token tok) {
  return ast_manager_alloc(m, tok, AST_STATEMENT_LIST, TYPE_UNDEF, 0, 0);
}

ASTNode *ast_decl_list(AST_Manager *m, Token tok) {
  return ast_manager_alloc(m, tok, AST_STATEMENT_LIST, TYPE_UNDEF, 0, 0);
}

ASTNode *ast_handler_eff_list(AST_Manager *m, Token tok) {
  return ast_manager_alloc(m, tok, AST_HND_EFF_LIST, TYPE_UNDEF, 0, 0);
}

ASTNode *ast_const_bind(AST_Manager *m, Token tok, AST_Id ty, AST_Id l, AST_Id r) {
  return ast_manager_alloc(m, tok, AST_CONST_BIND, ty, l, r);
}

ASTNode *ast_mut_bind(AST_Manager *m, Token tok, AST_Id ty, AST_Id l, AST_Id r) {
  return ast_manager_alloc(m, tok, AST_MUT_BIND, ty, l, r);
}

ASTNode *ast_fun_decl(AST_Manager *m, Token tok, AST_Id ty, AST_Id l, AST_Id r) {
  return ast_manager_alloc(m, tok, AST_FUN_DECL, ty, l, r);
}

ASTNode *ast_eff_decl(AST_Manager *m, Token tok, AST_Id ty, AST_Id l, AST_Id r) {
  return ast_manager_alloc(m, tok, AST_EFF_DECL, ty, l, r);
}

ASTNode *ast_handler_eff_decl(AST_Manager *m, Token tok, AST_Id ty, AST_Id l,
                          AST_Id r) {
  return ast_manager_alloc(m, tok, AST_HND_EFF_DECL, ty, l, r);
}

ASTNode *ast_handler_decl(AST_Manager *m, Token tok, AST_Id l, AST_Id r) {
  return ast_manager_alloc(m, tok, AST_HND_DECL, TYPE_UNDEF, l, r);
}

const char *ast_kind_to_cstr(AST_Kind k) { return ast_kind_strs[k]; }

ASTNode *ast_ctrl_flow_if(AST_Manager *m, Token tok, AST_Id cond, AST_Id body,
                      AST_Id elif) {
  ASTNode *a = ast_manager_alloc(m, tok, AST_IF_DECL, TYPE_UNDEF, cond, body);
  return ast_manager_alloc(m, tok, AST_CTRL_FLOW_IF, TYPE_UNDEF, a->id, elif);
}

ASTNode *ast_ctrl_flow_ret(AST_Manager *m, Token tok, AST_Id expr) {
  return ast_manager_alloc(m, tok, AST_CTRL_FLOW_RETURN, TYPE_UNDEF, expr, 0);
}

ASTNode *ast_ctrl_flow_continue(AST_Manager *m, Token tok, AST_Id cont,
                            AST_Id expr) {
  return ast_manager_alloc(m, tok, AST_CTRL_FLOW_CONTINUE, TYPE_UNDEF, cont,
                           expr);
}

ASTNode *ast_hnd_eff_decl_args(AST_Manager *m, Token tok, AST_Id args,
                           AST_Id cont) {
  return ast_manager_alloc(m, tok, AST_HND_EFF_ARGS, TYPE_UNDEF, args, cont);
}

ASTNode *ast_with_hnd(AST_Manager *m, Token tok, AST_Id call, AST_Id hnd) {
  return ast_manager_alloc(m, tok, AST_WITH_HND, TYPE_UNDEF, call, hnd);
}
