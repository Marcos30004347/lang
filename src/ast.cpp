#include "ast.hpp"
#include "stdlib.h"
#include "token.hpp"

#include <cassert>

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

void ast_manager_init(AST_Manager* m) {
	m->size = 0;
	m->root = 0;
	m->tail = 0;
}

u64 ast_manager_reserve(AST_Manager* m) {

}

ast* ast_manager_get(AST_Manager* m, u64 id) {
	AST_Bucket* b = m->root;
	for(u64 i = 0; i < id/AST_BUCKET_SIZE; i++) {
		b = b->next;
	}
	
	return b->data[id % AST_BUCKET_SIZE];
}

ast* ast_manager_alloc(AST_Manager* m) {
	return ast_manager_get(m, ast_manager_reserve(m));
}

void ast_destroy(ast *root) {
  if (root->left)
    ast_destroy(root->left);
  if (root->right)
    ast_destroy(root->right);
  free(root);
}

ast *ast_alloc(ast_kind kind, token tok, ast *type, ast *l, ast *r) {
  ast *a = (ast *)malloc(sizeof(ast));
  a->kind = kind;
  a->left = l;
  a->right = r;
  a->type = type;
  a->tok = tok;
  return a;
}

ast *ast_symbol(token tok) {
  return ast_alloc(AST_SYM_DECL, tok, TYPE_UNDEF, 0, 0);
}

ast *ast_i32_lit(token tok) {
  return ast_alloc(AST_I32_DECL, tok, TYPE_UNDEF, 0, 0);
}

ast *ast_bin_add(token tok, ast *l, ast *r) {
  return ast_alloc(AST_OP_BIN_ADD, tok, TYPE_UNDEF, l, r);
}

ast *ast_bin_sub(token tok, ast *l, ast *r) {
  return ast_alloc(AST_OP_BIN_SUB, tok, TYPE_UNDEF, l, r);
}

ast *ast_bin_mul(token tok, ast *l, ast *r) {
  return ast_alloc(AST_OP_BIN_MUL, tok, TYPE_UNDEF, l, r);
}

ast *ast_bin_div(token tok, ast *l, ast *r) {
  return ast_alloc(AST_OP_BIN_DIV, tok, TYPE_UNDEF, l, r);
}

ast *ast_una_add(token tok, ast *l, ast *r) {
  return ast_alloc(AST_OP_UNA_ADD, tok, TYPE_UNDEF, l, r);
}

ast *ast_una_sub(token tok, ast *l, ast *r) {
  return ast_alloc(AST_OP_UNA_SUB, tok, TYPE_UNDEF, l, r);
}

ast *ast_bin_gt(token tok, ast *l, ast *r) {
  return ast_alloc(AST_OP_BIN_GT, tok, TYPE_UNDEF, l, r);
}
ast *ast_bin_lt(token tok, ast *l, ast *r) {
  return ast_alloc(AST_OP_BIN_LT, tok, TYPE_UNDEF, l, r);
}
ast *ast_bin_ge(token tok, ast *l, ast *r) {
  return ast_alloc(AST_OP_BIN_GE, tok, TYPE_UNDEF, l, r);
}
ast *ast_bin_le(token tok, ast *l, ast *r) {
  return ast_alloc(AST_OP_BIN_LE, tok, TYPE_UNDEF, l, r);
}
ast *ast_bin_ne(token tok, ast *l, ast *r) {
  return ast_alloc(AST_OP_BIN_NE, tok, TYPE_UNDEF, l, r);
}

ast *ast_bin_eq(token tok, ast *l, ast *r) {
  return ast_alloc(AST_OP_BIN_EQ, tok, TYPE_UNDEF, l, r);
}

ast *ast_member_access(token tok, ast *l, ast *r) {
  return ast_alloc(AST_OP_MEMBER_ACCESS, tok, TYPE_UNDEF, l, r);
}

ast *ast_assignment(token tok, ast *l, ast *r) {
  return ast_alloc(AST_OP_BIN_ASSIGN, tok, TYPE_UNDEF, l, r);
}

ast *ast_call(token tok, ast *l, ast *r, bool effectfull) {
	if(effectfull) {
		return ast_alloc(AST_EFF_CALL, tok, TYPE_UNDEF, l, r);
	}
	
  return ast_alloc(AST_FUN_CALL, tok, TYPE_UNDEF, l, r);
}

ast *ast_call_arg_list(token tok) {
  return ast_alloc(AST_CALL_ARGS_LIST, tok, TYPE_UNDEF, 0, 0);
}

ast *ast_decl_arg_list(token tok) {
  return ast_alloc(AST_DECL_ARGS_LIST, tok, TYPE_UNDEF, 0, 0);
}

ast *ast_statement_list(token tok) {
  return ast_alloc(AST_STATEMENT_LIST, tok, TYPE_UNDEF, 0, 0);
}

ast *ast_decl_list(token tok) {
  return ast_alloc(AST_STATEMENT_LIST, tok, TYPE_UNDEF, 0, 0);
}

ast *ast_handler_eff_list(token tok) {
  return ast_alloc(AST_HND_EFF_LIST, tok, TYPE_UNDEF, 0, 0);
}

ast *ast_const_bind(token tok, ast *ty, ast *l, ast *r) {
  return ast_alloc(AST_CONST_BIND, tok, ty, l, r);
}

ast *ast_mut_bind(token tok, ast *ty, ast *l, ast *r) {
  return ast_alloc(AST_MUT_BIND, tok, ty, l, r);
}

ast *ast_fun_decl(token tok, ast *ty, ast *l, ast *r) {
  return ast_alloc(AST_FUN_DECL, tok, ty, l, r);
}

ast *ast_eff_decl(token tok, ast *ty, ast *l, ast *r) {
  return ast_alloc(AST_EFF_DECL, tok, ty, l, r);
}

ast *ast_handler_eff_decl(token tok, ast *ty, ast *l, ast *r) {
  return ast_alloc(AST_HND_EFF_DECL, tok, ty, l, r);
}

ast *ast_handler_decl(token tok, ast *l, ast *r) {
  return ast_alloc(AST_HND_DECL, tok, TYPE_UNDEF, l, r);
}

const char *ast_kind_to_cstr(ast_kind k) { return ast_kind_strs[k]; }

ast *ast_bind_get_sym(ast* a) {
	assert(a->kind == AST_MUT_BIND || a->kind == AST_CONST_BIND);

	return a->left;
}

ast* ast_ctrl_flow_if(token tok, ast* cond, ast* body, ast* elif) {
	ast* a = ast_alloc(AST_IF_DECL, tok, TYPE_UNDEF, cond, body);
	return ast_alloc(AST_CTRL_FLOW_IF, tok, TYPE_UNDEF, a, elif);
}

ast* ast_ctrl_flow_ret(token tok, ast* expr) {
	return ast_alloc(AST_CTRL_FLOW_RETURN, tok, TYPE_UNDEF, expr, 0);
}

ast* ast_ctrl_flow_continue(token tok, ast* cont, ast* expr) {
	return ast_alloc(AST_CTRL_FLOW_CONTINUE, tok, TYPE_UNDEF, cont, expr);
}

ast* ast_hnd_eff_decl_args(token tok, ast* args, ast* cont) {
	return ast_alloc(AST_HND_EFF_ARGS, tok, TYPE_UNDEF, args, cont);
}

ast* ast_with_hnd(token tok, ast* call, ast* hnd) {

	return ast_alloc(AST_WITH_HND, tok, TYPE_UNDEF, call, hnd);
}
