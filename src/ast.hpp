#pragma once

#include "token.hpp"

enum ast_kind {
  // Compounds
  AST_STATEMENT_LIST = 0,
  AST_CALL_ARGS_LIST,
  AST_DECL_ARGS_LIST,
  AST_HND_EFF_LIST,
  AST_DECL_LIST,
	AST_IF_DECL,
	// left element is the effect arguments, right is continuation info
	AST_HND_EFF_ARGS,
	AST_WITH_HND,
	
	// Flow
	AST_CTRL_FLOW_IF,
	AST_CTRL_FLOW_ELSE, // TODO: remove
	AST_CTRL_FLOW_RETURN,
	AST_CTRL_FLOW_CONTINUE,

  // Bindings
  AST_CONST_BIND, // constant
  AST_MUT_BIND,   // mutable

  // Declarations
  AST_SYM_DECL,
  AST_I32_DECL,
  AST_FUN_DECL,
  AST_EFF_DECL,
  AST_HND_DECL,
  AST_HND_EFF_DECL,

  // Types
  AST_TYPE_I32,

  // Binary operators
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

  // Unary operators
  AST_OP_UNA_SUB,
  AST_OP_UNA_ADD,

  // Applications
  AST_FUN_CALL,
  AST_EFF_CALL,
};

struct ast {
  ast_kind kind;
  token tok;
  ast *type;
  ast *left;
  ast *right;
};

#define AST_BUCKET_SIZE 128

struct AST_Bucket {
	ast data[AST_BUCKET_SIZE];
	AST_Bucket* next;
};

struct AST_Manager {
	u64 size;
	AST_Bucket* root;
	AST_Bucket* tail;
};

void ast_destroy(ast *);

ast *ast_symbol(token tok);
ast *ast_i32_lit(token tok);

ast *ast_const_bind(token tok, ast *ty, ast *l, ast *r);
ast *ast_mut_bind(token tok, ast *ty, ast *l, ast *r);
ast *ast_bin_gt(token tok, ast *l, ast *r);
ast *ast_bin_lt(token tok, ast *l, ast *r);
ast *ast_bin_ge(token tok, ast *l, ast *r);
ast *ast_bin_le(token tok, ast *l, ast *r);
ast *ast_bin_ne(token tok, ast *l, ast *r);
ast *ast_bin_eq(token tok, ast *l, ast *r);
ast *ast_bin_add(token tok, ast *l, ast *r);
ast *ast_bin_sub(token tok, ast *l, ast *r);
ast *ast_bin_mul(token tok, ast *l, ast *r);
ast *ast_bin_div(token tok, ast *l, ast *r);
ast *ast_una_add(token tok, ast *l, ast *r);
ast *ast_una_sub(token tok, ast *l, ast *r);
ast *ast_assignment(token tok, ast *l, ast *r);
ast *ast_member_access(token tok, ast *l, ast *r);
ast *ast_call(token tok, ast *sym, ast *params, bool effectfull);

ast *ast_fun_decl(token tok, ast *ty, ast *l, ast *r);
ast *ast_eff_decl(token tok, ast *ty, ast *l, ast *r);
ast *ast_handler_eff_decl(token tok, ast *ty, ast *l, ast *r);
ast *ast_handler_decl(token tok, ast *l, ast *r);

ast *ast_decl_list(token tok);
ast *ast_statement_list(token tok);
ast *ast_decl_arg_list(token tok);
ast *ast_call_arg_list(token tok);
ast *ast_handler_eff_list(token tok);
ast* ast_ctrl_flow_if(token tok, ast* cond, ast* body, ast* elif);
ast* ast_ctrl_flow_ret(token tok, ast* expr);
ast* ast_ctrl_flow_continue(token tok, ast* cont, ast* expr);
ast *ast_bind_get_sym(ast* a);
ast* ast_hnd_eff_decl_args(token tok, ast* args, ast* cont);
ast* ast_with_hnd(token tok, ast* call, ast* hnd);

const char *ast_kind_to_cstr(ast_kind k);
