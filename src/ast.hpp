#pragma once

#include "lexer.hpp"

enum AST_Kind {
  // Compounds
	AST_NULL_NODE = 0,
  AST_STATEMENT_LIST,
  AST_CALL_ARGS_LIST,
  AST_DECL_ARGS_LIST,
  AST_HND_EFF_LIST,
  AST_DECL_LIST,
	AST_TYPE_TUPLE_MEM_LIST,
  AST_IF_DECL,
	AST_TUPLE_LIST,
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
	AST_TYPE_BIND, // unnitialized value
	
  // Declarations
  AST_SYM_DECL,
  AST_I32_DECL,
  AST_FUN_DECL,
  AST_CLOS_DECL,
  AST_EFF_DECL,
  AST_HND_DECL,
  AST_HND_EFF_DECL,

  // Types
  AST_TYPE_I32,
  AST_TYPE_UNIT,
	AST_TYPE_ARROW,
	AST_TYPE_TUPLE,
	AST_TYPE_TYPE,

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

typedef u32 AST_Id;

typedef struct AST_Bucket AST_Bucket;

struct AST_Node {
  // This AST Node Id
  AST_Id id;
  // The token that resulted on this Node
  Token tok;
  AST_Id left;
  AST_Id right;
  AST_Kind kind;
  AST_Bucket *bucket;
};

#define AST_BUCKET_SIZE 128

struct AST_Bucket {
  u64 id;
  AST_Node data[AST_BUCKET_SIZE];
  AST_Bucket *prev;
  AST_Bucket *next;
};

struct AST_Manager {
  u64 size;
  AST_Bucket *root;
  AST_Bucket *tail;
};

void ast_manager_init(AST_Manager *m);
void ast_manager_free(AST_Manager *m);

AST_Node *ast_manager_get(AST_Manager *m, AST_Id id);
AST_Node *ast_manager_get_relative(AST_Manager *m, AST_Node *from, AST_Id id);

AST_Node *ast_symbol(AST_Manager *m, Token tok);
AST_Node *ast_i32_lit(AST_Manager *m, Token tok);

AST_Node *ast_const_bind(AST_Manager *m, Token tok, AST_Node* l,
                        AST_Node* r);
AST_Node *ast_mut_bind(AST_Manager *m, Token tok,  AST_Node* l, AST_Node* r);
AST_Node *ast_bin_gt(AST_Manager *m, Token tok, AST_Node* l, AST_Node* r);
AST_Node *ast_bin_lt(AST_Manager *m, Token tok, AST_Node* l, AST_Node* r);
AST_Node *ast_bin_ge(AST_Manager *m, Token tok, AST_Node* l, AST_Node* r);
AST_Node *ast_bin_le(AST_Manager *m, Token tok, AST_Node* l, AST_Node* r);
AST_Node *ast_bin_ne(AST_Manager *m, Token tok, AST_Node* l, AST_Node* r);
AST_Node *ast_bin_eq(AST_Manager *m, Token tok, AST_Node* l, AST_Node* r);
AST_Node *ast_bin_add(AST_Manager *m, Token tok, AST_Node* l, AST_Node* r);
AST_Node *ast_bin_sub(AST_Manager *m, Token tok, AST_Node* l, AST_Node* r);
AST_Node *ast_bin_mul(AST_Manager *m, Token tok, AST_Node* l, AST_Node* r);
AST_Node *ast_bin_div(AST_Manager *m, Token tok, AST_Node* l, AST_Node* r);
AST_Node *ast_una_add(AST_Manager *m, Token tok, AST_Node* l, AST_Node* r);
AST_Node *ast_una_sub(AST_Manager *m, Token tok, AST_Node* l, AST_Node* r);
AST_Node *ast_assignment(AST_Manager *m, Token tok, AST_Node* l, AST_Node* r);
AST_Node *ast_member_access(AST_Manager *m, Token tok, AST_Node* l, AST_Node* r);
AST_Node *ast_call(AST_Manager *m, Token tok, AST_Node* sym, AST_Node* params,
                  bool effectfull);

AST_Node *ast_fun_decl(AST_Manager *m, Token tok, AST_Node* l, AST_Node* r);
AST_Node *ast_eff_decl(AST_Manager *m, Token tok, AST_Node* l, AST_Node* r);
AST_Node *ast_handler_eff_decl(AST_Manager *m, Token tok,  AST_Node* l,
                              AST_Node* r);
AST_Node *ast_handler_decl(AST_Manager *m, Token tok, AST_Node* l, AST_Node* r);

AST_Node *ast_decl_list(AST_Manager *m, Token tok);
AST_Node *ast_statement_list(AST_Manager *m, Token tok);
AST_Node *ast_decl_arg_list(AST_Manager *m, Token tok);
AST_Node *ast_call_arg_list(AST_Manager *m, Token tok);
AST_Node *ast_handler_eff_list(AST_Manager *m, Token tok);
AST_Node *ast_ctrl_flow_if(AST_Manager *m, Token tok, AST_Node* cond, AST_Node* body,
                          AST_Node* elif);
AST_Node *ast_ctrl_flow_ret(AST_Manager *m, Token tok, AST_Node* expr);
AST_Node *ast_ctrl_flow_continue(AST_Manager *m, Token tok, AST_Node* cont,
                                AST_Node* expr);
AST_Node *ast_hnd_eff_decl_args(AST_Manager *m, Token tok, AST_Node* args,
                               AST_Node* cont);
AST_Node *ast_with_hnd(AST_Manager *m, Token tok, AST_Node* call, AST_Node* hnd);

const char *ast_kind_to_cstr(AST_Kind k);

AST_Node* ast_type_type(AST_Manager* m, Token tok);

AST_Node* ast_type_i32(AST_Manager* m, Token tok);
AST_Node* ast_type_unit(AST_Manager* m, Token tok);

AST_Node* ast_type_arrow(AST_Manager* m, Token tok, AST_Node* l, AST_Node* r);

AST_Node* ast_type_tuple_arg(AST_Manager* m, Token tok, AST_Node* mem, AST_Node* tail);

AST_Node* ast_type_tuple(AST_Manager* m, Token tok, AST_Node* members);

AST_Node* ast_node_null(AST_Manager* m);

AST_Node* ast_tuple_list(AST_Manager* m, Token tok, AST_Node* l, AST_Node* tail);
AST_Node *ast_type_bind(AST_Manager *m, Token tok, AST_Node* sym, AST_Node* type); 
