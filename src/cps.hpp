#pragma once

/*
        Continuation Passing Style Analysis
*/

#include "ast.hpp"
#include "context.hpp"
#include "error.hpp"
#include "lexer.hpp"
#include "parser.hpp"
#include <assert.h>


struct CPS_Analysis {
	Scope* scope;
};

void init_cps_analysis(CPS_Analysis* anaylisis) {
	anaylisis->scope = scope_create(NULL);
}

AST_Node *body_split(CPS_Analysis *analysis, Parser *p, AST_Node *body) {
  assert(body->kind == AST_PROGRAM_POINT);

  AST_Manager *m = &p->ast_man;

  Token token = lexer_undef_token();

  AST_Node *program_point = ast_program_point_get_decl(m, body);

  assert(program_point->kind == AST_BIND_CONSTANT ||
         program_point->kind == AST_BIND_VARIABLE ||
         program_point->kind == AST_OP_BIN_ASSIGN ||
         program_point->kind == AST_FUNCTION_CALL);

  AST_Node *continuation = ast_manager_get_relative(m, body, body->right);


  if (program_point->kind == AST_BIND_CONSTANT ||
      program_point->kind == AST_BIND_VARIABLE) {
		scope_push(analysis->scope, program_point);

		AST_Node *expr = ast_bind_get_expr(m, program_point);

    if (expr->kind == AST_FUNCTION_CALL) {

		 AST_Node* func = ast_manager_get_relative(m, expr, expr->left);
		 AST_Node* sign = scope_find(analysis->scope, p, func);
 
		if(ast_is_null_node(sign)) {
			parser_error(p, func->tok, "Could not find definition");
		}

		// get the type of the function, an arrow type
		AST_Node* arrw = ast_type_bind_get_type(m, sign);
		// get the return type of the function
		AST_Node* type = ast_manager_get_relative(m, arrw, arrw->right);


    AST_Node *bind = ast_bind_get_type_bind(m, program_point);

		bind->right = type->id;
			
		AST_Node *args = ast_decl_args(m, token, bind, ast_node_null(m));

      // TODO: not use any as type here and try to infer the type
      AST_Node *csign = ast_function_signature(m, token, args, ast_type_any(m, token));
      AST_Node *cdecl = ast_function_literal(m, token, csign, continuation);

      ast_call_push_argument(m, token, expr, cdecl);

			body->right = ast_node_null(m)->id;
			body->left = expr->id;
    }
		
    return continuation;
  }

  if (program_point->kind == AST_OP_BIN_ASSIGN) {
    AST_Node *symb =
        ast_manager_get_relative(m, program_point, program_point->left);
    AST_Node *expr =
        ast_manager_get_relative(m, program_point, program_point->right);

    if (expr->kind == AST_FUNCTION_CALL) {

		 AST_Node* func = ast_manager_get_relative(m, expr, expr->left);
		 AST_Node* sign = scope_find(analysis->scope, p, func);
 
		if(ast_is_null_node(sign)) {
			parser_error(p, func->tok, "Could not find definition");
		}
		// get the type of the function, an arrow type
		AST_Node* arrw = ast_type_bind_get_type(m, sign);
		// get the return type of the function
		AST_Node* type = ast_manager_get_relative(m, arrw, arrw->right);


      AST_Node *bind = ast_constant_bind(m, token, symb, type);
      AST_Node *args = ast_decl_args(m, token, bind, ast_node_null(m));

      AST_Node *csign = ast_function_signature(m, token, args, ast_type_any(m, token));
      AST_Node *cdecl = ast_function_literal(m, token, csign, continuation);

      ast_call_push_argument(m, token, expr, cdecl);
			
			body->right = ast_node_null(m)->id;
			body->left = expr->id;
    }

    return continuation;
  }

  if (program_point->kind == AST_FUNCTION_CALL) {
    // TODO: use type of the symbol, need to find the definition on the context

		// need to get the return type of the function
		AST_Node* symb = ast_manager_get_relative(m, program_point, program_point->left);

		AST_Node* sign = scope_find(analysis->scope, p, symb);

		if(ast_is_null_node(sign)) {
			parser_error(p, symb->tok, "Could not find definition");
		}
		
		// get the type of the function, an arrow type
		AST_Node* arrw = ast_type_bind_get_type(m, sign);
		// get the return type of the function
		AST_Node* type = ast_manager_get_relative(m, arrw, arrw->right);

		AST_Node* carg = ast_type_bind(m, token, ast_temp_node(m), type);
		AST_Node* bind = ast_decl_args(m, token, carg, ast_node_null(m));

    // TODO: not use any as type here and try to infer the type
    AST_Node *csign = ast_function_signature(m, token, bind, ast_type_any(m, token));
    AST_Node *cdecl = ast_function_literal(m, token, csign, continuation);

    ast_call_push_argument(m, token, program_point, cdecl);
					
		body->right = ast_node_null(m)->id;

		return continuation;
  }

  return continuation;
}

AST_Node *body_to_cps(CPS_Analysis *analysis, Parser *p, AST_Node *body,
                      AST_Node *cont_symb) {
  assert(body->kind == AST_PROGRAM_POINT);

  AST_Manager *m = &p->ast_man;

  Token token = lexer_undef_token();

  AST_Node *parent = ast_node_null(m);

  while (!ast_is_null_node(body)) {
    AST_Node *program_point = ast_program_point_get_decl(m, body);

    if (program_point->kind == AST_CTRL_FLOW_RETURN) {
      AST_Node *expr = ast_ctrl_flow_return_get_expression(m, program_point);
      AST_Node *call = ast_call(m, token, cont_symb, expr);

      body->left = call->id;
			body->right = ast_node_null(m)->id;
 
			return ast_node_null(m);
    }

    if (program_point->kind == AST_BIND_CONSTANT ||
        program_point->kind == AST_BIND_VARIABLE ||
        program_point->kind == AST_OP_BIN_ASSIGN ||
        program_point->kind == AST_FUNCTION_CALL) {
      return body_to_cps(analysis, p, body_split(analysis, p, body), cont_symb);
    }

    parent = body;
    body = ast_program_point_get_tail(m, body);
  }

  AST_Node *args = ast_call_arg_list(m, token);
  AST_Node *call = ast_call(m, token, cont_symb, args);

  AST_Node *continuation = ast_program_point(m, token);

  continuation->left = call->left;

  parent->right = continuation->id;

  return ast_node_null(m);
}

void ast_to_cps(CPS_Analysis *analysis, Parser *p, AST_Node *node) {
	if(ast_is_null_node(node)) return;
	
  AST_Manager *m = &p->ast_man;
	
  Token token = lexer_undef_token();

  if (node->kind == AST_FUNCTION_LITERAL) {
    AST_Node *signature = ast_function_literal_get_signature(m, node);

    AST_Node *cont_symb = ast_temp_node(m);
    AST_Node *cont_type = ast_type_any(m, token);
    AST_Node *cont_bind = ast_type_bind(m, token, cont_symb, cont_type);

    ast_function_literal_push_argument(m, token, node, cont_bind);

    AST_Node *body = ast_function_literal_get_body(m, node);

		body_to_cps(analysis, p, body, cont_symb);

		return;
  }

	if(node->kind == AST_BIND_CONSTANT || node->kind == AST_BIND_VARIABLE) {
		// push current definition to the scope
		scope_push(analysis->scope, ast_manager_get_relative(m, node, node->left));
		ast_to_cps(analysis, p, ast_manager_get_relative(m, node, node->right));
		return;
	}
	
	if(node->kind == AST_PROGRAM_POINT) {
		ast_to_cps(analysis, p, ast_manager_get_relative(m, node, node->left));
		ast_to_cps(analysis, p, ast_manager_get_relative(m, node, node->right));

		return;
	}

	if(node->kind == AST_HANDLER_LITERAL) {
		ast_to_cps(analysis, p, ast_manager_get_relative(m, node, node->right));
	}
	
	if(node->kind == AST_HANDLER_LITERAL) {
		ast_to_cps(analysis, p, ast_manager_get_relative(m, node, node->right));
	}
}
