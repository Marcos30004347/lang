
#include "parser.hpp"
#include "ast.hpp"
#include "lexer.hpp"
#include "types.hpp"

#include "error.hpp"
#include "stdio.h"
#include "string.h"
#include "stdlib.h"

// void debug_print_token(Parser* p, Token t) {
// 	i8 buf[256];
// 	token_get_id(&p->lex, t, buf);
// 	printf("%s\n", buf);
// }

AST_Node *parser_parse_expr(Parser *p);
AST_Node *parser_parse_statements(Parser *p);
AST_Node *parser_parse_decl(Parser *p);
AST_Node *parser_parse_arrow(Parser *p);

void parser_init(Parser *p, u64 id, i8 *buffer, u64 size) {
  lexer_init(&p->lex, id, buffer, size);
  ast_manager_init(&p->ast_man);
}

void parser_destroy(Parser *p) { lexer_destroy(&p->lex); }

Token parser_curr_tok(Parser *p) { return p->lex.curr; }

Token parser_prev_tok(Parser *p) { return p->lex.prev; }

Token parser_read_token(Parser *p, Token_Type kind) {
  if (p->lex.curr.type != kind) {
    char tmp[256];
    sprintf(tmp, "expecting Token %i", kind);
    parser_error(p, parser_curr_tok(p), tmp);
  }

  return lexer_read_token(&p->lex);
}

AST_Node *parser_parse_symbol(Parser *p) {
  Token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_ID);

  return ast_symbol(&p->ast_man, tok);
}

AST_Node *parse_call_args(Parser *p) {
  parser_read_token(p, TOKEN_OPEN_PARENTHESIS);

  AST_Node *root = ast_call_arg_list(&p->ast_man, parser_curr_tok(p));

  if (parser_curr_tok(p).type != TOKEN_CLOSE_PARENTHESIS) {

    AST_Node *tmp = root;

    tmp->left = parser_parse_expr(p)->id;

    while (parser_curr_tok(p).type == TOKEN_COMMA) {
      parser_read_token(p, TOKEN_COMMA);

      AST_Node *next = ast_call_arg_list(&p->ast_man, parser_curr_tok(p));

      next->left = parser_parse_expr(p)->id;

      next->right = 0;

      tmp->right = next->id;

      tmp = next;
    }
  }

  parser_read_token(p, TOKEN_CLOSE_PARENTHESIS);
  return root;
}

AST_Node *parser_parse_lit(Parser *p) {
  Token tok = parser_curr_tok(p);

  if (tok.type == TOKEN_I32_LIT) {
    parser_read_token(p, TOKEN_I32_LIT);
    return ast_i32_lit(&p->ast_man, tok);
  }
	
	if (tok.type == TOKEN_UNIT) {
    parser_read_token(p, TOKEN_UNIT);
    return ast_type_unit(&p->ast_man, tok);
  }

	if (tok.type == TOKEN_I32) {
    parser_read_token(p, TOKEN_I32);
    return ast_type_i32(&p->ast_man, tok);
  }
	
  if (tok.type == TOKEN_TYPE) {
    parser_read_token(p, TOKEN_TYPE);
    return ast_type_type(&p->ast_man, tok);
  }

  parser_error(p, parser_curr_tok(p), "invalid literal declaration");

  return 0;
}

// PRIMARY → LITERAL | SYMBOL | '('EXPRESSION')'
AST_Node *parser_parse_primary(Parser *p) {
  AST_Node *expr = 0;

  if (parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) {
    parser_read_token(p, TOKEN_OPEN_PARENTHESIS);
    expr = parser_parse_expr(p);
    parser_read_token(p, TOKEN_CLOSE_PARENTHESIS);
    return expr;
  }

  if (parser_curr_tok(p).type == TOKEN_ID) {
    return parser_parse_symbol(p);
  }

  return parser_parse_lit(p);
}

// UNARY → ('!' | '-' | '+' | '++' | '--' | '~' )UNARY | UNARY('++' | '--') |
// CALL
AST_Node *parser_parse_postfix_suffix_unary(Parser *p) {
  // TODO
  return parser_parse_primary(p);
}

AST_Node *parser_parse_call_args(Parser *p) {
  bool effectfull = false;

  Token tok = parser_curr_tok(p);
  AST_Node *arg = parse_call_args(p);
  AST_Node *tail = 0;

  if (parser_curr_tok(p).type == TOKEN_EXCLAMATION ||
      parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) {

    if (parser_curr_tok(p).type == TOKEN_EXCLAMATION) {
      effectfull = true;
      parser_read_token(p, TOKEN_EXCLAMATION);
    }

    return ast_call(&p->ast_man, tok, arg, parser_parse_call_args(p),
                    effectfull);
  }

  return arg;
}

AST_Node *parser_parse_call_tail(Parser *p, AST_Node *head) {
  b8 effectfull = false;

  if (parser_curr_tok(p).type == TOKEN_EXCLAMATION) {
    effectfull = true;

    parser_read_token(p, TOKEN_EXCLAMATION);
  }

  Token tok = parser_curr_tok(p);

  AST_Node *args = parse_call_args(p);

  AST_Node *call = ast_call(&p->ast_man, tok, head, args, effectfull);

  if (parser_curr_tok(p).type == TOKEN_EXCLAMATION ||
      parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) {
    return parser_parse_call_tail(p, call);
  }

  return call;
}

AST_Node *parser_parse_call(Parser *p) {
  AST_Node *root = parser_parse_postfix_suffix_unary(p);

  b8 effectfull = false;

  if (parser_curr_tok(p).type == TOKEN_EXCLAMATION) {
    effectfull = true;

    parser_read_token(p, TOKEN_EXCLAMATION);
  }

  Token tok = parser_curr_tok(p);

  if (parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) {
    AST_Node *args = parse_call_args(p);

    AST_Node *call = ast_call(&p->ast_man, tok, root, args, effectfull);

    if (parser_curr_tok(p).type == TOKEN_EXCLAMATION ||
        parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) {
      return parser_parse_call_tail(p, call);
    }

    if (parser_curr_tok(p).type == TOKEN_KEYWORD_WITH) {
      Token tok = parser_curr_tok(p);

      parser_read_token(p, TOKEN_KEYWORD_WITH);

      AST_Node *hnd = parser_parse_decl(p);

      return ast_with_hnd(&p->ast_man, tok, call, hnd);
    }

    return call;
  }

  if (effectfull) {
    parser_error(p, tok, "effect call expect arguments");
  }

  return root;
}

AST_Node *parser_parse_array_access(Parser *p) {
  // TODO: here should be parser array access
  return parser_parse_call(p);
}

AST_Node *parser_parse_member_access(Parser *p) {
  AST_Node *root = parser_parse_array_access(p);

  if (parser_curr_tok(p).type == TOKEN_DOT) {
    return ast_member_access(&p->ast_man, parser_curr_tok(p), root,
                             parser_parse_member_access(p));
  }

  return root;
}

// UNARY → ('!' | '-' | '+' | '++' | '--' | '~' )UNARY | UNARY('++' | '--') |
// CALL
AST_Node *parser_parse_unary(Parser *p) {
  AST_Node *root = 0;

  if (parser_curr_tok(p).type == TOKEN_MINUS ||
      parser_curr_tok(p).type == TOKEN_PLUS) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, tok.type);

    if (tok.type == TOKEN_MINUS) {
      return ast_una_sub(&p->ast_man, tok, root, parser_parse_unary(p));
    }

    if (tok.type == TOKEN_PLUS) {
      return ast_una_add(&p->ast_man, tok, root, parser_parse_unary(p));
    }
  }

  return parser_parse_member_access(p);
}

// FACTOR → UNARY (('/' | '*' | '%' ) FACTOR)*
AST_Node *parser_parse_factor(Parser *p) {
  AST_Node *root = parser_parse_unary(p);

  if (parser_curr_tok(p).type == TOKEN_SLASH ||
      parser_curr_tok(p).type == TOKEN_ASTERISK) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, tok.type);

    if (tok.type == TOKEN_SLASH) {
      return ast_bin_div(&p->ast_man, tok, root, parser_parse_factor(p));
    }

    if (tok.type == TOKEN_ASTERISK) {
      return ast_bin_mul(&p->ast_man, tok, root, parser_parse_factor(p));
    }
  }

  return root;
}

// TERM → FACTOR (('+' | '-') TERM)*
AST_Node *parser_parse_term(Parser *p) {
  AST_Node *root = parser_parse_factor(p);
  if (parser_curr_tok(p).type == TOKEN_PLUS ||
      parser_curr_tok(p).type == TOKEN_MINUS) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, TOKEN_PLUS);

    if (tok.type == TOKEN_PLUS) {
      return ast_bin_add(&p->ast_man, tok, root, parser_parse_term(p));
    }

    if (tok.type == TOKEN_MINUS) {
      return ast_bin_sub(&p->ast_man, tok, root, parser_parse_term(p));
    }
  }

  return root;
}

// SHIFT → TERM (( '>>' | '<<'  ) COMPARISON)*
AST_Node *parser_parse_bitwise_shift(Parser *p) {
  // TODO: bitwise shift should be parsed here
  return parser_parse_term(p);
}

// COMPARISON → SHIFT (( '>' | '>=' | '<' | '<='  ) COMPARISON)*
AST_Node *parser_parse_comparison(Parser *p) {
  // TODO: comparison expressions should go here
  AST_Node *root = parser_parse_bitwise_shift(p);

  if (parser_curr_tok(p).type == TOKEN_GREATER) {
    Token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_GREATER);

    return ast_bin_gt(&p->ast_man, curr, root, parser_parse_comparison(p));
  }

  if (parser_curr_tok(p).type == TOKEN_SMALLER) {
    Token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_SMALLER);

    return ast_bin_lt(&p->ast_man, curr, root, parser_parse_comparison(p));
  }

  if (parser_curr_tok(p).type == TOKEN_GREATER_EQUAL) {
    Token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_GREATER_EQUAL);

    return ast_bin_ge(&p->ast_man, curr, root, parser_parse_comparison(p));
  }

  if (parser_curr_tok(p).type == TOKEN_SMALLER_EQUAL) {
    Token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_SMALLER_EQUAL);

    return ast_bin_le(&p->ast_man, curr, root, parser_parse_comparison(p));
  }

  return root;
}

// EQUALITY → COMPARISON (('!=' | '==') EQUALITY)*
AST_Node *parser_parse_equality(Parser *p) {
  // TODO: equality comparisons expressions should go here
  AST_Node *root = parser_parse_comparison(p);

  if (parser_curr_tok(p).type == TOKEN_EXCLAMATION_EQUAL) {
    Token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_EXCLAMATION_EQUAL);

    return ast_bin_ne(&p->ast_man, curr, root, parser_parse_equality(p));
  }

  if (parser_curr_tok(p).type == TOKEN_EQUAL_EQUAL) {
    Token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_EQUAL_EQUAL);

    return ast_bin_eq(&p->ast_man, curr, root, parser_parse_equality(p));
  }

  return root;
}

// BITWISE → BOOL (( '&' | '|' | '^'  ) BITWISE)*
AST_Node *parser_parse_bitwise(Parser *p) {
  // TODO: bitwise expressions should go here
  return parser_parse_equality(p);
}

// BOOL → TERM (( '&&' | '^^' | '||') BOOL)*
AST_Node *parser_parse_booleans(Parser *p) {
  // TODO: booleans expressions should go here
  return parser_parse_bitwise(p);
}

// AST_Node *parser_parse_type_bind(Parser *p) {
// 	printf("kkkk\n");

// 	debug_print_token(p, parser_curr_tok(p));

// 	AST_Node *root = parser_parse_booleans(p);

// 	printf("---kkkk\n");
// 	debug_print_token(p, parser_curr_tok(p));

// 	if (parser_curr_tok(p).type == TOKEN_COLON) {

// 		if (root->kind != AST_SYM_DECL) {
//       parser_error(p, root->tok, "type bind expects symbol on the left side");
//     }

//     Token tok = parser_curr_tok(p);

// 		parser_read_token(p, TOKEN_COLON);
		
//     if (parser_curr_tok(p).type == TOKEN_COLON ||
//         parser_curr_tok(p).type == TOKEN_EQUAL) {
//       return ast_type_bind(&p->ast_man, tok, root, ast_node_null(&p->ast_man));
//     }

//     AST_Node *type = parser_parse_arrow(p);

//     return ast_type_bind(&p->ast_man, tok, root, type);
//   }

//   return root;
// }


AST_Node *parser_parse_arrow(Parser *p) {
	AST_Node *root = parser_parse_booleans(p);

	AST_Node* tmp = root;

	AST_Node * head = NULL;
	
	if (parser_curr_tok(p).type == TOKEN_ARROW) {
    Token tok = parser_curr_tok(p);

		parser_read_token(p, TOKEN_ARROW);

		AST_Node *tail = parser_parse_arrow(p);

		tmp = ast_type_arrow(&p->ast_man, tok, tmp, tail);

		if(head == NULL) {
			head = tmp;
		}
	}
	
  return head ? head : root;
}



AST_Node *parser_parse_if_statement(Parser *p) {
  Token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_KEYWORD_IF);

  AST_Node *cond = parser_parse_expr(p);

  AST_Node *body = parser_parse_statements(p);

  AST_Node *cont = 0;

  if (parser_curr_tok(p).type == TOKEN_KEYWORD_ELSE) {
    parser_read_token(p, TOKEN_KEYWORD_ELSE);
    cont = parser_parse_if_statement(p);
  }

  return ast_ctrl_flow_if(&p->ast_man, tok, cond, body, cont);
}

AST_Node *parser_parse_ret_statement(Parser *p) {
  Token tok = parser_curr_tok(p);
  parser_read_token(p, TOKEN_KEYWORD_RETURN);
  AST_Node *ret = ast_ctrl_flow_ret(&p->ast_man, tok, parser_parse_expr(p));

  if (parser_prev_tok(p).type != TOKEN_CLOSE_CURLY_BRACE) {
    parser_read_token(p, TOKEN_SEMI_COLON);
  }

  return ret;
}

AST_Node *parser_parse_statement(Parser *p) {
  if (parser_curr_tok(p).type == TOKEN_KEYWORD_IF) {
    return parser_parse_if_statement(p);
  }

  if (parser_curr_tok(p).type == TOKEN_KEYWORD_RETURN) {
    return parser_parse_ret_statement(p);
  }

  AST_Node *expr = parser_parse_expr(p);

	if (parser_prev_tok(p).type != TOKEN_CLOSE_CURLY_BRACE) {
		parser_read_token(p, TOKEN_SEMI_COLON);
		
		while(parser_curr_tok(p).type == TOKEN_SEMI_COLON) {
			parser_read_token(p, TOKEN_SEMI_COLON);
		}
	}

  return expr;
}

AST_Node *parser_parse_statements(Parser *p) {
	
  parser_read_token(p, TOKEN_OPEN_CURLY_BRACE);

	AST_Node *stmt = ast_statement_list(&p->ast_man, parser_curr_tok(p));

  AST_Node *tmp = stmt;

  while (parser_curr_tok(p).type != TOKEN_CLOSE_CURLY_BRACE) {
    tmp->left = parser_parse_statement(p)->id;

    if (parser_curr_tok(p).type == TOKEN_CLOSE_CURLY_BRACE) {
      break;
    }

    AST_Node *r = ast_statement_list(&p->ast_man, parser_curr_tok(p));
    tmp->right = r->id;
    tmp = r;
  }

  parser_read_token(p, TOKEN_CLOSE_CURLY_BRACE);

  return stmt;
}

AST_Node *parser_parse_decl_args(Parser *p) {
	Token tok = parser_curr_tok(p);
	
	parser_read_token(p, TOKEN_OPEN_PARENTHESIS);

	AST_Node* args = ast_decl_arg_list(&p->ast_man, tok);

	AST_Node* tmp = args;
	
	while(parser_curr_tok(p).type != TOKEN_CLOSE_PARENTHESIS) {
		AST_Node* sym = parser_parse_symbol(p);

		Token tok = parser_curr_tok(p);
		parser_read_token(p, TOKEN_COLON);
		
		AST_Node* typ = parser_parse_arrow(p);
		
		AST_Node* arg =  ast_type_bind(&p->ast_man, tok, sym, typ);

		tmp->left = arg->id;

		if(parser_curr_tok(p).type != TOKEN_COMMA) {
			break;
		}
		
		AST_Node* tail = ast_decl_arg_list(&p->ast_man, parser_curr_tok(p));

		tmp->right = tail->id;
		tmp = tail;

		parser_read_token(p, TOKEN_COMMA);
	}

  parser_read_token(p, TOKEN_CLOSE_PARENTHESIS);

  return args;
}

AST_Node *parser_parse_eff_args(Parser *p) {
	Token tok = parser_curr_tok(p);
	
	parser_read_token(p, TOKEN_OPEN_PARENTHESIS);

	AST_Node* args = ast_decl_arg_list(&p->ast_man, tok);

	AST_Node* tmp = args;
	
	while(parser_curr_tok(p).type != TOKEN_CLOSE_PARENTHESIS && parser_curr_tok(p).type != TOKEN_SEMI_COLON) {
		AST_Node* sym = parser_parse_symbol(p);
		Token tok = parser_curr_tok(p);

		parser_read_token(p, TOKEN_COLON);
		
		AST_Node* typ = parser_parse_arrow(p);
		
		AST_Node* arg =  ast_type_bind(&p->ast_man, tok, sym, typ);

		tmp->left = arg->id;

		if (parser_curr_tok(p).type != TOKEN_COMMA || parser_curr_tok(p).type == TOKEN_SEMI_COLON) {
			break;
		}
		
		AST_Node* tail = ast_decl_arg_list(&p->ast_man, parser_curr_tok(p));

		tmp->right = tail->id;
		tmp = tail;

		parser_read_token(p, TOKEN_COMMA);
	}

  if (parser_curr_tok(p).type == TOKEN_SEMI_COLON) {
    parser_read_token(p, TOKEN_SEMI_COLON);

		AST_Node* sym = parser_parse_symbol(p);

		Token tok = parser_curr_tok(p);

		parser_read_token(p, TOKEN_COLON);
		
		AST_Node* typ = parser_parse_arrow(p);

		AST_Node* cont = ast_type_bind(&p->ast_man, tok, sym, typ);
		
    args = ast_hnd_eff_decl_args(&p->ast_man, tok, args, cont);
  }

  parser_read_token(p, TOKEN_CLOSE_PARENTHESIS);

  return args;
}

AST_Node *parser_parse_fun_decl(Parser *p) {
  Token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_KEYWORD_FN);

  AST_Node *args = parser_parse_decl_args(p);

  // if (args->kind != AST_TUPLE_LIST) {
  //   parser_error(p, tok,
  //                "function arguments cannot receive continuations in the same "
  //                "way as effects");
  // }

  AST_Node *type = ast_node_null(&p->ast_man);

  Token arr = lexer_undef_token();

  if (parser_curr_tok(p).type == TOKEN_ARROW) {
    arr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_ARROW);

    type = parser_parse_arrow(p);
  }

  AST_Node *ty = ast_type_arrow(&p->ast_man, arr, args, type);

  AST_Node *body = parser_parse_statements(p);

  return ast_fun_decl(&p->ast_man, tok, ty, body);
}

AST_Node *parser_parse_eff_decl(Parser *p) {
	
  Token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_KEYWORD_EF);

  AST_Node *args = parser_parse_eff_args(p);

  AST_Node *type = ast_node_null(&p->ast_man);

  Token arr = lexer_undef_token();

  if (parser_curr_tok(p).type == TOKEN_ARROW) {
    arr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_ARROW);

    type = parser_parse_arrow(p);
  }

  AST_Node *ty = ast_type_arrow(&p->ast_man, arr, args, type);

  if (parser_curr_tok(p).type == TOKEN_OPEN_CURLY_BRACE) {
    AST_Node *b = parser_parse_statements(p);

    return ast_handler_eff_decl(&p->ast_man, tok, type, b);
  }

  if (args->kind == AST_HND_EFF_ARGS) {
    parser_error(
        p, tok,
        "effect declaration outside a handler cannot have continuations");
  }

  parser_read_token(p, TOKEN_SEMI_COLON);

  return ast_eff_decl(&p->ast_man, tok, ty, ast_node_null(&p->ast_man));
}

AST_Node *parser_parse_handler_decl(Parser *p) {

  Token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_KEYWORD_HANDLER);
  parser_read_token(p, TOKEN_OPEN_CURLY_BRACE);

  AST_Node *eff_list = ast_handler_eff_list(&p->ast_man, parser_curr_tok(p));

  AST_Node *tmp = eff_list;

  while (parser_curr_tok(p).type != TOKEN_CLOSE_CURLY_BRACE) {
    Token eff_tok = parser_curr_tok(p);

    tmp->left = parser_parse_expr(p)->id;

    if (parser_curr_tok(p).type == TOKEN_CLOSE_CURLY_BRACE) {
      break;
    }

    AST_Node *r = ast_handler_eff_list(&p->ast_man, parser_curr_tok(p));
    tmp->right = r->id;
    tmp = r;
  }

  parser_read_token(p, TOKEN_CLOSE_CURLY_BRACE);
  return ast_handler_decl(&p->ast_man, tok, eff_list,
                          ast_node_null(&p->ast_man));
}

AST_Node *parser_parse_tuple(Parser *p) {
  AST_Node *root = parser_parse_arrow(p);
	
	AST_Node *tmp = root;
	
	AST_Node *head = NULL;

	if (parser_curr_tok(p).type == TOKEN_COMMA) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, TOKEN_COMMA);

    AST_Node *tail = parser_parse_tuple(p);

		if(tail->kind != AST_TUPLE_LIST) {
			tail = ast_tuple_list(&p->ast_man, tail->tok, tail, ast_node_null(&p->ast_man));
		}
		
    return ast_tuple_list(&p->ast_man, tok, tmp, tail);
  }

  return root;
}

AST_Node *parser_parse_decl(Parser *p) {
	
  if (parser_curr_tok(p).type == TOKEN_KEYWORD_FN) {
    return parser_parse_fun_decl(p);
  }

  if (parser_curr_tok(p).type == TOKEN_KEYWORD_EF) {
    return parser_parse_eff_decl(p);
  }

  if (parser_curr_tok(p).type == TOKEN_KEYWORD_HANDLER) {
    return parser_parse_handler_decl(p);
  }
	
	// TODO: here here here
  return parser_parse_tuple(p);
}


AST_Node* parser_parse_type_bind(Parser* p) {
	AST_Node* a = parser_parse_decl(p);
	if(parser_curr_tok(p).type == TOKEN_COLON) {
		Token tok = parser_curr_tok(p);

		parser_read_token(p, TOKEN_COLON);
		
		if(parser_curr_tok(p).type == TOKEN_COLON || parser_curr_tok(p).type == TOKEN_EQUAL) {
			return ast_type_bind(&p->ast_man, tok, a, ast_node_null(&p->ast_man));
		}
		
		AST_Node* b = parser_parse_tuple(p);

		return ast_type_bind(&p->ast_man, tok, a, b);
	}

	return a;
}


// EXPRESSION → IDENTIFIER ('=' | '|=' | '&=' | '+=' | '-=' ) ASSIGNMENT |
// EQUALITY | EXPRESSION ? EXPRESSION : EXPRESSION
AST_Node *parser_parse_expr(Parser *p) {
  AST_Node *root = parser_parse_type_bind(p);
  // assignment a = b
  if (parser_curr_tok(p).type == TOKEN_EQUAL) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, TOKEN_EQUAL);

    if (root->kind == AST_TYPE_BIND) {
      return ast_mut_bind(&p->ast_man, parser_curr_tok(p), root,
                          parser_parse_decl(p));
    }

    return ast_assignment(&p->ast_man, parser_curr_tok(p), root,
                          parser_parse_decl(p));
  }

  // assignment a : b
  if (parser_curr_tok(p).type == TOKEN_COLON) {
    if (root->kind != AST_TYPE_BIND) {
      parser_error(p, root->tok, "expecting type bind before const assignment");
    }

    Token tok = parser_curr_tok(p);

    parser_read_token(p, TOKEN_COLON);

    return ast_const_bind(&p->ast_man, tok, root, parser_parse_decl(p));
  }

  // // binding a := b, a :: b
  // if (parser_curr_tok(p).type == TOKEN_COLON) {
  //   if (root->kind != AST_SYM_DECL) {
  //     parser_error(p, root->tok, "left side of a binding needs to be a
  //     symbol");
  //   }

  //   parser_read_token(p, TOKEN_COLON);

  //   AST_Node *type = ast_node_null(&p->ast_man);

  //   if (parser_curr_tok(p).type != TOKEN_COLON &&
  //       parser_curr_tok(p).type != TOKEN_EQUAL) {
  //     type = parser_parse_arrow(p);
  //   }

  //   if (parser_curr_tok(p).type == TOKEN_COLON) {
  //     parser_read_token(p, TOKEN_COLON);
  //     return ast_const_bind(&p->ast_man, parser_curr_tok(p), type, root,
  //                           parser_parse_decl(p));
  //   }

  //   if (parser_curr_tok(p).type == TOKEN_EQUAL) {
  //     parser_read_token(p, TOKEN_EQUAL);
  //     return ast_mut_bind(&p->ast_man, parser_curr_tok(p), type, root,
  //                         parser_parse_decl(p));
  //   }

  //   parser_error(p, parser_curr_tok(p), "expecting '=' or ':'");
  // }

  return root;
}

AST_Node *parser_parse(Parser *p) {
  if (parser_curr_tok(p).type == TOKEN_EOF) {
    return 0;
  }

  AST_Node *root = ast_decl_list(&p->ast_man, parser_curr_tok(p));

  root->left = parser_parse_expr(p)->id;

  AST_Node *tail = parser_parse(p);

  root->right = tail ? tail->id : ast_node_null(&p->ast_man)->id;

  return root;
}


void print_ast_rec(i8* prefix, Parser* p, AST_Node* a, b8 is_left, b8 f = 1) {
	printf("%s", prefix);

	if(is_left) {
		if(f) printf("├──"); else printf("   ");
	} else {
		if(f) printf("└──"); else printf("   ");
	}
	if(a->id == 0) {
		printf("NULL\n");
		return;
	}
	printf("AST:{ kind: %s", ast_kind_to_cstr(a->kind));

	if(a->kind == AST_I32_DECL) {
		printf(",val: %lu", a->tok.buf);
	}
	
 	if(a->kind == AST_SYM_DECL) {
		i8 buff[a->tok.size + 1];
		
		token_get_id(&p->lex, a->tok, buff);

		printf(",sym: '%s'", buff);
	}

	printf(" }\n");

	if(a->left == 0 && a->right == 0) return;
	
		//printf("%*c|__", tabs, ' ');
		i8* buf0 = (i8*)malloc(strlen(prefix) + 10);
		strcpy(buf0, prefix);
		strcat(buf0, is_left ? "│   " : "    ");
		print_ast_rec(buf0, p, ast_manager_get_relative(&p->ast_man, a, a->left), 1);
		free(buf0);
	

		i8* buf1 = (i8*)malloc(strlen(prefix) + 10);
		strcpy(buf1, prefix);
		strcat(buf1, is_left ? "│   " : "    ");
		print_ast_rec(buf1, p, ast_manager_get_relative(&p->ast_man, a, a->right), 0);
		free(buf1);
	
} 

void print_ast(Parser* p, AST_Node* n) {
	i8 s[1];

	s[0] = '\0';
	
	print_ast_rec(s, p, n, 0, 0);
}
