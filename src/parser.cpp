
#include "parser.hpp"
#include "ast.hpp"
#include "registry.hpp"
#include "token.hpp"
#include "types.hpp"

#include "error.hpp"
#include "stdio.h"

void _print_ast(parser* p, ast* a, int tabs = 0) {
	printf(" AST:{ kind: %s", ast_kind_to_cstr(a->kind));
	if(a->kind == AST_I32_DECL) {
		printf(",val: %lu", a->tok.buf);
	}
	
 	if(a->kind == AST_SYM_DECL) {
		i8 buff[256];
		token_get_id(&p->lex, a->tok, buff);
		printf(",sym: '%s'", buff);
	}

	printf(" }\n");
	if(a->left) {
		printf("%*c|__", tabs, ' ');
		_print_ast(p, a->left, tabs + 4);
	}

	if(a->right) {
		printf("%*c|__", tabs, ' ');
		_print_ast(p, a->right, tabs + 4);
	}
} 

ast* parser_parse_expr(parser* p);
ast *parser_parse_statements(parser *p);
ast *parser_parse_decl(parser *p);

void parser_init(parser *p, u64 id, i8 *buffer, u64 size) {
  lexer_init(&p->lex, id, buffer, size);
}

void parser_destroy(parser *p) { lexer_destroy(&p->lex); }

token parser_curr_tok(parser *p) { return p->lex.curr; }

token parser_prev_tok(parser *p) { return p->lex.prev; }

token parser_read_token(parser *p, token_type kind) {
  if (p->lex.curr.type != kind) {
    char tmp[256];
    sprintf(tmp, "expecting token %i", kind);
    parser_error(p, parser_curr_tok(p), tmp);
  }

  return lexer_read_token(&p->lex);
}

ast* parser_parse_symbol(parser* p) {
	token tok = parser_curr_tok(p);

	parser_read_token(p, TOKEN_ID);
	
	return ast_symbol(tok);
}

ast *parse_call_args(parser *p) {
  parser_read_token(p, TOKEN_OPEN_PARENTHESIS);

  ast *root = ast_call_arg_list(parser_curr_tok(p));

  if (parser_curr_tok(p).type != TOKEN_CLOSE_PARENTHESIS) {

    ast *tmp = root;

    tmp->left = parser_parse_expr(p);

    while (parser_curr_tok(p).type == TOKEN_COMMA) {
      parser_read_token(p, TOKEN_COMMA);
      ast *next = ast_call_arg_list(parser_curr_tok(p));

      next->left = parser_parse_expr(p);

      next->right = 0;

      tmp->right = next;
      tmp = tmp->right;
    }
  }

  parser_read_token(p, TOKEN_CLOSE_PARENTHESIS);
  return root;
}

ast *parser_parse_lit(parser *p) {
  token tok = parser_curr_tok(p);

  if (tok.type == TOKEN_I32_LIT) {
    parser_read_token(p, TOKEN_I32_LIT);
    return ast_i32_lit(tok);
  }

  parser_error(p, parser_curr_tok(p), "invalid literal declaration");

  return 0;
}

// PRIMARY → LITERAL | SYMBOL | '('EXPRESSION')'
ast *parser_parse_primary(parser *p) {
  ast *expr = 0;

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
ast *parser_parse_postfix_suffix_unary(parser *p) {
  // TODO
  return parser_parse_primary(p);
}

ast* parser_parse_call_args(parser* p) {
	bool effectfull = false;
	
	token tok = parser_curr_tok(p);
	ast *arg = parse_call_args(p);
	ast *tail = 0;

	if(parser_curr_tok(p).type == TOKEN_EXCLAMATION ||
		 parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) {

		if (parser_curr_tok(p).type == TOKEN_EXCLAMATION) {
			effectfull = true;
			parser_read_token(p, TOKEN_EXCLAMATION);
		}

		return ast_call(tok, arg, parser_parse_call_args(p), effectfull);
	}
		
	return arg;
}

ast* parser_parse_call_tail(parser* p, ast* head) {
	b8 effectfull = false;
	
	if (parser_curr_tok(p).type == TOKEN_EXCLAMATION) {
    effectfull = true;

    parser_read_token(p, TOKEN_EXCLAMATION);
  }

	token tok = parser_curr_tok(p);
	
	ast* args = parse_call_args(p);

	ast* call = ast_call(tok, head, args, effectfull);

	if(parser_curr_tok(p).type == TOKEN_EXCLAMATION || parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) {
		return parser_parse_call_tail(p, call);
	}
	
	return call;
} 

ast *parser_parse_call(parser *p) {
  ast *root = parser_parse_postfix_suffix_unary(p);

  b8 effectfull = false;

	if (parser_curr_tok(p).type == TOKEN_EXCLAMATION) {
    effectfull = true;

    parser_read_token(p, TOKEN_EXCLAMATION);
  }

	token tok = parser_curr_tok(p);
	
	if(parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) {
		ast* args = parse_call_args(p);
		
		ast* call = ast_call(tok, root, args, effectfull);
	
		if(parser_curr_tok(p).type == TOKEN_EXCLAMATION || parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) {
			return parser_parse_call_tail(p, call);
		}


		if(parser_curr_tok(p).type == TOKEN_KEYWORD_WITH) {
			token tok = parser_curr_tok(p);

			parser_read_token(p, TOKEN_KEYWORD_WITH);

			ast* hnd = parser_parse_decl(p);
			
			return ast_with_hnd(tok, call, hnd);
		}
		
		return call;
		
	}

	if(effectfull) {
		parser_error(p, tok, "effect call expect arguments");
	}

  return root;
}

ast *parser_parse_array_access(parser *p) {
  // TODO: here should be parser array access
  return parser_parse_call(p);
}

ast *parser_parse_member_access(parser *p) {
  ast *root = parser_parse_array_access(p);

  if (parser_curr_tok(p).type == TOKEN_DOT) {
    return ast_member_access(parser_curr_tok(p), root,
                             parser_parse_member_access(p));
  }

  return root;
}

// UNARY → ('!' | '-' | '+' | '++' | '--' | '~' )UNARY | UNARY('++' | '--') |
// CALL
ast *parser_parse_unary(parser *p) {
  ast *root = 0;

  if (parser_curr_tok(p).type == TOKEN_MINUS ||
      parser_curr_tok(p).type == TOKEN_PLUS) {
    token tok = parser_curr_tok(p);

    parser_read_token(p, tok.type);

    if (tok.type == TOKEN_MINUS) {
      return ast_una_sub(tok, root, parser_parse_unary(p));
    }

    if (tok.type == TOKEN_PLUS) {
      return ast_una_add(tok, root, parser_parse_unary(p));
    }
  }

  return parser_parse_member_access(p);
}

// FACTOR → UNARY (('/' | '*' | '%' ) FACTOR)*
ast *parser_parse_factor(parser *p) {
  ast *root = parser_parse_unary(p);

  if (parser_curr_tok(p).type == TOKEN_SLASH ||
      parser_curr_tok(p).type == TOKEN_ASTERISK) {
    token tok = parser_curr_tok(p);

    parser_read_token(p, tok.type);

    if (tok.type == TOKEN_SLASH) {
      return ast_bin_div(tok, root, parser_parse_factor(p));
    }

    if (tok.type == TOKEN_ASTERISK) {
      return ast_bin_mul(tok, root, parser_parse_factor(p));
    }
  }

  return root;
}

// TERM → FACTOR (('+' | '-') TERM)*
ast *parser_parse_term(parser *p) {
  ast *root = parser_parse_factor(p);
  if (parser_curr_tok(p).type == TOKEN_PLUS ||
      parser_curr_tok(p).type == TOKEN_MINUS) {
    token tok = parser_curr_tok(p);

    parser_read_token(p, TOKEN_PLUS);

    if (tok.type == TOKEN_PLUS) {
      return ast_bin_add(tok, root, parser_parse_term(p));
    }

    if (tok.type == TOKEN_MINUS) {
      return ast_bin_sub(tok, root, parser_parse_term(p));
    }
  }

  return root;
}

// SHIFT → TERM (( '>>' | '<<'  ) COMPARISON)*
ast *parser_parse_bitwise_shift(parser *p) {
  // TODO: bitwise shift should be parsed here
  return parser_parse_term(p);
}

// COMPARISON → SHIFT (( '>' | '>=' | '<' | '<='  ) COMPARISON)*
ast *parser_parse_comparison(parser *p) {
  // TODO: comparison expressions should go here
  ast *root = parser_parse_bitwise_shift(p);

  if (parser_curr_tok(p).type == TOKEN_GREATER) {
    token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_GREATER);

    return ast_bin_gt(curr, root, parser_parse_comparison(p));
  }

  if (parser_curr_tok(p).type == TOKEN_SMALLER) {
    token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_SMALLER);

    return ast_bin_lt(curr, root, parser_parse_comparison(p));
  }

  if (parser_curr_tok(p).type == TOKEN_GREATER_EQUAL) {
    token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_GREATER_EQUAL);

    return ast_bin_ge(curr, root, parser_parse_comparison(p));
  }

  if (parser_curr_tok(p).type == TOKEN_SMALLER_EQUAL) {
    token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_SMALLER_EQUAL);

    return ast_bin_le(curr, root, parser_parse_comparison(p));
  }

  return root;
}

// EQUALITY → COMPARISON (('!=' | '==') EQUALITY)*
ast *parser_parse_equality(parser *p) {
  // TODO: equality comparisons expressions should go here
  ast *root = parser_parse_comparison(p);

  if (parser_curr_tok(p).type == TOKEN_EXCLAMATION_EQUAL) {
    token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_EXCLAMATION_EQUAL);

    return ast_bin_ne(curr, root, parser_parse_equality(p));
  }

  if (parser_curr_tok(p).type == TOKEN_EQUAL_EQUAL) {
    token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_EQUAL_EQUAL);

    return ast_bin_eq(curr, root, parser_parse_equality(p));
  }

  return root;
}

// BITWISE → BOOL (( '&' | '|' | '^'  ) BITWISE)*
ast *parser_parse_bitwise(parser *p) {
  // TODO: bitwise expressions should go here
  return parser_parse_equality(p);
}

// BOOL → TERM (( '&&' | '^^' | '||') BOOL)*
ast *parser_parse_booleans(parser *p) {
  // TODO: booleans expressions should go here
  return parser_parse_bitwise(p);
}


ast *parser_parse_ternary(parser *p) {
	if(parser_curr_tok(p).type == TOKEN_KEYWORD_CONTINUE) {
		token tok = parser_curr_tok(p);
		parser_read_token(p, TOKEN_KEYWORD_CONTINUE);

		ast* cont = parser_parse_ternary(p);
		ast* expr = 0;

		if(parser_curr_tok(p).type == TOKEN_KEYWORD_WITH) {
			parser_read_token(p, TOKEN_KEYWORD_WITH);
			expr = parser_parse_decl(p);
		}

		return ast_ctrl_flow_continue(tok, cont, expr);
	}

  return parser_parse_booleans(p);
}


ast *parser_parse_if_statement(parser *p) {
  token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_KEYWORD_IF);

  ast *cond = parser_parse_expr(p);

  ast *body = parser_parse_statements(p);

  ast *cont = 0;

  if (parser_curr_tok(p).type == TOKEN_KEYWORD_ELSE) {
    parser_read_token(p, TOKEN_KEYWORD_ELSE);
    cont = parser_parse_if_statement(p);
  }

  return ast_ctrl_flow_if(tok, cond, body, cont);
}

ast *parser_parse_ret_statement(parser *p) {
  token tok = parser_curr_tok(p);
  parser_read_token(p, TOKEN_KEYWORD_RETURN);
  ast *ret = ast_ctrl_flow_ret(tok, parser_parse_expr(p));

	if(parser_prev_tok(p).type != TOKEN_CLOSE_CURLY_BRACE) {
		parser_read_token(p, TOKEN_SEMI_COLON);
	}

	return ret;
}

ast *parser_parse_statement(parser *p) {
  if (parser_curr_tok(p).type == TOKEN_KEYWORD_IF) {
    return parser_parse_if_statement(p);
  }

  if (parser_curr_tok(p).type == TOKEN_KEYWORD_RETURN) {
    return parser_parse_ret_statement(p);
  }

  ast *expr = parser_parse_expr(p);

  parser_read_token(p, TOKEN_SEMI_COLON);

  return expr;
}

ast *parser_parse_statements(parser *p) {
  parser_read_token(p, TOKEN_OPEN_CURLY_BRACE);
  ast *stmt = ast_statement_list(parser_curr_tok(p));

  ast *tmp = stmt;

  while (parser_curr_tok(p).type != TOKEN_CLOSE_CURLY_BRACE) {
    tmp->left = parser_parse_statement(p);

    if (parser_curr_tok(p).type == TOKEN_CLOSE_CURLY_BRACE) {
      break;
    }

    tmp->right = ast_statement_list(parser_curr_tok(p));
    tmp = tmp->right;
  }

  parser_read_token(p, TOKEN_CLOSE_CURLY_BRACE);

  return stmt;
}

ast *parser_parse_decl_args(parser *p) {
  parser_read_token(p, TOKEN_OPEN_PARENTHESIS);

  ast *args = ast_decl_arg_list(parser_curr_tok(p));
  ast *tmp = args;

  while (parser_curr_tok(p).type != TOKEN_CLOSE_PARENTHESIS) {
    token tok = parser_curr_tok(p);
		
    ast *id = parser_parse_symbol(p);
		ast* ty = 0;

		if(parser_curr_tok(p).type == TOKEN_COLON) {
			parser_read_token(p, TOKEN_COLON);

			// TODO: use proper type parsing function
			ast *ty = parser_parse_ternary(p);
		}

    tmp->left = ast_const_bind(tok, ty, id, 0);

    if (parser_curr_tok(p).type != TOKEN_COMMA) {
      break;
    }

		parser_read_token(p, TOKEN_COMMA);
		
    tmp->right = ast_decl_arg_list(parser_curr_tok(p));
		tmp = tmp->right;
  }

  parser_read_token(p, TOKEN_CLOSE_PARENTHESIS);

  return args;
}

ast *parser_parse_eff_args(parser *p) {
	token tok = parser_curr_tok(p);
	
  parser_read_token(p, TOKEN_OPEN_PARENTHESIS);

  ast *args = ast_decl_arg_list(parser_curr_tok(p));
  ast *tmp = args;

  while (parser_curr_tok(p).type == TOKEN_ID) {
    ast *id = parser_parse_symbol(p);
		ast* ty = 0;

		if(parser_curr_tok(p).type == TOKEN_COLON) {
			parser_read_token(p, TOKEN_COLON);

			// TODO: use proper type parsing function
			ast *ty = parser_parse_ternary(p);
		}

    tmp->left = ast_const_bind(tok, ty, id, 0);

    if (parser_curr_tok(p).type != TOKEN_COMMA) {
      break;
    }

		parser_read_token(p, TOKEN_COMMA);
		
    tmp->right = ast_decl_arg_list(parser_curr_tok(p));
		tmp = tmp->right;
  }
	
	if(parser_curr_tok(p).type == TOKEN_SEMI_COLON) {
		parser_read_token(p, TOKEN_SEMI_COLON);
		
		// TODO: parse continuation properly including types 
		ast* cont = parser_parse_symbol(p);
		
		args = ast_hnd_eff_decl_args(tok, args, cont);
	}
	
  parser_read_token(p, TOKEN_CLOSE_PARENTHESIS);
  return args;
}


ast *parser_parse_fun_decl(parser *p) {
  token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_KEYWORD_FN);

  ast *args = parser_parse_decl_args(p);

	if(args->kind != AST_DECL_ARGS_LIST) {
		parser_error(p, tok, "function arguments cannot receive continuations in the same way as effects");
	}
	
  ast *type = 0;
  if (parser_curr_tok(p).type == TOKEN_ARROW) {
    parser_read_token(p, TOKEN_ARROW);

    // TODO: use proper type parsing function
    type = parser_parse_ternary(p);
  }

  ast *body = parser_parse_statements(p);

  return ast_fun_decl(tok, type, args, body);
}

ast *parser_parse_eff_decl(parser *p) {
  token tok = parser_curr_tok(p);
	
  parser_read_token(p, TOKEN_KEYWORD_EF);
	ast *args = parser_parse_eff_args(p);
	ast *type = 0;

	if(parser_curr_tok(p).type == TOKEN_ARROW) {
		
		parser_read_token(p, TOKEN_ARROW);
		// TODO: use proper type parsing function
		type = parser_parse_ternary(p);
	}
	
	if(parser_curr_tok(p).type == TOKEN_OPEN_CURLY_BRACE) {
		return ast_handler_eff_decl(tok, type, args,  parser_parse_statements(p));
	}
	if(args->kind == AST_HND_EFF_ARGS) {
		parser_error(p, tok, "effect declaration outside a handler cannot have continuations");
	}
	parser_read_token(p, TOKEN_SEMI_COLON);
  return ast_eff_decl(tok, type, args, 0);
}

ast *parser_parse_handler_decl(parser *p) {

  token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_KEYWORD_HANDLER);
  parser_read_token(p, TOKEN_OPEN_CURLY_BRACE);

  ast *eff_list = ast_handler_eff_list(parser_curr_tok(p));

  ast *tmp = eff_list;
	
	// i8 buf[256];
	// token_get_id(&p->lex, parser_curr_tok(p), buf);
	// printf("-- curr = '%s'\n", buf);
  while (parser_curr_tok(p).type != TOKEN_CLOSE_CURLY_BRACE) {
		token eff_tok = parser_curr_tok(p);
    tmp->left = parser_parse_expr(p);
 
		if(tmp->left->right->kind != AST_HND_EFF_DECL) {
			parser_error(p, eff_tok, "expecting an handler effect declaration");
		}

		if (parser_curr_tok(p).type == TOKEN_CLOSE_CURLY_BRACE) {
      break;
    }
    tmp->right = ast_handler_eff_list(parser_curr_tok(p));
    tmp = tmp->right;
  }

  parser_read_token(p, TOKEN_CLOSE_CURLY_BRACE);
	return ast_handler_decl(tok, eff_list, 0);
}

ast *parser_parse_decl(parser *p) {
  if (parser_curr_tok(p).type == TOKEN_KEYWORD_FN) {
    return parser_parse_fun_decl(p);
  }

  if (parser_curr_tok(p).type == TOKEN_KEYWORD_EF) {
			ast* t = parser_parse_eff_decl(p);
			return t;
  }

  if (parser_curr_tok(p).type == TOKEN_KEYWORD_HANDLER) {
    return parser_parse_handler_decl(p);
  }

  return parser_parse_ternary(p);
}

// EXPRESSION → IDENTIFIER ('=' | '|=' | '&=' | '+=' | '-=' ) ASSIGNMENT |
// EQUALITY | EXPRESSION ? EXPRESSION : EXPRESSION
ast *parser_parse_expr(parser *p) {
  ast *root = parser_parse_decl(p);

	// i8 buf[256];
	// token_get_id(&p->lex, root->tok, buf);
	// printf("curr = '%s'\n", buf);
	
  // assignment a = b
  if (parser_curr_tok(p).type == TOKEN_EQUAL) {
    if (root->kind != AST_SYM_DECL) {
      parser_error(p, root->tok,
                   "left side of assignment needs to be a symbol");
    }

    parser_read_token(p, TOKEN_EQUAL);
		
    return ast_assignment(parser_curr_tok(p), root, parser_parse_decl(p));
  }

  // binding a := b, a :: b
  if (parser_curr_tok(p).type == TOKEN_COLON) {
    if (root->kind != AST_SYM_DECL) {
      parser_error(p, root->tok, "left side of a binding needs to be a symbol");
    }

    parser_read_token(p, TOKEN_COLON);

    ast *type = NULL;

    if (parser_curr_tok(p).type != TOKEN_COLON &&
        parser_curr_tok(p).type != TOKEN_EQUAL) {
      // TODO: use proper type parsing function
      type = parser_parse_ternary(p);
    }

    if (parser_curr_tok(p).type == TOKEN_COLON) {
      parser_read_token(p, TOKEN_COLON);
       ast* t = ast_const_bind(parser_curr_tok(p), type, root, parser_parse_decl(p));
			 return t;
    }

    if (parser_curr_tok(p).type == TOKEN_EQUAL) {
      parser_read_token(p, TOKEN_EQUAL);
      return ast_mut_bind(parser_curr_tok(p), type, root, parser_parse_decl(p));
    }

    parser_error(p, parser_curr_tok(p), "expecting '=' or ':'");
  }

  return root;
}


ast *parser_parse(parser *p) {
	if (parser_curr_tok(p).type == TOKEN_EOF) {
    return 0;
  }

  ast *root = ast_decl_list(parser_curr_tok(p));

  root->left = parser_parse_expr(p);
  root->right = parser_parse(p);

  return root;
}
