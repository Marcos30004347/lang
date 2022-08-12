
#include "parser.hpp"
#include "ast.hpp"
#include "lexer.hpp"
#include "types.hpp"

#include "error.hpp"
#include "stdio.h"

ASTNode *parser_parse_expr(Parser *p);
ASTNode *parser_parse_statements(Parser *p);
ASTNode *parser_parse_decl(Parser *p);

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

ASTNode *parser_parse_symbol(Parser *p) {
  Token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_ID);

  return ast_symbol(&p->ast_man, tok);
}

ASTNode *parse_call_args(Parser *p) {
  parser_read_token(p, TOKEN_OPEN_PARENTHESIS);

  ASTNode *root = ast_call_arg_list(&p->ast_man, parser_curr_tok(p));

  if (parser_curr_tok(p).type != TOKEN_CLOSE_PARENTHESIS) {

    ASTNode *tmp = root;

    tmp->left = parser_parse_expr(p)->id;

    while (parser_curr_tok(p).type == TOKEN_COMMA) {
      parser_read_token(p, TOKEN_COMMA);

      ASTNode *next = ast_call_arg_list(&p->ast_man, parser_curr_tok(p));

      next->left = parser_parse_expr(p)->id;

      next->right = 0;

      tmp->right = next->id;

      tmp = next;
    }
  }

  parser_read_token(p, TOKEN_CLOSE_PARENTHESIS);
  return root;
}

ASTNode *parser_parse_lit(Parser *p) {
  Token tok = parser_curr_tok(p);

  if (tok.type == TOKEN_I32_LIT) {
    parser_read_token(p, TOKEN_I32_LIT);
    return ast_i32_lit(&p->ast_man, tok);
  }

  parser_error(p, parser_curr_tok(p), "invalid literal declaration");

  return 0;
}

// PRIMARY → LITERAL | SYMBOL | '('EXPRESSION')'
ASTNode *parser_parse_primary(Parser *p) {
  ASTNode *expr = 0;

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
ASTNode *parser_parse_postfix_suffix_unary(Parser *p) {
  // TODO
  return parser_parse_primary(p);
}

ASTNode *parser_parse_call_args(Parser *p) {
  bool effectfull = false;

  Token tok = parser_curr_tok(p);
  ASTNode *arg = parse_call_args(p);
  ASTNode *tail = 0;

  if (parser_curr_tok(p).type == TOKEN_EXCLAMATION ||
      parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) {

    if (parser_curr_tok(p).type == TOKEN_EXCLAMATION) {
      effectfull = true;
      parser_read_token(p, TOKEN_EXCLAMATION);
    }

    return ast_call(&p->ast_man, tok, arg->id, parser_parse_call_args(p)->id,
                    effectfull);
  }

  return arg;
}

ASTNode *parser_parse_call_tail(Parser *p, ASTNode *head) {
  b8 effectfull = false;

  if (parser_curr_tok(p).type == TOKEN_EXCLAMATION) {
    effectfull = true;

    parser_read_token(p, TOKEN_EXCLAMATION);
  }

  Token tok = parser_curr_tok(p);

  ASTNode *args = parse_call_args(p);

  ASTNode *call = ast_call(&p->ast_man, tok, head->id, args->id, effectfull);

  if (parser_curr_tok(p).type == TOKEN_EXCLAMATION ||
      parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) {
    return parser_parse_call_tail(p, call);
  }

  return call;
}

ASTNode *parser_parse_call(Parser *p) {
  ASTNode *root = parser_parse_postfix_suffix_unary(p);

  b8 effectfull = false;

  if (parser_curr_tok(p).type == TOKEN_EXCLAMATION) {
    effectfull = true;

    parser_read_token(p, TOKEN_EXCLAMATION);
  }

  Token tok = parser_curr_tok(p);

  if (parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) {
    ASTNode *args = parse_call_args(p);

    ASTNode *call = ast_call(&p->ast_man, tok, root->id, args->id, effectfull);

    if (parser_curr_tok(p).type == TOKEN_EXCLAMATION ||
        parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) {
      return parser_parse_call_tail(p, call);
    }

    if (parser_curr_tok(p).type == TOKEN_KEYWORD_WITH) {
      Token tok = parser_curr_tok(p);

      parser_read_token(p, TOKEN_KEYWORD_WITH);

      ASTNode *hnd = parser_parse_decl(p);

      return ast_with_hnd(&p->ast_man, tok, call->id, hnd->id);
    }

    return call;
  }

  if (effectfull) {
    parser_error(p, tok, "effect call expect arguments");
  }

  return root;
}

ASTNode *parser_parse_array_access(Parser *p) {
  // TODO: here should be parser array access
  return parser_parse_call(p);
}

ASTNode *parser_parse_member_access(Parser *p) {
  ASTNode *root = parser_parse_array_access(p);

  if (parser_curr_tok(p).type == TOKEN_DOT) {
    return ast_member_access(&p->ast_man, parser_curr_tok(p), root->id,
                             parser_parse_member_access(p)->id);
  }

  return root;
}

// UNARY → ('!' | '-' | '+' | '++' | '--' | '~' )UNARY | UNARY('++' | '--') |
// CALL
ASTNode *parser_parse_unary(Parser *p) {
  ASTNode *root = 0;

  if (parser_curr_tok(p).type == TOKEN_MINUS ||
      parser_curr_tok(p).type == TOKEN_PLUS) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, tok.type);

    if (tok.type == TOKEN_MINUS) {
      return ast_una_sub(&p->ast_man, tok, root->id, parser_parse_unary(p)->id);
    }

    if (tok.type == TOKEN_PLUS) {
      return ast_una_add(&p->ast_man, tok, root->id, parser_parse_unary(p)->id);
    }
  }

  return parser_parse_member_access(p);
}

// FACTOR → UNARY (('/' | '*' | '%' ) FACTOR)*
ASTNode *parser_parse_factor(Parser *p) {
  ASTNode *root = parser_parse_unary(p);

  if (parser_curr_tok(p).type == TOKEN_SLASH ||
      parser_curr_tok(p).type == TOKEN_ASTERISK) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, tok.type);

    if (tok.type == TOKEN_SLASH) {
      return ast_bin_div(&p->ast_man, tok, root->id,
                         parser_parse_factor(p)->id);
    }

    if (tok.type == TOKEN_ASTERISK) {
      return ast_bin_mul(&p->ast_man, tok, root->id,
                         parser_parse_factor(p)->id);
    }
  }

  return root;
}

// TERM → FACTOR (('+' | '-') TERM)*
ASTNode *parser_parse_term(Parser *p) {
  ASTNode *root = parser_parse_factor(p);
  if (parser_curr_tok(p).type == TOKEN_PLUS ||
      parser_curr_tok(p).type == TOKEN_MINUS) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, TOKEN_PLUS);

    if (tok.type == TOKEN_PLUS) {
      return ast_bin_add(&p->ast_man, tok, root->id, parser_parse_term(p)->id);
    }

    if (tok.type == TOKEN_MINUS) {
      return ast_bin_sub(&p->ast_man, tok, root->id, parser_parse_term(p)->id);
    }
  }

  return root;
}

// SHIFT → TERM (( '>>' | '<<'  ) COMPARISON)*
ASTNode *parser_parse_bitwise_shift(Parser *p) {
  // TODO: bitwise shift should be parsed here
  return parser_parse_term(p);
}

// COMPARISON → SHIFT (( '>' | '>=' | '<' | '<='  ) COMPARISON)*
ASTNode *parser_parse_comparison(Parser *p) {
  // TODO: comparison expressions should go here
  ASTNode *root = parser_parse_bitwise_shift(p);

  if (parser_curr_tok(p).type == TOKEN_GREATER) {
    Token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_GREATER);

    return ast_bin_gt(&p->ast_man, curr, root->id,
                      parser_parse_comparison(p)->id);
  }

  if (parser_curr_tok(p).type == TOKEN_SMALLER) {
    Token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_SMALLER);

    return ast_bin_lt(&p->ast_man, curr, root->id,
                      parser_parse_comparison(p)->id);
  }

  if (parser_curr_tok(p).type == TOKEN_GREATER_EQUAL) {
    Token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_GREATER_EQUAL);

    return ast_bin_ge(&p->ast_man, curr, root->id,
                      parser_parse_comparison(p)->id);
  }

  if (parser_curr_tok(p).type == TOKEN_SMALLER_EQUAL) {
    Token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_SMALLER_EQUAL);

    return ast_bin_le(&p->ast_man, curr, root->id,
                      parser_parse_comparison(p)->id);
  }

  return root;
}

// EQUALITY → COMPARISON (('!=' | '==') EQUALITY)*
ASTNode *parser_parse_equality(Parser *p) {
  // TODO: equality comparisons expressions should go here
  ASTNode *root = parser_parse_comparison(p);

  if (parser_curr_tok(p).type == TOKEN_EXCLAMATION_EQUAL) {
    Token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_EXCLAMATION_EQUAL);

    return ast_bin_ne(&p->ast_man, curr, root->id,
                      parser_parse_equality(p)->id);
  }

  if (parser_curr_tok(p).type == TOKEN_EQUAL_EQUAL) {
    Token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_EQUAL_EQUAL);

    return ast_bin_eq(&p->ast_man, curr, root->id,
                      parser_parse_equality(p)->id);
  }

  return root;
}

// BITWISE → BOOL (( '&' | '|' | '^'  ) BITWISE)*
ASTNode *parser_parse_bitwise(Parser *p) {
  // TODO: bitwise expressions should go here
  return parser_parse_equality(p);
}

// BOOL → TERM (( '&&' | '^^' | '||') BOOL)*
ASTNode *parser_parse_booleans(Parser *p) {
  // TODO: booleans expressions should go here
  return parser_parse_bitwise(p);
}

ASTNode *parser_parse_ternary(Parser *p) {
  if (parser_curr_tok(p).type == TOKEN_KEYWORD_CONTINUE) {
    Token tok = parser_curr_tok(p);
    parser_read_token(p, TOKEN_KEYWORD_CONTINUE);

    ASTNode *cont = parser_parse_ternary(p);
    ASTNode *expr = 0;

    if (parser_curr_tok(p).type == TOKEN_KEYWORD_WITH) {
      parser_read_token(p, TOKEN_KEYWORD_WITH);
      expr = parser_parse_decl(p);
    }

    return ast_ctrl_flow_continue(&p->ast_man, tok, cont->id, expr->id);
  }

  return parser_parse_booleans(p);
}

ASTNode *parser_parse_if_statement(Parser *p) {
  Token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_KEYWORD_IF);

  ASTNode *cond = parser_parse_expr(p);

  ASTNode *body = parser_parse_statements(p);

  ASTNode *cont = 0;

  if (parser_curr_tok(p).type == TOKEN_KEYWORD_ELSE) {
    parser_read_token(p, TOKEN_KEYWORD_ELSE);
    cont = parser_parse_if_statement(p);
  }

  return ast_ctrl_flow_if(&p->ast_man, tok, cond->id, body->id, cont->id);
}

ASTNode *parser_parse_ret_statement(Parser *p) {
  Token tok = parser_curr_tok(p);
  parser_read_token(p, TOKEN_KEYWORD_RETURN);
  ASTNode *ret = ast_ctrl_flow_ret(&p->ast_man, tok, parser_parse_expr(p)->id);

  if (parser_prev_tok(p).type != TOKEN_CLOSE_CURLY_BRACE) {
    parser_read_token(p, TOKEN_SEMI_COLON);
  }

  return ret;
}

ASTNode *parser_parse_statement(Parser *p) {
  if (parser_curr_tok(p).type == TOKEN_KEYWORD_IF) {
    return parser_parse_if_statement(p);
  }

  if (parser_curr_tok(p).type == TOKEN_KEYWORD_RETURN) {
    return parser_parse_ret_statement(p);
  }

  ASTNode *expr = parser_parse_expr(p);

  parser_read_token(p, TOKEN_SEMI_COLON);

  return expr;
}

ASTNode *parser_parse_statements(Parser *p) {
  parser_read_token(p, TOKEN_OPEN_CURLY_BRACE);
  ASTNode *stmt = ast_statement_list(&p->ast_man, parser_curr_tok(p));

  ASTNode *tmp = stmt;

  while (parser_curr_tok(p).type != TOKEN_CLOSE_CURLY_BRACE) {
    tmp->left = parser_parse_statement(p)->id;

    if (parser_curr_tok(p).type == TOKEN_CLOSE_CURLY_BRACE) {
      break;
    }

    ASTNode *r = ast_statement_list(&p->ast_man, parser_curr_tok(p));
    tmp->right = r->id;
    tmp = r;
  }

  parser_read_token(p, TOKEN_CLOSE_CURLY_BRACE);

  return stmt;
}

ASTNode *parser_parse_decl_args(Parser *p) {
  parser_read_token(p, TOKEN_OPEN_PARENTHESIS);

  ASTNode *args = ast_decl_arg_list(&p->ast_man, parser_curr_tok(p));
  ASTNode *tmp = args;

  while (parser_curr_tok(p).type != TOKEN_CLOSE_PARENTHESIS) {
    Token tok = parser_curr_tok(p);

    ASTNode *id = parser_parse_symbol(p);
    ASTNode *type = 0;

    if (parser_curr_tok(p).type == TOKEN_COLON) {
      parser_read_token(p, TOKEN_COLON);

      // TODO: use proper type parsing function
      type = parser_parse_ternary(p);
    }

    AST_Id ty = type ? type->id : 0;

    tmp->left = ast_const_bind(&p->ast_man, tok, ty, id->id, 0)->id;

    if (parser_curr_tok(p).type != TOKEN_COMMA) {
      break;
    }

    parser_read_token(p, TOKEN_COMMA);

    ASTNode *r = ast_decl_arg_list(&p->ast_man, parser_curr_tok(p));
    tmp->right = r->id;
    tmp = r;
  }

  parser_read_token(p, TOKEN_CLOSE_PARENTHESIS);

  return args;
}

ASTNode *parser_parse_eff_args(Parser *p) {
  Token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_OPEN_PARENTHESIS);

  ASTNode *args = ast_decl_arg_list(&p->ast_man, parser_curr_tok(p));
  ASTNode *tmp = args;

  while (parser_curr_tok(p).type == TOKEN_ID) {
    ASTNode *id = parser_parse_symbol(p);

    ASTNode *type = NULL;

    if (parser_curr_tok(p).type == TOKEN_COLON) {
      parser_read_token(p, TOKEN_COLON);

      // TODO: use proper type parsing function
      type = parser_parse_ternary(p);
    }

    AST_Id ty = type ? type->id : 0;

    tmp->left = ast_const_bind(&p->ast_man, tok, ty, id->id, 0)->id;

    if (parser_curr_tok(p).type != TOKEN_COMMA) {
      break;
    }

    parser_read_token(p, TOKEN_COMMA);

    ASTNode *r = ast_decl_arg_list(&p->ast_man, parser_curr_tok(p));

    tmp->right = r->id;

    tmp = r;
  }

  if (parser_curr_tok(p).type == TOKEN_SEMI_COLON) {
    parser_read_token(p, TOKEN_SEMI_COLON);

    // TODO: parse continuation properly including types
    ASTNode *cont = parser_parse_symbol(p);

    args = ast_hnd_eff_decl_args(&p->ast_man, tok, args->id, cont->id);
  }

  parser_read_token(p, TOKEN_CLOSE_PARENTHESIS);
  return args;
}

ASTNode *parser_parse_fun_decl(Parser *p) {
  Token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_KEYWORD_FN);

  ASTNode *args = parser_parse_decl_args(p);

  if (args->kind != AST_DECL_ARGS_LIST) {
    parser_error(p, tok,
                 "function arguments cannot receive continuations in the same "
                 "way as effects");
  }

  ASTNode *type = 0;

  if (parser_curr_tok(p).type == TOKEN_ARROW) {
    parser_read_token(p, TOKEN_ARROW);

    // TODO: use proper type parsing function
    type = parser_parse_ternary(p);
  }

  AST_Id ty = type ? type->id : 0;

  ASTNode *body = parser_parse_statements(p);

  return ast_fun_decl(&p->ast_man, tok, ty, args->id, body->id);
}

ASTNode *parser_parse_eff_decl(Parser *p) {
  Token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_KEYWORD_EF);
  ASTNode *args = parser_parse_eff_args(p);

  ASTNode *type = 0;

  if (parser_curr_tok(p).type == TOKEN_ARROW) {

    parser_read_token(p, TOKEN_ARROW);
    // TODO: use proper type parsing function
    type = parser_parse_ternary(p);
  }

  AST_Id ty = type ? type->id : 0;

  if (parser_curr_tok(p).type == TOKEN_OPEN_CURLY_BRACE) {
    ASTNode *b = parser_parse_statements(p);
    return ast_handler_eff_decl(&p->ast_man, tok, ty, args->id, b->id);
  }
  if (args->kind == AST_HND_EFF_ARGS) {
    parser_error(
        p, tok,
        "effect declaration outside a handler cannot have continuations");
  }

  parser_read_token(p, TOKEN_SEMI_COLON);
  return ast_eff_decl(&p->ast_man, tok, ty, args->id, 0);
}

ASTNode *parser_parse_handler_decl(Parser *p) {

  Token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_KEYWORD_HANDLER);
  parser_read_token(p, TOKEN_OPEN_CURLY_BRACE);

  ASTNode *eff_list = ast_handler_eff_list(&p->ast_man, parser_curr_tok(p));

  ASTNode *tmp = eff_list;

  while (parser_curr_tok(p).type != TOKEN_CLOSE_CURLY_BRACE) {
    Token eff_tok = parser_curr_tok(p);

    tmp->left = parser_parse_expr(p)->id;

    if (parser_curr_tok(p).type == TOKEN_CLOSE_CURLY_BRACE) {
      break;
    }

    ASTNode *r = ast_handler_eff_list(&p->ast_man, parser_curr_tok(p));
    tmp->right = r->id;
    tmp = r;
  }

  parser_read_token(p, TOKEN_CLOSE_CURLY_BRACE);
  return ast_handler_decl(&p->ast_man, tok, eff_list->id, 0);
}

ASTNode *parser_parse_decl(Parser *p) {
  if (parser_curr_tok(p).type == TOKEN_KEYWORD_FN) {
    return parser_parse_fun_decl(p);
  }

  if (parser_curr_tok(p).type == TOKEN_KEYWORD_EF) {
    return parser_parse_eff_decl(p);
  }

  if (parser_curr_tok(p).type == TOKEN_KEYWORD_HANDLER) {
    return parser_parse_handler_decl(p);
  }

  return parser_parse_ternary(p);
}

// EXPRESSION → IDENTIFIER ('=' | '|=' | '&=' | '+=' | '-=' ) ASSIGNMENT |
// EQUALITY | EXPRESSION ? EXPRESSION : EXPRESSION
ASTNode *parser_parse_expr(Parser *p) {
  ASTNode *root = parser_parse_decl(p);

  // assignment a = b
  if (parser_curr_tok(p).type == TOKEN_EQUAL) {
    if (root->kind != AST_SYM_DECL) {
      parser_error(p, root->tok,
                   "left side of assignment needs to be a symbol");
    }

    parser_read_token(p, TOKEN_EQUAL);

    return ast_assignment(&p->ast_man, parser_curr_tok(p), root->id,
                          parser_parse_decl(p)->id);
  }

  // binding a := b, a :: b
  if (parser_curr_tok(p).type == TOKEN_COLON) {
    if (root->kind != AST_SYM_DECL) {
      parser_error(p, root->tok, "left side of a binding needs to be a symbol");
    }

    parser_read_token(p, TOKEN_COLON);

    ASTNode *type = NULL;

    if (parser_curr_tok(p).type != TOKEN_COLON &&
        parser_curr_tok(p).type != TOKEN_EQUAL) {
      // TODO: use proper type parsing function
      type = parser_parse_ternary(p);
    }

    AST_Id ty = type ? type->id : 0;

    if (parser_curr_tok(p).type == TOKEN_COLON) {
      parser_read_token(p, TOKEN_COLON);
      return ast_const_bind(&p->ast_man, parser_curr_tok(p), ty, root->id,
                            parser_parse_decl(p)->id);
    }

    if (parser_curr_tok(p).type == TOKEN_EQUAL) {
      parser_read_token(p, TOKEN_EQUAL);
      return ast_mut_bind(&p->ast_man, parser_curr_tok(p), type->id, root->id,
                          parser_parse_decl(p)->id);
    }

    parser_error(p, parser_curr_tok(p), "expecting '=' or ':'");
  }

  return root;
}

ASTNode *parser_parse(Parser *p) {
  if (parser_curr_tok(p).type == TOKEN_EOF) {
    return 0;
  }

  ASTNode *root = ast_decl_list(&p->ast_man, parser_curr_tok(p));

  root->left = parser_parse_expr(p)->id;

  ASTNode *tail = parser_parse(p);

  root->right = tail ? tail->id : 0;

  return root;
}
