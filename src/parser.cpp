#include "parser.hpp"
#include "ast.hpp"
#include "lexer.hpp"
#include "types.hpp"
#include <assert.h>

#include "error.hpp"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"

// void debug_print_token(Parser* p, Token t) {
// 	i8 buf[256];
// 	token_get_id(&p->lex, t, buf);
// 	printf("%s\n", buf);
// }

AST_Node* parser_parse_expr(Parser* p);
AST_Node* parser_parse_statements(Parser* p);
AST_Node* parser_parse_decl(Parser* p);
AST_Node* parser_parse_arrow(Parser* p);

void parser_init(Parser* p, u64 id, i8* buffer, u64 size) {
  lexer_init(&p->lex, id, buffer, size);
  ast_manager_init(&p->ast_man);
}

void parser_destroy(Parser* p) { lexer_destroy(&p->lex); }

Token parser_curr_tok(Parser* p) { return p->lex.curr; }

Token parser_prev_tok(Parser* p) { return p->lex.prev; }

Token parser_read_token(Parser* p, Token_Type kind) {
  if (p->lex.curr.type != kind) {
    char tmp[256];
    sprintf(tmp, "expecting Token %i", kind);
    parser_error(p, parser_curr_tok(p), tmp);
  }

  return lexer_read_token(&p->lex);
}

AST_Node* parser_parse_symbol(Parser* p) {
  Token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_ID);

  AST_Node* root = ast_symbol(&p->ast_man, tok);

  if (parser_curr_tok(p).type == TOKEN_APHOSTROPHE) {
    parser_read_token(p, TOKEN_APHOSTROPHE);
    return ast_teamplate_type_variable(&p->ast_man, tok, root);
  }

  return root;
}

AST_Node* parse_call_args(Parser* p) {
  parser_read_token(p, TOKEN_OPEN_PARENTHESIS);

  AST_Node* root = parser_parse_expr(p); // ast_call_arg_list(&p->ast_man, parser_curr_tok(p));
  if (root->kind != AST_DECL_ARGS_LIST) { root = ast_decl_args(&p->ast_man, root->tok, root, ast_node_null(&p->ast_man)); }

  // if (parser_curr_tok(p).type != TOKEN_CLOSE_PARENTHESIS) {

  //   AST_Node *tmp = root;

  //   tmp->left = parser_parse_expr(p)->id;

  // while (parser_curr_tok(p).type == TOKEN_COMMA) {
  //   parser_read_token(p, TOKEN_COMMA);

  //   AST_Node *next = ast_call_arg_list(&p->ast_man, parser_curr_tok(p));

  //   next->left = parser_parse_expr(p)->id;

  //   next->right = 0;

  //   tmp->right = next->id;

  //   tmp = next;
  // }
  //}

  parser_read_token(p, TOKEN_CLOSE_PARENTHESIS);

  return root;
}

AST_Node* parser_parse_struct(Parser* p) {
  Token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_KEYWORD_STRUCT);

  AST_Node* body = parser_parse_statements(p);

  return ast_type_struct(&p->ast_man, tok, body);
}

AST_Node* parser_parse_handler_literal(Parser* p) {
  Token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_KEYWORD_HANDLER);

  parser_read_token(p, TOKEN_OPEN_CURLY_BRACE);

  AST_Node* effs = ast_program_point(&p->ast_man, parser_curr_tok(p));

  AST_Node* list = effs;

  while (parser_curr_tok(p).type != TOKEN_CLOSE_CURLY_BRACE) {
    Token eff_tok = parser_curr_tok(p);

    AST_Node* eff = parser_parse_expr(p); // parser_parse_eff_decl(p)->id;

    list->left = eff->id;

    if (parser_curr_tok(p).type == TOKEN_CLOSE_CURLY_BRACE) { break; }

    AST_Node* tail = ast_program_point(&p->ast_man, parser_curr_tok(p));

    list->right = tail->id;

    list = tail;
  }

  parser_read_token(p, TOKEN_CLOSE_CURLY_BRACE);

  return ast_handler_literal(&p->ast_man, tok, effs, ast_node_null(&p->ast_man));
}

AST_Node* parser_parse_lit(Parser* p) {
  Token tok = parser_curr_tok(p);
  if (parser_curr_tok(p).type == TOKEN_KEYWORD_STRUCT) { return parser_parse_struct(p); }

  if (parser_curr_tok(p).type == TOKEN_KEYWORD_HANDLER) { return parser_parse_handler_literal(p); }

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

  if (tok.type == TOKEN_CLOSE_PARENTHESIS) { return ast_node_null(&p->ast_man); }

  parser_error(p, parser_curr_tok(p), "invalid literal declaration");

  return 0;
}

// PRIMARY → LITERAL | SYMBOL | '('EXPRESSION')'
AST_Node* parser_parse_primary(Parser* p) {
  if (parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, TOKEN_OPEN_PARENTHESIS);

    AST_Node* root = parser_parse_expr(p);

    parser_read_token(p, TOKEN_CLOSE_PARENTHESIS);

    if (root->kind == AST_DECL_ARGS_LIST || root->kind == AST_NULL_NODE || root->kind == AST_BIND_TYPE || root->kind == AST_BIND_CONSTANT
        || root->kind == AST_BIND_VARIABLE) {

      if (root->kind != AST_DECL_ARGS_LIST) { root = ast_decl_args(&p->ast_man, tok, root, ast_node_null(&p->ast_man)); }

      AST_Node* type = ast_node_null(&p->ast_man);

      if (parser_curr_tok(p).type == TOKEN_ARROW) {
        parser_read_token(p, TOKEN_ARROW);
        type = parser_parse_arrow(p);
      }

      AST_Node* body = parser_parse_statements(p);

      AST_Node* decl = ast_function_signature(&p->ast_man, tok, root, type);

      return ast_function_literal(&p->ast_man, tok, decl, body);
    }

    return root;
  }

  if (parser_curr_tok(p).type == TOKEN_ID) { return parser_parse_symbol(p); }

  return parser_parse_lit(p);
}

// UNARY → ('!' | '-' | '+' | '++' | '--' | '~' )UNARY | UNARY('++' | '--') |
// CALL
AST_Node* parser_parse_postfix_suffix_unary(Parser* p) {
  // TODO
  return parser_parse_primary(p);
}

AST_Node* parser_parse_call_args(Parser* p) {
  bool effectfull = false;

  Token tok      = parser_curr_tok(p);
  AST_Node* arg  = parse_call_args(p);
  AST_Node* tail = 0;

  if (parser_curr_tok(p).type == TOKEN_EXCLAMATION || parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) {

    if (parser_curr_tok(p).type == TOKEN_EXCLAMATION) {
      effectfull = true;
      parser_read_token(p, TOKEN_EXCLAMATION);
    }

    return ast_call(&p->ast_man, tok, arg, parser_parse_call_args(p), effectfull);
  }

  return arg;
}

AST_Node* parser_parse_call_tail(Parser* p, AST_Node* head) {
  b8 effectfull = false;

  if (parser_curr_tok(p).type == TOKEN_EXCLAMATION) {
    effectfull = true;

    parser_read_token(p, TOKEN_EXCLAMATION);
  }

  Token tok = parser_curr_tok(p);

  AST_Node* args = parse_call_args(p);

  AST_Node* call = ast_call(&p->ast_man, tok, head, args, effectfull);

  if (parser_curr_tok(p).type == TOKEN_EXCLAMATION || parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) { return parser_parse_call_tail(p, call); }

  return call;
}

AST_Node* parser_parse_call(Parser* p) {
  AST_Node* root = parser_parse_postfix_suffix_unary(p);

  b8 effectfull = false;

  if (parser_curr_tok(p).type == TOKEN_EXCLAMATION) {
    effectfull = true;

    parser_read_token(p, TOKEN_EXCLAMATION);
  }

  Token tok = parser_curr_tok(p);

  if (parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) {
    AST_Node* args = parse_call_args(p);

    AST_Node* call = ast_call(&p->ast_man, tok, root, args, effectfull);

    if (parser_curr_tok(p).type == TOKEN_EXCLAMATION || parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) { return parser_parse_call_tail(p, call); }

    if (parser_curr_tok(p).type == TOKEN_KEYWORD_WITH) {
      Token tok = parser_curr_tok(p);

      parser_read_token(p, TOKEN_KEYWORD_WITH);

      AST_Node* hnd = parser_parse_decl(p);

      return ast_with_handler(&p->ast_man, tok, call, hnd);
    }

    return call;
  }

  if (effectfull) { return ast_type_yield(&p->ast_man, tok, root); }

  return root;
}

AST_Node* parser_parse_array_access(Parser* p) {
  // TODO: here should be parser array access
  return parser_parse_call(p);
}

AST_Node* parser_parse_member_access(Parser* p) {
  AST_Node* root = parser_parse_array_access(p);

  if (parser_curr_tok(p).type == TOKEN_DOT) {
    parser_read_token(p, TOKEN_DOT);
    return ast_member_access(&p->ast_man, parser_curr_tok(p), root, parser_parse_member_access(p));
  }

  return root;
}

// UNARY → ('!' | '-' | '+' | '++' | '--' | '~' )UNARY | UNARY('++' | '--') |
// CALL
AST_Node* parser_parse_unary(Parser* p) {
  AST_Node* root = 0;

  if (parser_curr_tok(p).type == TOKEN_MINUS || parser_curr_tok(p).type == TOKEN_PLUS) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, tok.type);

    if (tok.type == TOKEN_MINUS) { return ast_una_sub(&p->ast_man, tok, root, parser_parse_unary(p)); }

    if (tok.type == TOKEN_PLUS) { return ast_una_add(&p->ast_man, tok, root, parser_parse_unary(p)); }
  }

  return parser_parse_member_access(p);
}

// FACTOR → UNARY (('/' | '*' | '%' ) FACTOR)*
AST_Node* parser_parse_factor(Parser* p) {
  AST_Node* root = parser_parse_unary(p);

  if (parser_curr_tok(p).type == TOKEN_SLASH || parser_curr_tok(p).type == TOKEN_ASTERISK) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, tok.type);

    if (tok.type == TOKEN_SLASH) { return ast_bin_div(&p->ast_man, tok, root, parser_parse_factor(p)); }

    if (tok.type == TOKEN_ASTERISK) { return ast_bin_mul(&p->ast_man, tok, root, parser_parse_factor(p)); }
  }

  return root;
}

// TERM → FACTOR (('+' | '-') TERM)*
AST_Node* parser_parse_term(Parser* p) {
  AST_Node* root = parser_parse_factor(p);
  if (parser_curr_tok(p).type == TOKEN_PLUS || parser_curr_tok(p).type == TOKEN_MINUS) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, TOKEN_PLUS);

    if (tok.type == TOKEN_PLUS) { return ast_bin_add(&p->ast_man, tok, root, parser_parse_term(p)); }

    if (tok.type == TOKEN_MINUS) { return ast_bin_sub(&p->ast_man, tok, root, parser_parse_term(p)); }
  }

  return root;
}

// SHIFT → TERM (( '>>' | '<<'  ) COMPARISON)*
AST_Node* parser_parse_bitwise_shift(Parser* p) {
  // TODO: bitwise shift should be parsed here
  return parser_parse_term(p);
}

// COMPARISON → SHIFT (( '>' | '>=' | '<' | '<='  ) COMPARISON)*
AST_Node* parser_parse_comparison(Parser* p) {
  // TODO: comparison expressions should go here
  AST_Node* root = parser_parse_bitwise_shift(p);

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
AST_Node* parser_parse_equality(Parser* p) {
  // TODO: equality comparisons expressions should go here
  AST_Node* root = parser_parse_comparison(p);

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
AST_Node* parser_parse_bitwise(Parser* p) {
  // TODO: bitwise expressions should go here
  AST_Node* root = parser_parse_equality(p);

  if (parser_curr_tok(p).type == TOKEN_PIPE) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, TOKEN_PIPE);

    return ast_type_union(&p->ast_man, tok, root, parser_parse_bitwise(p));
  }

  return root;
}

AST_Node* parser_parse_eff_type(Parser* p) {
  Token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_KEYWORD_EFFECT);

  AST_Node* type = parser_parse_arrow(p);

  parser_read_token(p, TOKEN_SEMI_COLON);

  ast_change_kind(type, AST_TYPE_EFFECT);

  return type;
}

// BOOL → TERM (( '&&' | '^^' | '||') BOOL)*
AST_Node* parser_parse_booleans(Parser* p) {
  if (parser_curr_tok(p).type == TOKEN_KEYWORD_EFFECT) { return parser_parse_eff_type(p); }

  AST_Node* root = parser_parse_bitwise(p);
  // TODO
  return root;
}

AST_Node* parser_parse_arrow(Parser* p) {
  // b8 resume = false;

  Token tok = parser_curr_tok(p);

  // if (parser_curr_tok(p).type == TOKEN_KEYWORD_RESUME) {
  //   parser_read_token(p, TOKEN_KEYWORD_RESUME);
  //   resume = true;
  // }

  AST_Node* root = parser_parse_booleans(p);

  AST_Node* tmp = root;

  AST_Node* head = NULL;
  // printf("asdas %i\n", resume);
  // print_ast(p, root);

  if (/*resume ||*/ parser_curr_tok(p).type == TOKEN_ARROW) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, TOKEN_ARROW);

    AST_Node* tail = parser_parse_arrow(p);

    tmp = ast_type_arrow(&p->ast_man, tok, tmp, tail);

    if (head == NULL) { head = tmp; }
  }

  root = head ? head : root;

  // if (resume == true) {
  //   ast_change_kind(root, AST_RESUME_TYPE_DECL);
  //   // 	parser_error(p, tok, "resume expects arrow type");
  // }

  return root;
}

AST_Node* parser_parse_if_statement(Parser* p) {
  Token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_KEYWORD_IF);

  AST_Node* cond = parser_parse_expr(p);
  AST_Node* body = ast_node_null(&p->ast_man);

  if (parser_curr_tok(p).type == TOKEN_KEYWORD_THEN) {
    parser_read_token(p, TOKEN_KEYWORD_THEN);

    body = parser_parse_expr(p);
  } else {
    body = parser_parse_statements(p);
  }

  AST_Node* cont = ast_node_null(&p->ast_man);

  if (parser_curr_tok(p).type == TOKEN_KEYWORD_ELSE) {
    parser_read_token(p, TOKEN_KEYWORD_ELSE);

    if (parser_curr_tok(p).type == TOKEN_OPEN_CURLY_BRACE) {
      cont = parser_parse_statements(p);
    } else {
      cont = parser_parse_expr(p);
    }
  }

  return ast_ctrl_flow_if(&p->ast_man, tok, cond, body, cont);
}

AST_Node* parser_parse_ret_statement(Parser* p) {
  Token tok = parser_curr_tok(p);
  parser_read_token(p, TOKEN_KEYWORD_RETURN);
  AST_Node* ret = ast_ctrl_flow_ret(&p->ast_man, tok, parser_parse_expr(p));

  if (parser_prev_tok(p).type != TOKEN_CLOSE_CURLY_BRACE) { parser_read_token(p, TOKEN_SEMI_COLON); }

  return ret;
}

AST_Node* parser_parse_statement(Parser* p) {
  if (parser_curr_tok(p).type == TOKEN_KEYWORD_IF) { return parser_parse_if_statement(p); }

  if (parser_curr_tok(p).type == TOKEN_KEYWORD_RETURN) { return parser_parse_ret_statement(p); }

  AST_Node* expr = parser_parse_expr(p);

  if (parser_prev_tok(p).type != TOKEN_CLOSE_CURLY_BRACE) {
    parser_read_token(p, TOKEN_SEMI_COLON);

    while (parser_curr_tok(p).type == TOKEN_SEMI_COLON) {
      parser_read_token(p, TOKEN_SEMI_COLON);
    }
  }

  return expr;
}

AST_Node* parser_parse_statements(Parser* p) {

  parser_read_token(p, TOKEN_OPEN_CURLY_BRACE);

  AST_Node* stmt = ast_program_point(&p->ast_man, parser_curr_tok(p));

  AST_Node* tmp = stmt;

  while (parser_curr_tok(p).type != TOKEN_CLOSE_CURLY_BRACE) {
    tmp->left = parser_parse_statement(p)->id;

    if (parser_curr_tok(p).type == TOKEN_CLOSE_CURLY_BRACE) { break; }

    AST_Node* r = ast_program_point(&p->ast_man, parser_curr_tok(p));
    tmp->right  = r->id;
    tmp         = r;
  }

  parser_read_token(p, TOKEN_CLOSE_CURLY_BRACE);

  return stmt;
}

AST_Node* parser_parse_decl(Parser* p) { return parser_parse_arrow(p); }

AST_Node* parser_parse_type_bind(Parser* p) {
  AST_Node* a = parser_parse_decl(p);

  if (parser_curr_tok(p).type == TOKEN_COLON) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, TOKEN_COLON);

    if (parser_curr_tok(p).type == TOKEN_COLON || parser_curr_tok(p).type == TOKEN_EQUAL) { return ast_type_bind(&p->ast_man, tok, a, ast_node_null(&p->ast_man)); }

    AST_Node* b = parser_parse_arrow(p);

    return ast_type_bind(&p->ast_man, tok, a, b);
  }

  return a;
}

// EXPRESSION → IDENTIFIER ('=' | '|=' | '&=' | '+=' | '-=' ) ASSIGNMENT |
// EQUALITY | EXPRESSION ? EXPRESSION : EXPRESSION
AST_Node* parser_parse_expr(Parser* p) {
  AST_Node* root = parser_parse_type_bind(p);

  // assignment a = b
  if (parser_curr_tok(p).type == TOKEN_EQUAL) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, TOKEN_EQUAL);

    if (root->kind == AST_BIND_TYPE) { return ast_variable_bind(&p->ast_man, parser_curr_tok(p), root, parser_parse_type_bind(p)); }

    return ast_assignment(&p->ast_man, parser_curr_tok(p), root, parser_parse_type_bind(p));
  }

  // assignment a : b
  if (parser_curr_tok(p).type == TOKEN_COLON) {
    if (root->kind != AST_BIND_TYPE) { parser_error(p, root->tok, "expecting type bind before const assignment"); }

    Token tok = parser_curr_tok(p);

    parser_read_token(p, TOKEN_COLON);

    return ast_constant_bind(&p->ast_man, tok, root, parser_parse_type_bind(p));
  }

  if (parser_curr_tok(p).type == TOKEN_COMMA) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, TOKEN_COMMA);

    AST_Node* tail = parser_parse_expr(p);

    if (tail->kind != AST_DECL_ARGS_LIST) { tail = ast_decl_args(&p->ast_man, tail->tok, tail, ast_node_null(&p->ast_man)); }

    return ast_decl_args(&p->ast_man, tok, root, tail);
  }

  return root;
}

AST_Node* parser_parse(Parser* p) {
  if (parser_curr_tok(p).type == TOKEN_EOF) { return ast_manager_get(&p->ast_man, p->ast_man.statements_list_root_id); }

  // AST_Node *root = ast_decl_list(&p->ast_man, parser_curr_tok(p));

  ast_manager_push_decl(&p->ast_man, parser_parse_expr(p));

  // root->left = parser_parse_expr(p)->id;

  return parser_parse(p);
}

void print_ast_rec(i8* prefix, Parser* p, AST_Node* a, b8 is_left, b8 f = 1) {
  printf("%s", prefix);

  if (is_left) {
    if (f) printf("├──");
    else printf("   ");
  } else {
    if (f) printf("└──");
    else printf("   ");
  }

  if (ast_is_null_node(a)) {
    printf("NULL\n");
    return;
  }

  i8* st = ast_kind_to_cstr(a->kind);

  if (a->kind >= __AST_KIND_END) printf("AST:{ temporary: %s", st);
  else printf("AST:{ kind: %s", st);

  free(st);

  if (a->kind == AST_NATURAL_LITERAL) { printf(",val: %lu", a->tok.buf); }

  if (a->kind == AST_SYMBOL_LITERAL) {
    i8 buff[a->tok.size + 1];

    token_get_id(&p->lex, a->tok, buff);

    printf(",sym: '%s'", buff);
  }

  printf(" }\n");

  if (a->left == 0 && a->right == 0) return;

  // printf("%*c|__", tabs, ' ');
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

b8 parser_is_same_symbol(Parser* p, AST_Node* a, AST_Node* b) {
  if (a->kind != b->kind) return false;

  if (ast_is_temporary(&p->ast_man, a) && ast_is_temporary(&p->ast_man, b)) { return a->kind == b->kind; }

  assert(a->kind == AST_SYMBOL_LITERAL);
  assert(b->kind == AST_SYMBOL_LITERAL);

  Token t0 = a->tok;
  Token t1 = b->tok;

  if (t0.size != t1.size) return false;
  if (t0.buf != t1.buf) return false;

  for (u64 i = 0; i < t0.size; i++) {
    if (p->lex.file_buf[t0.pos + i] != p->lex.file_buf[t1.pos + i]) { return false; }
  }

  return true;
}

void print_ast_to_program(Parser* p, AST_Node* n, u32 scope) {

  if (ast_is_temporary(&p->ast_man, n)) {
    i8* st = ast_kind_to_cstr(n->kind);
    printf("%%%s", st);
    free(st);
    return;
  }

  if (n->kind == AST_NATURAL_LITERAL) {
    printf("%lu", n->tok.buf);
    return;
  }

  if (n->kind == AST_SYMBOL_LITERAL) {
    i8 buff[n->tok.size + 1];

    token_get_id(&p->lex, n->tok, buff);

    printf("%s", buff);
    return;
  }

  if (n->kind == AST_DECL_ARGS_LIST) {
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->left), scope);
    if (n->right) printf(", ");
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->right), scope);
    return;
  }
  if (n->kind == AST_TYPE_UNIT) {
    printf("unit");
    return;
  }

  if (n->kind == AST_TYPE_I32) {
    printf("i32");
    return;
  }
  if (n->kind == AST_TYPE_ARROW) {
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->left), scope);
    printf(" -> ");
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->right), scope);
    return;
  }

  if (n->kind == AST_TYPE_ANY) {
    printf("any");
    return;
  }

  if (n->kind == AST_FUN_SIGNATURE) {
    printf("(");
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->left), scope);
    printf(")");
    if (n->right) printf(" -> ");
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->right), scope);
    return;
  }

  if (n->kind == AST_FUNCTION_LITERAL) {
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->left), scope);
    printf(" {\n");
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->right), scope + 2);

    for (i32 i = 0; i < scope; i++)
      printf(" ");
    printf("}\n");
    return;
  }

  if (n->kind == AST_PROGRAM_POINT) {
    for (i32 i = 0; i < scope; i++)
      printf(" ");

    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->left), scope);
    printf("\n");
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->right), scope);

    return;
  }

  if (n->kind == AST_BIND_CONSTANT) {
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->left), scope);
    printf(": ");
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->right), scope);
    printf("");
    return;
  }

  if (n->kind == AST_BIND_VARIABLE) {
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->left), scope);
    printf("= ");
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->right), scope);
    printf("");
    return;
  }

  if (n->kind == AST_BIND_TYPE) {
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->left), scope);
    if (n->right) printf(" : ");
    else printf(" :");

    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->right), scope);
    return;
  }

  if (n->kind == AST_OP_BIN_ASSIGN) {
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->left), scope);
    printf(" = ");
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->right), scope);
    printf("");
    return;
  }

  if (n->kind == AST_FUNCTION_CALL) {
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->left), scope);
    printf("(");
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->right), scope);
    printf(")");
    return;
  }

  if (n->kind == AST_CTRL_FLOW_RETURN) {
    printf("return ");
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->left), scope);
    printf("");
    return;
  }

  if (n->kind == AST_CTRL_FLOW_IF) {
    printf("if ");
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->left), scope);
    printf(" {\n");
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->right), scope + 2);

    for (i32 i = 0; i < scope; i++)
      printf(" ");
    printf("}");
    return;
  }

  if (n->kind == AST_CTRL_FLOW_IF_ELSE) {
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->left), scope);
    printf(" else {\n");
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->right), scope + 2);
    for (i32 i = 0; i < scope; i++)
      printf(" ");
    printf("}");
    return;
  }

  if (n->kind == AST_OP_BIN_ASSIGN) {
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->left), scope);
    printf(" = ");
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->right), scope);
    return;
  }

  if (n->kind == AST_OP_BIN_ADD) {
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->left), scope);
    printf(" + ");
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->right), scope);
    return;
  }

  if (n->kind == AST_OP_BIN_SUB) {
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->left), scope);
    printf(" - ");
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->right), scope);
    return;
  }

  if (n->kind == AST_OP_BIN_MUL) {
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->left), scope);
    printf(" * ");
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->right), scope);
    return;
  }

  if (ast_is_binary_operation(n)) {
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->left), scope);
    printf(" op ");
    print_ast_to_program(p, ast_manager_get_relative(&p->ast_man, n, n->right), scope);
    return;
  }
}
