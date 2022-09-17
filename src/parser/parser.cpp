#include "parser.hpp"

#include "ast.hpp"
#include "ast/ast_control_flow.hpp"
#include "ast/ast_declaration.hpp"
#include "ast/ast_function.hpp"
#include "ast/ast_literals.hpp"
#include "ast/ast_manager.hpp"
#include "ast/ast_operations.hpp"
#include "ast/ast_program_point.hpp"
#include "ast/ast_types.hpp"

#include "compiler/symbol_table.hpp"
#include "lexer.hpp"
#include "types.hpp"

#include <assert.h>

#include "stdio.h"
#include "stdlib.h"
#include "string.h"

using namespace compiler;

namespace parser {

void parser_error(Parser* p, Token tok, const i8* msg) {
  printf("Error: '%s' at line %u and column %u!\n", msg, tok.row, tok.col);
  exit(1);
}

void debug_print_token(Parser* p, Token t) {
  i8 buf[256];
  token_get_id(p->lexer, t, buf);
  printf("%s\n", buf);
}

ast::Node* parser_parse_expr(Parser* p);
ast::Node* parser_parse_statements(Parser* p);
ast::Node* parser_parse_decl(Parser* p);
ast::Node* parser_parse_arrow(Parser* p);

Parser* parser_create(u64 id, i8* buffer, u64 size) {
  Parser* p = new Parser();

  p->ast_manager = ast::manager_create();

  lexer_init(p->lexer, id, buffer, size);

  return p;
}

void parser_destroy(Parser* p) {
  ast::manager_destroy(p->ast_manager);
  lexer_destroy(p->lexer);
  delete p;
}

Token parser_curr_tok(Parser* p) {
  return p->lexer->curr;
}

Token parser_prev_tok(Parser* p) {
  return p->lexer->prev;
}

Token parser_read_token(Parser* p, Token_Type kind) {
  if (p->lexer->curr.type != kind) {
    char tmp[256];
    sprintf(tmp, "expecting Token %i", kind);
    parser_error(p, parser_curr_tok(p), tmp);
  }

  return lexer_read_token(p->lexer);
}

ast::Node* parser_parse_symbol(Parser* p) {
  Token token = parser_curr_tok(p);

  parser_read_token(p, TOKEN_ID);

  ast::Literal_Symbol_Node* root = ast::create_node_literal_symbol(p, symbol::from_token(p->symbol_table, p->lexer, token));

  if (parser_curr_tok(p).type == TOKEN_APHOSTROPHE) {
    parser_read_token(p, TOKEN_APHOSTROPHE);
    return ast::create_node_type_variable(p, root);
  }

  return root;
}

ast::Node* parse_call_args(Parser* p) {
  parser_read_token(p, TOKEN_OPEN_PARENTHESIS);

  ast::Node* root = parser_parse_expr(p);

  if (!ast::is_instance< ast::Declarations_List_Node* >(root)) {
    root = ast::create_node_declarations_list(p, root, NULL);
  }

  parser_read_token(p, TOKEN_CLOSE_PARENTHESIS);

  return root;
}

ast::Node* parser_parse_struct(Parser* p) {
  Token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_KEYWORD_STRUCT);

  ast::Node* body = parser_parse_statements(p);

  return ast::create_node_literal_struct(p, ast::is_instance< ast::ProgramPoint_List_Node* >(body));
}

// ast::Node* parser_parse_handler_literal(Parser* p) {
//   Token tok = parser_curr_tok(p);

//   parser_read_token(p, TOKEN_KEYWORD_HANDLER);

//   parser_read_token(p, TOKEN_OPEN_CURLY_BRACE);

//   ast::Node* effs = ast::create_node_program_point(p, 0, 0);

//   ast::Node* list = effs;

//   while (parser_curr_tok(p).type != TOKEN_CLOSE_CURLY_BRACE) {
//     Token eff_tok = parser_curr_tok(p);

//     ast::Node* eff = parser_parse_expr(p); // parser_parse_eff_decl(p)->id;

//     list->left = eff->id;

//     if (parser_curr_tok(p).type == TOKEN_CLOSE_CURLY_BRACE) {
//       break;
//     }

//     ast::Node* tail = ast::create_node_program_point(p, 0, 0);

//     list->right = tail->id;

//     list = tail;
//   }

//   parser_read_token(p, TOKEN_CLOSE_CURLY_BRACE);

//   return ast::handler_literal(p->ast_manager, tok, effs, ast::node_null(p->ast_manager));
// }

ast::Node* parser_parse_lit(Parser* p) {
  Token tok = parser_curr_tok(p);
  if (parser_curr_tok(p).type == TOKEN_KEYWORD_STRUCT) {
    return parser_parse_struct(p);
  }

  // if (parser_curr_tok(p).type == TOKEN_KEYWORD_HANDLER) {
  //   return parser_parse_handler_literal(p);
  // }

  if (tok.type == TOKEN_I32_LIT) {
    parser_read_token(p, TOKEN_I32_LIT);
    return ast::create_node_literal_int32(p, symbol::from_token(p->symbol_table, p->lexer, tok));
  }

  if (tok.type == TOKEN_UNIT) {
    parser_read_token(p, TOKEN_UNIT);
    return ast::create_node_type_unit(p);
  }

  if (tok.type == TOKEN_I32) {
    parser_read_token(p, TOKEN_I32);
    return ast::create_node_type_i32(p);
  }

  if (tok.type == TOKEN_TYPE) {
    parser_read_token(p, TOKEN_TYPE);
    return ast::create_node_type_any(p);
  }

  if (tok.type == TOKEN_CLOSE_PARENTHESIS) {
    return ast::create_node_literal_nothing(p);
  }

  parser_error(p, parser_curr_tok(p), "invalid literal declaration");

  return 0;
}

// PRIMARY → LITERAL | SYMBOL | '('EXPRESSION')'
ast::Node* parser_parse_primary(Parser* p) {
  if (parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, TOKEN_OPEN_PARENTHESIS);

    ast::Node* root = parser_parse_expr(p);

    parser_read_token(p, TOKEN_CLOSE_PARENTHESIS);

    if (ast::is_instance< ast::Declarations_List_Node* >(root) || ast::is_instance< ast::Declaration_Constant_Node* >(root)
        || ast::is_instance< ast::Declaration_Variable_Node* >(root)) {

      ast::Declarations_List_Node* arguments = NULL;

      if (!ast::is_instance< ast::Declarations_List_Node* >(root)) {
        arguments = ast::create_node_declarations_list(p, root, NULL);
      } else {
        arguments = ast::as< ast::Declarations_List_Node* >(root);
      }

      ast::Node* type = ast::create_node_literal_undefined(p);

      if (parser_curr_tok(p).type == TOKEN_ARROW) {
        parser_read_token(p, TOKEN_ARROW);

        type = parser_parse_arrow(p);
      }

      ast::Node* body = parser_parse_statements(p);

      if (ast::is_instance< ast::ProgramPoint_List_Node* >(body) || ast::is_instance< ast::Literal_Nothing_Node* >(body)) {
        parser_error(p, p->lexer->curr, "Expecting function body");
      }

      return ast::create_node_function_literal(p, arguments, type, ast::is_instance< ast::ProgramPoint_List_Node* >(body));
    }

    return root;
  }

  if (parser_curr_tok(p).type == TOKEN_ID) {
    return parser_parse_symbol(p);
  }

  return parser_parse_lit(p);
}

// UNARY → ('!' | '-' | '+' | '++' | '--' | '~' )UNARY | UNARY('++' | '--') |
// CALL
ast::Node* parser_parse_postfix_suffix_unary(Parser* p) {
  // TODO
  return parser_parse_primary(p);
}

ast::Node* parser_parse_call_args(Parser* p) {
  bool effectfull = false;

  Token tok = parser_curr_tok(p);

  ast::Node* expr = parse_call_args(p);
  ast::Node* tail = 0;

  if (parser_curr_tok(p).type == TOKEN_EXCLAMATION || parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) {

    if (parser_curr_tok(p).type == TOKEN_EXCLAMATION) {
      effectfull = true;
      parser_read_token(p, TOKEN_EXCLAMATION);
    }

    ast::Node* arguments = parser_parse_call_args(p);

    if (!ast::is_instance< ast::Declarations_List_Node* >(arguments)) {
      parser_error(p, p->lexer->curr, "Expecting arguments");
    }

    return ast::create_node_function_call(p, expr, ast::as< ast::Declarations_List_Node* >(arguments));
  }

  return expr;
}

ast::Node* parser_parse_call_tail(Parser* p, ast::Node* head) {
  b8 effectfull = false;

  if (parser_curr_tok(p).type == TOKEN_EXCLAMATION) {
    effectfull = true;

    parser_read_token(p, TOKEN_EXCLAMATION);
  }

  Token tok = parser_curr_tok(p);

  ast::Node* arguments = parse_call_args(p);

  if (!ast::is_instance< ast::Declarations_List_Node* >(arguments)) {
    parser_error(p, p->lexer->curr, "Expecting arguments");
  }

  ast::Node* call = ast::create_node_function_call(p, head, ast::as< ast::Declarations_List_Node* >(arguments));

  if (parser_curr_tok(p).type == TOKEN_EXCLAMATION || parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) {
    return parser_parse_call_tail(p, call);
  }

  return call;
}

ast::Node* parser_parse_call(Parser* p) {
  ast::Node* root = parser_parse_postfix_suffix_unary(p);

  b8 effectfull = false;

  if (parser_curr_tok(p).type == TOKEN_EXCLAMATION) {
    effectfull = true;

    parser_read_token(p, TOKEN_EXCLAMATION);
  }

  Token tok = parser_curr_tok(p);

  if (parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) {
    ast::Node* arguments = parse_call_args(p);

    if (!ast::is_instance< ast::Declarations_List_Node* >(arguments)) {
      parser_error(p, p->lexer->curr, "Expecting arguments");
    }

    ast::Node* call = ast::create_node_function_call(p, root, ast::as< ast::Declarations_List_Node* >(arguments));

    if (parser_curr_tok(p).type == TOKEN_EXCLAMATION || parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) {
      return parser_parse_call_tail(p, call);
    }

    // if (parser_curr_tok(p).type == TOKEN_KEYWORD_WITH) {
    //   Token tok = parser_curr_tok(p);

    //   parser_read_token(p, TOKEN_KEYWORD_WITH);

    //   ast::Node* hnd = parser_parse_decl(p);

    //   return ast::with_handler(p->ast_manager, tok, call, hnd);
    // }

    return call;
  }

  // if (effectfull) {
  //   return ast::type_yield(p->ast_manager, tok, root);
  // }

  return root;
}

ast::Node* parser_parse_array_access(Parser* p) {
  // TODO: here should be parser array access
  return parser_parse_call(p);
}

ast::Node* parser_parse_member_access(Parser* p) {
  ast::Node* root = parser_parse_array_access(p);

  if (parser_curr_tok(p).type == TOKEN_DOT) {
    parser_read_token(p, TOKEN_DOT);
    return ast::create_node_member_access(p, root, parser_parse_member_access(p));
  }

  return root;
}

// UNARY → ('!' | '-' | '+' | '++' | '--' | '~' )UNARY | UNARY('++' | '--') |
// CALL
ast::Node* parser_parse_unary(Parser* p) {
  ast::Node* root = 0;

  // if (parser_curr_tok(p).type == TOKEN_MINUS || parser_curr_tok(p).type == TOKEN_PLUS) {
  //   Token tok = parser_curr_tok(p);

  //   parser_read_token(p, tok.type);

  //   if (tok.type == TOKEN_MINUS) {
  //     return ast::una_sub(p->ast_manager, tok, root, parser_parse_unary(p));
  //   }

  //   if (tok.type == TOKEN_PLUS) {
  //     return ast::una_add(p->ast_manager, tok, root, parser_parse_unary(p));
  //   }
  // }

  return parser_parse_member_access(p);
}

// FACTOR → UNARY (('/' | '*' | '%' ) FACTOR)*
ast::Node* parser_parse_factor(Parser* p) {
  ast::Node* root = parser_parse_unary(p);

  if (parser_curr_tok(p).type == TOKEN_SLASH || parser_curr_tok(p).type == TOKEN_ASTERISK) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, tok.type);

    if (tok.type == TOKEN_SLASH) {
      return ast::create_node_arithmetic_div(p, root, parser_parse_factor(p));
    }

    if (tok.type == TOKEN_ASTERISK) {
      return ast::create_node_arithmetic_mul(p, root, parser_parse_factor(p));
    }
  }

  return root;
}

// TERM → FACTOR (('+' | '-') TERM)*
ast::Node* parser_parse_term(Parser* p) {
  ast::Node* root = parser_parse_factor(p);
  if (parser_curr_tok(p).type == TOKEN_PLUS || parser_curr_tok(p).type == TOKEN_MINUS) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, TOKEN_PLUS);

    if (tok.type == TOKEN_PLUS) {
      return ast::create_node_arithmetic_add(p, root, parser_parse_term(p));
    }

    if (tok.type == TOKEN_MINUS) {
      return ast::create_node_arithmetic_sub(p, root, parser_parse_term(p));
    }
  }

  return root;
}

// SHIFT → TERM (( '>>' | '<<'  ) COMPARISON)*
ast::Node* parser_parse_bitwise_shift(Parser* p) {
  // TODO: bitwise shift should be parsed here
  return parser_parse_term(p);
}

// COMPARISON → SHIFT (( '>' | '>=' | '<' | '<='  ) COMPARISON)*
ast::Node* parser_parse_comparison(Parser* p) {
  // TODO: comparison expressions should go here
  ast::Node* root = parser_parse_bitwise_shift(p);

  if (parser_curr_tok(p).type == TOKEN_GREATER) {
    Token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_GREATER);

    return ast::create_node_logical_greater_than(p, root, parser_parse_comparison(p));
  }

  if (parser_curr_tok(p).type == TOKEN_SMALLER) {
    Token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_SMALLER);

    return ast::create_node_logical_less_than(p, root, parser_parse_comparison(p));
  }

  if (parser_curr_tok(p).type == TOKEN_GREATER_EQUAL) {
    Token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_GREATER_EQUAL);

    return ast::create_node_logical_greater_equal_than(p, root, parser_parse_comparison(p));
  }

  if (parser_curr_tok(p).type == TOKEN_SMALLER_EQUAL) {
    Token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_SMALLER_EQUAL);

    return ast::create_node_logical_less_equal_than(p, root, parser_parse_comparison(p));
  }

  return root;
}

// EQUALITY → COMPARISON (('!=' | '==') EQUALITY)*
ast::Node* parser_parse_equality(Parser* p) {
  // TODO: equality comparisons expressions should go here
  ast::Node* root = parser_parse_comparison(p);

  if (parser_curr_tok(p).type == TOKEN_EXCLAMATION_EQUAL) {
    Token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_EXCLAMATION_EQUAL);

    return ast::create_node_logical_not_equal_to(p, root, parser_parse_equality(p));
  }

  if (parser_curr_tok(p).type == TOKEN_EQUAL_EQUAL) {
    Token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_EQUAL_EQUAL);

    return ast::create_node_logical_equal_to(p, root, parser_parse_equality(p));
  }

  return root;
}

// BITWISE → BOOL (( '&' | '|' | '^'  ) BITWISE)*
ast::Node* parser_parse_bitwise(Parser* p) {
  // TODO: bitwise expressions should go here
  ast::Node* root = parser_parse_equality(p);

  // if (parser_curr_tok(p).type == TOKEN_PIPE) {
  //   Token tok = parser_curr_tok(p);

  //   parser_read_token(p, TOKEN_PIPE);

  //   return ast::type_union(p->ast_manager, tok, root, parser_parse_bitwise(p));
  // }

  return root;
}

// ast::Node* parser_parse_eff_type(Parser* p) {
//   Token tok = parser_curr_tok(p);

//   parser_read_token(p, TOKEN_KEYWORD_EFFECT);

//   ast::Node* type = parser_parse_arrow(p);

//   parser_read_token(p, TOKEN_SEMI_COLON);

//   ast::change_kind(type, AST::TYPE_EFFECT);

//   return type;
// }

// BOOL → TERM (( '&&' | '^^' | '||') BOOL)*
ast::Node* parser_parse_booleans(Parser* p) {
  // if (parser_curr_tok(p).type == TOKEN_KEYWORD_EFFECT) {
  //   return parser_parse_eff_type(p);
  // }

  ast::Node* root = parser_parse_bitwise(p);
  // TODO
  return root;
}

ast::Node* parser_parse_arrow(Parser* p) {
  // b8 resume = false;

  Token tok = parser_curr_tok(p);

  // if (parser_curr_tok(p).type == TOKEN_KEYWORD_RESUME) {
  //   parser_read_token(p, TOKEN_KEYWORD_RESUME);
  //   resume = true;
  // }

  ast::Node* root = parser_parse_booleans(p);

  ast::Node* tmp = root;

  ast::Node* head = NULL;
  // printf("asdas %i\n", resume);
  // print_ast(p, root);

  if (/*resume ||*/ parser_curr_tok(p).type == TOKEN_ARROW) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, TOKEN_ARROW);

    ast::Node* tail = parser_parse_arrow(p);

    tmp = ast::create_node_type_arrow(p, tmp, tail);

    if (head == NULL) {
      head = tmp;
    }
  }

  root = head ? head : root;

  // if (resume == true) {
  //   ast::change_kind(root, AST::RESUME_TYPE_DECL);
  //   // 	parser_error(p, tok, "resume expects arrow type");
  // }

  return root;
}

ast::Node* parser_parse_if_statement(Parser* p) {
  Token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_KEYWORD_IF);

  ast::Node* cond = parser_parse_expr(p);

  ast::Node* body = NULL;

  if (parser_curr_tok(p).type == TOKEN_KEYWORD_THEN) {
    parser_read_token(p, TOKEN_KEYWORD_THEN);

    body = parser_parse_expr(p);
  } else {
    body = parser_parse_statements(p);
  }

  ast::Node* cont = NULL;

  if (parser_curr_tok(p).type == TOKEN_KEYWORD_ELSE) {
    parser_read_token(p, TOKEN_KEYWORD_ELSE);

    if (parser_curr_tok(p).type == TOKEN_OPEN_CURLY_BRACE) {
      cont = parser_parse_statements(p);
    } else {
      cont = parser_parse_expr(p);
    }
  }

  ast::Node* statement = ast::create_node_if_statement(p, cond, body);

  if (cont) {
    assert(ast::is_instance< ast::If_Node_Statement* >(statement));

    ast::Node* elif = cont;

    if (!ast::is_instance< ast::Elif_List_Node* >(elif)) {

      if (!ast::is_instance< ast::If_Node_Statement* >(elif)) {
        elif = ast::create_node_if_statement(p, ast::create_node_literal_true(p), elif);
      }

      elif = ast::create_node_elif_list(p, ast::as< ast::If_Node_Statement* >(elif), NULL);
    }

    return ast::create_node_elif_list(p, ast::as< ast::If_Node_Statement* >(statement), ast::as< ast::Elif_List_Node* >(elif));
  }

  return statement;
}

ast::Node* parser_parse_ret_statement(Parser* p) {
  Token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_KEYWORD_RETURN);

  ast::Node* ret = ast::create_node_return_statement(p, parser_parse_expr(p));

  if (parser_prev_tok(p).type != TOKEN_CLOSE_CURLY_BRACE) {
    parser_read_token(p, TOKEN_SEMI_COLON);
  }

  return ret;
}

ast::Node* parser_parse_statement(Parser* p) {
  if (parser_curr_tok(p).type == TOKEN_KEYWORD_IF) {
    return parser_parse_if_statement(p);
  }

  if (parser_curr_tok(p).type == TOKEN_KEYWORD_RETURN) {
    return parser_parse_ret_statement(p);
  }

  ast::Node* expr = parser_parse_expr(p);

  if (parser_prev_tok(p).type != TOKEN_CLOSE_CURLY_BRACE) {
    parser_read_token(p, TOKEN_SEMI_COLON);

    while (parser_curr_tok(p).type == TOKEN_SEMI_COLON) {
      parser_read_token(p, TOKEN_SEMI_COLON);
    }
  }

  return expr;
}

ast::Node* parser_parse_statements(Parser* p) {

  parser_read_token(p, TOKEN_OPEN_CURLY_BRACE);

  ast::Node* root = ast::create_node_program_point(p, NULL, NULL);

  ast::Node* iterator = root;

  while (parser_curr_tok(p).type != TOKEN_CLOSE_CURLY_BRACE) {
    iterator->left = parser_parse_statement(p)->id;

    if (parser_curr_tok(p).type == TOKEN_CLOSE_CURLY_BRACE) {
      break;
    }

    ast::Node* r = ast::create_node_program_point(p, NULL, NULL);

    iterator->right = r->id;
    iterator        = r;
  }

  parser_read_token(p, TOKEN_CLOSE_CURLY_BRACE);

  return root;
}

ast::Node* parser_parse_decl(Parser* p) {
  return parser_parse_arrow(p);
}

ast::Node* parser_parse_assignment(Parser* p) {
  Token root_token = parser_curr_tok(p);

  ast::Node* root = parser_parse_decl(p);

  if (parser_curr_tok(p).type == TOKEN_COLON) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, TOKEN_COLON);

    ast::Node* type = (parser_curr_tok(p).type != TOKEN_COLON && parser_curr_tok(p).type != TOKEN_EQUAL) ? parser_parse_arrow(p)
                                                                                                         : ast::create_node_literal_nothing(p);
    if (parser_curr_tok(p).type == TOKEN_COLON) {
      if (!ast::is_instance< ast::Literal_Symbol_Node* >(root)) {
        parser_error(p, root_token, "Constant identifier is not a symbol!");
      }

      parser_read_token(p, TOKEN_COLON);

      root = ast::create_constant_declaration(p, ast::as< ast::Literal_Symbol_Node* >(root), type);
    }

    if (parser_curr_tok(p).type == TOKEN_EQUAL) {
      if (!ast::is_instance< ast::Literal_Symbol_Node* >(root)) {
        parser_error(p, root_token, "Variable identifier is not a symbol!");
      }

      parser_read_token(p, TOKEN_EQUAL);

      root = ast::create_variable_declaration(p, ast::as< ast::Literal_Symbol_Node* >(root), type);
    }
  }

  if (parser_curr_tok(p).type == TOKEN_EQUAL) {
    parser_read_token(p, TOKEN_EQUAL);

    ast::Node* expression = parser_parse_decl(p);

    root = ast::create_node_assignment(p, root, expression);
  }

  return root;
}

// EXPRESSION → IDENTIFIER ('=' | '|=' | '&=' | '+=' | '-=' ) ASSIGNMENT |
// EQUALITY | EXPRESSION ? EXPRESSION : EXPRESSION
ast::Node* parser_parse_expr(Parser* p) {
  ast::Node* root = parser_parse_assignment(p);

  if (parser_curr_tok(p).type == TOKEN_COMMA) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, TOKEN_COMMA);

    ast::Node* tail = parser_parse_expr(p);

    if (!ast::is_instance< ast::Declarations_List_Node* >(tail)) {
      tail = ast::create_node_declarations_list(p, tail, NULL);
    }

    return ast::create_node_declarations_list(p, root, ast::as< ast::Declarations_List_Node* >(tail));
  }

  return root;
}

ast::Node* parser_parse(Parser* p) {
  if (parser_curr_tok(p).type == TOKEN_EOF) {
    return ast::manager_get(p->ast_manager, p->ast_manager->statements_list_root_id);
  }

  // ast::Node *root = ast::decl_list(p->ast_manager, parser_curr_tok(p));

  ast::manager_push_decl(p->ast_manager, parser_parse_expr(p));

  // root->left = parser_parse_expr(p)->id;

  return parser_parse(p);
}

void print_ast_rec(i8* prefix, Parser* p, ast::Node* a, b8 is_left, b8 f = 1) {
  printf("%s", prefix);

  if (is_left) {
    if (f)
      printf("├──");
    else
      printf("   ");
  } else {
    if (f)
      printf("└──");
    else
      printf("   ");
  }

  if (ast::is_instance< ast::Literal_Nothing_Node* >(a)) {
    printf("NULL\n");
    return;
  }

  i8* st = ast::kind_to_cstr(a->kind);

  if (a->kind >= __AST_KIND_END)
    printf("AST:{ temporary: %s", st);
  else
    printf("AST:{ kind: %s", st);

  free(st);

  if (a->kind == AST_NATURAL_LITERAL) {
    symbol::Symbol s = symbol::get_symbol(p->symbol_table, a->left);
    printf(",val: ");

    for (u64 i = 0; i < s.size; i++) {
      printf("%c", symbol::char_at(&s, i));
    }
  }

  if (a->kind == AST_SYMBOL_LITERAL) {
    symbol::Symbol s = symbol::get_symbol(p->symbol_table, a->left);
    printf(",sym: ");

    for (u64 i = 0; i < s.size; i++) {
      printf("%c", symbol::char_at(&s, i));
    }
  }

  printf(" }\n");

  if (a->left == 0 && a->right == 0)
    return;

  // printf("%*c|__", tabs, ' ');
  i8* buf0 = (i8*)malloc(strlen(prefix) + 10);
  strcpy(buf0, prefix);
  strcat(buf0, is_left ? "│   " : "    ");
  print_ast_rec(buf0, p, ast::manager_get_relative(p->ast_manager, a, a->left), 1);
  free(buf0);

  i8* buf1 = (i8*)malloc(strlen(prefix) + 10);
  strcpy(buf1, prefix);
  strcat(buf1, is_left ? "│   " : "    ");
  print_ast_rec(buf1, p, ast::manager_get_relative(p->ast_manager, a, a->right), 0);
  free(buf1);
}

void print_ast(Parser* p, ast::Node* n) {
  i8 s[1];

  s[0] = '\0';

  print_ast_rec(s, p, n, 0, 0);
}

b8 parser_is_same_symbol(Parser* p, ast::Node* a, ast::Node* b) {
  if (a->kind != b->kind)
    return false;

  if (ast::is_temporary(a) && ast::is_temporary(b)) {
    return a->kind == b->kind;
  }

  assert(a->kind == AST_SYMBOL_LITERAL);
  assert(b->kind == AST_SYMBOL_LITERAL);

  symbol::Symbol s0 = symbol::get_symbol(p->symbol_table, a->left);
  symbol::Symbol s1 = symbol::get_symbol(p->symbol_table, b->left);

  if (s0.size != s1.size || s0.crc64 != s1.crc64) {
    return false;
  }

  for (u64 i = 0; i < s0.size; i++) {
    if (symbol::char_at(&s0, i) != symbol::char_at(&s1, i)) {
      return false;
    }
  }

  return true;
}

void print_ast_to_program(Parser* p, ast::Node* n, u32 scope) {

  if (ast::is_temporary(n)) {
    i8* st = ast::kind_to_cstr(n->kind, __AST_KIND_END);
    printf("t%s", st);
    free(st);
    return;
  }

  if (n->kind == AST_NATURAL_LITERAL) {
    symbol::Symbol s = symbol::get_symbol(p->symbol_table, n->left);

    for (u64 i = 0; i < s.size; i++) {
      printf("%c", symbol::char_at(&s, i));
    }
    return;
  }

  if (n->kind == AST_SYMBOL_LITERAL) {
    symbol::Symbol s = symbol::get_symbol(p->symbol_table, n->left);

    for (u64 i = 0; i < s.size; i++) {
      printf("%c", symbol::char_at(&s, i));
    }
    return;
  }

  if (n->kind == AST_DECL_ARGS_LIST) {
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    if (n->right)
      printf(", ");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope);
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
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(" -> ");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope);
    return;
  }

  if (n->kind == AST_TYPE_ANY) {
    printf("any");
    return;
  }

  if (n->kind == AST_FUN_SIGNATURE) {
    printf("(");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(")");
    if (n->right)
      printf(" -> ");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope);
    return;
  }

  if (n->kind == AST_FUNCTION_LITERAL) {
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(" {\n");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope + 2);

    for (i32 i = 0; i < scope; i++)
      printf(" ");
    printf("}\n");
    return;
  }

  if (n->kind == AST_PROGRAM_POINT) {
    for (i32 i = 0; i < scope; i++)
      printf(" ");

    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf("\n");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope);

    return;
  }

  if (n->kind == AST_BIND_CONSTANT) {
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(" :: ");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope);
    printf("");
    return;
  }

  if (n->kind == AST_BIND_VARIABLE) {
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(" := ");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope);
    printf("");
    return;
  }

  if (n->kind == AST_BIND_TYPE) {
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    // if (n->right) printf(" : ");
    // else printf(" :");
    // print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope);
    return;
  }

  if (n->kind == AST_OP_BIN_ASSIGN) {
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(" = ");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope);
    printf("");
    return;
  }

  if (n->kind == AST_FUNCTION_CALL) {
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf("(");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope);
    printf(")");
    return;
  }

  if (n->kind == AST_CTRL_FLOW_RETURN) {
    printf("return ");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf("");
    return;
  }

  if (n->kind == AST_CTRL_FLOW_IF) {
    printf("if ");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(" {\n");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope + 2);

    for (i32 i = 0; i < scope; i++)
      printf(" ");
    printf("}\n");
    return;
  }

  if (n->kind == AST_CTRL_FLOW_IF_ELSE) {
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    for (i32 i = 0; i < scope; i++)
      printf(" ");

    printf("else {\n");

    // for (i32 i = 0; i < scope + 2; i++)
    //   printf(" ");

    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope + 2);
    for (i32 i = 0; i < scope; i++)
      printf(" ");
    printf("}");
    return;
  }

  if (n->kind == AST_OP_BIN_ASSIGN) {
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(" = ");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope);
    return;
  }

  if (n->kind == AST_OP_BIN_ADD) {
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(" + ");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope);
    return;
  }

  if (n->kind == AST_OP_BIN_SUB) {
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(" - ");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope);
    return;
  }

  if (n->kind == AST_OP_BIN_MUL) {
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(" * ");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope);
    return;
  }

  if (n->kind == AST_UNDEFINED_NODE) {
    printf("undefined");
    return;
  }
  if (n->kind == AST_UNITIALIZED_NODE) {
    printf(".{}");
    return;
  }

  if (n->kind == AST_TYPE_POINTER) {
    printf("ptr");
    printf("[");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf("]");
    return;
  }

  if (n->kind == AST_OP_MEMBER_ACCESS) {
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(".");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope);
    return;
  }

  if (n->kind == AST_CTRL_FLOW_CASE) {
    printf("case ");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(" {\n");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope + 2);
    for (i32 i = 0; i < scope; i++)
      printf(" ");
    printf("}");
    return;
  }

  if (n->kind == AST_CTRL_FLOW_MATCH) {
    printf("match ");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(" {\n");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope + 2);
    for (i32 i = 0; i < scope; i++)
      printf(" ");
    printf("}");
    return;
  }
  if (n->kind == AST_OP_MEMBER_ACCESS) {
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(".");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope);
    return;
  }

  if (n->kind == AST_TYPE_STRUCT) {
    printf("struct {\n");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope + 2);
    for (i32 i = 0; i < scope; i++)
      printf(" ");

    printf("}\n");
    return;
  }

  if (n->kind == AST_OP_POINTER_LOAD) {
    printf("ld[");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf("]");
    return;
  }
  if (n->kind == AST_OP_ADDRESS_OF) {
    printf("$");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf("");
    return;
  }
  // if (n->kind == _AST_BUILD_STACK_CLOSURE_OBJECT) {
  //   printf("_build_stack_closure(");
  //   print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
  //   printf(", ");
  //   print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope);
  //   printf(")");

  //   return;
  // }

  // if (n->kind == _AST_BUILD_HEAP_CLOSURE_OBJECT) {
  //   printf("_build_heap_closure(");
  //   print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
  //   printf(", ");
  //   print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope);
  //   printf(")");

  //   return;
  // }

  if (n->kind == _AST_GET_CLOSURE_HANDLER) {
    printf("_get_closure_object_handler(");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(")");

    return;
  }
  if (n->kind == _AST_CAPTURE_VARIABLE_INTO_ENVIRONMENT) {
    printf("_capture_variable_into_environment(");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(")");
    return;
  }

  if (n->kind == _AST_BORROW_VARIABLE_INTO_ENVIRONMENT) {
    printf("_borrow_variable_into_environment(");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(")");
    return;
  }

  if (n->kind == _AST_GET_CLOSURE_ENVIRONMENT_BUFFER) {
    printf("_get_closure_object_environment_buffer(");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(")");

    return;
  }

  if (n->kind == _AST_GET_CLOSURE_ENVIRONMENT_BUFFER_SIZE) {
    printf("_get_closure_object_environment_buffer_size(");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(")");

    return;
  }
  if (n->kind == _AST_UPDATE_CLOSURE_ENVIRONMENT_BUFFER_HEADER) {
    printf("_update_closure_buffer_header(");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(")");

    return;
  }

  if (n->kind == _AST_SETUP_CLOSURE_ENVIRONMENT_BUFFER_HEADER) {
    printf("_setup_closure_buffer_header(");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(")");

    return;
  }

  if (n->kind == _AST_BUILD_CLOSURE_OBJECT) {
    printf("_build_closure_object(");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left));
    printf(")");
    return;
  }

  if (n->kind == _AST_BITSET_BINARY_UNION) {
    printf("_bitset_binary_union(");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left));
    printf(", ");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right));
    printf(")");
    return;
  }

  if (n->kind == _AST_BITSET_BINARY_INTERSECTION) {
    printf("_bitset_binary_intersection(");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left));
    printf(", ");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right));
    printf(")");
    return;
  }

  if (n->kind == _AST_ALLOCATE_HEAP_BUFFER) {
    printf("_malloc(");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left));
    printf(")");
    return;
  }

  if (n->kind == _AST_IS_CLOSURE_OBJECT_LOCAL) {
    printf("_is_closure_local(");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left));
    printf(")");
    return;
  }

  if (n->kind == _AST_GET_CLOSURE_OBJECT_BITSET) {
    printf("_get_closure_bitset(");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left));
    printf(")");
    return;
  }

  if (n->kind == _AST_SET_CLOSURE_OBJECT_EXTERN) {
    printf("_set_closure_object_as_extern(");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left));
    printf(")");
    return;
  }

  if (n->kind == _AST_REALLOCATE_HEAP_BUFFER) {
    printf("_realloc(");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left));
    printf(")");
    return;
  }

  if (n->kind == _AST_SIZE_OF) {
    printf("_size_of(");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left));
    printf(")");
    return;
  }

  if (n->kind == _AST_TYPE_OF) {
    printf("_type_of(");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left));
    printf(")");
    return;
  }

  if (n->kind == _AST_SIZE_OF) {
    printf("_size_of(");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(")");

    return;
  }

  if (n->kind == AST_OP_BIN_NE) {
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(" != ");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope);
    return;
  }
  if (n->kind == AST_OP_BIN_GT) {
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(" > ");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope);
    return;
  }
  if (n->kind == AST_OP_BIN_GE) {
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(" >= ");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope);
    return;
  }

  if (n->kind == AST_OP_BIN_EQ) {
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(" == ");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope);
    return;
  }

  if (ast::is_binary_operation(n)) {
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->left), scope);
    printf(" op ");
    print_ast_to_program(p, ast::manager_get_relative(p->ast_manager, n, n->right), scope);
    return;
  }
}

} // namespace parser
