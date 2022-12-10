#include "parser.hpp"

#include "ast/ast_control_flow.hpp"
#include "ast/ast_declaration.hpp"
#include "ast/ast_function.hpp"
#include "ast/ast_kind.hpp"
#include "ast/ast_literals.hpp"
#include "ast/ast_manager.hpp"
#include "ast/ast_operations.hpp"
#include "ast/ast_program_point.hpp"
#include "ast/ast_types.hpp"

#include "compiler/symbol_table.hpp"
#include "lexer.hpp"
#include "types.hpp"

#include <assert.h>
#include <cstdio>

#include "stdio.h"
#include "stdlib.h"
#include "string.h"

using namespace compiler;

namespace parser {
using ast::AST_OP_MEMBER_ACCESS;

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
ast::ProgramPoint* parser_parse_statements(Parser* p);
ast::Node* parser_parse_decl(Parser* p);
ast::Node* parser_parse_arrow(Parser* p);

Parser* parser_create(u64 id, const i8* buffer, u64 size) {
  Parser* p = new Parser();

  p->ast_manager = ast::manager_create();

  // TODO(marcos): refactor this to lexer_create function
  p->lexer = new Lexer();
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

  ast::Literal_Symbol_Node* root = ast::create_node_literal_symbol(p->ast_manager, symbol::from_token(p->ast_manager->symbol_table, p->lexer, token));

  if (parser_curr_tok(p).type == TOKEN_APHOSTROPHE) {
    parser_read_token(p, TOKEN_APHOSTROPHE);
    return ast::create_node_type_variable(p->ast_manager, root);
  }

  return root;
}

ast::Node* parse_call_args(Parser* p) {
  parser_read_token(p, TOKEN_OPEN_PARENTHESIS);

  ast::Node* root = parser_parse_expr(p);

  if (root && !ast::is_instance< ast::Literal_Nothing_Node* >(root) && !ast::is_instance< ast::Declarations_List_Node* >(root)) {
    root = ast::create_node_declarations_list(p->ast_manager, root, NULL);
  }

  parser_read_token(p, TOKEN_CLOSE_PARENTHESIS);

  return root;
}

ast::Node* parser_parse_struct(Parser* p) {
  Token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_KEYWORD_STRUCT);

  ast::ProgramPoint* body = parser_parse_statements(p);

  return ast::create_node_literal_struct(p->ast_manager, body);
}

ast::Node* parser_parse_handler(Parser* p) {
  Token tok = parser_curr_tok(p);

  parser_read_token(p, TOKEN_KEYWORD_HANDLER);

  ast::ProgramPoint* body = parser_parse_statements(p);

  return ast::create_node_literal_handler(p->ast_manager, body);
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

  if (parser_curr_tok(p).type == TOKEN_KEYWORD_HANDLER) {
    return parser_parse_handler(p);
  }

  if (tok.type == TOKEN_I32_LIT) {
    parser_read_token(p, TOKEN_I32_LIT);
    return ast::create_node_literal_natural(p->ast_manager, symbol::from_token(p->ast_manager->symbol_table, p->lexer, tok));
  }

  if (tok.type == TOKEN_UNIT) {
    parser_read_token(p, TOKEN_UNIT);
    return ast::create_node_type_unit(p->ast_manager);
  }

  if (tok.type == TOKEN_I32) {
    parser_read_token(p, TOKEN_I32);
    return ast::create_node_type_i32(p->ast_manager);
  }

  if (tok.type == TOKEN_TYPE) {
    parser_read_token(p, TOKEN_TYPE);
    return ast::create_node_type_any(p->ast_manager);
  }

  if (tok.type == TOKEN_CLOSE_PARENTHESIS) {
    return ast::create_node_literal_nothing(p->ast_manager);
  }

  if (tok.type == TOKEN_KEYWORD_HANDLER_TYPE) {
    parser_read_token(p, TOKEN_KEYWORD_HANDLER_TYPE);
    return ast::create_node_type_handler(p->ast_manager);
  }

  if (tok.type == TOKEN_KEYWORD_STRUCT_TYPE) {
    parser_read_token(p, TOKEN_KEYWORD_HANDLER_TYPE);
    return ast::create_node_type_struct(p->ast_manager);
  }

  debug_print_token(p, parser_curr_tok(p));

  parser_error(p, parser_curr_tok(p), "invalid literal declaration");

  return 0;
}

// PRIMARY → LITERAL | SYMBOL | '('EXPRESSION')'
ast::Node* parser_parse_primary(Parser* p) {
  if (parser_curr_tok(p).type == TOKEN_OPEN_PARENTHESIS) {
    Token tok = parser_curr_tok(p);

    b8 effectfull = false;

    if (parser_curr_tok(p).type == TOKEN_EXCLAMATION) {
      parser_read_token(p, TOKEN_EXCLAMATION);

      effectfull = true;
    }

    parser_read_token(p, TOKEN_OPEN_PARENTHESIS);

    ast::Node* root = parser_parse_expr(p);

    parser_read_token(p, TOKEN_CLOSE_PARENTHESIS);

    if (ast::is_instance< ast::Literal_Nothing_Node* >(root) || ast::is_instance< ast::Declarations_List_Node* >(root) || ast::is_instance< ast::Declaration_Constant_Node* >(root)
        || ast::is_instance< ast::Declaration_Variable_Node* >(root)) {

      ast::Declarations_List_Node* arguments = NULL;

      if (ast::is_semantic_node(root) && !ast::is_instance< ast::Declarations_List_Node* >(root)) {
        arguments = ast::create_node_declarations_list(p->ast_manager, root, NULL);
      } else {
        arguments = ast::as< ast::Declarations_List_Node* >(root);
      }

      ast::Node* type = ast::create_node_literal_undefined(p->ast_manager);

      if (parser_curr_tok(p).type == TOKEN_ARROW) {
        parser_read_token(p, TOKEN_ARROW);

        type = parser_parse_arrow(p);
      }

      if (parser_curr_tok(p).type == TOKEN_OPEN_CURLY_BRACE) {
        ast::ProgramPoint* body = parser_parse_statements(p);

        return ast::create_node_function_literal(p->ast_manager, arguments, type, body);
      }

      return ast::create_node_effect_declaration(p->ast_manager, arguments, type);
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

    if (effectfull) {
      return ast::create_node_effect_call(p->ast_manager, expr, ast::as< ast::Declarations_List_Node* >(arguments));
    }

    return ast::create_node_function_call(p->ast_manager, expr, ast::as< ast::Declarations_List_Node* >(arguments));
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

  ast::Node* call = NULL;

  if (effectfull) {
    call = ast::create_node_effect_call(p->ast_manager, head, ast::as< ast::Declarations_List_Node* >(arguments));
  } else {
    call = ast::create_node_function_call(p->ast_manager, head, ast::as< ast::Declarations_List_Node* >(arguments));
  }

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

    // if (!ast::is_instance< ast::Declarations_List_Node* >(arguments)) {
    //   parser_error(p, p->lexer->curr, "Expecting arguments");
    // }
    ast::Node* call = NULL;

    if (effectfull) {
      call = ast::create_node_effect_call(p->ast_manager, root, ast::as< ast::Declarations_List_Node* >(arguments));
    } else {
      call = ast::create_node_function_call(p->ast_manager, root, ast::as< ast::Declarations_List_Node* >(arguments));
    }

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
  ast::Node* root = parser_parse_call(p);

  if (parser_curr_tok(p).type == TOKEN_KEYWORD_WITH) {
    parser_read_token(p, TOKEN_KEYWORD_WITH);

    ast::Node* node = parser_parse_expr(p);

    ast::Declarations_List_Node* handlers = NULL;

    if (ast::Declarations_List_Node* l = ast::is_instance< ast::Declarations_List_Node* >(node)) {
      handlers = l;
    } else {
      handlers = ast::create_node_declarations_list(p->ast_manager, node, NULL);
    }

    return ast::create_node_with_statement(p->ast_manager, root, handlers);
  }

  return root;
}

ast::Node* parser_parse_member_access(Parser* p) {
  ast::Node* root = parser_parse_array_access(p);

  if (parser_curr_tok(p).type == TOKEN_DOT) {
    parser_read_token(p, TOKEN_DOT);
    ast::Node* access = parser_parse_member_access(p);

    if (ast::is_semantic_node(access) && !ast::is_instance< ast::Member_Access_Node* >(access)) {
      access = ast::create_node_member_access(p->ast_manager, access, NULL);
    }

    return ast::create_node_member_access(p->ast_manager, root, access);
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
      return ast::create_node_arithmetic_div(p->ast_manager, root, parser_parse_factor(p));
    }

    if (tok.type == TOKEN_ASTERISK) {
      return ast::create_node_arithmetic_mul(p->ast_manager, root, parser_parse_factor(p));
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
      return ast::create_node_arithmetic_add(p->ast_manager, root, parser_parse_term(p));
    }

    if (tok.type == TOKEN_MINUS) {
      return ast::create_node_arithmetic_sub(p->ast_manager, root, parser_parse_term(p));
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

    return ast::create_node_logical_greater_than(p->ast_manager, root, parser_parse_comparison(p));
  }

  if (parser_curr_tok(p).type == TOKEN_SMALLER) {
    Token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_SMALLER);

    return ast::create_node_logical_less_than(p->ast_manager, root, parser_parse_comparison(p));
  }

  if (parser_curr_tok(p).type == TOKEN_GREATER_EQUAL) {
    Token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_GREATER_EQUAL);

    return ast::create_node_logical_greater_equal_than(p->ast_manager, root, parser_parse_comparison(p));
  }

  if (parser_curr_tok(p).type == TOKEN_SMALLER_EQUAL) {
    Token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_SMALLER_EQUAL);

    return ast::create_node_logical_less_equal_than(p->ast_manager, root, parser_parse_comparison(p));
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

    return ast::create_node_logical_not_equal_to(p->ast_manager, root, parser_parse_equality(p));
  }

  if (parser_curr_tok(p).type == TOKEN_EQUAL_EQUAL) {
    Token curr = parser_curr_tok(p);

    parser_read_token(p, TOKEN_EQUAL_EQUAL);

    return ast::create_node_logical_equal_to(p->ast_manager, root, parser_parse_equality(p));
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

    tmp = ast::create_node_type_arrow(p->ast_manager, tmp, tail);

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
      cont = parser_parse_if_statement(p);
    }
  }

  ast::If_Node_Statement* statement = ast::create_node_if_statement(p->ast_manager, cond, body);

  if (cont) {
    assert(ast::is_instance< ast::If_Node_Statement* >(statement));

    ast::Node* elif = cont;

    if (!ast::is_instance< ast::Elif_List_Node* >(elif)) {

      if (!ast::is_instance< ast::If_Node_Statement* >(elif)) {
        elif = ast::create_node_if_statement(p->ast_manager, ast::create_node_literal_true(p->ast_manager), elif);
      }

      elif = ast::create_node_elif_list(p->ast_manager, ast::as< ast::If_Node_Statement* >(elif), NULL);
    }

    return ast::create_node_elif_list(p->ast_manager, ast::as< ast::If_Node_Statement* >(statement), ast::as< ast::Elif_List_Node* >(elif));
  }

  return ast::create_node_elif_list(p->ast_manager, statement, NULL);
}

ast::Return_Node_Statement* parser_parse_return_statement(Parser* p) {
  parser_read_token(p, TOKEN_KEYWORD_RETURN);

  if (parser_curr_tok(p).type == TOKEN_SEMI_COLON) {
    return ast::create_node_return_statement(p->ast_manager, ast::create_node_literal_nothing(p->ast_manager));
  }

  return ast::create_node_return_statement(p->ast_manager, parser_parse_expr(p));
}

ast::Resume_Node_Statement* parser_parse_resume_statement(Parser* p) {
  parser_read_token(p, TOKEN_KEYWORD_RESUME);

  parser_read_token(p, TOKEN_OPEN_PARENTHESIS);

  ast::Resume_Node_Statement* resume = ast::create_node_resume_statement(p->ast_manager, parser_parse_expr(p));

  parser_read_token(p, TOKEN_CLOSE_PARENTHESIS);

  return resume;
}

ast::Node* parser_parse_statement(Parser* p) {
  if (parser_curr_tok(p).type == TOKEN_KEYWORD_IF) {
    return parser_parse_if_statement(p);
  }

  if (parser_curr_tok(p).type == TOKEN_KEYWORD_RETURN) {
    ast::Node* root = parser_parse_return_statement(p);

    parser_read_token(p, TOKEN_SEMI_COLON);

    while (parser_curr_tok(p).type == TOKEN_SEMI_COLON) {
      parser_read_token(p, TOKEN_SEMI_COLON);
    }

    return root;
  }

  // if (parser_curr_tok(p).type == TOKEN_KEYWORD_RESUME) {
  //   ast::Node* root = parser_parse_resume_statement(p);

  //   parser_read_token(p, TOKEN_SEMI_COLON);

  //   while (parser_curr_tok(p).type == TOKEN_SEMI_COLON) {
  //     parser_read_token(p, TOKEN_SEMI_COLON);
  //   }

  //   return root;
  // }

  ast::Node* expr = parser_parse_expr(p);

  if (parser_prev_tok(p).type != TOKEN_CLOSE_CURLY_BRACE) {
    parser_read_token(p, TOKEN_SEMI_COLON);

    while (parser_curr_tok(p).type == TOKEN_SEMI_COLON) {
      parser_read_token(p, TOKEN_SEMI_COLON);
    }
  }

  return expr;
}

ast::ProgramPoint* parser_parse_statements(Parser* p) {
  parser_read_token(p, TOKEN_OPEN_CURLY_BRACE);

  ast::ProgramPoint* root = ast::create_node_program_point(p->ast_manager, NULL, NULL);

  ast::ProgramPoint* iterator = root;

  while (parser_curr_tok(p).type != TOKEN_CLOSE_CURLY_BRACE) {
    ast::Node* statement = parser_parse_statement(p);

    iterator->left = statement->id;

    if (parser_curr_tok(p).type == TOKEN_CLOSE_CURLY_BRACE) {
      break;
    }

    ast::ProgramPoint* r = ast::create_node_program_point(p->ast_manager, NULL, NULL);

    iterator->right = r->id;
    iterator = r;
  }

  parser_read_token(p, TOKEN_CLOSE_CURLY_BRACE);

  return root;
}

ast::Node* parser_parse_decl(Parser* p) {
  ast::Node* root = parser_parse_arrow(p);
  return root;
}

ast::Node* parser_parse_assignment(Parser* p) {
  Token root_token = parser_curr_tok(p);

  ast::Node* root = parser_parse_decl(p);

  if (parser_curr_tok(p).type == TOKEN_COLON) {
    Token tok = parser_curr_tok(p);

    parser_read_token(p, TOKEN_COLON);

    ast::Node* type = (parser_curr_tok(p).type != TOKEN_COLON && parser_curr_tok(p).type != TOKEN_EQUAL) ? parser_parse_arrow(p) : ast::create_node_literal_nothing(p->ast_manager);

    if (parser_curr_tok(p).type == TOKEN_COLON) {
      if (!ast::is_instance< ast::Literal_Symbol_Node* >(root)) {
        parser_error(p, root_token, "Constant identifier is not a symbol!");
      }

      parser_read_token(p, TOKEN_COLON);

      root = ast::create_constant_declaration(p->ast_manager, ast::as< ast::Literal_Symbol_Node* >(root), type);

      return ast::create_node_assignment(p->ast_manager, root, parser_parse_expr(p));
    }

    if (parser_curr_tok(p).type == TOKEN_EQUAL) {
      if (!ast::is_instance< ast::Literal_Symbol_Node* >(root)) {
        parser_error(p, root_token, "Variable identifier is not a symbol!");
      }

      parser_read_token(p, TOKEN_EQUAL);

      root = ast::create_variable_declaration(p->ast_manager, ast::as< ast::Literal_Symbol_Node* >(root), type);

      return ast::create_node_assignment(p->ast_manager, root, parser_parse_expr(p));
    }

    return ast::create_variable_declaration(p->ast_manager, ast::as< ast::Literal_Symbol_Node* >(root), type);
  }

  if (parser_curr_tok(p).type == TOKEN_COLON) {
    if (!ast::is_instance< ast::Literal_Symbol_Node* >(root)) {
      parser_error(p, root_token, "Constant identifier is not a symbol!");
    }

    parser_read_token(p, TOKEN_COLON);

    return ast::create_node_assignment(p->ast_manager, root, parser_parse_expr(p));
  }

  if (parser_curr_tok(p).type == TOKEN_EQUAL) {
    if (!ast::is_instance< ast::Literal_Symbol_Node* >(root)) {
      parser_error(p, root_token, "Variable identifier is not a symbol!");
    }

    parser_read_token(p, TOKEN_EQUAL);

    return ast::create_node_assignment(p->ast_manager, root, parser_parse_expr(p));
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
      tail = ast::create_node_declarations_list(p->ast_manager, tail, NULL);
    }

    return ast::create_node_declarations_list(p->ast_manager, root, ast::as< ast::Declarations_List_Node* >(tail));
  }

  return root;
}

ast::Node* parser_parse(Parser* p) {
  if (parser_curr_tok(p).type == TOKEN_EOF) {
    return ast::manager_get(p->ast_manager, p->ast_manager->statements_list_root_id);
  }

  // ast::Node *root = ast::decl_list(p->ast_manager, parser_curr_tok(p));
  ast::Node* node = parser_parse_expr(p);

  ast::manager_push_decl(p->ast_manager, node);

  if (ast::Variable_Assignment_Node* var = ast::is_instance< ast::Variable_Assignment_Node* >(node)) {
    ast::Node* right = var->get_right_operand(p->ast_manager);

    if (!ast::is_instance< ast::Function_Literal_Node* >(right) && !ast::is_instance< ast::Literal_Struct_Node* >(right)
        && !ast::is_instance< ast::Literal_Handler_Node* >(right)) {
      parser_read_token(p, TOKEN_SEMI_COLON);
    }
  } else {
    parser_read_token(p, TOKEN_SEMI_COLON);
  }
  // root->left = parser_parse_expr(p)->id;

  return parser_parse(p);
}

void print_ast_rec(i8* prefix, ast::Manager* p, ast::Node* a, b8 is_left, b8 f = 1) {
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

  if (a == 0) {
    printf("NULL\n");
    return;
  }

  if (ast::is_instance< ast::Literal_Nothing_Node* >(a)) {
    printf("NOTHING\n");
    return;
  }

  i8* st = ast::kind_to_cstr(a->kind);

  if (a->kind >= ast::__AST_KIND_END)
    printf("AST:{ temporary: %s", st);
  else
    printf("AST:{ kind: %s", st);

  free(st);

  if (a->kind == ast::AST_NATURAL_LITERAL) {
    printf(",val: ");
    symbol::Symbol s = symbol::get_symbol(p->symbol_table, a->left);

    for (u64 i = 0; i < s.size; i++) {
      printf("%c", symbol::char_at(&s, i));
    }

    printf(" }\n");
    return;
  }

  if (a->kind == ast::AST_SYMBOL_LITERAL) {
    printf(",sym: ");

    symbol::Symbol s = symbol::get_symbol(p->symbol_table, a->left);

    for (u64 i = 0; i < s.size; i++) {
      printf("%c", symbol::char_at(&s, i));
    }

    printf(" }\n");

    return;
  }

  printf(" }\n");

  if (a->left == 0 && a->right == 0)
    return;

  // printf("%*c|__", tabs, ' ');
  i8* buf0 = (i8*)malloc(strlen(prefix) + 10);
  strcpy(buf0, prefix);
  strcat(buf0, is_left ? "│   " : "    ");
  print_ast_rec(buf0, p, ast::manager_get_relative(p, a, a->left), 1);
  free(buf0);

  i8* buf1 = (i8*)malloc(strlen(prefix) + 10);
  strcpy(buf1, prefix);
  strcat(buf1, is_left ? "│   " : "    ");
  print_ast_rec(buf1, p, ast::manager_get_relative(p, a, a->right), 0);
  free(buf1);
}

void print_ast(ast::Manager* p, ast::Node* n) {
  i8 s[1];

  s[0] = '\0';

  print_ast_rec(s, p, n, 0, 0);
}

b8 parser_is_same_symbol(Parser* p, ast::Node* a, ast::Node* b) {
  if (a->kind != b->kind)
    return false;

  // if (ast::is_temporary(a) && ast::is_temporary(b)) {
  //   return a->kind == b->kind;
  // }

  assert(a->kind == ast::AST_SYMBOL_LITERAL);
  assert(b->kind == ast::AST_SYMBOL_LITERAL);

  symbol::Symbol s0 = symbol::get_symbol(p->ast_manager->symbol_table, a->left);
  symbol::Symbol s1 = symbol::get_symbol(p->ast_manager->symbol_table, b->left);

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

void print_ast_ir(ast::Manager* ast_manager, ast::Node* n, u32 scope) {

  // if (ast::is_temporary(n)) {
  //   i8* st = ast::kind_to_cstr(n->kind, ast::__AST_KIND_END);
  //   printf("t%s", st);
  //   free(st);
  //   return;
  // }

  if (n->kind == ast::AST_NATURAL_LITERAL) {
    symbol::Symbol s = symbol::get_symbol(ast_manager->symbol_table, n->left);

    for (u64 i = 0; i < s.size; i++) {
      printf("%c", symbol::char_at(&s, i));
    }
    return;
  }

  if (n->kind == ast::AST_SYMBOL_LITERAL) {
    symbol::Symbol s = symbol::get_symbol(ast_manager->symbol_table, n->left);

    for (u64 i = 0; i < s.size; i++) {
      printf("%c", symbol::char_at(&s, i));
    }
    return;
  }

  if (n->kind == ast::AST_DECL_ARGS_LIST) {
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    if (n->right)
      printf(", ");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope);
    return;
  }
  if (n->kind == ast::AST_TYPE_UNIT) {
    printf("unit");
    return;
  }

  if (n->kind == ast::AST_TYPE_I32) {
    printf("i32");
    return;
  }
  if (n->kind == ast::AST_TYPE_ARROW) {
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf(" -> ");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope);
    return;
  }

  if (n->kind == ast::AST_TYPE_ANY) {
    printf("any");
    return;
  }

  if (n->kind == ast::AST_FUN_SIGNATURE) {
    printf("(");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf(")");
    if (n->right)
      printf(" -> ");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope);
    return;
  }

  if (n->kind == ast::AST_EFFECT_LITERAL) {
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    return;
  }

  if (n->kind == ast::AST_FUNCTION_LITERAL) {
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf(" {\n");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope + 2);

    for (i32 i = 0; i < scope; i++)
      printf(" ");
    printf("}\n");
    return;
  }

  if (n->kind == ast::AST_PROGRAM_POINT) {
    for (i32 i = 0; i < scope; i++)
      printf(" ");

    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf("\n");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope);

    return;
  }

  if (n->kind == ast::AST_DECLARATION_CONSTANT) {
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf(" : ");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope);

    return;
  }

  if (n->kind == ast::AST_DECLARATION_VARIABLE) {
    printf("mut ");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf(" : ");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope);

    return;
  }

  if (n->kind == ast::AST_OP_BIN_ASSIGN) {
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf(" = ");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope);

    return;
  }

  if (n->kind == ast::AST_EFFECT_CALL) {
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf("!(");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope);
    printf(")");
    return;
  }

  if (n->kind == ast::AST_FUNCTION_CALL) {
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf("(");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope);
    printf(")");
    return;
  }

  if (n->kind == ast::AST_CTRL_FLOW_RETURN) {
    printf("return ");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    return;
  }

  if (n->kind == ast::AST_WITH_STATEMENT) {
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf(" with ");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope);
    return;
  }

  if (n->kind == ast::AST_CTRL_FLOW_IF) {
    printf("\n");
    for (i32 i = 0; i < scope; i++)
      printf(" ");

    printf("if ");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf(" {\n");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope + 2);

    for (i32 i = 0; i < scope; i++)
      printf(" ");
    printf("}\n");
    return;
  }
  if (n->kind == ast::AST_CAST_TYPE) {
    printf("(");

    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), 0);
    printf(")(");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), 0);
    printf(")");

    return;
  }

  if (n->kind == ast::AST_CTRL_FLOW_IF_ELSE) {
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);

    if (ast::is_semantic_node(ast::right_of(ast_manager, n))) {
      for (i32 i = 0; i < scope; i++)
        printf(" ");

      printf("else {\n");

      for (i32 i = 0; i < scope + 2; i++)
        printf(" ");

      print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope + 2);

      for (i32 i = 0; i < scope; i++)
        printf(" ");
      printf("}\n");
    }
    return;
  }

  if (n->kind == ast::AST_OP_BIN_ASSIGN) {
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf(" = ");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope);
    return;
  }

  if (n->kind == ast::AST_OP_BIN_ADD) {
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf(" + ");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope);
    return;
  }

  if (n->kind == ast::AST_OP_BIN_SUB) {
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf(" - ");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope);
    return;
  }

  if (n->kind == ast::AST_OP_BIN_MUL) {
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf(" × ");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope);
    return;
  }

  if (n->kind == ast::AST_TYPE_EVIDENCE_CONTEXT) {
    printf("context");
    return;
  }

  if (n->kind == ast::AST_UNDEFINED_NODE) {
    printf("undefined");
    return;
  }
  if (n->kind == ast::AST_UNITIALIZED_NODE) {
    printf(".{}");
    return;
  }

  if (n->kind == ast::AST_TYPE_POINTER) {
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf("*");
    return;
  }

  if (n->kind == ast::AST_OP_MEMBER_ACCESS) {
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf(".");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope);
    return;
  }

  if (n->kind == ast::AST_CTRL_FLOW_CASE) {
    printf("case ");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf(" {\n");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope + 2);
    for (i32 i = 0; i < scope; i++)
      printf(" ");
    printf("}");
    return;
  }

  if (n->kind == ast::AST_CTRL_FLOW_MATCH) {
    printf("match ");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf(" {\n");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope + 2);
    for (i32 i = 0; i < scope; i++)
      printf(" ");
    printf("}");
    return;
  }

  if (n->kind == ast::AST_HANDLER_LITERAL) {
    printf("handler ");
    printf(" {\n");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope + 2);
    for (i32 i = 0; i < scope; i++)
      printf(" ");
    printf("}");
    return;
  }

  if (n->kind == ast::AST_TYPE_HANDLER) {
    printf("handler_t");
    return;
  }

  if (n->kind == ast::AST_PROMPT_HANDLER) {
    printf("prompt ");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    return;
  }

  if (n->kind == ast::AST_BUBBLE_HANDLER) {
    printf("bubble handler ");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    return;
  }

  if (n->kind == ast::AST_CTRL_FLOW_RESUME) {
    printf("resume(");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf(")");
    return;
  }
  if (n->kind == ast::AST_OP_MEMBER_ACCESS) {
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf(".");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope);
    return;
  }

  if (n->kind == ast::AST_STRUCT_LITERAL) {
    printf("struct {\n");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope + 2);
    for (i32 i = 0; i < scope; i++)
      printf(" ");

    printf("}\n");
    return;
  }
  if (n->kind == ast::AST_TYPE_STRUCT) {
    printf("struct");
    return;
  }

  if (n->kind == ast::AST_OP_POINTER_VALUE) {
    printf("*(");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf(")");
    return;
  }

  if (n->kind == ast::AST_OP_ADDRESS_OF) {
    printf("&");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    return;
  }

  if (n->kind == ast::AST_TRUE_LITERAL) {
    printf("true");
    return;
  }

  if (n->kind == ast::AST_FALSE_LITERAL) {
    printf("false");
    return;
  }

  if (n->kind == ast::AST_OP_BIN_NE) {
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf(" != ");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope);
    return;
  }

  if (n->kind == ast::AST_OP_BIN_GT) {
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf(" > ");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope);
    return;
  }

  if (n->kind == ast::AST_OP_BIN_GE) {
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf(" >= ");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope);
    return;
  }

  if (n->kind == ast::AST_OP_BIN_EQ) {
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf(" == ");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope);
    return;
  }

  if (ast::is_binary_operation(n)) {
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->left), scope);
    printf(" op ");
    print_ast_ir(ast_manager, ast::manager_get_relative(ast_manager, n, n->right), scope);
    return;
  }
}

} // namespace parser
