#include "compiler/compiler.hpp"
#include "lib/set.hpp"
#include "tests.hpp"

#include "continuations/continuations.hpp"
#include "parser/parser.hpp"

using namespace compiler;
using namespace symbol;
using namespace parser;

void should_cps_convert_call_programs() {
  const i8* prog = "main :: () -> i32 {"
                   "  f!();"
                   "  g!();"
                   "  return 0;"
                   "}";

  Parser* parser = parser::parser_create(-1, prog, strlen(prog));

  ast::Node* node = parser_parse(parser);

  cps::CPS_Data* info = cps::cps_data_create();

  cps::convert_to_cps_style(info, parser, node);

  print_ast_ir(parser->ast_manager, node);

  parser::parser_destroy(parser);

  cps::cps_data_destroy(info);
}

void should_cps_convert_call_assignments_programs() {
  const i8* prog = "main :: () -> i32 {"
                   "  x : i32 : f!();"
                   "  y : i32 : g!();"
                   "  z : i32 : x + y;"
                   "  return z;"
                   "}";

  Parser* parser = parser::parser_create(-1, prog, strlen(prog));

  ast::Node* node = parser_parse(parser);

  cps::CPS_Data* info = cps::cps_data_create();

  cps::convert_to_cps_style(info, parser, node);

  print_ast_ir(parser->ast_manager, node);

  parser::parser_destroy(parser);

  cps::cps_data_destroy(info);
}

void should_cps_convert_branch_programs() {
  const i8* prog = "main :: () -> i32 {"
                   "  x : i32 = 0;"
                   "  if x {"
                   "    x = 4;"
                   "  } else {"
                   "    x = g(x);"
                   "    x = x + 3;"
                   "  }"
                   "  u : i32 : f(x);"
                   "  q : i32 : q(x);"
                   "  y : i32 : g(x);"
                   "  z : i32 : x + y;"
                   "  return z;"
                   "}";

  Parser* parser = parser::parser_create(-1, prog, strlen(prog));

  ast::Node* node = parser_parse(parser);

  cps::CPS_Data* info = cps::cps_data_create();

  cps::convert_to_cps_style(info, parser, node);

  print_ast_ir(parser->ast_manager, node);

  cps::cps_data_destroy(info);

  parser::parser_destroy(parser);
}

int main() {
  TEST(should_cps_convert_call_programs);
  TEST(should_cps_convert_call_assignments_programs);
  TEST(should_cps_convert_branch_programs);
}
