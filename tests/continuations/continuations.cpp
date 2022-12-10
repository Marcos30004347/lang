#include "compiler/compiler.hpp"
#include "continuations/handler.hpp"
#include "lib/set.hpp"
#include "stackframe/stackframe.hpp"
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

  handler::Handler_Pass_Data* hd_data = handler::handler_pass_data_create();

  handler::handler_conversion_pass(hd_data, parser->ast_manager, node);

  stackframe::Stack_Frame_Data* sf_data = stackframe::create_stack_frame_data(hd_data);

  stackframe::allocate_stack_frame(sf_data, parser->ast_manager, node);

  cps::CPS_Data* info = cps::cps_data_create(sf_data);

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

  ast::Node*                  node    = parser_parse(parser);
  handler::Handler_Pass_Data* hd_data = handler::handler_pass_data_create();

  handler::handler_conversion_pass(hd_data, parser->ast_manager, node);

  stackframe::Stack_Frame_Data* sf_data = stackframe::create_stack_frame_data(hd_data);

  stackframe::allocate_stack_frame(sf_data, parser->ast_manager, node);

  cps::CPS_Data* info = cps::cps_data_create(sf_data);

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

  ast::Node*                  node    = parser_parse(parser);
  handler::Handler_Pass_Data* hd_data = handler::handler_pass_data_create();

  handler::handler_conversion_pass(hd_data, parser->ast_manager, node);

  stackframe::Stack_Frame_Data* sf_data = stackframe::create_stack_frame_data(hd_data);

  stackframe::allocate_stack_frame(sf_data, parser->ast_manager, node);

  cps::CPS_Data* info = cps::cps_data_create(sf_data);

  cps::convert_to_cps_style(info, parser, node);

  print_ast_ir(parser->ast_manager, node);

  cps::cps_data_destroy(info);

  parser::parser_destroy(parser);
}

void should_cps_convert_effectfull_programs() {
  const i8* prog = "count : unit -> i32;"
                   "counter : handler_t : handler {"
                   "  x : i32 = 0;"
                   "  count :: () -> i32 {"
                   "    x = x + 1;"
                   "    resume(x);"
                   "  }"
                   "}"
                   "f :: () -> i32 {"
                   "  x : i32 : count!();"
                   "  test : handler_t : handler {"
                   "     u : i32 = 0;"
                   "    g :: () -> i32 {"
                   "      q : i32 = x + u;"
                   "      resume(q);"
                   "    }"
                   "  }"
                   "  y : i32 : count!();"
                   "  z : i32 : count!();"
                   "  return z;"
                   "}"
                   "main :: () -> i32 {"
                   "  x : i32 : f() with counter;"
                   "  return x;"
                   "}";

  Parser* parser = parser::parser_create(-1, prog, strlen(prog));

  ast::Node* node = parser_parse(parser);

  handler::Handler_Pass_Data* hd_data = handler::handler_pass_data_create();

  handler::handler_conversion_pass(hd_data, parser->ast_manager, node);

  stackframe::Stack_Frame_Data* sf_data = stackframe::create_stack_frame_data(hd_data);

  stackframe::allocate_stack_frame(sf_data, parser->ast_manager, node);

  cps::CPS_Data* info = cps::cps_data_create(sf_data);

  cps::convert_to_cps_style(info, parser, node);

  print_ast_ir(parser->ast_manager, node);

  cps::cps_data_destroy(info);

  parser::parser_destroy(parser);
}

int main() {
  TEST(should_cps_convert_call_programs);
  TEST(should_cps_convert_call_assignments_programs);
  TEST(should_cps_convert_branch_programs);
  TEST(should_cps_convert_effectfull_programs);
}
