#include "continuations/continuations.hpp"
#include "tests.hpp"

#include "parser/parser.hpp"
#include "stackframe/stackframe.hpp"

using namespace compiler;
using namespace symbol;
using namespace parser;

void should_stack_frame_allote_program() {

  const i8* prog = "main : unit -> i32 : () -> i32 {"
                   "  x : i32 = 0;"
                   "  if x {"
                   "    x = 4;"
                   "  } else {"
                   "    a : i32 = 0;"
                   "    c : i32 = 0;"
                   "    b : i32 = 0;"
                   "    x = g(x);"
                   "    x = x + a;"
                   "  }"
                   "  u : i32 : f(x);"
                   "  q : i32 : q(x);"
                   "  y : i32 : g(x);"
                   "  z : i32 : x + y;"
                   "  return z;"
                   "}";

  Parser*    parser = parser_create(-1, prog, strlen(prog));
  ast::Node* node   = parser_parse(parser);

  handler::Handler_Pass_Data* hd_data = handler::handler_pass_data_create();

  handler::handeler_conversion_pass(hd_data, parser->ast_manager, node);

  cps::CPS_Data* info = cps::cps_data_create(hd_data);

  cps::convert_to_cps_style(info, parser, node);

  print_ast_ir(parser->ast_manager, node);
  stackframe::Stack_Frame_Data* data = stackframe::create_stack_frame_data(info);

  stackframe::allocate_stack_frame(data, parser->ast_manager, node);
  print_ast_ir(parser->ast_manager, node);

  stackframe::destroy_stack_frame_data(data);

  parser_destroy(parser);
}
void should_stack_frame_allote_effectfull_program0() {

  const i8* prog = "count : unit -> i32 : () -> i32;"
                   "g : unit -> i32 : () -> i32;"
                   "k : unit -> i32 : () -> i32;"
                   "w : unit -> i32 : () -> i32;"
                   "counter : handler_t : handler {"
                   "  x : i32 = 0;"
                   "  count : unit -> i32 : () -> i32 {"
                   "    x = x + 1;"
                   "    resume(x);"
                   "  }"
                   "}"
                   "f : unit -> i32 : () -> i32 {"
                   "  x : i32 : count!();"
                   "  test : handler_t : handler {"
                   "    u : i32 = 0;"
                   "    g : unit -> i32 : () -> i32 {"
                   "      q : i32 = x + u;"
                   "      resume(q);"
                   "    }"
                   "    k : unit -> i32 : () -> i32 {"
                   "      q : i32 = x + u;"
                   "      resume(q);"
                   "    }"
                   "    w : unit -> i32 : () -> i32 {"
                   "      q : i32 = x + u;"
                   "      resume(q);"
                   "    }"

                   "  }"
                   "  y : i32 : count!();"
                   "  z : i32 : count!();"
                   "  return z;"
                   "}"
                   "main : unit -> i32: () -> i32 {"
                   "  x : i32 : f() with counter;"
                   "  return x;"
                   "}";

  Parser*    parser = parser_create(-1, prog, strlen(prog));
  ast::Node* node   = parser_parse(parser);

  handler::Handler_Pass_Data* hd_data = handler::handler_pass_data_create();

  handler::handeler_conversion_pass(hd_data, parser->ast_manager, node);

  cps::CPS_Data* info = cps::cps_data_create(hd_data);

  cps::convert_to_cps_style(info, parser, node);

  print_ast_ir(parser->ast_manager, node);

  stackframe::Stack_Frame_Data* data = stackframe::create_stack_frame_data(info);

  stackframe::allocate_stack_frame(data, parser->ast_manager, node);
  print_ast_ir(parser->ast_manager, node);

  stackframe::destroy_stack_frame_data(data);

  parser_destroy(parser);
}
void should_stack_frame_allote_effectfull_program1() {

  const i8* prog = "ask : unit -> i32 : () -> i32;"
                   "read : handler_t : handler {"
                   "  ask : unit -> i32 : () -> i32 {"
                   "    resume(1);"
                   "  }"
                   "}"
                   "f : unit -> i32 : () -> i32 {"
                   "  x: i32 : ask!();"
                   "  y: i32 : ask!();"
                   "  z: i32 : x + y;"
                   "  return z;"
                   "}";

  Parser* parser = parser_create(-1, prog, strlen(prog));

  ast::Node* node = parser_parse(parser);

  handler::Handler_Pass_Data* hd_data = handler::handler_pass_data_create();

  handler::handeler_conversion_pass(hd_data, parser->ast_manager, node);

  cps::CPS_Data* info = cps::cps_data_create(hd_data);

  cps::convert_to_cps_style(info, parser, node);

  stackframe::Stack_Frame_Data* data = stackframe::create_stack_frame_data(info);

  stackframe::allocate_stack_frame(data, parser->ast_manager, node);

  print_ast_ir(parser->ast_manager, node);

  stackframe::destroy_stack_frame_data(data);

  parser_destroy(parser);
}

int main() {
  TEST(should_stack_frame_allote_program);
  TEST(should_stack_frame_allote_effectfull_program0);
  TEST(should_stack_frame_allote_effectfull_program1);
}
