#include "continuations/continuations.hpp"
#include "tests.hpp"

#include "parser/parser.hpp"
#include "stackframe/stackframe.hpp"

using namespace compiler;
using namespace symbol;
using namespace parser;

void should_stack_frame_allote_program() {

  const i8* prog = "main :: () -> i32 {"
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

  cps::CPS_Data* info = cps::cps_data_create();

  cps::convert_to_cps_style(info, parser, node);

  print_ast_ir(parser->ast_manager, node);
  stackframe::Stack_Frame_Data* data = stackframe::create_stack_frame_data(info);

  stackframe::allocate_stack_frame(data, parser->ast_manager, node);
  print_ast_ir(parser->ast_manager, node);

  stackframe::destroy_stack_frame_data(data);

  parser_destroy(parser);
}

int main() {
  TEST(should_stack_frame_allote_program);
}
