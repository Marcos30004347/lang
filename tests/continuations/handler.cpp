#include "compiler/compiler.hpp"
#include "lib/set.hpp"
#include "tests.hpp"

#include "continuations/continuations.hpp"
#include "continuations/handler.hpp"
#include "parser/parser.hpp"

using namespace compiler;
using namespace symbol;
using namespace parser;

void should_cps_convert_effectfull_programs() {
  const i8* prog = "count : unit -> i32;"
                   "counter : handler_t : handler {"
                   "  x : i32 = 0;"
                   "  count : unit -> i32 : () -> i32 {"
                   "    x = x + 1;"
                   "    resume(x);"
                   "  }"
                   "}"
                   "f :: () -> i32 {"
                   "  x : i32 : count!();"
                   "  test : handler_t : handler {"
                   "    u : i32 = 0;"
                   "    g : unit -> i32 : () -> i32 {"
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

  handler::Handler_Pass_Data* info = handler::handler_pass_data_create();

  handler::handler_conversion_pass(info, parser->ast_manager, node);

  print_ast_ir(parser->ast_manager, node);

  handler::handler_pass_data_destroy(info);

  parser::parser_destroy(parser);
}

int main() {
  TEST(should_cps_convert_effectfull_programs);
}
