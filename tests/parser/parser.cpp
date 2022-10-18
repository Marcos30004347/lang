#include "tests.hpp"

#include "parser/parser.hpp"
#include <cstdio>

using namespace compiler;
using namespace symbol;
using namespace parser;

void should_create_and_delete_parsers() {
  Parser* parser = parser_create(-1, "", 0);
  parser_destroy(parser);
}

void should_parse_programs() {
  Parser* parser = NULL;

  const i8* prog1 = "main :: () {"
                    "  x :: 0;"
                    "  y :: 1;"
                    "  z :: x + y;"
                    "  return z;"
                    "}";

  parser           = parser_create(-1, prog1, strlen(prog1));
  ast::Node* node0 = parser_parse(parser);
  print_ast(parser->ast_manager, node0); // TODO(marcos): compare node0 to the expected ast representation
  parser_destroy(parser);

  const i8* prog2 = "main :: () {"
                    "  return;"
                    "}";

  parser           = parser_create(-1, prog2, strlen(prog2));
  ast::Node* node1 = parser_parse(parser);
  print_ast(parser->ast_manager, node1); // TODO(marcos): compare node1 to the expected ast representation
  parser_destroy(parser);

  const i8* prog3 = "main :: (x:i32, y:i32) {"
                    "  return x + y;"
                    "}";

  parser           = parser_create(-1, prog3, strlen(prog3));
  ast::Node* node2 = parser_parse(parser);
  print_ast(parser->ast_manager, node2); // TODO(marcos): compare node2 to the expected ast representation
  parser_destroy(parser);

  const i8* prog4 = "main :: (x:i32, y:i32) -> i32 {"
                    "  f :: (z:i32) -> i32 {"
                    "    return x + y + z;"
                    "  }"
                    "  return f(4);"
                    "}";

  parser           = parser_create(-1, prog4, strlen(prog4));
  ast::Node* node3 = parser_parse(parser);
  print_ast(parser->ast_manager, node3); // TODO(marcos): compare node3 to the expected ast representation
  parser_destroy(parser);

  const i8* prog5 = "main :: (x:i32, y:i32) -> i32 {"
                    "  a : i32 : b.c.d;"
                    "  return 0;"
                    "}";

  parser           = parser_create(-1, prog5, strlen(prog5));
  ast::Node* node4 = parser_parse(parser);
  print_ast(parser->ast_manager, node4); // TODO(marcos): compare node4 to the expected ast representation
  parser_destroy(parser);

  const i8* prog6 = "main :: (x:i32, y:i32) -> i32 {"
                    "  a : i32 : g(x);"
                    "  b : i32 : teste!(3);"
                    "  return 0;"
                    "}";

  parser           = parser_create(-1, prog6, strlen(prog6));
  ast::Node* node5 = parser_parse(parser);
  print_ast(parser->ast_manager, node5); // TODO(marcos): compare node5 to the expected ast representation
  parser_destroy(parser);
}

void should_parse_efectfull_programs() {
  Parser* parser = NULL;

  const i8* prog1 = "count : unit -> i32;"
                    "counter :: handler {"
                    "  x : i32 = 0;"
                    "  count :: () -> i32 {"
                    "    x = x + 1;"
                    "    resume(x);"
                    "  }"
                    "}"
                    "f :: () -> i32 {"
                    "  x : i32 : count!();"
                    "  test :: handler {"
                    "   u : i32 = 0;"
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
                    "  return f() with counter;"
                    "}";

  parser           = parser_create(-1, prog1, strlen(prog1));
  ast::Node* node0 = parser_parse(parser);
  print_ast(parser->ast_manager, node0); // TODO(marcos): compare node0 to the expected ast representation
  parser_destroy(parser);
}

int main() {
  TEST(should_create_and_delete_parsers);
  TEST(should_parse_programs);
  TEST(should_parse_efectfull_programs);
}
