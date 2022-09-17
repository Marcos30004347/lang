#include "ast.hpp"
#include "parser/parser.hpp"
#include "tests.hpp"
#include "utils.hpp"
#include <cstring>

using namespace parser;
using namespace ast;

i8* buf;

void should_parse_expressions() {
  Parser* p = parser_create(0, buf, strlen(buf));

  Node* root = parser_parse(p);

  print_ast(p, root);

  parser_destroy(p);
}

int main() {
  buf = read_from_file("../examples/state.magic");

  TEST(should_parse_expressions);
  return 0;
}
