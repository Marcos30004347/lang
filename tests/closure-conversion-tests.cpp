#include "ast.hpp"
#include "closures.hpp"
#include "context.hpp"
#include "error.hpp"
#include "parser.hpp"
#include "tests.hpp"
#include "utils.hpp"

#include <cstring>
#include <vector>

i8* buf;

void should_closure_convert_ast() {
  Parser p;

  parser_init(&p, 0, buf, strlen(buf));

  AST_Node* root = parser_parse(&p);

  print_ast(&p, root);

  closure_conversion(&p, root);

  parser_destroy(&p);
}

int main() {
  buf = read_from_file("../examples/closures.lang");

  TEST(should_closure_convert_ast);

  return 0;
}
