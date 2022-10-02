#include "compiler/compiler.hpp"
#include "lib/set.hpp"
#include "tests.hpp"

#include "continuations/closures.hpp"
#include "parser/parser.hpp"

using namespace compiler;
using namespace symbol;
using namespace parser;

void should_closure_convert_cps_branch_programs() {
  const i8* prog = "g :: (x:i32) {"
                   "  return x;"
                   "}"
                   "main :: () -> i32 {"
                   "  x : i32 = 0;"
                   "  if x {"
                   "    x = 4;"
                   "  } else {"
                   "    x = g(x);"
                   "    x = x + 3;"
                   "  }"
                   "  w : i32 : 3;"
                   "  y : i32 : g(x);"
                   "  z : i32 : x + y;"
                   "  e : i32 : z + w;"
                   "  return e;"
                   "}";

  cps::CPS_Data* info = cps::cps_result_create();

  Compiler* compiler = compiler_create();

  ast::Node* node = compiler->parse(prog, strlen(prog));

  cps::convert_to_cps_style(info, compiler, node);

  print_ast_ir(compiler->parser->ast_manager, node);

  closures::CPS_Closure_Data* data = closures::cps_closure_data_create(info);

  closures::convert_cps_closures(compiler->parser->ast_manager, node, data);

  closures::cps_closure_data_destroy(data);

  compiler::compiler_destroy(compiler);
}

int main() {
  TEST(should_closure_convert_cps_branch_programs);
}
