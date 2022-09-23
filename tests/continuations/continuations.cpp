#include "compiler/compiler.hpp"
#include "lib/set.hpp"
#include "tests.hpp"

#include "continuations.hpp"
#include "parser/parser.hpp"

using namespace compiler;
using namespace symbol;
using namespace parser;

void should_cps_convert_call_programs() {
  const i8* prog = "main :: () -> i32 {"
                   "  f();"
                   "  g();"
                   "  return 0;"

                   "}";

  CPS_Conversion_Info* info = cps_info_create();

  Compiler* compiler = compiler_create();

  ast::Node* node = compiler->parse(prog, strlen(prog));

  cps_conversion(info, compiler, node);

  print_ast_ir(compiler->parser, node);

  compiler_destroy(compiler);
  cps_info_destroy(info);
}

void should_cps_convert_call_assignments_programs() {
  const i8* prog = "main :: () -> i32 {"
                   "  x : i32 : f();"
                   "  y : i32 : g();"
                   "  z : i32 : x + y;"
                   "  return z;"
                   "}";

  CPS_Conversion_Info* info = cps_info_create();

  Compiler* compiler = compiler_create();

  ast::Node* node = compiler->parse(prog, strlen(prog));

  cps_conversion(info, compiler, node);

  print_ast_ir(compiler->parser, node);

  compiler_destroy(compiler);
  cps_info_destroy(info);
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
                   "  y : i32 : g();"
                   "  z : i32 : x + y;"
                   "  return z;"
                   "}";

  CPS_Conversion_Info* info = cps_info_create();

  Compiler* compiler = compiler_create();

  ast::Node* node = compiler->parse(prog, strlen(prog));

  cps_conversion(info, compiler, node);

  print_ast_ir(compiler->parser, node);

  cps_info_destroy(info);

  compiler_destroy(compiler);
}

int main() {
  TEST(should_cps_convert_call_programs);
  TEST(should_cps_convert_call_assignments_programs);
  TEST(should_cps_convert_branch_programs);
}
