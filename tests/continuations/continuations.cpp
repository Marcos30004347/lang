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
                   "  f();"
                   "  g();"
                   "  return 0;"
                   "}";

  cps::CPS_Data* info = cps::cps_result_create();

  compiler::Compiler* compiler = compiler_create();

  ast::Node* node = compiler->parse(prog, strlen(prog));

  cps::convert_to_cps_style(info, compiler, node);

  print_ast_ir(compiler->parser->ast_manager, node);

  compiler::compiler_destroy(compiler);

  cps::cps_result_destroy(info);
}

void should_cps_convert_call_assignments_programs() {
  const i8* prog = "main :: () -> i32 {"
                   "  x : i32 : f();"
                   "  y : i32 : g();"
                   "  z : i32 : x + y;"
                   "  return z;"
                   "}";

  cps::CPS_Data* info = cps::cps_result_create();

  compiler::Compiler* compiler = compiler_create();

  ast::Node* node = compiler->parse(prog, strlen(prog));

  cps::convert_to_cps_style(info, compiler, node);

  print_ast_ir(compiler->parser->ast_manager, node);

  compiler::compiler_destroy(compiler);

  cps::cps_result_destroy(info);
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
                   "  y : i32 : g(x);"
                   "  z : i32 : x + y;"
                   "  return z;"
                   "}";

  cps::CPS_Data* info = cps::cps_result_create();

  Compiler* compiler = compiler_create();

  ast::Node* node = compiler->parse(prog, strlen(prog));

  cps::convert_to_cps_style(info, compiler, node);

  print_ast_ir(compiler->parser->ast_manager, node);

  cps::cps_result_destroy(info);

  compiler::compiler_destroy(compiler);
}

int main() {
  TEST(should_cps_convert_call_programs);
  TEST(should_cps_convert_call_assignments_programs);
  TEST(should_cps_convert_branch_programs);
}
