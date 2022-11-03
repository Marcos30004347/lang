#include "compiler/compiler.hpp"
#include "lib/set.hpp"
#include "stackframe/stackframe.hpp"
#include "tests.hpp"

#include "continuations/bubbling.hpp"

#include "compiler/compiler.hpp"
#include "compiler/transpiler.hpp"
#include "parser/parser.hpp"

using namespace compiler;
using namespace symbol;
using namespace parser;

void should_bubble_branch_programs() {
  const i8* prog = "some : i32 -> i32 : (a: i32) -> i32 {"
                   "  return a;"
                   "}"
                   "g : i32 -> i32 : (b:i32) -> i32 {"
                   "  return b;"
                   "}"
                   "main : unit -> i32 : () -> i32 {"
                   "  x : i32 = 0;"
                   "  if x {"
                   "    x = 4;"
                   "  } else {"
                   "    x = g(x);"
                   "    x = x + 3;"
                   "  }"
                   "  w : i32 : 3;"
                   "  y : i32 : g(x);"
                   "  x = g(x);"
                   "  z : i32 : x + y;"
                   "  e : i32 : z + w;"
                   "  q : i32 : some!(e);"
                   "  return q;"
                   "}";

  Compiler* compiler = compiler_create();

  ast::Node*                  node    = compiler->parse(prog, strlen(prog));
  handler::Handler_Pass_Data* hd_data = handler::handler_pass_data_create();

  handler::handeler_conversion_pass(hd_data, compiler->parser->ast_manager, node);

  cps::CPS_Data* info = cps::cps_data_create(hd_data);

  cps::convert_to_cps_style(info, compiler->parser, node);

  stackframe::Stack_Frame_Data* sf_data = stackframe::create_stack_frame_data(info);

  stackframe::allocate_stack_frame(sf_data, compiler->parser->ast_manager, node);

  bubbling::Bubbling_Data* bubbling_data = bubbling::bubbling_data_create(sf_data);

  bubbling::add_bubbling_yields(bubbling_data, compiler->parser->ast_manager, node);

  print_ast_ir(compiler->parser->ast_manager, node);

  bubbling::bubbling_data_delete(bubbling_data);

  compiler::compiler_destroy(compiler);
}

void should_bubble_effectfull_program() {

  const i8* prog = "ask : unit -> i32 : () -> i32;"
                   ""
                   "read : handler_t : handler {"
                   "  ask : unit -> i32 : () -> i32 {"
                   "    resume(1);"
                   "  }"
                   "}"
                   ""
                   "f : unit -> i32 : () -> i32 {"
                   // "  x: i32 : ask!();"
                   //"  y: i32 : ask!();"
                   // "  z: i32 : x + y;"
                   "  return 0;"
                   "}"
                   "g : unit -> i32 : () -> i32 {"
                   "  f() with read;"
                   "  return 0;"
                   "}";

  Parser* parser = parser_create(-1, prog, strlen(prog));

  ast::Node* node = parser_parse(parser);

  handler::Handler_Pass_Data* hd_data = handler::handler_pass_data_create();

  handler::handeler_conversion_pass(hd_data, parser->ast_manager, node);
  printf("/*\n");
  print_ast_ir(parser->ast_manager, node);
  printf("====================================\n");
  printf("====================================\n");
  cps::CPS_Data* info = cps::cps_data_create(hd_data);

  cps::convert_to_cps_style(info, parser, node);

  print_ast_ir(parser->ast_manager, node);
  printf("====================================\n");
  printf("====================================\n");
  stackframe::Stack_Frame_Data* data = stackframe::create_stack_frame_data(info);

  stackframe::allocate_stack_frame(data, parser->ast_manager, node);

  print_ast_ir(parser->ast_manager, node);
  printf("====================================\n");
  printf("====================================\n");
  bubbling::Bubbling_Data* bubbling_data = bubbling::bubbling_data_create(data);

  bubbling::add_bubbling_yields(bubbling_data, parser->ast_manager, node);

  print_ast_ir(parser->ast_manager, node);
  printf("====================================\n");
  printf("====================================\n");

  printf("*/\n");
  bubbling::bubbling_data_delete(bubbling_data);

  compiler::transpiler::output_c_code(parser->ast_manager, node);

  parser_destroy(parser);
}

int main() {
  // TEST(should_bubble_branch_programs);
  TEST(should_bubble_effectfull_program);
}
