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

void should_bubble_effectfull_program() {

  const i8* prog = "ask : i32 -> i32 : (a: i32) -> i32;\n"
                   "f : unit -> i32 : () -> i32 {\n"
                   "  x: i32 : ask!(4);\n"
                   "  return x;\n"
                   "}\n"
                   "g : unit -> i32 : () -> i32 {\n"
                   "  w : i32 = 0;"
                   "  read : handler_t : handler {\n"
                   "    ask : unit -> i32 : (a: i32) -> i32 {\n"
                   "      one : i32 = 1;\n"
                   "      x : i32 = resume(one);\n"
                   "      y : i32 = resume(one);\n"
                   "      z : i32 = x + w + y + a;\n"
                   "			return z;\n"
                   "    }\n"
                   "  }\n"
                   "  f() with read;\n"
                   "  return 0;\n"
                   "}\n";

  Parser* parser = parser_create(-1, prog, strlen(prog));

  ast::Node* node = parser_parse(parser);

  handler::Handler_Pass_Data* hd_data = handler::handler_pass_data_create();

  handler::handler_conversion_pass(hd_data, parser->ast_manager, node);
  printf("/*\n");
  print_ast_ir(parser->ast_manager, node);
  printf("====================================\n");
  printf("====================================\n");

  stackframe::Stack_Frame_Data* data = stackframe::create_stack_frame_data(hd_data);

  stackframe::allocate_stack_frame(data, parser->ast_manager, node);

  print_ast_ir(parser->ast_manager, node);

  printf("====================================\n");
  printf("====================================\n");

  cps::CPS_Data* info = cps::cps_data_create(data);

  cps::convert_to_cps_style(info, parser, node);

  print_ast_ir(parser->ast_manager, node);
  printf("====================================\n");
  printf("====================================\n");
  bubbling::Bubbling_Data* bubbling_data = bubbling::bubbling_data_create(info);

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
