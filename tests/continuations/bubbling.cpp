#include "compiler/compiler.hpp"
#include "lib/set.hpp"
#include "stackframe/stackframe.hpp"
#include "tests.hpp"

#include "continuations/bubbling.hpp"
#include "continuations/closures.hpp"

#include "parser/parser.hpp"

using namespace compiler;
using namespace symbol;
using namespace parser;

void should_closure_convert_cps_branch_programs() {
  const i8* prog = "some :: (x: i32) -> i32 {"
                   "  return x;"
                   "}"
                   "g : i32 -> i32 : (x:i32) -> i32 {"
                   "  return x;"
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

  //  print_ast_ir(compiler->parser->ast_manager, node);

  stackframe::Stack_Frame_Data* sf_data = stackframe::create_stack_frame_data(info);

  stackframe::allocate_stack_frame(sf_data, compiler->parser->ast_manager, node);

  // print_ast_ir(compiler->parser->ast_manager, node);

  closures::CPS_Closure_Data* data = closures::cps_closure_data_create(sf_data);

  closures::convert_cps_closures(compiler->parser->ast_manager, node, data);

  bubbling::Bubbling_Data* bubbling_data = bubbling::bubbling_data_create(data);

  bubbling::add_bubbling_yields(bubbling_data, compiler->parser->ast_manager, node);

  print_ast_ir(compiler->parser->ast_manager, node);

  bubbling::bubbling_data_delete(bubbling_data);

  compiler::compiler_destroy(compiler);
}

int main() {
  TEST(should_closure_convert_cps_branch_programs);
}
