#include "compiler/compiler.hpp"
#include "lib/set.hpp"
#include "stackframe/stackframe.hpp"
#include "tests.hpp"

#include "continuations/closures.hpp"
#include "parser/parser.hpp"

void should_closure_convert_cps_simple_programs() {
  const i8* prog = "g :: (x:i32) -> i32 {"
                   "  return x;"
                   "}"
                   "main : unit -> i32 : () -> i32 {"
                   "  x : i32 = 0;"
                   "  y : i32 = g(x);"
                   "  z : i32 = x + y;"
                   "  w : i32 = g(z);"
                   "  return w;"
                   "}";

  compiler::Compiler* compiler = compiler::compiler_create();

  ast::Node* node = compiler->parse(prog, strlen(prog));

  handler::Handler_Pass_Data* hd_data = handler::handler_pass_data_create();

  handler::handeler_conversion_pass(hd_data, compiler->parser->ast_manager, node);

  cps::CPS_Data* info = cps::cps_data_create(hd_data);

  stackframe::Stack_Frame_Data* sf_data = stackframe::create_stack_frame_data(info);

  parser::print_ast_ir(compiler->parser->ast_manager, node);

  cps::convert_to_cps_style(info, compiler->parser, node);

  stackframe::allocate_stack_frame(sf_data, compiler->parser->ast_manager, node);

  closures::CPS_Closure_Data* data = closures::cps_closure_data_create(sf_data);

  closures::convert_cps_closures(compiler->parser->ast_manager, node, data);

  parser::print_ast_ir(compiler->parser->ast_manager, node);

  stackframe::destroy_stack_frame_data(sf_data);

  compiler::compiler_destroy(compiler);
}

void should_closure_convert_cps_branch_programs() {
  const i8* prog = "some :: (x: i32) -> i32 {"
                   "  return x;"
                   "}"
                   "g : i32 -> i32 : (x:i32) -> i32 {"
                   "  return x;"
                   "}"
                   "main : unit -> i32 : (a: i32, b: i32) -> i32 {"
                   "  x : i32 = 0;"
                   "  if a {"
                   "    x = 4;"
                   "  } else {"
                   "    x = g(x);"
                   "    x = x + b;"
                   "  }"
                   "  w : i32 : 3;"
                   "  y : i32 : g(x);"
                   "  x = g(x);"
                   "  z : i32 : x + y;"
                   "  e : i32 : z + w;"
                   "  if x {"
                   "    z = g(z);"
                   "  } else if y {"
                   "    e = g(e);"
                   "    e = e + a;"
                   "  } else {"
                   "    e = g(e);"
                   "    e = e + b;"
                   "    w : i32 : g(7);"
                   "    e = w + 9;"
                   "  }"
                   "  if x {"
                   "    z = g(z);"
                   "  } else {"
                   "    e = g(e);"
                   "    e = e + 4;"
                   "  }"

                   "  q : i32 : some!(e);"
                   "  return q;"
                   "}";

  compiler::Compiler* compiler = compiler::compiler_create();

  ast::Node* node = compiler->parse(prog, strlen(prog));

  handler::Handler_Pass_Data* hd_data = handler::handler_pass_data_create();

  handler::handeler_conversion_pass(hd_data, compiler->parser->ast_manager, node);

  cps::CPS_Data* info = cps::cps_data_create(hd_data);

  stackframe::Stack_Frame_Data* sf_data = stackframe::create_stack_frame_data(info);

  parser::print_ast_ir(compiler->parser->ast_manager, node);

  cps::convert_to_cps_style(info, compiler->parser, node);

  parser::print_ast_ir(compiler->parser->ast_manager, node);

  stackframe::allocate_stack_frame(sf_data, compiler->parser->ast_manager, node);

  parser::print_ast_ir(compiler->parser->ast_manager, node);

  closures::CPS_Closure_Data* data = closures::cps_closure_data_create(sf_data);

  closures::convert_cps_closures(compiler->parser->ast_manager, node, data);

  parser::print_ast_ir(compiler->parser->ast_manager, node);

  closures::cps_closure_data_destroy(data);

  compiler::compiler_destroy(compiler);
}

int main() {
  TEST(should_closure_convert_cps_simple_programs);
  TEST(should_closure_convert_cps_branch_programs);
}
