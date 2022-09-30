#include "ast/ast_declaration.hpp"
#include "ast/ast_function.hpp"
#include "ast/ast_kind.hpp"
#include "ast/ast_literals.hpp"
#include "ast/ast_manager.hpp"
#include "ast/ast_operations.hpp"
#include "ast/ast_program_point.hpp"
#include "compiler/symbol_table.hpp"
#include "parser/parser.hpp"
#include "tests.hpp"

#include "context/context.hpp"

void should_create_and_delete_context() {
  context::Context* context = context::context_create(NULL);

  context::context_destroy(context);
}

void should_register_declarations_in_context() {
  context::Context* global_ctx = context::context_create(NULL);

  const i8* src = " A :: struct {"
                  "  y : unit;"
                  "}"
                  " B :: struct {"
                  "  x : i32;"
                  "  a : A;"
                  "}"
                  ""
                  "f :: () {"
                  "  t : i32 = 0;"
                  "  q : i32;"
                  "  j : B;"
                  "  t;"
                  "  j.x;"
                  "  j.a;"
                  "  j.a.y;"
                  "  return;"
                  "}";

  parser::Parser* parser = parser::parser_create(0, src, strlen(src));

  ast::Node* root = parser::parser_parse(parser);

  ast::ProgramPoint_List_Node* program = ast::is_instance< ast::ProgramPoint_List_Node* >(root);

  assert(program);

  ast::Variable_Assignment_Node*  decl      = NULL;
  ast::Declaration_Variable_Node* var       = NULL;
  ast::Declaration_Constant_Node* con       = NULL;
  ast::Function_Literal_Node*     f         = NULL;
  ast::Member_Access_Node*        access    = NULL;
  ast::Literal_Struct_Node*       structure = NULL;
  ast::Literal_Symbol_Node*       symbol    = NULL;

  // A :: struct { ... }
  decl = ast::is_instance< ast::Variable_Assignment_Node* >(program->get_statement(parser->ast_manager));

  assert(decl);

  con = ast::is_instance< ast::Declaration_Constant_Node* >(decl->get_left_operand(parser->ast_manager));

  assert(con);

  structure = ast::is_instance< ast::Literal_Struct_Node* >(decl->get_right_operand(parser->ast_manager));

  assert(structure);

  context::context_declare(global_ctx, parser->ast_manager, con);
  context::context_define_struct(global_ctx, parser->ast_manager, con->get_symbol(parser->ast_manager), structure);

  program = program->get_next_program_point(parser->ast_manager);

  // B :: struct {...}
  decl = ast::is_instance< ast::Variable_Assignment_Node* >(program->get_statement(parser->ast_manager));

  assert(decl);

  con = ast::is_instance< ast::Declaration_Constant_Node* >(decl->get_left_operand(parser->ast_manager));

  assert(con);

  structure = ast::is_instance< ast::Literal_Struct_Node* >(decl->get_right_operand(parser->ast_manager));

  assert(structure);

  context::context_declare(global_ctx, parser->ast_manager, con);
  context::context_define_struct(global_ctx, parser->ast_manager, con->get_symbol(parser->ast_manager), structure);

  program = program->get_next_program_point(parser->ast_manager);

  // f :: () { ... }
  decl = ast::is_instance< ast::Variable_Assignment_Node* >(program->get_statement(parser->ast_manager));

  con = ast::is_instance< ast::Declaration_Constant_Node* >(decl->get_left_operand(parser->ast_manager));

  assert(con);

  context::context_declare(global_ctx, parser->ast_manager, con);

  assert(decl);

  f = ast::is_instance< ast::Function_Literal_Node* >(decl->get_right_operand(parser->ast_manager));

  assert(f);

  program = f->get_body(parser->ast_manager);

  assert(program);

  context::Context* f_ctx = context::context_create(global_ctx);
  // t: i32
  decl = ast::is_instance< ast::Variable_Assignment_Node* >(program->get_statement(parser->ast_manager));

  var = ast::is_instance< ast::Declaration_Variable_Node* >(decl->get_left_operand(parser->ast_manager));

  assert(var);

  context::context_declare(f_ctx, parser->ast_manager, var);

  program = program->get_next_program_point(parser->ast_manager);
  // q: i32
  var = ast::is_instance< ast::Declaration_Variable_Node* >(program->get_statement(parser->ast_manager));

  assert(var);

  context::context_declare(f_ctx, parser->ast_manager, var);

  program = program->get_next_program_point(parser->ast_manager);
  // j: B
  var = ast::is_instance< ast::Declaration_Variable_Node* >(program->get_statement(parser->ast_manager));

  assert(var);

  context::context_declare(f_ctx, parser->ast_manager, var);
  // variables accesses

  // t
  program = program->get_next_program_point(parser->ast_manager);
  symbol  = ast::is_instance< ast::Literal_Symbol_Node* >(program->get_statement(parser->ast_manager));
  assert(symbol);
  ast::Node* type0 = context::context_type_of(f_ctx, parser->ast_manager, symbol);

  assert(type0->kind == ast::AST_TYPE_I32);

  // j.x
  program = program->get_next_program_point(parser->ast_manager);
  access  = ast::is_instance< ast::Member_Access_Node* >(program->get_statement(parser->ast_manager));
  assert(access);
  ast::Node* type1 = context::context_type_of(f_ctx, parser->ast_manager, access);

  assert(type1->kind == ast::AST_TYPE_I32);

  // j.a
  program = program->get_next_program_point(parser->ast_manager);
  access  = ast::is_instance< ast::Member_Access_Node* >(program->get_statement(parser->ast_manager));
  assert(access);
  ast::Node* type2            = context::context_type_of(f_ctx, parser->ast_manager, access);
  symbol                      = ast::is_instance< ast::Literal_Symbol_Node* >(type2);
  compiler::symbol::Symbol id = symbol->get_symbol(parser->ast_manager);

  assert(symbol && compiler::symbol::is_equal(parser->ast_manager->symbol_table, &id, "A"));

  // j.a
  program = program->get_next_program_point(parser->ast_manager);
  access  = ast::is_instance< ast::Member_Access_Node* >(program->get_statement(parser->ast_manager));
  assert(access);
  ast::Node* type3 = context::context_type_of(f_ctx, parser->ast_manager, access);

  assert(type3->kind == ast::AST_TYPE_UNIT);

  parser::parser_destroy(parser);
  context::context_destroy(f_ctx);
  context::context_destroy(global_ctx);
}

int main() {
  TEST(should_create_and_delete_context);
  TEST(should_register_declarations_in_context);
}
