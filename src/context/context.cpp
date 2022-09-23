#include "context.hpp"
#include "ast/ast_declaration.hpp"
#include "ast/ast_literals.hpp"
#include "ast/ast_manager.hpp"
#include "ast/ast_operations.hpp"
#include "ast/ast_program_point.hpp"
#include "ast/ast_types.hpp"
#include "compiler/compiler.hpp"
#include "lib/set.hpp"
#include "lib/table.hpp"

#include <assert.h>
#include <cstdio>

namespace context {

Abstract_Memory* create_abstract_memory() {
  Abstract_Memory* mem = new Abstract_Memory();

  mem->counter    = 0;
  mem->heap       = 0;
  mem->references = 0;

  return mem;
}

void destroy_values(lib::SetNode< Address >* s) {
  lib::set_delete(s);
}

void destroy_abstract_memory(Abstract_Memory* mem) {

  mem->counter = 0;

  // lib::table_delete(mem->values, destroy_values);
  lib::table_delete(mem->heap);

  delete mem;
}

Context* create_global_context(Abstract_Memory* m) {
  Context* ctx = new Context();

  m->references += 1;

  ctx->memory       = m;
  ctx->parent       = 0;
  ctx->level        = 0;
  ctx->declarations = 0;

  return ctx;
}

Context* context_create(Context* parent, Abstract_Memory* m) {
  if (parent == 0) {
    return create_global_context(m ? m : create_abstract_memory());
  }

  Context* ctx = new Context();

  ctx->memory       = m ? m : parent->memory;
  ctx->parent       = parent;
  ctx->level        = parent->level + 1;
  ctx->declarations = 0;

  if (m) {
    m->references += 1;
  } else {
    parent->memory += 1;
  }

  return ctx;
}

void context_destroy(Context* context) {
  if (context->level == 0) {
    if (context->memory->references == 1) {
      destroy_abstract_memory(context->memory);
    } else {
      context->memory->references -= 1;
    }
  }

  lib::table_delete(context->declarations);

  delete context;

  // TODO(marcos): free all unreferenced values
}

Value init_members_context(compiler::Compiler* compiler, Context* ctx, ast::Node* value, ast::Node* type) {
  assert(ast::is_instance< ast::Type_Struct_Node* >(type));

  while (ast::is_instance< ast::Literal_Symbol_Node* >(value)) {
    ast::Literal_Symbol_Node* symbol = ast::as< ast::Literal_Symbol_Node* >(value);

    Declaration* declaration = get_declaration(compiler, ctx, symbol);

    assert(ast::is_instance< ast::Type_Struct_Node* >(declaration->type));

    assert(declaration->values);

    lib::SetNode< Address >* values = declaration->values;

    assert(size(values) == 1);

    value = lib::search(ctx->memory->heap, values->key)->value;
  }

  assert(ast::is_instance< ast::Literal_Struct_Node* >(value));

  ast::Literal_Struct_Node* literal = ast::as< ast::Literal_Struct_Node* >(value);

  ast::ProgramPoint_List_Node* members = literal->get_members(compiler->parser->ast_manager);

  Context* struct_context = context_create(NULL, ctx->memory);

  while (members) {
    ast::Node* member = members->get_statement(compiler->parser->ast_manager);

    declare(compiler, struct_context, member);

    members = members->get_next_program_point(compiler->parser->ast_manager);
  }

  Value result;

  result.value   = 0;
  result.members = struct_context;

  return result;
}

void declare(compiler::Compiler* compiler, Context* ctx, ast::Node* declaration) {
  assert(ast::is_declaration_node(declaration));

  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(declaration)) {
    ast::Literal_Symbol_Node* id = var->get_symbol(compiler->parser->ast_manager);

    ast::Node* type = var->get_type(compiler->parser->ast_manager);

    Value val;

    if (ast::is_instance< ast::Literal_Symbol_Node* >(type)) {
      ast::Literal_Symbol_Node* type_symbol = ast::as< ast::Literal_Symbol_Node* >(type);

      Declaration* type_declaration = get_declaration(compiler, ctx, type_symbol);

      if (ast::is_instance< ast::Type_Struct_Node* >(type_declaration->type)) {
        assert(size(type_declaration->values) == 1);

        ast::Node* type_value = lib::search(ctx->memory->heap, type_declaration->values->key)->value;

        val = init_members_context(compiler, ctx, type_value, type_declaration->type);
      } else {
        val.members = 0;
        val.value   = 0;
      }
    }

    Declaration declaration = {.symbol = id, .is_constant = 1, .type = type, .values = 0};

    ctx->memory->counter += 1;

    lib::insert(ctx->memory->heap, ctx->memory->counter, val);
    lib::insert(declaration.values, ctx->memory->counter);
    lib::insert(ctx->declarations, id->get_symbol_id(), declaration);
  }

  if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(declaration)) {
    ast::Literal_Symbol_Node* id = var->get_symbol(compiler->parser->ast_manager);

    ast::Node* type = var->get_type(compiler->parser->ast_manager);

    Value val;

    if (ast::is_instance< ast::Literal_Symbol_Node* >(type)) {
      ast::Literal_Symbol_Node* type_symbol = ast::as< ast::Literal_Symbol_Node* >(type);

      Declaration* type_declaration = get_declaration(compiler, ctx, type_symbol);

      if (ast::is_instance< ast::Type_Struct_Node* >(type_declaration->type)) {
        assert(size(type_declaration->values) == 1);

        ast::Node* type_value = lib::search(ctx->memory->heap, type_declaration->values->root->key)->value;

        val = init_members_context(compiler, ctx, type_value, type_declaration->type);
      } else {
        val.members = 0;
        val.value   = 0;
      }
    }

    Declaration declaration = {.symbol = id, .is_constant = 1, .type = type, .values = 0};

    ctx->memory->counter += 1;

    lib::insert(ctx->memory->heap, ctx->memory->counter, val);
    lib::insert(declaration.values, ctx->memory->counter);
    lib::insert(ctx->declarations, id->get_symbol_id(), declaration);
  }
}

Declaration* get_declaration(compiler::Compiler* compiler, Context* ctx, ast::Literal_Symbol_Node* node) {
  if (ctx == 0) {
    return 0;
  }

  assert(ast::is_instance< ast::Literal_Symbol_Node* >(node));

  ast::Literal_Symbol_Node* symbol = ast::as< ast::Literal_Symbol_Node* >(node);

  Declaration* declaration = lib::search(ctx->declarations, symbol->get_symbol_id());

  if (declaration) {
    return declaration;
  }

  return get_declaration(compiler, ctx->parent, node);
}

void set_value(compiler::Compiler* compiler, Context* ctx, ast::Node* x, ast::Node* y) {
  if (ast::Member_Access_Node* var = ast::is_instance< ast::Member_Access_Node* >(x)) {
    ast::Node* left  = var->get_left_operand(compiler->parser->ast_manager);
    ast::Node* right = var->get_right_operand(compiler->parser->ast_manager);

    assert(ast::is_instance< ast::Literal_Symbol_Node* >(left));

    Declaration* declaration = get_declaration(compiler, ctx, ast::as< ast::Literal_Symbol_Node* >(left));

    assert(declaration);
  }

  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(x)) {
    ast::Literal_Symbol_Node* id = var->get_symbol(compiler->parser->ast_manager);

    Declaration* declaration = get_declaration(compiler, ctx, id);

    Value value;

    ast::Node* type = declaration->type;

    if (ast::is_instance< ast::Type_Struct_Node* >(type)) {
    } else {
    }

    ctx->memory->counter += 1;

    lib::insert(ctx->memory->heap, ctx->memory->counter, value);

    lib::set_delete(declaration->values);

    declaration->values = 0;

    lib::insert(declaration->values, ctx->memory->counter);
  }
}

void set_reference(compiler::Compiler* compiler, Context* ctx, ast::Node* x, ast::Node* y);

} // namespace context
