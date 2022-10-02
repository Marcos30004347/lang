#include "context.hpp"

#include "ast/ast_declaration.hpp"
#include "ast/ast_literals.hpp"
#include "ast/ast_manager.hpp"
#include "ast/ast_operations.hpp"
#include "ast/ast_program_point.hpp"
#include "lib/table.hpp"
#include "parser/parser.hpp"

#include <stdio.h>

#include <assert.h>

namespace context {
struct Declaration {
  b8 constant;

  ast::Literal_Symbol_Node* symbol;

  ast::Node* type;

  ast::Node* bind;
};

struct Scope {
  lib::Table< compiler::symbol::Id, Declaration* >* scope;

  lib::Table< compiler::symbol::Id, ast::Literal_Struct_Node* >* structures;

  Scope* parent;
};

struct Context {
  Scope*   scope;
  Context* parent;
};

Scope* scope_create(Scope* parent) {
  Scope* scope = new Scope();

  scope->scope      = lib::table_create< compiler::symbol::Id, Declaration* >();
  scope->parent     = parent;
  scope->structures = lib::table_create< compiler::symbol::Id, ast::Literal_Struct_Node* >();

  return scope;
}

Scope* scope_destroy(Scope* scope) {
  Scope* parent = scope->parent;
  lib::table_delete(scope->scope);
  lib::table_delete(scope->structures);
  delete scope;
  return parent;
}

Context* context_create(Context* parent) {
  Context* context = new Context();

  context->parent = parent;

  context->scope = scope_create(NULL);

  return context;
}

Declaration* declaration_create(ast::Node* declaration, ast::Literal_Symbol_Node* symbol, ast::Node* type) {
  assert(ast::is_declaration_node(declaration));

  Declaration* data = new Declaration();

  data->bind   = declaration;
  data->symbol = symbol;
  data->type   = type;

  data->constant = ast::is_instance< ast::Declaration_Constant_Node* >(declaration);

  return data;
}

Context* context_destroy(Context* context) {
  Context* parent = context->parent;

  scope_destroy(context->scope);

  delete context;

  return parent;
}

void context_push_scope(Context* ctx) {
  ctx->scope = scope_create(ctx->scope);
}

void context_pop_scope(Context* ctx) {
  ctx->scope = scope_destroy(ctx->scope);
}

void context_declare(Context* ctx, ast::Manager* m, ast::Declaration_Constant_Node* declaration) {
  ast::Literal_Symbol_Node* symbol = declaration->get_symbol(m);

  ast::Node* type = declaration->get_type(m);

  Declaration* data = declaration_create(declaration, symbol, type);

  lib::insert(ctx->scope->scope, symbol->get_symbol(m).id, data);
}

void context_declare(Context* ctx, ast::Manager* m, ast::Declaration_Variable_Node* declaration) {

  ast::Literal_Symbol_Node* symbol = declaration->get_symbol(m);
  ast::Node*                type   = declaration->get_type(m);

  Declaration* data = declaration_create(declaration, symbol, type);

  // setup_declaration_values(ctx, p, d, type, ast::create_node_literal_undefined(p->ast_manager),
  // program_point);

  lib::insert(ctx->scope->scope, symbol->get_symbol(m).id, data);
}

void context_define_struct(Context* ctx, ast::Manager* p, ast::Literal_Symbol_Node* id, ast::Literal_Struct_Node* structure) {
  lib::insert(ctx->scope->structures, id->get_symbol_id(), structure);
}

ast::Literal_Struct_Node* scope_get_struct_definition(Scope* ctx, ast::Manager* m, ast::Literal_Symbol_Node* id) {
  if (ctx == NULL) {
    return NULL;
  }

  ast::Literal_Struct_Node** ref = lib::search(ctx->structures, id->get_symbol_id());

  if (ref) {
    return *ref;
  }

  return scope_get_struct_definition(ctx->parent, m, id);
}

ast::Literal_Struct_Node* context_get_struct_definition(Context* ctx, ast::Manager* m, ast::Literal_Symbol_Node* id) {
  if (ctx == NULL) {
    return NULL;
  }

  if (ast::Literal_Struct_Node* structure = scope_get_struct_definition(ctx->scope, m, id)) {
    return structure;
  }

  return context_get_struct_definition(ctx->parent, m, id);
}

ast::Node* context_type_of(Scope* ctx, ast::Manager* m, compiler::symbol::Id symbol) {
  if (ctx == NULL) {
    return NULL;
  }

  Declaration** decl = lib::search(ctx->scope, symbol);

  if (decl) {
    Declaration* local = *decl;

    return local->type;
  }

  return context_type_of(ctx->parent, m, symbol);
}

ast::Node* context_type_of(Context* ctx, ast::Manager* m, compiler::symbol::Id symbol) {
  if (ctx == NULL) {
    return NULL;
  }

  if (ast::Node* node = context_type_of(ctx->scope, m, symbol)) {
    return node;
  }

  return context_type_of(ctx->parent, m, symbol);
}

ast::Node* scope_type_of(Scope* ctx, ast::Manager* m, ast::Literal_Symbol_Node* symbol) {
  if (ctx == NULL) {
    return NULL;
  }

  Declaration** decl = lib::search(ctx->scope, symbol->get_symbol_id());

  if (decl) {
    Declaration* local = *decl;

    return local->type;
  }

  return scope_type_of(ctx->parent, m, symbol);
}

ast::Node* context_type_of(Context* ctx, ast::Manager* m, ast::Literal_Symbol_Node* symbol) {
  if (ctx == NULL) {
    return NULL;
  }

  if (ast::Node* node = scope_type_of(ctx->scope, m, symbol)) {
    return node;
  }

  return context_type_of(ctx->parent, m, symbol);
}

ast::Node* context_type_of(Context* ctx, ast::Manager* m, ast::Member_Access_Node* access) {
  if (ctx == NULL) {
    return NULL;
  }

  ast::Literal_Symbol_Node* object = ast::is_instance< ast::Literal_Symbol_Node* >(access->get_object(m));

  assert(object);

  ast::Node* type = context_type_of(ctx, m, object);

  assert(type);

  while (ast::is_semantic_node(access->get_access(m))) {
    if (ast::Literal_Symbol_Node* s = ast::is_instance< ast::Literal_Symbol_Node* >(type)) {
      type = context_get_struct_definition(ctx, m, s);
    }

    assert(ast::is_instance< ast::Literal_Struct_Node* >(type));

    ast::Literal_Struct_Node* structure = ast::as< ast::Literal_Struct_Node* >(type);

    ast::ProgramPoint_List_Node* members = structure->get_members(m);

    ast::Literal_Symbol_Node* symbol = ast::as< ast::Literal_Symbol_Node* >(access->get_access(m));

    ast::Node* accessed = NULL;

    while (ast::is_semantic_node(members) && !ast::is_semantic_node(accessed)) {
      ast::Node* declaration = members->get_statement(m);

      if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(declaration)) {
        ast::Literal_Symbol_Node* name = var->get_symbol(m);

        compiler::symbol::Symbol symbol_a = symbol->get_symbol(m);
        compiler::symbol::Symbol symbol_b = name->get_symbol(m);

        if (compiler::symbol::is_equal(m->symbol_table, &symbol_a, &symbol_b)) {
          accessed = declaration;
        }
      }

      if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(declaration)) {
        ast::Literal_Symbol_Node* name = var->get_symbol(m);

        compiler::symbol::Symbol symbol_a = symbol->get_symbol(m);
        compiler::symbol::Symbol symbol_b = name->get_symbol(m);

        if (compiler::symbol::is_equal(m->symbol_table, &symbol_a, &symbol_b)) {
          accessed = declaration;
        }
      }

      members = members->get_next_program_point(m);
    }

    if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(accessed)) {
      object = var->get_symbol(m);
      type   = var->get_type(m);
    }

    if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(accessed)) {
      object = var->get_symbol(m);
      type   = var->get_type(m);
    }

    access = access->get_next_accesses(m);
  }

  assert(ast::is_semantic_node(type));

  return type;
}

// ast::Node* context_type_of(Context* ctx, ast::Manager* m, ast::Member_Access_Node* access) {
//   if (ctx == NULL) {
//     return NULL;
//   }

//   if (ast::Node* node = scope_type_of(ctx, ctx->scope, m, access)) {
//     return node;
//   }

//   return context_type_of(ctx->parent, m, access);
// }
b8 scope_find(Scope* scope, ast::Literal_Symbol_Node* symbol) {
  if (scope == NULL) {
    return false;
  }

  if (lib::search(scope->scope, symbol->get_symbol_id()) != NULL) {
    return true;
  }

  return scope_find(scope->parent, symbol);
}

b8 context_is_local(Context* ctx, ast::Literal_Symbol_Node* symbol) {
  return scope_find(ctx->scope, symbol);
}

b8 scope_find(Scope* scope, compiler::symbol::Id symbol) {
  if (scope == NULL) {
    return false;
  }

  if (lib::search(scope->scope, symbol) != NULL) {
    return true;
  }

  return scope_find(scope->parent, symbol);
}

b8 context_is_local(Context* ctx, compiler::symbol::Id symbol) {
  return scope_find(ctx->scope, symbol);
}

b8 context_is_defined(Context* ctx, compiler::symbol::Id symbol) {
  if (ctx == NULL) {
    return false;
  }

  if (context_is_local(ctx, symbol)) {
    return true;
  }

  return context_is_defined(ctx->parent, symbol);
}

b8 context_is_defined(Context* ctx, ast::Literal_Symbol_Node* symbol) {
  if (ctx == NULL) {
    return false;
  }

  if (context_is_local(ctx, symbol)) {
    return true;
  }

  return context_is_defined(ctx->parent, symbol);
}

void declaration_print(Declaration* d, ast::Manager* m, int tabs) {
  if (d == NULL) {
    return;
  }

  parser::print_ast_ir(m, d->symbol);

  printf("\n");
}

void scope_print_rec(lib::TableNode< compiler::symbol::Id, Declaration* >* decl, ast::Manager* m, int tabs) {
  if (decl == NULL) {
    return;
  }
  for (u32 i = 0; i < tabs; i++)
    printf(" ");

  declaration_print(decl->val, m, tabs);
  scope_print_rec(decl->left, m, tabs);
  scope_print_rec(decl->right, m, tabs);
}

void scope_print(Scope* ctx, ast::Manager* m, int tabs) {
  if (ctx == NULL) {
    return;
  }

  for (u32 i = 0; i < tabs; i++) {
    printf(" ");
  }

  printf("scope {\n");

  tabs = tabs + 2;

  scope_print_rec(ctx->scope->root, m, tabs);

  tabs = tabs - 2;

  scope_print(ctx->parent, m, tabs + 2);

  for (u32 i = 0; i < tabs; i++) {
    printf(" ");
  }

  printf("}");
  printf("\n");
}

void context_print(Context* ctx, ast::Manager* m, int tabs) {
  if (ctx == NULL) {
    return;
  }

  for (u32 i = 0; i < tabs; i++) {
    printf(" ");
  }

  printf("context {\n");

  tabs = tabs + 2;

  scope_print(ctx->scope, m, tabs);

  tabs = tabs - 2;

  context_print(ctx->parent, m, tabs + 2);

  for (u32 i = 0; i < tabs; i++) {
    printf(" ");
  }

  printf("}");
  printf("\n");
}

Context* context_from_declarations_list(ast::Manager* m, ast::Declarations_List_Node* node, Context* parent) {
  Context* ctx = context_create(parent);

  while (ast::is_semantic_node(node)) {
    ast::Node* arg = node->get_declaration(m);

    if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(arg)) {
      context_declare(ctx, m, var);
    }

    if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(arg)) {
      context_declare(ctx, m, var);
    }

    node = node->get_next_declaration(m);
  }

  return ctx;
}

} // namespace context
