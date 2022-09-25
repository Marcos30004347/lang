#include "context.hpp"
#include "ast/ast_declaration.hpp"
#include "ast/ast_function.hpp"
#include "ast/ast_literals.hpp"
#include "ast/ast_manager.hpp"
#include "ast/ast_operations.hpp"
#include "ast/ast_program_point.hpp"
#include "ast/ast_types.hpp"
#include "compiler/symbol_table.hpp"
#include "lib/table.hpp"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unordered_map>

struct Declaration {
  b8 constant;

  ast::Literal_Symbol_Node* symbol;

  ast::Node* type;

  ast::Node* bind;
};

struct Context {
  lib::Table< compiler::symbol::Id, Declaration* >* scope;

  lib::Table< compiler::symbol::Id, ast::Literal_Struct_Node* >* structures;

  Context* parent;
};

Context* context_create(Context* parent) {
  Context* context = new Context();

  context->parent = parent;

  context->scope = lib::table_create< compiler::symbol::Id, Declaration* >();

  context->structures = lib::table_create< compiler::symbol::Id, ast::Literal_Struct_Node* >();

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

  lib::table_delete(context->scope);
  lib::table_delete(context->structures);

  delete context;

  return parent;
}

void context_declare(Context* ctx, parser::Parser* p, ast::Node* declaration) {
  ast::Manager* m = p->ast_manager;

  if (ast::Variable_Assignment_Node* assignment =
          ast::is_instance< ast::Variable_Assignment_Node* >(declaration)) {
    ast::Node* left  = assignment->get_left_operand(p->ast_manager);
    ast::Node* right = assignment->get_right_operand(p->ast_manager);

    context_declare(ctx, p, left);

    if (ast::Literal_Struct_Node* structure = ast::is_instance< ast::Literal_Struct_Node* >(right)) {
      assert(ast::is_instance< ast::Declaration_Constant_Node* >(left));

      ast::Declaration_Constant_Node* decl   = ast::as< ast::Declaration_Constant_Node* >(left);
      ast::Literal_Symbol_Node*       symbol = decl->get_symbol(p->ast_manager);

      lib::insert(ctx->structures, symbol->get_symbol(p->ast_manager).id, structure);
    }

    return;
  }

  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(declaration)) {
    ast::Literal_Symbol_Node* symbol = var->get_symbol(p->ast_manager);
    ast::Node*                type   = var->get_type(p->ast_manager);

    Declaration* data = declaration_create(declaration, symbol, type);

    lib::insert(ctx->scope, symbol->get_symbol(p->ast_manager).id, data);

    return;
  }

  if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(declaration)) {
    ast::Literal_Symbol_Node* symbol = var->get_symbol(p->ast_manager);
    ast::Node*                type   = var->get_type(p->ast_manager);

    Declaration* data = declaration_create(declaration, symbol, type);

    // setup_declaration_values(ctx, p, d, type, ast::create_node_literal_undefined(p->ast_manager),
    // program_point);

    lib::insert(ctx->scope, symbol->get_symbol(p->ast_manager).id, data);

    return;
  }
}

ast::Node* context_type_of(Context* ctx, parser::Parser* p, ast::Literal_Symbol_Node* symbol) {
  ast::Manager* m = p->ast_manager;

  if (ctx == NULL) {
    return ast::create_node_literal_nothing(m);
  }

  if (ast::is_instance< ast::Member_Access_Node* >(symbol)) {
    ast::Member_Access_Node* access = ast::as< ast::Member_Access_Node* >(symbol);

    ast::Node* object      = access->get_object(p->ast_manager);
    ast::Node* object_type = context_type_of(ctx, symbol);

    assert(ast::is_instance< ast::Literal_Symbol_Node* >(object_type));

    ast::Literal_Symbol_Node* symbol = ast::as< ast::Literal_Symbol_Node* >(object_type);

    ast::Literal_Struct_Node** structure_ref = lib::search(ctx->structures, symbol->get_symbol_id());

    assert(structure_ref);

    ast::Node* type = *structure_ref;

    while (ast::is_semantic_node(access)) {

      assert(ast::is_instance< ast::Literal_Struct_Node* >(type));

      ast::Literal_Struct_Node* structure = ast::as< ast::Literal_Struct_Node* >(type);

      ast::ProgramPoint_List_Node* members = structure->get_members(m);

      ast::Node* accessed = access->get_access(m);

      assert(ast::is_instance< ast::Literal_Symbol_Node* >(accessed));

      ast::Literal_Symbol_Node* symbol = ast::as< ast::Literal_Symbol_Node* >(accessed);

      while (ast::is_semantic_node(members) && !ast::is_semantic_node(accessed)) {
        ast::Node* declaration = members->get_statement(m);

        if (ast::Declaration_Constant_Node* var =
                ast::is_instance< ast::Declaration_Constant_Node* >(declaration)) {
          ast::Literal_Symbol_Node* name = var->get_symbol(m);

          compiler::symbol::Symbol symbol_a = symbol->get_symbol(m);
          compiler::symbol::Symbol symbol_b = name->get_symbol(m);

          if (compiler::symbol::is_equal(m->symbol_table, &symbol_a, &symbol_b)) {
            accessed = declaration;
          }
        }

        if (ast::Declaration_Variable_Node* var =
                ast::is_instance< ast::Declaration_Variable_Node* >(declaration)) {
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

  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(symbol)) {
    symbol = var->get_symbol(m);
  }

  if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(symbol)) {
    symbol = var->get_symbol(m);
  }

  assert(ast::is_instance< ast::Literal_Symbol_Node* >(symbol));

  Declaration** decl = lib::search(ctx->scope, symbol->get_symbol_id());

  if (decl) {
    Declaration* local = *decl;

    return local->type;
  }

  return context_type_of(ctx->parent, p, symbol);
}

// Declaration* context_declaration_of(Context* ctx, parser::Parser* p, ast::Node* symbol, b8* is_local) {
//   return context_declaration_of_rec(ctx, p, symbol, ctx, is_local);
// }

// ast::Node* context_type_of(Context* ctx, parser::Parser* p, ast::Node* symbol) {
//   Declaration* declaration = context_declaration_of(ctx, p, symbol);

//   ast::Manager* m = p->ast_manager;

//   if (declaration == NULL) {
//     return ast::create_node_literal_nothing(p->ast_manager);
//   }

//   return declaration->type;
// }

// Assignments* context_values_of(Context* ctx, parser::Parser* p, ast::Node* symbol) {
//   Declaration* declaration = context_declaration_of(ctx, p, symbol);

//   ast::Manager* m = p->ast_manager;

//   if (declaration == NULL)
//     return NULL;

//   return &declaration->assignments;
// }

// Assignment* assignment_copy(Assignment* a) {
//   if (a == NULL) return NULL;
//   return assignment_create(a->value, a->point, assignment_copy(a->previous));
// }

// Declaration* declaration_copy(Declaration* d) {

//   if (d == NULL)
//     return NULL;

//   Declaration* r = declaration_create(d->bind, d->symbol, d->type);

//   r->previous_declaration = declaration_copy(d->previous_declaration);

//   // r->assignments = d->assignments; // assignment_copy(d->assignments);

//   return r;
// }

// Context* context_copy(Context* a) {
//   if (a == NULL)
//     return NULL;

//   Context* c = context_create(context_copy(a->parent));

//   c->last_declaration = declaration_copy(a->last_declaration);

//   return c;
// }

// void context_replace(Context* a, Context* b) {
//   if (a == NULL) {
//     assert(b == NULL);
//     return;
//   }

//   context_replace(a->parent, b->parent);

//   Declaration* t = a->last_declaration;

//   a->last_declaration = b->last_declaration;
//   b->last_declaration = t;
// }

// void assignment_print(Assignments* a, parser::Parser* p, int tabs) {
//   if (a == NULL) {
//     return;
//   }

//   ast::Manager* m = p->ast_manager;

//   u64 i = 0;

//   for (Assignments::iterator it = a->begin(); it != a->end(); it++) {
//     i = i + 1;

//     ast::Node* v = *it;

//     if (ast::is_instance< ast::Literal_Struct_Node* >(v)) {
//       printf("#struct_literal");
//     } else {
//       print_ast_ir(p, v, tabs);
//     }

//     if (i < a->size()) {
//       printf("\n ");
//     }
//   }
// }

void declaration_print(Declaration* d, parser::Parser* p, int tabs) {
  if (d == NULL)
    return;
  print_ast_ir(p, d->symbol);
  printf("\n");
}

void context_print_rec(lib::TableNode< compiler::symbol::Id, Declaration* >* decl, parser::Parser* p, int tabs) {
  for (u32 i = 0; i < tabs; i++)
    printf(" ");

  declaration_print(decl->val, p, tabs);
  context_print_rec(decl->left, p, tabs);
  context_print_rec(decl->right, p, tabs);
}

void context_print(Context* ctx, parser::Parser* p, int tabs) {
  if (ctx == NULL)
    return;

  Declaration* declaration = ctx->scope->root->val;

  for (u32 i = 0; i < tabs; i++) {
    printf(" ");
  }

  printf("context {\n");

  tabs = tabs + 2;

  context_print_rec(ctx->scope->root, p, tabs);

  tabs = tabs - 2;

  context_print(ctx->parent, p, tabs + 2);

  for (u32 i = 0; i < tabs; i++) {
    printf(" ");
  }

  printf("}");
  printf("\n");
}

Context* declaration_arguments_to_context(parser::Parser* p, ast::Node* node, Context* parent) {
  Context* ctx = context_create(parent);

  if (ast::is_instance< ast::Literal_Nothing_Node* >(node)) {
    return ctx;
  }

  ast::Manager* m = p->ast_manager;

  assert(ast::is_instance< ast::Declarations_List_Node* >(node));

  ast::Declarations_List_Node* list = ast::as< ast::Declarations_List_Node* >(node);

  while (!ast::is_instance< ast::Literal_Nothing_Node* >(list)) {
    ast::Node* arg = list->get_declaration(p->ast_manager);

    assert(ast::is_declaration_node(arg));

    context_declare(ctx, p, arg);

    list = list->get_next_declaration(p->ast_manager);
  }

  return ctx;
}

// Context* context_push(Context* r, ast::Node* n) {
//   assert(n->kind == AST_BIND_TYPE || n->kind == AST_BIND_CONSTANT || n->kind == AST_BIND_VARIABLE);

//   Context* c = (Context*)malloc(sizeof(Context));

//   c->prev = r;
//   c->decl = n->id;
//   c->next = NULL;

//   if (r) { r->next = c; }

//   return c;
// }

// Context* context_pop(Context* r) {
//   Context* p = r->prev;

//   free(r);

//   return p;
// }

// ast::Node* context_find(Context* env, parser::Parser** p, ast::Node* sym) {
//   ast::Manager* m = p->ast_manager;

//   if (env == NULL) return ast_node_null(m);

//   ast::Node* node = ast::manager_get(m, env->decl);

//   assert(node->kind == AST_BIND_TYPE || node->kind == AST_BIND_CONSTANT || node->kind ==
//   AST_BIND_VARIABLE);

//   ast::Node* root = node;

//   if (root->kind == AST_BIND_CONSTANT || root->kind == AST_BIND_VARIABLE) { root =
//   ast_bind_get_type_bind(m, root); }

//   ast::Node* symb = ast_type_bind_get_symbol(m, root);

//   if (parser_is_same_symbol(p, sym, symb)) { return node; }

//   return context_find(env->prev, p, sym);
// }

// void scope_init(Scope* scope, Scope* parent) {
//   scope->ctx    = NULL;
//   scope->parent = parent;
// }

// void scope_print_rec(Scope* s, parser::Parser** p) {
//   if (s == NULL) return;

//   printf("{ ");

//   Context* ctx = s->ctx;

//   while (ctx) {
//     ast::Node* n = ast::manager_get(p->ast_manager, ctx->decl);
//     ast::Node* s = ast_node_null(p->ast_manager);

//     if (n->kind == AST_BIND_TYPE) {
//       s = ast_type_bind_get_symbol(p->ast_manager, n);
//     } else {
//       ast::Node* t = ast_bind_get_type_bind(p->ast_manager, n);
//       s           = ast_type_bind_get_symbol(p->ast_manager, t);
//     }

//     i8 buff[256];

//     if (ast::is_temporary(p->ast_manager, s)) {
//       u64 t   = s->kind;
//       u64 i   = 1;
//       buff[0] = '%';
//       while (t) {
//         buff[i++] = t % 10 + '0';
//         t         = t / 10;
//       }
//       buff[i] = '\0';
//     } else {
//       token_get_id(&p->lex, s->tok, buff);
//     }
//     printf("%s", buff);

//     ctx = ctx->prev;
//     if (ctx) printf(", ");
//   }

//   printf(" }");

//   scope_print_rec(s->parent, p);
// }

// void scope_print(Scope* s, parser::Parser** p) {
//   scope_print_rec(s, p);
//   printf("\n");
// }

// Scope* scope_create(Scope* parent) {
//   Scope* s = (Scope*)malloc(sizeof(Scope));
//   scope_init(s, parent);
//   return s;
// }

// ast::Node* scope_find_rec(Scope* s, parser::Parser** p, ast::Node* sym, Scope** owner) {
//   ast::Manager* m = p->ast_manager;

//   if (s == NULL) return ast_node_null(m);

//   ast::Node* n = context_find(s->ctx, p, sym);

//   if (!ast::is_instance<ast::Literal_Nothing_Node*>(n)) {
//     if (owner) { *owner = s; }
//     return n;
//   }

//   return scope_find_rec(s->parent, p, sym, owner);
// }

// ast::Node* scope_find(Scope* s, parser::Parser** p, ast::Node* sym, b8* is_local) {
//   if (is_local) *is_local = false;

//   Scope* scope = NULL;

//   ast::Node* n = scope_find_rec(s, p, sym, &scope);

//   if (scope == s && is_local) { *is_local = true; }

//   return n;
// }

// ast::Node* scope_find_local(Scope* s, parser::Parser** p, ast::Node* sym) {
//   ast::Manager* m = p->ast_manager;
//   if (s == NULL) { return ast_node_null(m); }

//   return context_find(s->ctx, p, sym);
// }

// void scope_push(Scope* s, ast::Node* n) {
//   assert(n->kind == AST_BIND_TYPE || n->kind == AST_BIND_CONSTANT || n->kind == AST_BIND_VARIABLE);
//   s->ctx = context_push(s->ctx, n);
// }

// void scope_pop(Scope* s) { s->ctx = context_pop(s->ctx); }

// b8 scope_is_global(Scope* s) { return s->parent == NULL; }

// u64 get_scope_depth(Scope* s) {
//   u64 d = 0;

//   while (s) {
//     d = d + 1;
//     s = s->parent;
//   }

//   return d;
// }
