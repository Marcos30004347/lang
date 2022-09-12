#include "context.hpp"
#include "ast.hpp"
#include "parser.hpp"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unordered_map>

Context* context_create(Context* parent) {
  Context* ctx          = new Context();
  ctx->parent           = parent;
  ctx->last_declaration = NULL;
  return ctx;
}

// void assignment_destroy(Assignment* b) {
//   while (b) {
//     Assignment* a = b->previous;
//     delete b;
//     b = a;
//   }
// }

Declaration* declaration_create(AST_Node* declaration, AST_Node* symbol, AST_Node* type) {
  assert(declaration->kind == AST_BIND_TYPE);

  Declaration* d = new Declaration();

  d->bind                 = declaration;
  d->symbol               = symbol;
  d->type                 = type;
  d->assignments          = Assignments();
  d->context              = NULL;
  d->previous_declaration = NULL;

  return d;
}

void assignment_create(Declaration* d, AST_Node* value, AST_Node* program_point) {
  d->assignments = Assignments();
  d->assignments.insert(value);
}

void context_merge(Parser* p, Context* a, Context* b) {
  AST_Manager* m = &p->ast_man;

  Declaration* b_decl = b->last_declaration;

  while (b_decl) {
    Declaration* a_decl = context_declaration_of(a, p, b_decl->symbol);

    if (a_decl != NULL) {
      if (a_decl->context) {
        assert(b_decl->context);
        context_merge(p, a_decl->context, b_decl->context);
      } else {
        for (Assignments::iterator assignment = b_decl->assignments.begin(); assignment != b_decl->assignments.end(); assignment++) {
          a_decl->assignments.insert(*assignment);
        }
      }
    } else {
      Declaration* d = declaration_create(b_decl->bind, b_decl->symbol, b_decl->type);
      if (b_decl->context) {
        d->context = context_copy(b_decl->context);
      } else {

        d->assignments = b_decl->assignments;

        for (Assignments::iterator assignment = b_decl->assignments.begin(); assignment != b_decl->assignments.end(); assignment++) {
          d->assignments.insert(*assignment);
        }

        d->previous_declaration = a->last_declaration;
      }
      a->last_declaration = d;
    }

    b_decl = b_decl->previous_declaration;
  }
}

Context* context_destroy(Context* ctx) {
  Context* p = ctx->parent;

  while (ctx->last_declaration) {
    Declaration* t = ctx->last_declaration->previous_declaration;

    // assignment_destroy(ctx->last_declaration->assignments);

    if (ctx->last_declaration->context) { context_destroy(ctx->last_declaration->context); }

    delete ctx->last_declaration;

    ctx->last_declaration = t;
  }

  delete ctx;

  return p;
}

void setup_declaration_values(Context* ctx, Parser* p, Declaration* d, AST_Node* type, AST_Node* value, AST_Node* program_point) {
  AST_Node*    type_symbol = type;
  AST_Manager* m           = &p->ast_man;

  if (type_symbol->kind == AST_TYPE_POINTER) { type_symbol = ast_manager_get_relative(m, type_symbol, type_symbol->left); }

  Declaration* type_declaration = NULL;

  if (ast_is_temporary(m, type_symbol) || type_symbol->kind == AST_SYMBOL_LITERAL) { type_declaration = context_declaration_of(ctx, p, type_symbol); }

  if (type_declaration && type_declaration->type->kind == AST_TYPE_STRUCT) {
    AST_Node* struct_type    = type_declaration->type;
    AST_Node* struct_members = ast_manager_get_relative(m, struct_type, struct_type->left);

    d->context = context_create(NULL);
    assignment_create(d, NULL, program_point);

    while (!ast_is_null_node(struct_members)) {
      AST_Node* member = ast_program_point_get_decl(m, struct_members);

      context_declare(d->context, p, member, program_point);

      struct_members = ast_program_point_get_tail(m, struct_members);
    }
  } else {
    d->context = NULL;
    assignment_create(d, value, program_point);
  }
}

Declaration* context_declare(Context* ctx, Parser* p, AST_Node* declaration, AST_Node* program_point) {
  AST_Manager* m = &p->ast_man;

  if (declaration->kind == AST_BIND_TYPE) {
    AST_Node*    symbol = ast_type_bind_get_symbol(m, declaration);
    AST_Node*    type   = ast_type_bind_get_type(m, declaration);
    Declaration* d      = declaration_create(declaration, symbol, type);

    setup_declaration_values(ctx, p, d, type, ast_undefined(m), program_point);

    d->previous_declaration = ctx->last_declaration;

    ctx->last_declaration = d;

    return d;
  }

  assert(declaration->kind == AST_BIND_CONSTANT || declaration->kind == AST_BIND_VARIABLE);

  AST_Node* bind   = ast_bind_get_type_bind(m, declaration);
  AST_Node* symbol = ast_type_bind_get_symbol(m, bind);
  AST_Node* type   = ast_type_bind_get_type(m, bind);
  AST_Node* expr   = ast_bind_get_expr(m, declaration);

  Declaration* d = declaration_create(bind, symbol, type);

  setup_declaration_values(ctx, p, d, type, expr, program_point);

  d->previous_declaration = ctx->last_declaration;

  ctx->last_declaration = d;

  return d;
}

void context_assign(Context* ctx, Parser* p, AST_Node* assignment, AST_Node* program_point) {
  assert(assignment->kind == AST_OP_BIN_ASSIGN);

  AST_Manager* m    = &p->ast_man;
  AST_Node*    left = ast_manager_get_relative(m, assignment, assignment->left);

  while (left->kind == AST_OP_MEMBER_ACCESS) {
    AST_Node*    symbol = ast_manager_get_relative(m, left, left->left);
    Declaration* decl   = context_declaration_of(ctx, p, symbol);

    assert(decl);

    ctx = decl->context;

    left = ast_manager_get_relative(m, left, left->right);
  }

  Declaration* decl = context_declaration_of(ctx, p, left);

  // assignment_destroy(decl->assignments);

  assignment_create(decl, ast_manager_get_relative(m, assignment, assignment->right), program_point);
}

Declaration* context_declaration_of_rec(Context* ctx, Parser* p, AST_Node* symbol, Context* _c, b8* is_local) {

  if (ctx == NULL) {
    if (is_local) *is_local = false;
    return NULL;
  }

  AST_Manager* m = &p->ast_man;

  if (symbol->kind == AST_OP_MEMBER_ACCESS) {
    AST_Node* first  = ast_manager_get_relative(m, symbol, symbol->left);
    AST_Node* second = ast_manager_get_relative(m, symbol, symbol->left);

    Declaration* decl = context_declaration_of_rec(ctx, p, first, _c, is_local);

    return context_declaration_of_rec(decl->context, p, second, _c, is_local);
  }

  if (symbol->kind == AST_BIND_TYPE) { symbol = ast_type_bind_get_symbol(m, symbol); }

  assert(ast_is_temporary(m, symbol) || symbol->kind == AST_SYMBOL_LITERAL);

  Declaration* declaration = ctx->last_declaration;
  while (declaration) {

    if (parser_is_same_symbol(p, symbol, declaration->symbol)) {

      if (is_local) { *is_local = ctx == _c; }

      return declaration;
    }

    declaration = declaration->previous_declaration;
  }

  return context_declaration_of_rec(ctx->parent, p, symbol, _c, is_local);
}

Declaration* context_declaration_of(Context* ctx, Parser* p, AST_Node* symbol, b8* is_local) { return context_declaration_of_rec(ctx, p, symbol, ctx, is_local); }

AST_Node* context_type_of(Context* ctx, Parser* p, AST_Node* symbol) {
  Declaration* declaration = context_declaration_of(ctx, p, symbol);

  AST_Manager* m = &p->ast_man;

  if (declaration == NULL) return ast_node_null(m);

  return declaration->type;
}

Assignments* context_values_of(Context* ctx, Parser* p, AST_Node* symbol) {
  Declaration* declaration = context_declaration_of(ctx, p, symbol);

  AST_Manager* m = &p->ast_man;

  if (declaration == NULL) return NULL;

  return &declaration->assignments;
}

// Assignment* assignment_copy(Assignment* a) {
//   if (a == NULL) return NULL;
//   return assignment_create(a->value, a->point, assignment_copy(a->previous));
// }

Declaration* declaration_copy(Declaration* d) {

  if (d == NULL) return NULL;

  Declaration* r = declaration_create(d->bind, d->symbol, d->type);

  r->previous_declaration = declaration_copy(d->previous_declaration);

  r->assignments = d->assignments; // assignment_copy(d->assignments);

  return r;
}

Context* context_copy(Context* a) {
  if (a == NULL) return NULL;

  Context* c = context_create(context_copy(a->parent));

  c->last_declaration = declaration_copy(a->last_declaration);

  return c;
}

void context_replace(Context* a, Context* b) {
  if (a == NULL) {
    assert(b == NULL);
    return;
  }

  context_replace(a->parent, b->parent);

  Declaration* t = a->last_declaration;

  a->last_declaration = b->last_declaration;
  b->last_declaration = t;
}

void assignment_print(Assignments* a, Parser* p, int tabs) {
  if (a == NULL) return;
  AST_Manager* m = &p->ast_man;

  u64 i = 0;
  for (Assignments::iterator it = a->begin(); it != a->end(); it++) {
    i           = i + 1;
    AST_Node* v = *it;
    if (v->kind == AST_FUNCTION_LITERAL) v = ast_function_literal_get_signature(m, v);
    if (v->kind == AST_TYPE_STRUCT) printf("struct");
    else print_ast_to_program(p, v, tabs);
    if (i < a->size()) { printf("\n "); }
  }
}

void declaration_print(Declaration* d, Parser* p, int tabs) {
  if (d == NULL) return;
  printf("[| ");
  print_ast_to_program(p, d->symbol);
  printf(" |] = ( ");
  if (d->context) {
    context_print(d->context, p, tabs);
  } else {
    assignment_print(&d->assignments, p, tabs);
  }
  printf(" )\n");
}

void context_print(Context* ctx, Parser* p, int tabs) {
  if (ctx == NULL) return;
  Declaration* declaration = ctx->last_declaration;
  for (u32 i = 0; i < tabs; i++)
    printf(" ");
  printf("context {\n");
  tabs = tabs + 2;
  while (declaration) {
    for (u32 i = 0; i < tabs; i++)
      printf(" ");
    declaration_print(declaration, p, tabs);
    declaration = declaration->previous_declaration;
  }
  tabs = tabs - 2;
  context_print(ctx->parent, p, tabs + 2);

  for (u32 i = 0; i < tabs; i++)
    printf(" ");

  printf("}");
  printf("\n");
}

Context* declaration_arguments_to_context(Parser* p, AST_Node* node, Context* parent) {
  Context* ctx = context_create(parent);

  if (ast_is_null_node(node)) { return ctx; }

  AST_Manager* m = &p->ast_man;

  assert(node->kind == AST_DECL_ARGS_LIST);

  while (!ast_is_null_node(node)) {
    AST_Node* arg = ast_decl_list_get_elem(m, node);
    if (!ast_is_null_node(arg)) {
      if (arg->kind == AST_BIND_CONSTANT || arg->kind == AST_BIND_VARIABLE) { arg = ast_manager_get_relative(m, arg, arg->left); }
      assert(arg->kind == AST_BIND_TYPE);
      context_declare(ctx, p, arg, node);
    }

    node = ast_decl_list_get_tail(m, node);
  }
  return ctx;
}

// Context* context_push(Context* r, AST_Node* n) {
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

// AST_Node* context_find(Context* env, Parser* p, AST_Node* sym) {
//   AST_Manager* m = &p->ast_man;

//   if (env == NULL) return ast_node_null(m);

//   AST_Node* node = ast_manager_get(m, env->decl);

//   assert(node->kind == AST_BIND_TYPE || node->kind == AST_BIND_CONSTANT || node->kind == AST_BIND_VARIABLE);

//   AST_Node* root = node;

//   if (root->kind == AST_BIND_CONSTANT || root->kind == AST_BIND_VARIABLE) { root = ast_bind_get_type_bind(m, root); }

//   AST_Node* symb = ast_type_bind_get_symbol(m, root);

//   if (parser_is_same_symbol(p, sym, symb)) { return node; }

//   return context_find(env->prev, p, sym);
// }

// void scope_init(Scope* scope, Scope* parent) {
//   scope->ctx    = NULL;
//   scope->parent = parent;
// }

// void scope_print_rec(Scope* s, Parser* p) {
//   if (s == NULL) return;

//   printf("{ ");

//   Context* ctx = s->ctx;

//   while (ctx) {
//     AST_Node* n = ast_manager_get(&p->ast_man, ctx->decl);
//     AST_Node* s = ast_node_null(&p->ast_man);

//     if (n->kind == AST_BIND_TYPE) {
//       s = ast_type_bind_get_symbol(&p->ast_man, n);
//     } else {
//       AST_Node* t = ast_bind_get_type_bind(&p->ast_man, n);
//       s           = ast_type_bind_get_symbol(&p->ast_man, t);
//     }

//     i8 buff[256];

//     if (ast_is_temporary(&p->ast_man, s)) {
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

// void scope_print(Scope* s, Parser* p) {
//   scope_print_rec(s, p);
//   printf("\n");
// }

// Scope* scope_create(Scope* parent) {
//   Scope* s = (Scope*)malloc(sizeof(Scope));
//   scope_init(s, parent);
//   return s;
// }

// AST_Node* scope_find_rec(Scope* s, Parser* p, AST_Node* sym, Scope** owner) {
//   AST_Manager* m = &p->ast_man;

//   if (s == NULL) return ast_node_null(m);

//   AST_Node* n = context_find(s->ctx, p, sym);

//   if (!ast_is_null_node(n)) {
//     if (owner) { *owner = s; }
//     return n;
//   }

//   return scope_find_rec(s->parent, p, sym, owner);
// }

// AST_Node* scope_find(Scope* s, Parser* p, AST_Node* sym, b8* is_local) {
//   if (is_local) *is_local = false;

//   Scope* scope = NULL;

//   AST_Node* n = scope_find_rec(s, p, sym, &scope);

//   if (scope == s && is_local) { *is_local = true; }

//   return n;
// }

// AST_Node* scope_find_local(Scope* s, Parser* p, AST_Node* sym) {
//   AST_Manager* m = &p->ast_man;
//   if (s == NULL) { return ast_node_null(m); }

//   return context_find(s->ctx, p, sym);
// }

// void scope_push(Scope* s, AST_Node* n) {
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
