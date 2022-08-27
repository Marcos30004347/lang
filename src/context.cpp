#include "context.hpp"
#include "ast.hpp"
#include "parser.hpp"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

Context* context_push(Context* r, AST_Node* n) {
  assert(n->kind == AST_BIND_TYPE || n->kind == AST_BIND_CONSTANT || n->kind == AST_BIND_VARIABLE);

  Context* c = (Context*)malloc(sizeof(Context));

  c->prev = r;
  c->decl = n->id;
  c->next = NULL;

  if (r) {
    r->next = c;
  }

  return c;
}

Context* context_pop(Context* r) {
  Context* p = r->prev;

  free(r);

  return p;
}

AST_Node* context_find(Context* env, Parser* p, AST_Node* sym) {
  AST_Manager* m = &p->ast_man;

  if (env == NULL)
    return ast_node_null(m);

  AST_Node* node = ast_manager_get(m, env->decl);

  assert(node->kind == AST_BIND_TYPE || node->kind == AST_BIND_CONSTANT || node->kind == AST_BIND_VARIABLE);

  AST_Node* root = node;

  if (root->kind == AST_BIND_CONSTANT || root->kind == AST_BIND_VARIABLE) {
    root = ast_bind_get_type_bind(m, root);
  }

  AST_Node* symb = ast_type_bind_get_symbol(m, root);

  if (parser_is_same_symbol(p, sym, symb)) {
    return node;
  }

  return context_find(env->prev, p, sym);
}

void scope_init(Scope* scope, Scope* parent) {
  scope->ctx    = NULL;
  scope->parent = parent;
}

void scope_print_rec(Scope* s, Parser* p) {
  if (s == NULL)
    return;

  printf("{ ");

  Context* ctx = s->ctx;

  while (ctx) {
    AST_Node* n = ast_manager_get(&p->ast_man, ctx->decl);
    AST_Node* s = ast_node_null(&p->ast_man);

    if (n->kind == AST_BIND_TYPE) {
      s = ast_type_bind_get_symbol(&p->ast_man, n);
    } else {
      AST_Node* t = ast_bind_get_type_bind(&p->ast_man, n);
      s           = ast_type_bind_get_symbol(&p->ast_man, t);
    }

    i8 buff[256];

    if (ast_is_temporary(&p->ast_man, s)) {
      u64 t   = s->kind;
      u64 i   = 1;
      buff[0] = '%';
      while (t) {
        buff[i++] = t % 10 + '0';
        t         = t / 10;
      }
      buff[i] = '\0';
    } else {
      token_get_id(&p->lex, s->tok, buff);
    }
    printf("%s", buff);

    ctx = ctx->prev;
    if (ctx)
      printf(", ");
  }

  printf(" }");

  scope_print_rec(s->parent, p);
}

void scope_print(Scope* s, Parser* p) {
  scope_print_rec(s, p);
  printf("\n");
}

Scope* scope_create(Scope* parent) {
  Scope* s = (Scope*)malloc(sizeof(Scope));
  scope_init(s, parent);
  return s;
}

AST_Node* scope_find(Scope* s, Parser* p, AST_Node* sym) {
  AST_Manager* m = &p->ast_man;
  if (s == NULL)
    return ast_node_null(m);

  AST_Node* n = context_find(s->ctx, p, sym);

  if (!ast_is_null_node(n))
    return n;

  return scope_find(s->parent, p, sym);
}

AST_Node* scope_find_local(Scope* s, Parser* p, AST_Node* sym) {
  AST_Manager* m = &p->ast_man;
  if (s == NULL) {
    return ast_node_null(m);
  }

  return context_find(s->ctx, p, sym);
}

void scope_push(Scope* s, AST_Node* n) {
  assert(n->kind == AST_BIND_TYPE || n->kind == AST_BIND_CONSTANT || n->kind == AST_BIND_VARIABLE);
  s->ctx = context_push(s->ctx, n);
}

void scope_pop(Scope* s) { s->ctx = context_pop(s->ctx); }

b8 scope_is_global(Scope* s) { return s->parent == NULL; }
