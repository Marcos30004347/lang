#pragma once

#include "parser.hpp"

struct Scope {
	typedef struct Context Context;

	Context *ctx;
  Scope *parent;
};

Scope *scope_create(Scope *parent);

void scope_push(Scope *s, AST_Node *n);
void scope_pop(Scope *s);

void scope_print(Scope *s, Parser *p);

b8 scope_is_global(Scope *s);

AST_Node *scope_find(Scope *s, Parser *p, AST_Node *sym);
AST_Node *scope_find_local(Scope *s, Parser *p, AST_Node *sym);
