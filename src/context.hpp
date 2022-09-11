#pragma once

#include "ast.hpp"
#include "parser.hpp"

// struct Context {
//   AST_Id decl;

//   Context* next;
//   Context* prev;
// };

// struct Scope {
//   typedef struct Context Context;

//   Context* ctx;
//   Scope* parent;
// };

// Scope* scope_create(Scope* parent);

// void scope_push(Scope* s, AST_Node* n);
// void scope_pop(Scope* s);

// void scope_print(Scope* s, Parser* p);

// b8 scope_is_global(Scope* s);

// AST_Node* scope_find(Scope* s, Parser* p, AST_Node* sym, b8* is_local = 0);
// AST_Node* scope_find_local(Scope* s, Parser* p, AST_Node* sym);
// u64 get_scope_depth(Scope* s);

#include <unordered_map>
#include <unordered_set>

typedef struct Context Context;

// struct Assignment {
//   // Value being assigned
//   AST_Node* value;
//   // The program point in which the assignment happen
//   AST_Node* point;
// };

typedef std::unordered_set< AST_Node* > Assignments;

struct Declaration {
  AST_Node* symbol;
  AST_Node* type;
  AST_Node* bind;

  Context* context;

  Assignments  assignments;
  Declaration* previous_declaration;
};

struct Context {
  Declaration* last_declaration;

  Context* parent;
};

Context* context_create(Context* parent);
Context* context_destroy(Context* ctx);

Declaration* context_declare(Context* ctx, Parser* p, AST_Node* declaration, AST_Node* program_point);
void         context_assign(Context* ctx, Parser* p, AST_Node* assignment, AST_Node* program_point);

Declaration* context_declaration_of(Context* ctx, Parser* p, AST_Node* symbol, b8* is_local = NULL);

AST_Node* context_type_of(Context* ctx, Parser* p, AST_Node* symbol);

Assignments* context_values_of(Context* ctx, Parser* p, AST_Node* symbol);

void     context_merge(Parser* p, Context* a, Context* b);
void     context_replace(Context* a, Context* b);
Context* context_copy(Context* a);
void     context_print(Context* ctx, Parser* p, int tabs = 0);
Context* declaration_arguments_to_context(Parser* p, AST_Node* node, Context* parent = NULL);
