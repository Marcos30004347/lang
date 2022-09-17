#pragma once

#include "ast/ast.hpp"
#include "parser/parser.hpp"

#include <unordered_map>
#include <unordered_set>

typedef struct Context Context;

typedef std::unordered_set< ast::Node* > Assignments;

struct Declaration {
  ast::Node* symbol;
  ast::Node* type;
  ast::Node* bind;

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

Declaration*
context_declare(Context* ctx, parser::Parser* p, ast::Node* declaration, ast::Node* program_point);

void context_assign(Context* ctx, parser::Parser* p, ast::Node* assignment, ast::Node* program_point);

Declaration* context_declaration_of(Context* ctx, parser::Parser* p, ast::Node* symbol, b8* is_local = NULL);

ast::Node* context_type_of(Context* ctx, parser::Parser* p, ast::Node* symbol);

Assignments* context_values_of(Context* ctx, parser::Parser* p, ast::Node* symbol);

void context_merge(parser::Parser* p, Context* a, Context* b);

void context_replace(Context* a, Context* b);

Context* context_copy(Context* a);

void context_print(Context* ctx, parser::Parser* p, int tabs = 0);

Context* declaration_arguments_to_context(parser::Parser* p, ast::Node* node, Context* parent = NULL);
