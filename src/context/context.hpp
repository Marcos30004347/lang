#pragma once

#include "ast/ast.hpp"
#include "ast/ast_literals.hpp"
#include "ast/ast_manager.hpp"
#include "compiler/compiler.hpp"

#include "lib/set.hpp"
#include "lib/table.hpp"

namespace context {

typedef u64 Address;

typedef struct Context Context;

struct Declaration {
  b8 is_constant;

  ast::Node* type;

  ast::Literal_Symbol_Node* symbol;

  // map from a key to a set of keys to the heap field
  lib::Set< Address >* values;
};

struct Value {
  ast::Node* value;   // not null when value is not a struct instatiation
  Context*   members; // not null when value is a struct instatiation
};

struct Abstract_Memory {
  u64 references;
  u64 counter;

  // map from a key to a value
  lib::Table< Address, Value >* heap;
};

struct Context {
  // parent scope
  Context* parent;

  u64 level;

  // abstract heap that holds all possible values for declarations on this context
  Abstract_Memory* memory;

  // map from symbol ids to declarations
  lib::Table< compiler::symbol::Id, Declaration >* declarations;
};

Context* context_create(Context* parent, Abstract_Memory* storage);
void     context_destroy(Context* context);

Context* fork_context(Context* context);
void     merge_contexts(compiler::Compiler* compiler, Context* a, Context* b);

void         declare(compiler::Compiler* compiler, Context* ctx, ast::Node* declaration);
Declaration* get_declaration(compiler::Compiler* compiler, Context* ctx, ast::Literal_Symbol_Node* symbol);

void set_value(compiler::Compiler* compiler, Context* ctx, ast::Node* x, ast::Node* y);
void set_reference(compiler::Compiler* compiler, Context* ctx, ast::Node* x, ast::Node* y);

} // namespace context
