#pragma once

#include "ast/ast.hpp"
#include "ast/ast_declaration.hpp"
#include "ast/ast_literals.hpp"
#include "ast/ast_operations.hpp"
#include "compiler/symbol_table.hpp"
#include "parser/parser.hpp"

#include <unordered_map>
#include <unordered_set>

typedef struct Context Context;

// typedef std::unordered_set< ast::Node* > Assignments;

Context* context_create(Context* parent);
Context* context_destroy(Context* ctx);

void context_declare(Context* ctx, parser::Parser* p, ast::Declaration_Variable_Node* declaration);

void context_declare(Context* ctx, parser::Parser* p, ast::Declaration_Constant_Node* declaration);

ast::Node* context_type_of(Context* ctx, ast::Literal_Symbol_Node* symbol);

b8 context_is_local(Context* ctx, ast::Literal_Symbol_Node* symbol);

void context_print(Context* ctx, parser::Parser* p, int tabs = 0);

Context* declaration_arguments_to_context(parser::Parser* p, ast::Node* node, Context* parent = NULL);
