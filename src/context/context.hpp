#pragma once

#include "ast/ast.hpp"
#include "ast/ast_manager.hpp"
#include "compiler/symbol_table.hpp"

namespace context {

typedef struct Context Context;

Context* context_create(Context* parent);

Context* context_destroy(Context* ctx);

void context_push_scope(Context* ctx);
void context_pop_scope(Context* ctx);

void context_declare(Context* ctx, ast::Manager* p, ast::Declaration_Variable_Node* declaration);

void context_declare(Context* ctx, ast::Manager* p, ast::Declaration_Constant_Node* declaration);

void context_define_struct(Context* ctx, ast::Manager* p, ast::Literal_Symbol_Node* id, ast::Literal_Struct_Node* structure);

ast::Literal_Struct_Node* context_get_struct_definition(Context* ctx, ast::Manager* m, ast::Literal_Symbol_Node* id);

ast::Node* context_type_of(Context* ctx, ast::Manager* m, ast::Literal_Symbol_Node* symbol);
ast::Node* context_type_of(Context* ctx, ast::Manager* m, compiler::symbol::Id symbol);
ast::Node* context_type_of(Context* ctx, ast::Manager* m, ast::Member_Access_Node* symbol);

ast::Node* context_is_local(Context* ctx, ast::Literal_Symbol_Node* symbol);
ast::Node* context_is_defined(Context* ctx, ast::Literal_Symbol_Node* symbol);
ast::Node* context_is_defined(Context* ctx, compiler::symbol::Id symbol);

void context_print(Context* ctx, ast::Manager* m, int tabs = 0);

Context* context_from_declarations_list(ast::Manager* p, ast::Declarations_List_Node* node, Context* parent);

} // namespace context
