#pragma once

#include "ast/ast.hpp"
#include "ast/ast_literals.hpp"
#include "compiler/compiler.hpp"

namespace context {

struct Declaration {};

struct Values {};

struct Context {};

Declaration* get_declaration(compiler::Compiler* compiler, Context* ctx, ast::Literal_Symbol_Node* symbol);

Declaration*
declare(compiler::Compiler* compiler, Context* ctx, ast::Literal_Symbol_Node* symbol, ast::Node* value);

} // namespace context
