#pragma once

#include "ast/ast_program_point.hpp"
#include "ast_kind.hpp"
#include "ast_manager.hpp"

#include "compiler/compiler.hpp"
#include "compiler/symbol_table.hpp"
#include "parser/parser.hpp"

namespace ast {

struct Literal_Symbol_Node : Node {
  compiler::symbol::Symbol get_symbol(ast::Manager* manager);
  compiler::symbol::Id     get_symbol_id();
  void                     set_symbol_id(compiler::symbol::Id id);
};

struct Literal_Natural_Node : Node {
  compiler::symbol::Symbol get_symbol(ast::Manager* manager);
};

struct Literal_Nothing_Node : Node {};

struct Literal_Undefined_Node : Node {};

struct Literal_False_Node : Node {};

struct Literal_True_Node : Node {};

struct Literal_Struct_Node : Node {
  ProgramPoint_List_Node* get_members(ast::Manager* manager);
};

struct Literal_Handler_Node : Node {
  ProgramPoint_List_Node* get_body(ast::Manager* managet);
};

template <> Literal_Undefined_Node* is_instance<>(Node* node);
template <> Literal_Nothing_Node*   is_instance<>(Node* node);
template <> Literal_Symbol_Node*    is_instance<>(Node* node);
template <> Literal_Natural_Node*   is_instance<>(Node* node);
template <> Literal_True_Node*      is_instance<>(Node* node);
template <> Literal_False_Node*     is_instance<>(Node* node);
template <> Literal_Struct_Node*    is_instance<>(Node* node);
template <> Literal_Handler_Node*   is_instance<>(Node* node);

b8 is_semantic_node(Node*);

Literal_True_Node* create_node_literal_true(ast::Manager* manager);

Literal_False_Node* create_node_literal_false(ast::Manager* manager);

Literal_Undefined_Node* create_node_literal_undefined(ast::Manager* manager);

Literal_Nothing_Node* create_node_literal_nothing(ast::Manager* manager);

Literal_Natural_Node* create_node_literal_natural(ast::Manager* manager, compiler::symbol::Symbol val);

Literal_Symbol_Node* create_node_literal_symbol(ast::Manager* manager, compiler::symbol::Symbol val);

Literal_Struct_Node* create_node_literal_struct(ast::Manager* manager, ProgramPoint_List_Node* members);

Literal_Handler_Node* create_node_literal_handler(ast::Manager* manager, ProgramPoint_List_Node* statements);

void replace_ocurrences(ast::Manager* manager, ast::Node* node, ast::Literal_Symbol_Node* from, ast::Literal_Symbol_Node* to);
void replace_ocurrences(ast::Manager* manager, ast::Node* node, compiler::symbol::Symbol from, compiler::symbol::Symbol to);

} // namespace ast
