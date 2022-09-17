#pragma once

#include "ast/ast_program_point.hpp"
#include "ast_kind.hpp"
#include "ast_manager.hpp"

#include "compiler/compiler.hpp"
#include "compiler/symbol_table.hpp"
#include "parser/parser.hpp"

namespace ast {

struct Literal_Symbol_Node : Node {
  compiler::symbol::Symbol get_symbol(parser::Parser* parser);
};

struct Literal_Natural_Node : Node {
  compiler::symbol::Symbol get_symbol(parser::Parser* parser);
};

struct Literal_Nothing_Node : Node {};

struct Literal_Undefined_Node : Node {};

struct Literal_False_Node : Node {};

struct Literal_True_Node : Node {};

struct Literal_Struct_Node : Node {
  ProgramPoint_List_Node* get_members(parser::Parser* parser);
};

template <> Literal_Undefined_Node* is_instance<>(Node* node);
template <> Literal_Nothing_Node*   is_instance<>(Node* node);
template <> Literal_Symbol_Node*    is_instance<>(Node* node);
template <> Literal_Natural_Node*   is_instance<>(Node* node);
template <> Literal_True_Node*      is_instance<>(Node* node);
template <> Literal_False_Node*     is_instance<>(Node* node);
template <> Literal_Struct_Node*    is_instance<>(Node* node);

Literal_True_Node* create_node_literal_true(parser::Parser* parser);

Literal_False_Node* create_node_literal_false(parser::Parser* parser);

Literal_Undefined_Node* create_node_literal_undefined(parser::Parser* parser);

Literal_Nothing_Node* create_node_literal_nothing(parser::Parser* parser);

Literal_Natural_Node* create_node_literal_natural(parser::Parser* parser, compiler::symbol::Symbol val);

Literal_Symbol_Node* create_node_literal_symbol(parser::Parser* parser, compiler::symbol::Symbol val);

Literal_Struct_Node* create_node_literal_struct(parser::Parser* parser, ProgramPoint_List_Node* members);

} // namespace ast
