#pragma once

#include "types.hpp"

namespace ast {

enum AST_Kind {
  // Compounds
  AST_NULL_NODE = 0,

  AST_UNDEFINED_NODE,
  AST_UNITIALIZED_NODE,

  __AST_KIND_BEGIN,

  AST_PROGRAM_POINT,

  AST_CALL_ARGS_LIST,
  AST_DECL_ARGS_LIST,

  AST_WITH_STATEMENT,

  // Control Flow
  __AST_CTRL_FLOW_START,
  AST_CTRL_FLOW_IF,
  AST_CTRL_FLOW_IF_ELSE,
  AST_CTRL_FLOW_RETURN,
  AST_CTRL_FLOW_RESUME,
  AST_CTRL_FLOW_MATCH,
  AST_CTRL_FLOW_CASE,
  AST_PROMPT_HANDLER,
  AST_BUBBLE_HANDLER,
  __AST_CTRL_FLOW_END,

  // Bindings
  AST_DECLARATION_CONSTANT,
  AST_DECLARATION_VARIABLE,
  // AST_BIND_TYPE,

  // Declarations
  __AST_LITERAL_START,
  AST_SYMBOL_LITERAL,
  AST_NATURAL_LITERAL,
  AST_STRUCT_LITERAL,
  AST_FUNCTION_LITERAL,
  AST_EFFECT_LITERAL,
  AST_HANDLER_LITERAL,
  AST_TRUE_LITERAL,
  AST_FALSE_LITERAL,
  __AST_LITERAL_END,

  AST_FUN_SIGNATURE,

  // Types
  AST_CAST_TYPE,
  __AST_TYPE_KIND_START,
  AST_TYPE_I32,
  AST_TYPE_UNIT,
  AST_TYPE_ANY,
  AST_TYPE_ARROW,
  AST_TYPE_EFFECT,
  AST_TYPE_TUPLE,
  AST_TYPE_TYPE,
  AST_TYPE_UNION,
  AST_TYPE_YIELD,
  AST_TYPE_STRUCT,
  AST_TYPE_VARIABLE,
  AST_TYPE_POINTER,
  AST_TYPE_HANDLER,
  AST_TYPE_EVIDENCE_CONTEXT,
  __AST_TYPE_KIND_END,

  // Binary operators
  __AST_BINARY_OPERATOR_START,
  AST_OP_BIN_ASSIGN,
  AST_OP_BIN_ADD,
  AST_OP_BIN_SUB,
  AST_OP_BIN_MUL,
  AST_OP_BIN_DIV,
  AST_OP_BIN_GT,
  AST_OP_BIN_LT,
  AST_OP_BIN_GE,
  AST_OP_BIN_LE,
  AST_OP_BIN_NE,
  AST_OP_BIN_EQ,
  AST_OP_MEMBER_ACCESS,
  __AST_BINARY_OPERATOR_END,

  // Unary operators
  __AST_UNARY_OPERATOR_START,
  AST_OP_UNA_SUB,
  AST_OP_UNA_ADD,
  AST_OP_POINTER_VALUE,
  AST_OP_ADDRESS_OF,
  __AST_UNARY_OPERATOR_END,

  // Applications
  __AST_CALL_OPERATION_START,
  AST_FUNCTION_CALL,
  AST_EFFECT_CALL,
  __AST_CALL_OPERATION_END,

  __AST_KIND_END,
};

i8* kind_to_cstr(u64 k, u64 x = 0);

} // namespace ast
