#pragma once

#include "types.hpp"

#include <vector>
#include <unordered_map>

typedef u32 Symbol_Id;
typedef u32 Type_Id;
typedef u32 Members_Id;
typedef u32 Operand_Id;

struct StructMembers {
	Symbol_Id symbol_id;
	Type_Id   type_id;

	Members_Id next_id;
};

struct Type {
  enum Kind {
    UNIT_TYPE = 0,
    INT32_TYPE,
    ARROW_TYPE,
    UNION_TYPE,
    YIELD_TYPE,
  };

  Kind kind;
  u64 buffer_64x1;
};

struct Operand {
  enum Kind {
    VAR,
    TEMP,
    CONST,
  };

  Kind kind;
  u64 buffer_64x2[2];
};

struct Instruction {
  enum Kind {
    THREE_ADDRESS_CODE,
    PROMPT_HANDLER,
    YIELD,
    BRANCH,
    RETURN,
    PUSH_ARG,
    POP_ARG,
  };

  enum Code {
    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_CALL,
    OP_PERFORM,
  };

  Kind kind;
  Code code;

	Operand_Id op[3];
};

struct Basic_Block {
	std::vector<Instruction> inst;
};

struct Function_Block {
	std::vector<Basic_Block> block;
};

struct Handler_Block {
	std::vector<Function_Block> block;
};

struct Program {
	std::vector<Type> types;
	std::vector<Handler_Block> handlers;
	std::vector<Function_Block> declarations;
};
