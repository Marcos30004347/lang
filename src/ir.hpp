#pragma once

#include "registry.hpp"

#include <vector>

enum Op_Code {
	OP_ADD,
	OP_SUB,
	OP_MUL,
	OP_DIV,
	OP_CALL,
	OP_PERFORM,
};

enum Literal_Kind {
	LIT_VAR,
	LIT_NAT,
};

enum Instruction_Kind {
	THREE_ADDRESS_CODE,
	BRANCH,
	RETURN,
};

enum Type_Kind {
	I32,
	// TODO
};

struct Type {
	Type_Kind kind;
	u64 id;
};

struct Literal {
	Literal_Kind kind;
	Type type;
	u64 id;
};

struct Instruction {
	Instruction_Kind kind;

	Literal op[3];

	Op_Code code;
};

struct Basic_Block {
	u64 id;
	u64 size;
	Instruction* instructions;
};

struct Block_Arg {
	Literal symbol;
	Type type;
};

struct Function_Block {
	Block_Arg* arg;
	Basic_Block* bb;
};

struct Effect_Block {
	Block_Arg* arg;
	Basic_Block* bb;
};

struct Handler_Block {
	Block_Arg* arg;
	Effect_Block* bb;
};

struct Program {
	registry* symbol_table;

	Handler_Block* hand;
	Function_Block* decl;
};
