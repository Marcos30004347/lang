#pragma once

#include "registry.hpp"

#include <vector>
#include <unordered_map>

typedef u64 Symbol_Id;
typedef u64 Type_Id;

enum Op_Code {
	OP_ADD,
	OP_SUB,
	OP_MUL,
	OP_DIV,
	OP_CALL,
	OP_PERFORM,
};

enum Literal_Kind {
	LIT_SYM,
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
	Symbol_Id id;
	Type_Kind kind;
	std::unordered_map<Symbol_Id, Type_Id> members;
};

struct Literal {
	Symbol_Id id;
	Type_Id type;
	Literal_Kind kind;
};

struct Instruction {
	Instruction_Kind kind;

	Literal op[3];

	Op_Code code;
};

struct Basic_Block {
	u64 id;
	std::vector<Instruction> instructions;
};

struct Block_Arg {
	Symbol_Id symbol;
	Type_Id type_id;
};

struct Function_Block {
	std::vector<Block_Arg> arg;
	std::vector<Basic_Block> bb;
};

struct Effect_Block {
	std::vector<Block_Arg> arg;
	std::vector<Basic_Block> bb;
};

struct Handler_Block {
	std::vector<Block_Arg> arg;
	std::vector<Effect_Block> bb;
};

struct Program {
	std::vector<Type> types;
	std::vector<Handler_Block> handlers;
	std::vector<Function_Block> declarations;
};
