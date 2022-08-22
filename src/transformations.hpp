#pragma once


// #include "ast.hpp"
// #include "parser.hpp"
// #include <unordered_map>

// struct Closure {
//   AST_Id fun;
//   AST_Id env;
// };

// struct Context {
//   AST_Id decl;

//   Context *next;
//   Context *prev;
// };

// struct Scope {
//   Context *ctx;
//   Scope *parent;
// };

// struct ClosureConverter {
//   u32 level;

//   Parser *parser;

//   AST_Id prog_tail;

// 	Scope* closures;

//   u64 temporaries;
// };

// void closure_convert(Parser* p, AST_Node* n);
