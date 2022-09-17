#pragma once

#include "parser/parser.hpp"

#include "file_manager.hpp"

namespace compiler {

struct Compiler {
  parser::Parser* parser;

  ast::Node* parse(const i8* buffer, u64 n);
};

Compiler* compiler_create();
void      compiler_destroy(Compiler* compiler);
} // namespace compiler
