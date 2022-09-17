#pragma once

#include "parser/parser.hpp"

#include "file_manager.hpp"

namespace compiler {

struct Compiler {
  parser::Parser* parser;
};

Compiler* compiler_create(i8* buffer, u64 n);

} // namespace compiler
