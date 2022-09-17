#include "compiler.hpp"
#include "parser/parser.hpp"

namespace compiler {

Compiler* compiler_create(i8* buffer, u64 n) {
  Compiler* compiler = new Compiler();

  compiler->parser = parser::parser_create(-1, buffer, n);
  return compiler;
}

}; // namespace compiler
