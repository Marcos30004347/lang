#include "compiler.hpp"
#include "parser.hpp"

namespace compiler {

Compiler* compiler_create() {
  Compiler* compiler = new Compiler();
  compiler->parser   = parser::parser_create();
}

}; // namespace compiler
