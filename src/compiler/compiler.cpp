#include "compiler.hpp"
#include "parser/parser.hpp"

namespace compiler {

Compiler* compiler_create() {
  Compiler* compiler = new Compiler();

  compiler->parser = 0; // parser::parser_create(-1, buffer, n);

  return compiler;
}

void compiler_destroy(Compiler* compiler) {
  if (compiler->parser) {
    parser::parser_destroy(compiler->parser);
  }

  delete compiler;
}

ast::Node* Compiler::parse(const i8* buffer, u64 n) {
  this->parser = parser::parser_create(0, buffer, n);
  return parser::parser_parse(this->parser);
}

}; // namespace compiler
