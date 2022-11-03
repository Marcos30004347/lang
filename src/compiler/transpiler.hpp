#pragma once

#include "ast/ast_manager.hpp"

namespace compiler {
namespace transpiler {

void output_c_code(ast::Manager* m, ast::Node* node);

}
} // namespace compiler
