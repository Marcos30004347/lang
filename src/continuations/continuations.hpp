#pragma once

#include "ast/ast.hpp"

#include "compiler/compiler.hpp"

#include "lib/set.hpp"
#include "lib/table.hpp"
#include "parser/parser.hpp"

namespace cps {

struct Conversion_Result {
  lib::Table< compiler::symbol::Id, ast::Id >* continuation_literals;
  lib::Table< compiler::symbol::Id, ast::Id >* continuation_arguments;
};

Conversion_Result* cps_result_create();

void cps_result_destroy(Conversion_Result* info);

void convert_to_cps_style(Conversion_Result* info, compiler::Compiler* compiler, ast::Node* root);

} // namespace cps
