#pragma once

#include "ast/ast.hpp"

#include "compiler/compiler.hpp"

#include "lib/set.hpp"
#include "lib/table.hpp"
#include "parser/parser.hpp"

namespace cps {

struct CPS_Data {
  lib::Table< compiler::symbol::Id, ast::Id >* continuation_literals;
  lib::Table< compiler::symbol::Id, ast::Id >* continuation_arguments;
};

CPS_Data* cps_result_create();

void cps_result_destroy(CPS_Data* info);

void convert_to_cps_style(CPS_Data* info, compiler::Compiler* compiler, ast::Node* root);

} // namespace cps
