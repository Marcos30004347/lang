#pragma once

#include "ast/ast.hpp"
#include "ast/ast_manager.hpp"
#include "continuations.hpp"

namespace closures {

void convert_cps_closures(ast::Manager* manager, ast::Node* root, cps::CPS_Data* cps);

}
