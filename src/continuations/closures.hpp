#pragma once

#include "continuations.hpp"

#include "ast/ast.hpp"
#include "ast/ast_manager.hpp"
#include "context/context.hpp"

namespace closures {
typedef struct CPS_Closure_Data CPS_Closure_Data;

void convert_cps_closures(ast::Manager* manager, ast::Node* root, CPS_Closure_Data* cps);
void build_closures_environments(CPS_Closure_Data* data, ast::Manager* m, ast::Node* root, context::Context* ctx, context::Context* globals);

CPS_Closure_Data* cps_closure_data_create(cps::CPS_Data* cps_data);
void              cps_closure_data_destroy(CPS_Closure_Data* data);
} // namespace closures
