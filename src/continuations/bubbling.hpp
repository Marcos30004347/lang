#pragma once

#include "ast/ast.hpp"
#include "closures.hpp"
#include "continuations.hpp"

namespace bubbling {

typedef struct Bubbling_Data Bubbling_Data;

void add_bubbling_yields(Bubbling_Data* data, ast::Manager* m, ast::Node* root);

Bubbling_Data* bubbling_data_create(closures::CPS_Closure_Data* cps_closures_data);

void bubbling_data_delete(Bubbling_Data* data);

}; // namespace bubbling
