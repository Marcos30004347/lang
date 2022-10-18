#pragma once

#include "ast/ast.hpp"
#include "context/context.hpp"

namespace handler {

typedef struct Handler_Pass_Data Handler_Pass_Data;

void handeler_conversion_pass(Handler_Pass_Data* data, ast::Manager* m, ast::Node* root);

Handler_Pass_Data* handler_pass_data_create();

void handler_pass_data_destroy(Handler_Pass_Data* d);
b8   handler_pass_data_is_effect_function(Handler_Pass_Data* data, ast::Function_Literal_Node* lit);
b8   handler_pass_data_is_prompt_function(Handler_Pass_Data* data, ast::Function_Literal_Node* lit);

} // namespace handler