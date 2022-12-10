#pragma once

#include "ast/ast.hpp"
#include "ast/ast_function.hpp"
#include "context/context.hpp"

namespace handler {

typedef struct Handler_Pass_Data Handler_Pass_Data;

void handler_conversion_pass(Handler_Pass_Data* data, ast::Manager* m, ast::Node* root);

Handler_Pass_Data* handler_pass_data_create();

b8 is_prompt_site(Handler_Pass_Data* data, ast::Function_Call_Node* call);
b8 is_prompt_call(Handler_Pass_Data* data, ast::Function_Call_Node* call);
	
void handler_pass_data_destroy(Handler_Pass_Data* d);
// b8   handler_pass_data_is_effect_function(Handler_Pass_Data* data, ast::Function_Literal_Node* lit);
// b8   handler_pass_data_is_prompt_function(Handler_Pass_Data* data, ast::Function_Literal_Node* lit);
// b8   handler_pass_data_is_prompt_declaration(Handler_Pass_Data* data, ast::Node* decl);
b8 is_prompt_function(Handler_Pass_Data* data, ast::Function_Literal_Node* lit);
b8 handler_pass_data_is_context_argument(Handler_Pass_Data* data, ast::Declaration_Variable_Node* decl);
b8 handler_pass_data_is_context_argument(Handler_Pass_Data* data, ast::Declaration_Constant_Node* decl);
void add_context_argument(Handler_Pass_Data* data, ast::Manager* m, ast::Function_Literal_Node* f, ast::Node* decl);
void pass_context_argument(Handler_Pass_Data* data, ast::Manager* m, ast::Function_Call_Node* f);
} // namespace handler
