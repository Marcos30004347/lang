#pragma once

#include "ast/ast.hpp"
#include "ast/ast_declaration.hpp"
#include "ast/ast_function.hpp"
#include "continuations/continuations.hpp"

namespace stackframe {

typedef struct Stack_Frame_Data Stack_Frame_Data;

void allocate_stack_frame(Stack_Frame_Data* data, ast::Manager* m, ast::Node* root);

ast::ProgramPoint_List_Node* stack_frame_get_function_literal_setup_end(Stack_Frame_Data* data, ast::Manager*, ast::Function_Literal_Node* lit);

Stack_Frame_Data* create_stack_frame_data(cps::CPS_Data*);

void destroy_stack_frame_data(Stack_Frame_Data*);

u64            stack_frame_get_function_depth(Stack_Frame_Data* data, ast::Function_Literal_Node* lit);
cps::CPS_Data* stack_frame_data_get_cps_data(Stack_Frame_Data* data);

ast::Declaration_Variable_Node* stack_frame_get_function_stack_frame(Stack_Frame_Data* data, ast::Function_Literal_Node* lit);

ast::Variable_Assignment_Node* stack_frame_get_function_local_stack_frame_allocation(Stack_Frame_Data* data, ast::Function_Literal_Node* f);

ast::Literal_Symbol_Node* build_sp_symbol(ast::Manager* m, u64 depth);
} // namespace stackframe
