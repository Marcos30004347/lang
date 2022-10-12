#pragma once

#include "ast/ast_function.hpp"
#include "ast/ast_program_point.hpp"
#include "continuations.hpp"

#include "ast/ast.hpp"
#include "ast/ast_manager.hpp"
#include "context/context.hpp"
#include "stackframe/stackframe.hpp"

namespace closures {
typedef struct CPS_Closure_Data CPS_Closure_Data;

void convert_cps_closures(ast::Manager* manager, ast::Node* root, CPS_Closure_Data* cps);

CPS_Closure_Data*               cps_closure_data_create(stackframe::Stack_Frame_Data* sf_data);
void                            cps_closure_data_destroy(CPS_Closure_Data* data);
ast::Function_Literal_Node*     cps_closure_data_get_closure_handler(CPS_Closure_Data* data, ast::Manager* m, compiler::symbol::Id id);
ast::Function_Literal_Node*     cps_closure_data_get_closure_handler(CPS_Closure_Data* data, ast::Manager* m, ast::Literal_Symbol_Node* id);
ast::Literal_Struct_Node*       cps_closure_data_get_environment_struct_from_symbol(CPS_Closure_Data* data, ast::Manager* m, compiler::symbol::Id id);
ast::Literal_Struct_Node*       cps_closure_data_get_environment_struct_from_symbol(CPS_Closure_Data* data, ast::Manager* m, ast::Literal_Symbol_Node* id);
compiler::symbol::Id            cps_closure_data_get_environment_symbol_id(CPS_Closure_Data* data, ast::Manager* m, ast::Function_Literal_Node* id);
ast::Literal_Struct_Node*       cps_closure_data_get_environment_struct_from_function(CPS_Closure_Data* data, ast::Manager* m, ast::Function_Literal_Node* id);
ast::Literal_Struct_Node*       cps_closure_data_get_environment_struct_from_function(CPS_Closure_Data* data, ast::Manager* m, compiler::symbol::Id id);
stackframe::Stack_Frame_Data*   cps_closure_data_get_stackframe_data(CPS_Closure_Data* data);
ast::Declaration_Variable_Node* cps_closure_data_get_stackframe_argument(CPS_Closure_Data* data, ast::Function_Literal_Node* lit);
} // namespace closures
