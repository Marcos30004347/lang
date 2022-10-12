#pragma once

#include "ast/ast.hpp"

#include "ast/ast_declaration.hpp"
#include "ast/ast_function.hpp"
#include "ast/ast_literals.hpp"
#include "ast/ast_manager.hpp"
#include "compiler/compiler.hpp"

#include "lib/set.hpp"
#include "lib/table.hpp"
#include "parser/parser.hpp"

namespace cps {

typedef struct CPS_Data CPS_Data;

CPS_Data* cps_data_create();

void cps_data_destroy(CPS_Data* info);

void convert_to_cps_style(CPS_Data* info, parser::Parser* parser, ast::Node* root);

b8 is_continuation_closure(CPS_Data* info, ast::Manager* m, ast::Literal_Symbol_Node* symbol);
b8 is_continuation_closure(CPS_Data* info, ast::Manager* m, ast::Function_Literal_Node* function);
b8 is_temporary_variable(CPS_Data* info, ast::Manager* m, ast::Declaration_Variable_Node* decl);
b8 is_temporary_variable(CPS_Data* info, ast::Manager* m, ast::Declaration_Constant_Node* decl);
} // namespace cps
