#pragma once

#include "ast/ast.hpp"

#include "ast/ast_declaration.hpp"
#include "ast/ast_manager.hpp"
#include "compiler/compiler.hpp"

#include "lib/set.hpp"
#include "lib/table.hpp"
#include "parser/parser.hpp"

namespace cps {

typedef struct CPS_Data CPS_Data;

CPS_Data* cps_result_create();

void cps_result_destroy(CPS_Data* info);

void convert_to_cps_style(CPS_Data* info, compiler::Compiler* compiler, ast::Node* root);

b8 is_continuation_closure(CPS_Data* info, ast::Manager* m, ast::Declaration_Constant_Node* decl);

} // namespace cps
