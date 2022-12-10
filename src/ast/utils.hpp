#pragma once

#include "ast/ast_declaration.hpp"
#include "ast/ast_function.hpp"
#include "ast/ast_literals.hpp"
#include "ast/ast_manager.hpp"
#include "ast/ast_operations.hpp"
namespace ast {
namespace utils {

ast::Function_Call_Node* call(ast::Manager* m, ast::Node* name);
ast::Function_Call_Node* call(ast::Manager* m, ast::Node* name, ast::Node* argA);
ast::Function_Call_Node* call(ast::Manager* m, ast::Node* name, ast::Node* argA, ast::Node* argB);
ast::Function_Call_Node* call(ast::Manager* m, const i8* name, ast::Node* arg);
ast::Function_Call_Node* call(ast::Manager* m, const i8* name, ast::Node* arg0, ast::Node* arg1);
ast::Function_Call_Node* call(ast::Manager* m, const i8* name, ast::Node* arg0, ast::Node* arg1, ast::Node* arg2);
ast::Function_Call_Node* call(ast::Manager* m, const i8* name, ast::Node* arg0, ast::Node* arg1, ast::Node* arg2, ast::Node* arg3);
ast::Function_Call_Node* call(ast::Manager* m, const i8* name, ast::Node* arg0, ast::Node* arg1, ast::Node* arg2, ast::Node* arg3, ast::Node* arg4);

ast::Declaration_Variable_Node* variable(ast::Manager* m, const i8* name, ast::Node* type);
ast::Declaration_Constant_Node* constant(ast::Manager* m, const i8* name, ast::Node* type);

ast::Variable_Assignment_Node* assignment(ast::Manager* m, ast::Node* left, ast::Node* right);
ast::Literal_Symbol_Node* symbol(ast::Manager* m, const i8* name);
ast::Literal_Symbol_Node* natural(ast::Manager* m, u64 nat);
ast::Literal_Symbol_Node* prefix(ast::Manager* m, const i8* prefix, u64 nat);
ast::Literal_Symbol_Node* prefix(ast::Manager* m, const i8* prefix, ast::Literal_Symbol_Node* name);
ast::Literal_Symbol_Node* prefix(ast::Manager* m, ast::Literal_Symbol_Node* prefix, ast::Literal_Symbol_Node* name);

ast::Literal_Symbol_Node* get_symbol(ast::Manager* m, ast::Node* node);
ast::Node* get_nth_argument(ast::Manager* m, ast::Function_Call_Node* call, u64 n);
ast::Node* default_initialization(ast::Manager* m, ast::Node* type);
void add_argument_to_function_declaration(ast::Manager* m, ast::Function_Literal_Node* literal, ast::Variable_Assignment_Node* assignment, ast::Node* type);
} // namespace utils
} // namespace ast
