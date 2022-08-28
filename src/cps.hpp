#pragma once

// Continuation Passing Style Analysis.
// brief: the analysis transform a program into CPS style.
// The analysis expects :
// 1. The program is using three adress instructions.
// 2. calls are only made on simple assignment.
// 3. All variable types are know and the program typechecks.

// TODO: we need to get the funtions that may escape throught function arguments
// right now since we dont have pointer types, this is not possible, so we're
// just considering that a function may escape on an assignment to an constant
// argument for testing reasons, fix that when the language gets more features.

#include "ast.hpp"
#include "context.hpp"
#include "error.hpp"
#include "lexer.hpp"
#include "parser.hpp"

#include "stdio.h"
#include <assert.h>
#include <cstdio>

#include <iterator>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <vector>

// struct CPS_Function_Analysis {
//   // functions and continuations that may escape the current scope.
//   Scope* escaping_local_funcs;
//   // Scope* escaping_cont_funcs;

//   // // functions and continuations that cannot scape the current scope.
//   // Scope* known_user_funcs;
//   Scope* continuation_funcs;
// };

// void scope_add_function_literal_arguments(Scope* scope, Parser* p, AST_Node* lit) {
//   AST_Node* sign = ast_function_literal_get_signature(&p->ast_man, lit);
//   AST_Node* args = ast_function_signature_get_args(&p->ast_man, sign);

//   while (!ast_is_null_node(args)) {
//     AST_Node* bind = ast_manager_get_relative(&p->ast_man, args, args->left);
//     if (!ast_is_null_node(bind)) { scope_push(scope, bind); }
//     args = ast_manager_get_relative(&p->ast_man, args, args->right);
//   }
// }

// void cps_analisys(CPS_Function_Analysis* a, Scope* scope, Parser* p, AST_Node* node);

// AST_Node* body_split(CPS_Function_Analysis* a, Scope* scope, Parser* p, AST_Node* body) {
//   assert(body->kind == AST_PROGRAM_POINT);

//   AST_Manager* m = &p->ast_man;

//   Token token = lexer_undef_token();

//   AST_Node* program_point = ast_program_point_get_decl(m, body);

//   assert(program_point->kind == AST_BIND_CONSTANT || program_point->kind == AST_BIND_VARIABLE || program_point->kind == AST_OP_BIN_ASSIGN || program_point->kind == AST_FUNCTION_CALL);

//   AST_Node* continuation = ast_manager_get_relative(m, body, body->right);

//   if (program_point->kind == AST_BIND_CONSTANT || program_point->kind == AST_BIND_VARIABLE) {
//     AST_Node* expr      = ast_bind_get_expr(m, program_point);
//     AST_Node* type_bind = ast_bind_get_type_bind(m, program_point);
//     AST_Node* symbol    = ast_type_bind_get_symbol(m, type_bind);

//     scope_push(scope, program_point);

//     if (expr->kind == AST_FUNCTION_CALL) {
//       AST_Node* func = ast_manager_get_relative(m, expr, expr->left);
//       AST_Node* sign = scope_find(scope, p, func);

//       if (ast_is_null_node(sign)) { parser_error(p, func->tok, "Could not find definition"); }

//       // get the type of the function, an arrow type
//       if (sign->kind == AST_BIND_CONSTANT || sign->kind == AST_BIND_VARIABLE) { sign = ast_bind_get_type_bind(m, sign); }

//       assert(sign->kind == AST_BIND_TYPE);

//       AST_Node* arrw = ast_type_bind_get_type(m, sign);
//       AST_Node* bind = ast_node_null(m);

//       if (!ast_is_null_node(arrw)) {
//         // get the return type of the function
//         AST_Node* type = ast_manager_get_relative(m, arrw, arrw->right);

//         if (ast_is_null_node(type)) { type = ast_type_any(m, token); }

//         AST_Node* carg = ast_type_bind(m, token, symbol, type);
//         bind           = ast_decl_args(m, token, carg, ast_node_null(m));
//       } else {
//         AST_Node* carg = ast_type_bind(m, token, symbol, ast_type_any(m, token));
//         bind           = ast_decl_args(m, token, carg, ast_node_null(m));
//       }

//       AST_Node* csign = ast_function_signature(m, token, bind, ast_type_any(m, token));
//       AST_Node* cdecl = ast_function_literal(m, token, csign, continuation);

//       ast_call_push_argument(m, token, program_point, cdecl);

//       body->right = ast_node_null(m)->id;
//       body->left  = expr->id;

//       return continuation;
//     }

//     return continuation;
//   }

//   if (program_point->kind == AST_OP_BIN_ASSIGN) {
//     AST_Node* symb = ast_manager_get_relative(m, program_point, program_point->left);
//     AST_Node* expr = ast_manager_get_relative(m, program_point, program_point->right);

//     if (expr->kind == AST_FUNCTION_CALL) {

//       AST_Node* func = ast_manager_get_relative(m, expr, expr->left);
//       AST_Node* sign = scope_find(scope, p, func);

//       if (ast_is_null_node(sign)) { parser_error(p, func->tok, "Could not find definition"); }
//       // get the type of the function, an arrow type
//       if (sign->kind == AST_BIND_CONSTANT || sign->kind == AST_BIND_VARIABLE) { sign = ast_bind_get_type_bind(m, sign); }

//       assert(sign->kind == AST_BIND_TYPE);

//       AST_Node* arrw = ast_type_bind_get_type(m, sign);

//       AST_Node* bind = ast_node_null(m);

//       if (!ast_is_null_node(arrw)) {
//         // get the return type of the function
//         AST_Node* type = ast_manager_get_relative(m, arrw, arrw->right);

//         if (ast_is_null_node(type)) type = ast_type_any(m, token);

//         AST_Node* carg = ast_type_bind(m, token, ast_temp_node(m), type);
//         bind           = ast_decl_args(m, token, carg, ast_node_null(m));
//       }

//       AST_Node* csign = ast_function_signature(m, token, bind, ast_type_any(m, token));

//       AST_Node* cdecl = ast_function_literal(m, token, csign, continuation);

//       ast_call_push_argument(m, token, program_point, cdecl);

//       body->right = ast_node_null(m)->id;
//       body->left  = expr->id;

//       return continuation;
//     }

//     return continuation;
//   }

//   if (program_point->kind == AST_FUNCTION_CALL) {
//     // TODO: use type of the symbol, need to find the definition on the context

//     // need to get the return type of the function
//     AST_Node* symb = ast_manager_get_relative(m, program_point, program_point->left);

//     AST_Node* sign = scope_find(scope, p, symb);

//     if (ast_is_null_node(sign)) { parser_error(p, symb->tok, "Could not find definition"); }

//     // get the type of the function, an arrow type
//     if (sign->kind == AST_BIND_CONSTANT || sign->kind == AST_BIND_VARIABLE) { sign = ast_bind_get_type_bind(m, sign); }

//     assert(sign->kind == AST_BIND_TYPE);

//     AST_Node* arrw = ast_type_bind_get_type(m, sign);

//     AST_Node* bind = ast_node_null(m);

//     if (!ast_is_null_node(arrw)) {
//       // get the return type of the function
//       AST_Node* type = ast_manager_get_relative(m, arrw, arrw->right);

//       if (ast_is_null_node(type)) type = ast_type_any(m, token);

//       AST_Node* carg = ast_type_bind(m, token, ast_temp_node(m), type);
//       bind           = ast_decl_args(m, token, carg, ast_node_null(m));
//     }

//     AST_Node* csign = ast_function_signature(m, token, bind, ast_type_any(m, token));

//     AST_Node* cdecl = ast_function_literal(m, token, csign, continuation);

//     ast_call_push_argument(m, token, program_point, cdecl);

//     body->right = ast_node_null(m)->id;

//     return continuation;
//   }

//   return continuation;
// }

// void body_to_cps(CPS_Function_Analysis* a, Scope* scope, Parser* p, AST_Node* body, AST_Node* cont_symb) {
//   AST_Manager* m = &p->ast_man;

//   if (ast_is_null_node(body)) { return; }

//   assert(body->kind == AST_PROGRAM_POINT);

//   Token token = lexer_undef_token();

//   AST_Node* parent = ast_node_null(m);

//   while (!ast_is_null_node(body)) {
//     AST_Node* program_point = ast_program_point_get_decl(m, body);

//     if (program_point->kind == AST_CTRL_FLOW_RETURN) {
//       AST_Node* expr = ast_ctrl_flow_return_get_expression(m, program_point);
//       AST_Node* call = ast_call(m, token, cont_symb, expr);

//       body->left  = call->id;
//       body->right = ast_node_null(m)->id;
//       // NOTE(marcos): a return statement is a spliting point,
//       // no need to process the remaining statements.
//       return;
//     }

//     if (program_point->kind == AST_BIND_CONSTANT || program_point->kind == AST_BIND_VARIABLE || program_point->kind == AST_OP_BIN_ASSIGN) {

//       if (program_point->kind == AST_BIND_CONSTANT || program_point->kind == AST_BIND_VARIABLE) { scope_push(scope, program_point); }

//       AST_Node* right = ast_manager_get_relative(m, program_point, program_point->right);

//       if (right->kind == AST_FUNCTION_LITERAL) {
//         cps_analisys(a, scope, p, right);
//       } else if (right->kind == AST_FUNCTION_CALL) {

//         // NOTE(marcos): a function call statement is a spliting point,
//         // no need to process the remaining statements.
//         return body_to_cps(a, scope, p, body_split(a, scope, p, body), cont_symb);
//       }
//     }

//     else if (program_point->kind == AST_FUNCTION_CALL) {
//       AST_Node* sp = body_split(a, scope, p, body);

//       // NOTE(marcos): a function call statement is a spliting point,
//       // no need to process the remaining statements.
//       return body_to_cps(a, scope, p, sp, cont_symb);
//     }

//     else if (program_point->kind == AST_CTRL_FLOW_IF) {
//       body_to_cps(a, scope, p, ast_manager_get_relative(m, program_point, program_point->right), cont_symb);
//     }

//     else if (program_point->kind == AST_CTRL_FLOW_IF_ELSE) {
//       AST_Node* if_statement = ast_manager_get_relative(m, program_point, program_point->left);
//       body_to_cps(a, scope, p, ast_manager_get_relative(m, program_point, if_statement->right), cont_symb);
//       body_to_cps(a, scope, p, ast_manager_get_relative(m, program_point, program_point->right), cont_symb);
//     }

//     parent = body;
//     body   = ast_program_point_get_tail(m, body);
//   }

//   return;
// }

// void cps_analisys(CPS_Function_Analysis* a, Scope* scope, Parser* p, AST_Node* node) {
//   if (ast_is_null_node(node)) return;

//   AST_Manager* m = &p->ast_man;

//   Token token = lexer_undef_token();

//   if (node->kind == AST_FUNCTION_LITERAL) {
//     AST_Node* signature = ast_function_literal_get_signature(m, node);

//     AST_Node* cont_symb = ast_temp_node(m);
//     AST_Node* cont_type = ast_type_any(m, token);
//     AST_Node* cont_bind = ast_type_bind(m, token, cont_symb, cont_type);

//     ast_function_literal_push_argument(m, token, node, cont_bind);

//     scope_push(a->continuation_funcs, cont_bind);

//     scope = scope_create(scope);

//     scope_add_function_literal_arguments(scope, p, node);

//     AST_Node* body = ast_function_literal_get_body(m, node);
//     body_to_cps(a, scope, p, body, cont_symb);
//     // TODO: fix leak
//     scope = scope->parent;

//     return;
//   }

//   if (node->kind == AST_BIND_CONSTANT || node->kind == AST_BIND_VARIABLE) {
//     // push current definition to the scope
//     scope_push(scope, node);
//     cps_analisys(a, scope, p, ast_manager_get_relative(m, node, node->right));
//     return;
//   }

//   if (node->kind == AST_PROGRAM_POINT) {
//     cps_analisys(a, scope, p, ast_manager_get_relative(m, node, node->left));
//     cps_analisys(a, scope, p, ast_manager_get_relative(m, node, node->right));

//     return;
//   }

//   if (node->kind == AST_HANDLER_LITERAL) { cps_analisys(a, scope, p, ast_manager_get_relative(m, node, node->right)); }

//   if (node->kind == AST_HANDLER_LITERAL) { cps_analisys(a, scope, p, ast_manager_get_relative(m, node, node->right)); }
// }

// Scope* function_literal_args_to_scope(Parser* p, AST_Node* node, Scope* parent = NULL) {
//   Scope* scope = scope_create(parent);

//   if (ast_is_null_node(node)) { return scope; }

//   AST_Manager* m = &p->ast_man;

//   assert(node->kind == AST_DECL_ARGS_LIST);

//   while (!ast_is_null_node(node)) {
//     AST_Node* arg = ast_decl_list_get_elem(m, node);

//     if (arg->kind == AST_BIND_CONSTANT || arg->kind == AST_BIND_VARIABLE) { arg = ast_manager_get_relative(m, arg, arg->left); }

//     assert(arg->kind == AST_BIND_TYPE);

//     scope_push(scope, arg);

//     node = ast_decl_list_get_tail(m, node);
//   }

//   return scope;
// }

// Scope* function_literal_ptr_arrow_args_to_scope(Parser* p, AST_Node* node, Scope* parent = NULL) {
//   Scope* scope = scope_create(parent);

//   if (ast_is_null_node(node)) { return scope; }

//   AST_Manager* m = &p->ast_man;

//   assert(node->kind == AST_DECL_ARGS_LIST);

//   while (!ast_is_null_node(node)) {
//     AST_Node* arg = ast_decl_list_get_elem(m, node);

//     if (arg->kind == AST_BIND_CONSTANT && arg->kind == AST_BIND_VARIABLE) { arg = ast_manager_get_relative(m, arg, arg->left); }

//     assert(arg->kind == AST_BIND_TYPE);

//     AST_Node* type = ast_type_bind_get_type(m, arg);

//     // TODO: in the future only function ptrs needs to be verified
//     if (type->kind == AST_TYPE_ARROW) { scope_push(scope, arg); }

//     if (type->kind == AST_TYPE_POINTER) {
//       AST_Node* ptr_to = ast_manager_get_relative(m, type, type->left);

//       if (ptr_to->kind == AST_TYPE_ARROW) { scope_push(scope, arg); }
//     }

//     node = ast_decl_list_get_tail(m, node);
//   }

//   return scope;
// }

// typedef std::unordered_set<AST_Id> Edges;
// typedef std::unordered_map<AST_Id, Edges> Graph;

// void union_aliases(Graph& a, Graph& b) {
//   // TODO(marcos): use a more efficient algorithm

//   for (Graph::iterator a_it = a.begin(); a_it != a.end(); a_it++) {
//     if (b.find(a_it->first) == b.end()) continue;

//     Edges& b_m = b[a_it->first];

//     for (Edges::iterator b_it = b_m.begin(); b_it != b_m.end(); b_it++) {
//       a_it->second.insert(*b_it);
//     }
//   }
// }

// void search_all_symbols(Edges& edges, Parser* p, AST_Node* right) {
//   if (ast_is_null_node(right)) return;

//   AST_Manager* m = &p->ast_man;

//   if (right->kind == AST_SYMBOL_LITERAL) { edges.insert(right->id); }

//   AST_Node* l = ast_manager_get_relative(m, right, right->left);
//   AST_Node* r = ast_manager_get_relative(m, right, right->right);

//   search_all_symbols(edges, p, l);
//   search_all_symbols(edges, p, r);
// }

// void compute_function_aliases(Graph& alias, Parser* p, Scope* scope, AST_Node* node) {
//   // NOTE(marcos): All function arguments are constants, so the only way that a
//   // variable/closure may escape the declaring scope is throught a return or a
//   // pointer store, here we are making a full analysis, but keep in mind that
//   // invalid assignments should be caught during type checking(not implemented).
//   if (ast_is_null_node(node)) return;

//   AST_Manager* m = &p->ast_man;

//   if (node->kind == AST_FUNCTION_LITERAL) { return; }

//   if (node->kind == AST_FUNCTION_CALL) {
//     AST_Node* symbol = ast_manager_get_relative(m, node, node->left);
//     AST_Node* decl   = scope_find(scope, p, symbol);

//     // NOTE(marcos): teste if functions is defined withing the local scope
//     // if it is note local then there is no function escaping from the local
//     // scope.
//     if (decl->kind == AST_BIND_CONSTANT || decl->kind == AST_BIND_VARIABLE) {
//       AST_Node* function  = ast_manager_get_relative(m, decl, decl->right);
//       AST_Node* signature = ast_manager_get_relative(m, function, function->left);
//       AST_Node* arguments = ast_manager_get_relative(m, signature, signature->left);
//       AST_Node* body      = ast_manager_get_relative(m, function, function->right);

//       // FIXME(marcos): leak
//       scope = function_literal_args_to_scope(p, arguments, scope);

//       compute_function_aliases(alias, p, scope, body);
//     }

//     return;
//   }

//   if (node->kind == AST_BIND_CONSTANT || node->kind == AST_BIND_VARIABLE) {
//     scope_push(scope, node);

//     AST_Node* l = ast_manager_get_relative(m, node, node->left);
//     AST_Node* r = ast_manager_get_relative(m, node, node->right);

//     if (r->kind == AST_FUNCTION_LITERAL) {
//       return;
//       // compute_function_aliases(alias, p, scope, r);
//     }

//     AST_Node* left_symbol = ast_manager_get_relative(m, l, l->left);

//     Edges edges;

//     search_all_symbols(edges, p, r);

//     for (Edges::iterator edge = edges.begin(); edge != edges.end(); edge++) {
//       AST_Node* symb = ast_manager_get(m, *edge);

//       AST_Node* decl = scope_find(scope, p, symb);

//       if (ast_is_null_node(decl)) { parser_error(p, symb->tok, "3 symbol not founb"); }

//       if (decl->kind == AST_BIND_CONSTANT || decl->kind == AST_BIND_VARIABLE) { decl = ast_bind_get_type_bind(m, decl); }

//       AST_Node* type         = ast_type_bind_get_type(m, decl);
//       AST_Node* right_symbol = ast_type_bind_get_symbol(m, decl);

//       if (type->kind == AST_TYPE_ARROW) { alias[left_symbol->id].insert(right_symbol->id); }
//     }

//     return;
//   }

//   if (node->kind == AST_OP_BIN_ASSIGN || node->kind == AST_OP_POINTER_STORE) {
//     // TODO(marcos): in the case of a pointer store it should store all pointer
//     // values
//     AST_Node* symb = ast_manager_get_relative(m, node, node->left);
//     AST_Node* bind = scope_find(scope, p, symb);

//     if (ast_is_null_node(bind)) { parser_error(p, symb->tok, "2 symbol not founb"); }

//     if (bind->kind == AST_BIND_CONSTANT || bind->kind == AST_BIND_VARIABLE) { bind = ast_manager_get_relative(m, bind, bind->left); }

//     AST_Node* left_symbol = ast_manager_get_relative(m, bind, bind->left);

//     AST_Node* r = ast_manager_get_relative(m, node, node->right);

//     if (r->kind != AST_FUNCTION_LITERAL) { compute_function_aliases(alias, p, scope, r); }

//     Edges edges;

//     if (r->kind == AST_SYMBOL_LITERAL) {
//       AST_Node* decl = scope_find(scope, p, r);

//       if (decl->kind == AST_BIND_CONSTANT || decl->kind == AST_BIND_VARIABLE) { decl = ast_bind_get_type_bind(m, decl); }

//       AST_Node* symbol = ast_type_bind_get_symbol(m, decl);
//       AST_Node* type   = ast_type_bind_get_type(m, decl);

//       if (type->kind == AST_TYPE_POINTER) {
//         // NOTE(marcos): mayber there is something
//         // more fancy that need to be done here, but
//         // keep in mind that this is a pointer store
//         // and it is copying the content of node->right
//         // to node->left
//         edges = alias[symbol->id];
//       } else {
//         search_all_symbols(edges, p, r);
//       }
//     }

//     alias[bind->left].clear();

//     for (Edges::iterator edge = edges.begin(); edge != edges.end(); edge++) {
//       AST_Node* symb = ast_manager_get(m, *edge);

//       AST_Node* decl = scope_find(scope, p, symb);

//       if (ast_is_null_node(decl)) { parser_error(p, symb->tok, "1. symbol not founb"); }

//       if (decl->kind == AST_BIND_CONSTANT || decl->kind == AST_BIND_VARIABLE) { decl = ast_bind_get_type_bind(m, decl); }

//       AST_Node* type         = ast_type_bind_get_type(m, decl);
//       AST_Node* right_symbol = ast_type_bind_get_symbol(m, decl);

//       if (type->kind == AST_TYPE_ARROW) { alias[bind->id].insert(decl->id); }
//     }

//     return;
//   }

//   if (node->kind == AST_CTRL_FLOW_IF) {
//     Graph graph;

//     // FIXME: leak
//     scope = scope_create(scope);

//     AST_Node* body = ast_manager_get_relative(m, node, node->right);

//     compute_function_aliases(graph, p, scope, node);

//     union_aliases(alias, graph);
//     return;
//   }

//   if (node->kind == AST_CTRL_FLOW_IF_ELSE) {
//     Graph graph0;
//     Graph graph1;

//     // TODO: fix leak
//     scope = scope_create(scope);

//     AST_Node* l = ast_manager_get_relative(m, node, node->left);
//     AST_Node* r = ast_manager_get_relative(m, node, node->right);

//     compute_function_aliases(graph0, p, scope, l);
//     compute_function_aliases(graph1, p, scope, r);

//     union_aliases(alias, graph0);
//     union_aliases(alias, graph1);
//     return;
//   }

//   AST_Node* l = ast_manager_get_relative(m, node, node->left);
//   AST_Node* r = ast_manager_get_relative(m, node, node->right);

//   compute_function_aliases(alias, p, scope, l);
//   compute_function_aliases(alias, p, scope, r);
// }

// void search_arrow(Parser* p, AST_Node* arg, Scope* scope, Scope* calls) {
//   if (ast_is_null_node(arg)) return;

//   AST_Manager* m = &p->ast_man;

//   if (arg->kind == AST_SYMBOL_LITERAL) {
//     AST_Node* decl = scope_find(scope, p, arg);

//     if (decl->kind == AST_BIND_CONSTANT || decl->kind == AST_BIND_VARIABLE) { decl = ast_manager_get_relative(m, decl, decl->left); }

//     AST_Node* symbol = ast_type_bind_get_symbol(m, decl);
//     AST_Node* type   = ast_type_bind_get_type(m, decl);

//     if (type->kind == AST_TYPE_ARROW) { scope_push(calls, decl); }
//   }

//   AST_Node* l = ast_manager_get_relative(m, arg, arg->left);
//   AST_Node* r = ast_manager_get_relative(m, arg, arg->right);

//   search_arrow(p, l, scope, calls);
//   search_arrow(p, r, scope, calls);
// }

// void get_call_function_arguments(Parser* p, AST_Node* node, AST_Node* f, Scope* scope, Scope* calls) {
//   if (ast_is_null_node(node)) return;

//   AST_Manager* m = &p->ast_man;

//   if (node->kind == AST_BIND_CONSTANT || node->kind == AST_BIND_VARIABLE) { scope_push(scope, node); }

//   if (node->kind == AST_FUNCTION_CALL) {
//     AST_Node* l = ast_manager_get_relative(m, node, node->left);
//     AST_Node* r = ast_manager_get_relative(m, node, node->right);

//     if (parser_is_same_symbol(p, l, f)) { search_arrow(p, r, scope, calls); }
//   }

//   if (node->kind == AST_FUNCTION_LITERAL) {
//     // FIXME(marcos): leak
//     AST_Node* body = ast_manager_get_relative(m, node, node->right);
//     scope          = scope_create(scope);
//     return get_call_function_arguments(p, body, f, scope, calls);
//   }

//   AST_Node* l = ast_manager_get_relative(m, node, node->left);
//   AST_Node* r = ast_manager_get_relative(m, node, node->right);

//   get_call_function_arguments(p, l, f, scope, calls);
//   get_call_function_arguments(p, r, f, scope, calls);
// }

// AST_Node* get_function_continuation_argument(Parser* p, AST_Node* signature) {
//   AST_Manager* m = &p->ast_man;

//   AST_Node* head = ast_manager_get_relative(m, signature, signature->left);
//   AST_Node* decl = ast_node_null(m);

//   while (!ast_is_null_node(head)) {
//     decl = ast_manager_get_relative(m, head, head->left);
//     head = ast_manager_get_relative(m, head, head->right);
//   }

//   assert(decl->kind == AST_BIND_TYPE);

//   return ast_manager_get_relative(m, decl, decl->left);
// }

// Scope* get_escaping_returns(Parser* p, AST_Node* func) {
//   assert(func->kind == AST_FUNCTION_LITERAL);

//   AST_Manager* m = &p->ast_man;

//   Scope* scope = scope_create(NULL);

//   AST_Node* signature = ast_manager_get_relative(m, func, func->left);
//   AST_Node* body      = ast_manager_get_relative(m, func, func->right);
//   AST_Node* cont      = get_function_continuation_argument(p, signature);

//   Scope* returned = scope_create(NULL);

//   get_call_function_arguments(p, body, cont, scope, returned);

//   return returned;
// }

// void add_aliases(Graph& alias, Parser* p, AST_Node* node, Scope* escaping) {
//   AST_Manager* m = &p->ast_man;

//   Edges& edges = alias[node->id];

//   scope_push(escaping, node);

//   for (Edges::iterator to = edges.begin(); to != edges.end(); to++) {
//     AST_Node* decl = ast_manager_get(m, *to);
//     add_aliases(alias, p, decl, escaping);
//   }
// }

// Scope* get_escaping_arguments(Parser* p, AST_Node* arguments, AST_Node* body) {
//   AST_Manager* m = &p->ast_man;

//   // FIXME(marcos): leak
//   Scope* scope  = function_literal_args_to_scope(p, arguments);
//   Scope* arrows = function_literal_ptr_arrow_args_to_scope(p, arguments);

//   // if (arrows->ctx == NULL) {
//   //   return scope_create(NULL);
//   // }

//   Graph aliases;

//   // TODO(marcos): revise this graph when the language gets a
//   // more defined form, is those all functions that can be escaped
//   // through arguments ? right now it is, but when pointers gets
//   // implemented we may need a more robust pointer analysis.

//   compute_function_aliases(aliases, p, scope, body);

//   Scope* escaping = scope_create(NULL);

//   while (arrows) {
//     Context* ctx = arrows->ctx;

//     while (ctx) {
//       Edges& edges = aliases[ctx->decl];

//       for (Edges::iterator to = edges.begin(); to != edges.end(); to++) {
//         AST_Node* decl = ast_manager_get(m, *to);
//         add_aliases(aliases, p, decl, escaping);
//       }

//       ctx = ctx->prev;
//     }

//     arrows = arrows->parent;
//   }
//   return escaping;
// }

// void push_scope_declarations(AST_Manager* m, Scope* a, Scope* b) {
//   while (b) {
//     Context* ctx = b->ctx;

//     while (ctx) {
//       AST_Node* decl = ast_manager_get(m, ctx->decl);

//       scope_push(a, decl);

//       ctx = ctx->prev;
//     }

//     b = b->parent;
//   }
// }

// void escaping_analysis(CPS_Function_Analysis* a, Parser* p, AST_Node* node) {
//   assert(node->kind == AST_FUNCTION_LITERAL);

//   AST_Manager* manager = &p->ast_man;

//   AST_Node* signature = ast_manager_get_relative(manager, node, node->left);
//   AST_Node* arguments = ast_manager_get_relative(manager, signature, signature->left);
//   AST_Node* body      = ast_manager_get_relative(manager, node, node->right);

//   Scope* escaping = get_escaping_arguments(p, arguments, body);
//   Scope* returns  = get_escaping_returns(p, node);
//   Scope* arrows   = function_literal_ptr_arrow_args_to_scope(p, arguments);

//   // FIXME(marcos): leaks
//   push_scope_declarations(manager, a->escaping_local_funcs, escaping);
//   push_scope_declarations(manager, a->escaping_local_funcs, arrows);
//   push_scope_declarations(manager, a->escaping_local_funcs, returns);
// }

// void run_escaping_analysis(CPS_Function_Analysis* a, Parser* p, AST_Node* node) {
//   assert(node->kind == AST_PROGRAM_POINT);

//   AST_Manager* m = &p->ast_man;

//   while (!ast_is_null_node(node)) {
//     AST_Node* prog_point = ast_manager_get_relative(m, node, node->left);

//     if (prog_point->kind == AST_BIND_CONSTANT || prog_point->kind == AST_BIND_VARIABLE) {
//       AST_Node* decl = ast_bind_get_expr(m, prog_point);

//       if (decl->kind == AST_FUNCTION_LITERAL) { escaping_analysis(a, p, decl); }
//     }

//     node = ast_manager_get_relative(m, node, node->right);
//   }
// }

AST_Node* get_call_continuation_argument(Parser* p, AST_Node* call, AST_Node** arg = NULL) {
  AST_Manager* m = &p->ast_man;

  AST_Node* head = ast_manager_get_relative(m, call, call->right);
  AST_Node* decl = ast_node_null(m);

  while (!ast_is_null_node(head)) {
    if (arg) { *arg = head; }

    decl = ast_manager_get_relative(m, head, head->left);
    head = ast_manager_get_relative(m, head, head->right);
  }

  return decl;
}

// AST_Node* lift_call_continuation_to_declaration(Parser* p, AST_Node* statement, AST_Node* program_point) {
//   AST_Manager* m = &p->ast_man;
//   // continuation argument to declaration
//   AST_Node* arg_cont = ast_node_null(m);

//   Token undef = lexer_undef_token();

//   AST_Node* cont_symb = ast_temp_node(m);
//   AST_Node* cont_type = ast_type_any(m, undef);
//   AST_Node* cont_bind = ast_type_bind(m, undef, cont_symb, cont_type);

//   AST_Node* cont_func = get_call_continuation_argument(p, statement, &arg_cont);
//   AST_Node* cont_decl = ast_constant_bind(m, undef, cont_bind, cont_func);

//   assert(program_point->right == 0);

//   AST_Node* call_point = ast_program_point(m, undef);

//   // insert the declaration at the body of the function
//   call_point->left     = statement->id;
//   program_point->left  = cont_decl->id;
//   program_point->right = call_point->id;

//   // replace continuation literal by the continuation symbol on the call body
//   arg_cont->left = cont_symb->id;
//   return cont_decl;
// }

void replace_return_with_continuation_call(Parser* p, AST_Node* statement, AST_Node* continuation) {
  if (ast_is_null_node(statement)) return;
  if (statement->kind == AST_TYPE_STRUCT) return;
  if (statement->kind == AST_HANDLER_LITERAL) return;
  if (statement->kind == AST_FUNCTION_LITERAL) return;

  AST_Manager* m  = &p->ast_man;
  Token undefined = lexer_undef_token();

  if (statement->kind == AST_CTRL_FLOW_RETURN) {
    ast_change_kind(statement, AST_FUNCTION_CALL);

    AST_Node* symbol = ast_symbol(m, undefined);

    AST_Node* expression = ast_manager_get_relative(m, statement, statement->left);

    *symbol = *continuation;

    statement->left  = symbol->id;
    statement->right = ast_decl_args(m, undefined, expression, ast_node_null(m))->id;
  }

  AST_Node* l = ast_manager_get_relative(m, statement, statement->left);
  AST_Node* r = ast_manager_get_relative(m, statement, statement->right);

  replace_return_with_continuation_call(p, l, continuation);
  replace_return_with_continuation_call(p, r, continuation);
}

AST_Node* create_continuation_function(Parser* p, Scope* scope, AST_Node* argument, AST_Node* return_type, AST_Node* body) {
  AST_Manager* m  = &p->ast_man;
  Token undefined = lexer_undef_token();

  // NOTE(marcos): If the binding dosent have a type set it to be 'any',
  // this is just a quick patch since we dont have type check and
  // inference yet.
  if (!ast_is_null_node(argument)) {
    AST_Node* argument_type = ast_type_bind_get_type(m, argument);

    if (ast_is_null_node(argument_type)) { argument->right = ast_type_any(m, undefined)->id; }
  }

  AST_Node* arguments = ast_decl_args(m, undefined, argument, ast_node_null(m));
  AST_Node* signature = ast_function_signature(m, undefined, arguments, return_type);
  AST_Node* function  = ast_function_literal(m, undefined, signature, body);
  AST_Node* symbol    = ast_temp_node(m);
  AST_Node* type      = ast_type_any(m, undefined); // TODO: infer type from 'arguments' and 'return_type'
  AST_Node* bind      = ast_type_bind(m, undefined, symbol, type);

  return ast_constant_bind(m, undefined, bind, function);
}

AST_Node* function_call_cps_conversion(Parser* p, Scope* scope, AST_Node* program_point, AST_Node* call, AST_Node* continuation, AST_Node* bind);
void function_literal_cps_conversion(Parser* p, Scope* scope, AST_Node* function, AST_Node* continuation = NULL);
void program_point_cps_conversion(Parser* p, AST_Node* statements, Scope* scope);

void program_point_cps_conversion(Parser* p, AST_Node* statements, Scope* scope) {

  AST_Manager* m = &p->ast_man;

  while (!ast_is_null_node(statements)) {

    AST_Node* statement    = ast_program_point_get_decl(m, statements);
    AST_Node* continuation = ast_program_point_get_tail(m, statements);

    if (statement->kind == AST_BIND_CONSTANT || statement->kind == AST_BIND_VARIABLE) {
      AST_Node* left  = ast_bind_get_type_bind(m, statement);
      AST_Node* right = ast_bind_get_expr(m, statement);

      if (right->kind == AST_FUNCTION_LITERAL) { scope_push(scope, statement); }

      if (right->kind == AST_FUNCTION_CALL) {
        program_point_cps_conversion(p, continuation, scope);
        statements = function_call_cps_conversion(p, scope, statements, right, continuation, left);
      }
    }

    if (statement->kind == AST_OP_BIN_ASSIGN) { assert(false && "TODO: implement reassignment"); }

    if (statement->kind == AST_FUNCTION_CALL) {
      // break continuation
      // statements->right = ast_node_null(m)->id;
      program_point_cps_conversion(p, continuation, scope);
      statements = function_call_cps_conversion(p, scope, statements, statement, continuation, ast_node_null(m));
    }

    if (statement->kind == AST_CTRL_FLOW_IF) {
      AST_Node* right = ast_manager_get_relative(m, statement, statement->right);
      program_point_cps_conversion(p, right, scope);
    }

    if (statement->kind == AST_CTRL_FLOW_IF_ELSE) {
      AST_Node* list = statement;

      while (!ast_is_null_node(list) && list->kind == AST_CTRL_FLOW_IF_ELSE) {
        AST_Node* if_statement = ast_manager_get_relative(m, list, list->left);

        AST_Node* body = if_statement;

        if (if_statement->kind == AST_CTRL_FLOW_IF) { body = ast_manager_get_relative(m, if_statement, if_statement->right); }

        program_point_cps_conversion(p, body, scope);

        list = ast_manager_get_relative(m, list, list->right);
      }

      program_point_cps_conversion(p, list, scope);
    }

    statements = ast_program_point_get_tail(m, statements);
  }
}

AST_Node* function_call_cps_conversion(Parser* p, Scope* scope, AST_Node* program_point, AST_Node* call, AST_Node* continuation, AST_Node* bind) {
  AST_Manager* m     = &p->ast_man;
  AST_Node* function = ast_fun_call_get_call_sym(m, call);

  b8 is_local = false;

  AST_Node* local = scope_find(scope, p, function, &is_local);

  Token undefined = lexer_undef_token();

  AST_Node* continuation_symbol = ast_node_null(m);
  AST_Node* declaration         = ast_node_null(m);

  if (!ast_is_null_node(continuation)) {
    AST_Node* type = ast_type_any(m, undefined); // TODO: use a global context to find function and the type

    declaration = create_continuation_function(p, scope, bind, type, continuation);

    // AST_Node* body = ast_function_literal_get_body(m, declaration);

    // AST_Node* head = ast_manager_get_relative(m, body, body->left);

    // if()

    AST_Node* continuation_bind = ast_bind_get_type_bind(m, declaration);

    continuation_symbol = ast_type_bind_get_symbol(m, continuation_bind);
  }

  if (local->kind == AST_BIND_VARIABLE || local->kind == AST_BIND_CONSTANT) {
    AST_Node* closure = ast_bind_get_expr(m, local);

    if (closure->kind == AST_FUNCTION_LITERAL) {
      if (is_local) {
        // FIXME(marcos): memory leak
        Scope* closure_scope = scope_create(scope);

        function_literal_cps_conversion(p, closure_scope, closure, continuation_symbol);

        // AST_Node* old_body = ast_function_literal_get_body(m, closure);
        // AST_Node* new_body = ast_program_point(m, undefined);

        // new_body->left  = declaration->id;
        // new_body->right = old_body->id;

        // closure->right = new_body->id;
      }

      if (!ast_is_null_node(bind)) {
        program_point->left  = call->id;
        program_point->right = ast_node_null(m)->id;
      }

      // return program_point;
    }
  }

  // NOTE(marcos): call to global function
  if (ast_is_null_node(continuation)) { return program_point; }

  ast_call_push_argument(m, undefined, call, continuation_symbol);

  // if (!is_local) {

  AST_Node* statement = ast_program_point(m, undefined);

  statement->left = call->id;

  program_point->left  = declaration->id;
  program_point->right = statement->id;

  return statement;
  // }

  // return program_point;
}

void function_literal_cps_conversion(Parser* p, Scope* scope, AST_Node* function, AST_Node* continuation_symbol) {
  assert(function->kind == AST_FUNCTION_LITERAL);

  AST_Manager* m  = &p->ast_man;
  Token undefined = lexer_undef_token();

  AST_Node* statements = ast_function_literal_get_body(m, function);
  AST_Node* signature  = ast_function_literal_get_signature(m, function);

  if (continuation_symbol == NULL) {
    // add the continuation argument
    AST_Node* symbol = ast_temp_node(m);
    AST_Node* type   = ast_type_any(m, undefined);
    AST_Node* bind   = ast_constant_bind(m, undefined, symbol, type);

    ast_function_literal_push_argument(m, undefined, function, bind);

    continuation_symbol = symbol;
  }

  replace_return_with_continuation_call(p, statements, continuation_symbol);

  program_point_cps_conversion(p, statements, scope);
}

void cps_conversion(Parser* p, AST_Node* root) {
  assert(root->kind == AST_PROGRAM_POINT);

  AST_Manager* m = &p->ast_man;

  while (!ast_is_null_node(root)) {
    AST_Node* prog_point = ast_manager_get_relative(m, root, root->left);

    if (prog_point->kind == AST_BIND_CONSTANT || prog_point->kind == AST_BIND_VARIABLE) {
      AST_Node* decl = ast_bind_get_expr(m, prog_point);

      if (decl->kind == AST_FUNCTION_LITERAL) {
        // FIXME(marcos): memory leak
        Scope* scope = scope_create(NULL);

        function_literal_cps_conversion(p, scope, decl);
      }
    }

    root = ast_manager_get_relative(m, root, root->right);
  }
}

AST_Node* get_first_call(Parser* p, AST_Node* node) {
  AST_Manager* m = &p->ast_man;

  if (ast_is_null_node(node)) { return ast_node_null(m); }

  assert(node->kind == AST_PROGRAM_POINT);

  AST_Node* statement = ast_program_point_get_decl(m, node);
  AST_Node* tail      = ast_program_point_get_tail(m, node);

  if (statement->kind == AST_FUNCTION_CALL) { return statement; }

  return get_first_call(p, tail);
}

typedef std::unordered_map< AST_Id, std::unordered_set< AST_Id > > CPS_Ext_Graph_Ctx;

void build_extended_cps_graph_declaration(CPS_Ext_Graph_Ctx& context, Scope* scope, Parser* p, AST_Node* function, AST_Node* statement) {
  AST_Manager* m = &p->ast_man;

  if (statement->kind == AST_FUNCTION_CALL) {
    AST_Node* symbol    = ast_fun_call_get_call_sym(m, statement);
    AST_Node* arguments = ast_fun_call_get_call_args(m, statement);

    AST_Node* declaration = scope_find(scope, p, symbol);
    if (!ast_is_null_node(declaration)) {
      AST_Node* bind = declaration;

      if (bind->kind == AST_BIND_CONSTANT || bind->kind == AST_BIND_VARIABLE) { bind = ast_bind_get_type_bind(m, bind); }
      assert(bind->kind == AST_BIND_TYPE);

      AST_Node* expr = ast_bind_get_expr(m, declaration);

      if (expr->kind != AST_FUNCTION_LITERAL) { return; }

      context[function->id].insert(bind->id);

    } else {
      // is global function
      AST_Node* continuation = get_call_continuation_argument(p, statement);
      AST_Node* declaration  = scope_find(scope, p, continuation);

      if (ast_is_null_node(declaration) || (declaration->kind != AST_BIND_CONSTANT && declaration->kind != AST_BIND_VARIABLE)) { return; }

      AST_Node* expr = ast_bind_get_expr(m, declaration);
      if (expr->kind != AST_FUNCTION_LITERAL) { return; }

      AST_Node* bind = declaration;

      if (bind->kind == AST_BIND_CONSTANT || bind->kind == AST_BIND_VARIABLE) { bind = ast_bind_get_type_bind(m, bind); }
      assert(bind->kind == AST_BIND_TYPE);

      context[function->id].insert(bind->id);
    }
  }

  if (statement->kind == AST_CTRL_FLOW_IF) {
    AST_Node* l = ast_manager_get_relative(m, statement, statement->left);
    AST_Node* r = ast_manager_get_relative(m, statement, statement->right);

    build_extended_cps_graph_declaration(context, scope, p, function, l);
    build_extended_cps_graph_declaration(context, scope, p, function, r);
  }

  if (statement->kind == AST_CTRL_FLOW_IF_ELSE) {
    AST_Node* l = ast_manager_get_relative(m, statement, statement->left);
    AST_Node* r = ast_manager_get_relative(m, statement, statement->right);

    build_extended_cps_graph_declaration(context, scope, p, function, l);
    build_extended_cps_graph_declaration(context, scope, p, function, r);
  }

  if (statement->kind == AST_BIND_CONSTANT || statement->kind == AST_BIND_VARIABLE) {

    AST_Node* bind = ast_bind_get_type_bind(m, statement);
    AST_Node* expr = ast_bind_get_expr(m, statement);

    if (expr->kind == AST_FUNCTION_LITERAL) {
      scope_push(scope, statement);

      Scope* closure_scope = scope_create(scope);

      assert(bind->kind == AST_BIND_TYPE);

      context[bind->id] = std::unordered_set< AST_Id >();

      AST_Node* signature = ast_function_literal_get_signature(m, expr);
      AST_Node* arguments = ast_function_signature_get_args(m, signature);
      AST_Node* body      = ast_function_literal_get_body(m, expr);

      build_extended_cps_graph_declaration(context, closure_scope, p, bind, body);
    }
  }

  if (statement->kind == AST_PROGRAM_POINT) {
    while (!ast_is_null_node(statement)) {
      assert(statement->kind == AST_PROGRAM_POINT);

      AST_Node* closure_statement = ast_program_point_get_decl(m, statement);

      build_extended_cps_graph_declaration(context, scope, p, function, closure_statement);

      statement = ast_program_point_get_tail(m, statement);
    }
  }
}

void build_extended_cps_graph(CPS_Ext_Graph_Ctx& graph, Parser* p, AST_Node* root) {
  assert(root->kind == AST_PROGRAM_POINT);

  AST_Manager* m = &p->ast_man;

  while (!ast_is_null_node(root)) {
    AST_Node* statement = ast_program_point_get_decl(m, root);
    // FIXME(marcos): memory leak
    Scope* scope = scope_create(NULL);

    build_extended_cps_graph_declaration(graph, scope, p, ast_node_null(m), statement);

    root = ast_program_point_get_tail(m, root);
  }
}

void print_cps_extended_graph(CPS_Ext_Graph_Ctx& ctx, AST_Id node, Parser* p) {
  if (ctx[node].size() == 0) return;

  AST_Manager* m = &p->ast_man;

  AST_Node* bind = ast_manager_get(m, node);

  assert(bind->kind == AST_BIND_TYPE);

  AST_Node* symbol = ast_type_bind_get_symbol(m, bind);
  printf("from ");

  print_ast_to_program(p, symbol);

  printf(" to ");

  for (std::unordered_set< AST_Id >::iterator it = ctx[node].begin(); it != ctx[node].end(); it++) {
    AST_Node* to = ast_manager_get(m, *it);

    assert(to->kind == AST_BIND_TYPE);

    AST_Node* symbol = ast_type_bind_get_symbol(m, to);

    print_ast_to_program(p, symbol);

    printf(" ");
  }

  printf("\n");
}

void print_cps_extended_graph_context(CPS_Ext_Graph_Ctx& ctx, Parser* p) {
  AST_Manager* m = &p->ast_man;

  for (CPS_Ext_Graph_Ctx::iterator it = ctx.begin(); it != ctx.end(); it++) {
    print_cps_extended_graph(ctx, it->first, p);
  }
}
