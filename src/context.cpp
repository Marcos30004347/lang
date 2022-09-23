#include "context.hpp"
#include "ast/ast_declaration.hpp"
#include "ast/ast_function.hpp"
#include "ast/ast_literals.hpp"
#include "ast/ast_manager.hpp"
#include "ast/ast_operations.hpp"
#include "ast/ast_program_point.hpp"
#include "ast/ast_types.hpp"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unordered_map>

Context* context_create(Context* parent) {
  Context* ctx          = new Context();
  ctx->parent           = parent;
  ctx->last_declaration = NULL;
  return ctx;
}

// void assignment_destroy(Assignment* b) {
//   while (b) {
//     Assignment* a = b->previous;
//     delete b;
//     b = a;
//   }
// }

Declaration* declaration_create(ast::Node* declaration, ast::Node* symbol, ast::Node* type) {
  assert(
      ast::is_instance< ast::Variable_Assignment_Node* >(declaration)
      || ast::is_instance< ast::Declaration_Constant_Node* >(declaration));

  Declaration* d = new Declaration();

  d->bind   = declaration;
  d->symbol = symbol;
  d->type   = type;
  // d->assignments          = Assignments();
  d->context              = NULL;
  d->previous_declaration = NULL;

  return d;
}

// void assignment_create(Declaration* d, ast::Node* value, ast::Node* program_point) {
//   d->assignments = Assignments();
//   d->assignments.insert(value);
// }

void context_merge(parser::Parser* p, Context* a, Context* b) {
  ast::Manager* m = p->ast_manager;

  Declaration* b_decl = b->last_declaration;

  while (b_decl) {
    Declaration* a_decl = context_declaration_of(a, p, b_decl->symbol);

    if (a_decl != NULL) {
      if (a_decl->context) {
        assert(b_decl->context);
        context_merge(p, a_decl->context, b_decl->context);
      } else {
        // for (Assignments::iterator assignment = b_decl->assignments.begin();
        //      assignment != b_decl->assignments.end();
        //      assignment++) {
        //   a_decl->assignments.insert(*assignment);
        // }
      }
    } else {
      Declaration* d = declaration_create(b_decl->bind, b_decl->symbol, b_decl->type);
      if (b_decl->context) {
        d->context = context_copy(b_decl->context);
      } else {

        // d->assignments = b_decl->assignments;

        // for (Assignments::iterator assignment = b_decl->assignments.begin();
        //      assignment != b_decl->assignments.end();
        //      assignment++) {
        //   d->assignments.insert(*assignment);
        // }

        d->previous_declaration = a->last_declaration;
      }

      a->last_declaration = d;
    }

    b_decl = b_decl->previous_declaration;
  }
}

Context* context_destroy(Context* ctx) {
  Context* p = ctx->parent;

  while (ctx->last_declaration) {
    Declaration* t = ctx->last_declaration->previous_declaration;

    // assignment_destroy(ctx->last_declaration->assignments);

    if (ctx->last_declaration->context) {
      context_destroy(ctx->last_declaration->context);
    }

    delete ctx->last_declaration;

    ctx->last_declaration = t;
  }

  delete ctx;

  return p;
}

// void setup_declaration_values(
//     Context* ctx, parser::Parser* p, Declaration* d, ast::Node* type, ast::Node* value, ast::Node*
//     program_point) {
//   ast::Node*    type_symbol = type;
//   ast::Manager* m           = p->ast_manager;

//   if (ast::is_instance< ast::Type_Pointer_Node* >(type_symbol)) {
//     type_symbol = ast::manager_get_relative(m, type_symbol, type_symbol->left);
//   }

//   Declaration* type_decl = NULL;

//   if (ast::is_instance< ast::Literal_Symbol_Node* >(type_symbol)) {
//     type_decl = context_declaration_of(ctx, p, type_symbol);
//   }

//   if (type_decl && ast::is_instance< ast::Type_Struct_Node* >(type_decl->type)) {
//     assert(ast::is_instance< ast::Declaration_Constant_Node* >(type_decl->bind));

//     assert(type_decl->assignments.size() == 1);

//     ast::Node* value = *type_decl->assignments.begin();

//     assert(ast::is_instance< ast::Literal_Struct_Node* >(value));

//     ast::Literal_Struct_Node* struct_literal = ast::as< ast::Literal_Struct_Node* >(value);

//     ast::ProgramPoint_List_Node* struct_members = struct_literal->get_members(p->ast_manager);

//     d->context = context_create(NULL);

//     assignment_create(d, NULL, program_point);

//     while (struct_members) {
//       ast::Node* member = struct_members->get_statement(p->ast_manager);

//       context_declare(d->context, p, member, program_point);

//       struct_members = struct_members->get_next_program_point(p->ast_manager);
//     }
//   } else {
//     d->context = NULL;

//     assignment_create(d, value, program_point);
//   }
// }

Declaration* context_declare(Context* ctx, parser::Parser* p, ast::Node* declaration, ast::Node* program_point) {
  assert(ast::is_declaration_node(declaration));

  ast::Manager* m = p->ast_manager;

  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(declaration)) {
    ast::Node* symbol = var->get_symbol(p->ast_manager);
    ast::Node* type   = var->get_type(p->ast_manager);

    Declaration* d = declaration_create(declaration, symbol, type);

    // setup_declaration_values(ctx, p, d, type, ast::create_node_literal_undefined(p->ast_manager),
    // program_point);

    d->previous_declaration = ctx->last_declaration;

    ctx->last_declaration = d;

    return d;
  }

  if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(declaration)) {
    ast::Node* symbol = var->get_symbol(p->ast_manager);
    ast::Node* type   = var->get_type(p->ast_manager);

    Declaration* d = declaration_create(declaration, symbol, type);

    // setup_declaration_values(ctx, p, d, type, ast::create_node_literal_undefined(p->ast_manager),
    // program_point);

    d->previous_declaration = ctx->last_declaration;

    ctx->last_declaration = d;

    return d;
  }

  return NULL;
}

// void context_assign(Context* ctx, parser::Parser* p, ast::Node* node, ast::Node* program_point) {
//   assert(ast::is_instance< ast::Variable_Assignment_Node* >(node));

//   ast::Variable_Assignment_Node* assignment = ast::as< ast::Variable_Assignment_Node* >(node);

//   ast::Manager* m = p->ast_manager;

//   ast::Node* left = assignment->get_left_operand(p->ast_manager);

//   while (ast::Variable_Assignment_Node* assignment = ast::is_instance< ast::Variable_Assignment_Node* >(left)) {
//     ast::Node* symbol = assignment->get_left_operand(p->ast_manager);

//     Declaration* decl = context_declaration_of(ctx, p, symbol);

//     assert(decl);

//     ctx = decl->context;

//     left = assignment->get_right_operand(p->ast_manager);
//   }

//   Declaration* decl = context_declaration_of(ctx, p, left);

//   // assignment_create(decl, assignment->get_left_operand(p->ast_manager), program_point);
// }

Declaration*
context_declaration_of_rec(Context* ctx, parser::Parser* p, ast::Node* symbol, Context* _c, b8* is_local) {

  if (ctx == NULL) {
    if (is_local)
      *is_local = false;
    return NULL;
  }

  ast::Manager* m = p->ast_manager;

  if (ast::is_instance< ast::Member_Access_Node* >(symbol)) {
    ast::Member_Access_Node* access = ast::as< ast::Member_Access_Node* >(symbol);

    ast::Node* first  = access->get_left_operand(p->ast_manager);
    ast::Node* second = access->get_right_operand(p->ast_manager);

    Declaration* decl = context_declaration_of_rec(ctx, p, first, _c, is_local);

    return context_declaration_of_rec(decl->context, p, second, _c, is_local);
  }

  if (ast::Declaration_Constant_Node* var = ast::is_instance< ast::Declaration_Constant_Node* >(symbol)) {
    symbol = var->get_symbol(p->ast_manager);
  }

  if (ast::Declaration_Variable_Node* var = ast::is_instance< ast::Declaration_Variable_Node* >(symbol)) {
    symbol = var->get_symbol(p->ast_manager);
  }

  assert(ast::is_instance< ast::Literal_Symbol_Node* >(symbol));

  Declaration* declaration = ctx->last_declaration;

  while (declaration) {
    if (parser_is_same_symbol(p, symbol, declaration->symbol)) {

      if (is_local) {
        *is_local = ctx == _c;
      }

      return declaration;
    }

    declaration = declaration->previous_declaration;
  }

  return context_declaration_of_rec(ctx->parent, p, symbol, _c, is_local);
}

Declaration* context_declaration_of(Context* ctx, parser::Parser* p, ast::Node* symbol, b8* is_local) {
  return context_declaration_of_rec(ctx, p, symbol, ctx, is_local);
}

ast::Node* context_type_of(Context* ctx, parser::Parser* p, ast::Node* symbol) {
  Declaration* declaration = context_declaration_of(ctx, p, symbol);

  ast::Manager* m = p->ast_manager;

  if (declaration == NULL) {
    return ast::create_node_literal_nothing(p->ast_manager);
  }

  return declaration->type;
}

// Assignments* context_values_of(Context* ctx, parser::Parser* p, ast::Node* symbol) {
//   Declaration* declaration = context_declaration_of(ctx, p, symbol);

//   ast::Manager* m = p->ast_manager;

//   if (declaration == NULL)
//     return NULL;

//   return &declaration->assignments;
// }

// Assignment* assignment_copy(Assignment* a) {
//   if (a == NULL) return NULL;
//   return assignment_create(a->value, a->point, assignment_copy(a->previous));
// }

Declaration* declaration_copy(Declaration* d) {

  if (d == NULL)
    return NULL;

  Declaration* r = declaration_create(d->bind, d->symbol, d->type);

  r->previous_declaration = declaration_copy(d->previous_declaration);

  // r->assignments = d->assignments; // assignment_copy(d->assignments);

  return r;
}

Context* context_copy(Context* a) {
  if (a == NULL)
    return NULL;

  Context* c = context_create(context_copy(a->parent));

  c->last_declaration = declaration_copy(a->last_declaration);

  return c;
}

void context_replace(Context* a, Context* b) {
  if (a == NULL) {
    assert(b == NULL);
    return;
  }

  context_replace(a->parent, b->parent);

  Declaration* t = a->last_declaration;

  a->last_declaration = b->last_declaration;
  b->last_declaration = t;
}

// void assignment_print(Assignments* a, parser::Parser* p, int tabs) {
//   if (a == NULL) {
//     return;
//   }

//   ast::Manager* m = p->ast_manager;

//   u64 i = 0;

//   for (Assignments::iterator it = a->begin(); it != a->end(); it++) {
//     i = i + 1;

//     ast::Node* v = *it;

//     if (ast::is_instance< ast::Literal_Struct_Node* >(v)) {
//       printf("#struct_literal");
//     } else {
//       print_ast_ir(p, v, tabs);
//     }

//     if (i < a->size()) {
//       printf("\n ");
//     }
//   }
// }

void declaration_print(Declaration* d, parser::Parser* p, int tabs) {
  if (d == NULL)
    return;
  printf("[| ");
  print_ast_ir(p, d->symbol);
  printf(" |] = ( ");
  if (d->context) {
    context_print(d->context, p, tabs);
  } else {
    // assignment_print(&d->assignments, p, tabs);
  }
  printf(" )\n");
}

void context_print(Context* ctx, parser::Parser* p, int tabs) {
  if (ctx == NULL)
    return;
  Declaration* declaration = ctx->last_declaration;
  for (u32 i = 0; i < tabs; i++)
    printf(" ");
  printf("context {\n");
  tabs = tabs + 2;
  while (declaration) {
    for (u32 i = 0; i < tabs; i++)
      printf(" ");
    declaration_print(declaration, p, tabs);
    declaration = declaration->previous_declaration;
  }
  tabs = tabs - 2;
  context_print(ctx->parent, p, tabs + 2);

  for (u32 i = 0; i < tabs; i++)
    printf(" ");

  printf("}");
  printf("\n");
}

Context* declaration_arguments_to_context(parser::Parser* p, ast::Node* node, Context* parent) {
  Context* ctx = context_create(parent);

  if (ast::is_instance< ast::Literal_Nothing_Node* >(node)) {
    return ctx;
  }

  ast::Manager* m = p->ast_manager;

  assert(ast::is_instance< ast::Declarations_List_Node* >(node));

  ast::Declarations_List_Node* list = ast::as< ast::Declarations_List_Node* >(node);

  while (!ast::is_instance< ast::Literal_Nothing_Node* >(list)) {
    ast::Node* arg = list->get_declaration(p->ast_manager);

    assert(ast::is_declaration_node(arg));

    context_declare(ctx, p, arg, node);

    list = list->get_next_declaration(p->ast_manager);
  }

  return ctx;
}

// Context* context_push(Context* r, ast::Node* n) {
//   assert(n->kind == AST_BIND_TYPE || n->kind == AST_BIND_CONSTANT || n->kind == AST_BIND_VARIABLE);

//   Context* c = (Context*)malloc(sizeof(Context));

//   c->prev = r;
//   c->decl = n->id;
//   c->next = NULL;

//   if (r) { r->next = c; }

//   return c;
// }

// Context* context_pop(Context* r) {
//   Context* p = r->prev;

//   free(r);

//   return p;
// }

// ast::Node* context_find(Context* env, parser::Parser** p, ast::Node* sym) {
//   ast::Manager* m = p->ast_manager;

//   if (env == NULL) return ast_node_null(m);

//   ast::Node* node = ast::manager_get(m, env->decl);

//   assert(node->kind == AST_BIND_TYPE || node->kind == AST_BIND_CONSTANT || node->kind ==
//   AST_BIND_VARIABLE);

//   ast::Node* root = node;

//   if (root->kind == AST_BIND_CONSTANT || root->kind == AST_BIND_VARIABLE) { root =
//   ast_bind_get_type_bind(m, root); }

//   ast::Node* symb = ast_type_bind_get_symbol(m, root);

//   if (parser_is_same_symbol(p, sym, symb)) { return node; }

//   return context_find(env->prev, p, sym);
// }

// void scope_init(Scope* scope, Scope* parent) {
//   scope->ctx    = NULL;
//   scope->parent = parent;
// }

// void scope_print_rec(Scope* s, parser::Parser** p) {
//   if (s == NULL) return;

//   printf("{ ");

//   Context* ctx = s->ctx;

//   while (ctx) {
//     ast::Node* n = ast::manager_get(p->ast_manager, ctx->decl);
//     ast::Node* s = ast_node_null(p->ast_manager);

//     if (n->kind == AST_BIND_TYPE) {
//       s = ast_type_bind_get_symbol(p->ast_manager, n);
//     } else {
//       ast::Node* t = ast_bind_get_type_bind(p->ast_manager, n);
//       s           = ast_type_bind_get_symbol(p->ast_manager, t);
//     }

//     i8 buff[256];

//     if (ast::is_temporary(p->ast_manager, s)) {
//       u64 t   = s->kind;
//       u64 i   = 1;
//       buff[0] = '%';
//       while (t) {
//         buff[i++] = t % 10 + '0';
//         t         = t / 10;
//       }
//       buff[i] = '\0';
//     } else {
//       token_get_id(&p->lex, s->tok, buff);
//     }
//     printf("%s", buff);

//     ctx = ctx->prev;
//     if (ctx) printf(", ");
//   }

//   printf(" }");

//   scope_print_rec(s->parent, p);
// }

// void scope_print(Scope* s, parser::Parser** p) {
//   scope_print_rec(s, p);
//   printf("\n");
// }

// Scope* scope_create(Scope* parent) {
//   Scope* s = (Scope*)malloc(sizeof(Scope));
//   scope_init(s, parent);
//   return s;
// }

// ast::Node* scope_find_rec(Scope* s, parser::Parser** p, ast::Node* sym, Scope** owner) {
//   ast::Manager* m = p->ast_manager;

//   if (s == NULL) return ast_node_null(m);

//   ast::Node* n = context_find(s->ctx, p, sym);

//   if (!ast::is_instance<ast::Literal_Nothing_Node*>(n)) {
//     if (owner) { *owner = s; }
//     return n;
//   }

//   return scope_find_rec(s->parent, p, sym, owner);
// }

// ast::Node* scope_find(Scope* s, parser::Parser** p, ast::Node* sym, b8* is_local) {
//   if (is_local) *is_local = false;

//   Scope* scope = NULL;

//   ast::Node* n = scope_find_rec(s, p, sym, &scope);

//   if (scope == s && is_local) { *is_local = true; }

//   return n;
// }

// ast::Node* scope_find_local(Scope* s, parser::Parser** p, ast::Node* sym) {
//   ast::Manager* m = p->ast_manager;
//   if (s == NULL) { return ast_node_null(m); }

//   return context_find(s->ctx, p, sym);
// }

// void scope_push(Scope* s, ast::Node* n) {
//   assert(n->kind == AST_BIND_TYPE || n->kind == AST_BIND_CONSTANT || n->kind == AST_BIND_VARIABLE);
//   s->ctx = context_push(s->ctx, n);
// }

// void scope_pop(Scope* s) { s->ctx = context_pop(s->ctx); }

// b8 scope_is_global(Scope* s) { return s->parent == NULL; }

// u64 get_scope_depth(Scope* s) {
//   u64 d = 0;

//   while (s) {
//     d = d + 1;
//     s = s->parent;
//   }

//   return d;
// }
