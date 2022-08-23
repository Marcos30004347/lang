#include <__hash_table>
#include <algorithm>
#include <assert.h>

#include "ast.hpp"
#include "error.hpp"
#include "lexer.hpp"
#include "parser.hpp"
#include "transformations.hpp"

AST_Node *get_type_from_fun_decl(AST_Manager *m, AST_Node *n) {
  AST_Node *s = ast_function_literal_get_signature(m, n);

  AST_Node *a = ast_function_signature_get_args(m, s);
  AST_Node *r = ast_function_signature_get_return(m, s);

  AST_Node *type = ast_node_null(m);

  while (!ast_is_null_node(a)) {
    AST_Node *bind = ast_manager_get_relative(m, a, a->left);

    if (ast_is_null_node(type)) {
      type = ast_type_bind_get_type(m, bind);
    } else {
      AST_Node *t = ast_type_bind_get_type(m, bind);
      type = ast_bin_mul(m, bind->tok, type, t);
    }

    a = ast_manager_get_relative(m, a, a->right);
  }

  return ast_type_arrow(m, lexer_undef_token(), type, r);
}
void closure_convert_statment_list(ClosureConverter *c, Scope *local,
                                   Scope *exterior, AST_Node *n);
AST_Node *build_closure_environment_struct(ClosureConverter *conv, Scope *parent,
                             AST_Node *node);

Context *context_push(Context *r, AST_Node *n) {
  assert(n->kind == AST_BIND_TYPE || n->kind == AST_BIND_CONST ||
         n->kind == AST_BIND_VARIABLE);

  Context *c = (Context *)malloc(sizeof(Context));

  c->prev = r;
  c->decl = n->id;
  c->next = NULL;

  if (r) {
    r->next = c;
  }

  return c;
}

Context *context_pop(Context *r) {
  Context *p = r->prev;

  free(r);

  return p;
}

AST_Node *context_find(Context *env, Parser *p, AST_Node *sym) {
  AST_Manager *m = &p->ast_man;

  if (env == NULL)
    return ast_node_null(m);

  AST_Node *node = ast_manager_get(m, env->decl);

  assert(node->kind == AST_BIND_TYPE || node->kind == AST_BIND_CONST ||
         node->kind == AST_BIND_VARIABLE);

  AST_Node *root = node;

  if (root->kind == AST_BIND_CONST || root->kind == AST_BIND_VARIABLE) {
    root = ast_bind_get_type_bind(m, root);
  }

  AST_Node *symb = ast_type_bind_get_symbol(m, root);

  if (parser_is_same_symbol(p, sym, symb)) {
    return node;
  }

  return context_find(env->prev, p, sym);
}

AST_Node *context_to_struct(Context *env, Parser *p) {
  AST_Manager *m = &p->ast_man;
  AST_Node *members = ast_node_null(m);

  while (env) {
    AST_Node *decl = ast_manager_get(m, env->decl);

    members = ast_struct_member(m, lexer_undef_token(), decl, members);

    env = env->prev;
  }

  return ast_type_struct(m, lexer_undef_token(), members);
}

void scope_init(Scope *scope, Scope *parent) {
  scope->ctx = NULL;
  scope->parent = parent;
}

void scope_print(Scope *s, Parser *p) {
  if (s == NULL)
    return;

  printf("{ ");

  Context *ctx = s->ctx;

  while (ctx) {
    AST_Node *n = ast_manager_get(&p->ast_man, ctx->decl);

    AST_Node *s = ast_type_bind_get_symbol(&p->ast_man, n);

		i8 buff[256];
		
		if(ast_is_temporary(&p->ast_man, s)) {
			u64 t = s->kind;
			u64 i = 1;
			buff[0] = '%';
			while(t) {
				buff[i++] = t % 10 + '0';
				t = t / 10;
			}
			buff[i] = '\0';
		} else {
			token_get_id(&p->lex, s->tok, buff);
		}
		printf("%s", buff);

    ctx = ctx->prev;
    if (ctx)
      printf(", ");
  }

  printf(" }");

  scope_print(s->parent, p);
}

Scope *scope_create(Scope *parent) {
  Scope *s = (Scope *)malloc(sizeof(Scope));
  scope_init(s, parent);
  return s;
}

AST_Node *scope_find(Scope *s, Parser *p, AST_Node *sym) {
  AST_Manager *m = &p->ast_man;
  if (s == NULL)
    return ast_node_null(m);

  AST_Node *n = context_find(s->ctx, p, sym);

  if (!ast_is_null_node(n))
    return n;

  return scope_find(s->parent, p, sym);
}

AST_Node *scope_find_local(Scope *s, Parser *p, AST_Node *sym) {
  AST_Manager *m = &p->ast_man;
  if (s == NULL) {
    return ast_node_null(m);
  }

  return context_find(s->ctx, p, sym);
}

void scope_push(Scope *s, AST_Node *n) {
  assert(n->kind == AST_TYPE_BIND || n->kind == AST_BIND_CONST ||
         n->kind == AST_MUT_BIND);
  s->ctx = context_push(s->ctx, n);
}

void scope_pop(Scope *s) { s->ctx = context_pop(s->ctx); }

b8 scope_is_global(Scope *s) { return s->parent == NULL; }

void converter_init(ClosureConverter *c, Parser *p) {
  c->temporaries = AST_KIND_END + 1;
  c->parser = p;

  c->closures = (Scope *)malloc(sizeof(Scope));

  scope_init(c->closures, NULL);

  AST_Node *root = ast_manager_get(&p->ast_man, 1);

  while (root->right) {
    root = ast_manager_get_relative(&p->ast_man, root, root->right);
  }

  c->prog_tail = root->id;

  // c->fun_decls = 0;
}

u64 converter_get_temporary(ClosureConverter *c) {
  c->temporaries += 1;
  return c->temporaries;
}

void decl_list_to_scope_rec(Parser *p, AST_Node *decl_list, Scope *local) {
  if (ast_is_null_node(decl_list))
    return;

  assert(decl_list->kind == AST_DECL_ARGS_LIST);

  AST_Manager *m = &p->ast_man;

  AST_Node *elem = ast_decl_list_get_elem(m, decl_list);
  AST_Node *tail = ast_decl_list_get_tail(m, decl_list);

	assert(elem->kind == AST_BIND_TYPE || ast_is_null_node(elem));

  if (!ast_is_null_node(elem)) {
    scope_push(local, elem);
  }

  return decl_list_to_scope_rec(p, tail, local);
}

Scope *decl_list_to_scope(Parser *p, Scope *parent, AST_Node *decl_list) {
  Scope *s = (Scope *)malloc(sizeof(Scope));

  scope_init(s, parent);
	printf("******\n");
	scope_print(s, p);
	printf("\n");
	

	print_ast(p, decl_list);
	
  decl_list_to_scope_rec(p, decl_list, s);

	scope_print(s, p);
	printf("\n");
	
  return s;
}

Scope *build_func_decl_scope(ClosureConverter *conv, Scope *parent,
                             AST_Node *decl) {
  assert(decl->kind == AST_FUNCTION_LITERAL);

  AST_Manager *m = &conv->parser->ast_man;

  AST_Node *sign = ast_function_literal_get_signature(m, decl);
  AST_Node *args = ast_function_signature_get_args(m, sign);

  return decl_list_to_scope(conv->parser, parent, args);
}

AST_Node *convert_anonymous_closure(ClosureConverter *conv, Scope *local,
                                    AST_Node *func) {
  AST_Manager *m = &conv->parser->ast_man;

  AST_Node *cenv = build_closure_environment_struct(conv, local, func);

	if (scope_is_global(local)) {
    return ast_node_null(m);
  }
	
	AST_Node* etmp = ast_temp_node(m);
	// TODO: use pointer
	AST_Node* ebnd = ast_bind_type(m, lexer_undef_token(), etmp, cenv);
	AST_Node *earg = ast_function_literal_push_argument(m, lexer_undef_token(), func, ebnd);

  AST_Node *type = get_type_from_fun_decl(m, func);

  AST_Node *temp = ast_temp_node(m);
  // AST_Node *type = ast_type_closure(m, lexer_undef_token(), ty);
  AST_Node *bind = ast_bind_type(m, lexer_undef_token(), temp, type);

  // AST_Node *clos = ast_closure(m, lexer_undef_token(), fun, cenv);
  AST_Node *decl = ast_bind_constant(m, lexer_undef_token(), bind, func);

  ast_manager_push_decl(m, decl);

  scope_push(conv->closures, decl);

  return bind;
}

// AST_Node *convert_named_closure(ClosureConverter *conv, Scope *local,
//                                 AST_Node *l, AST_Node *r) {
//   AST_Manager *m = &conv->parser->ast_man;
//   AST_Node *cenv = closure_conversion(conv, local, r);

//   // if (scope_is_global(local)) {
//   //   return ast_node_null(m);
//   // }

//   AST_Node *ty = ast_type_bind_get_type(m, l);

//   AST_Node *temp = ast_temp_node(m);
//   AST_Node *type = ast_type_closure(m, lexer_undef_token(), ty);
//   AST_Node *bind = ast_type_bind(m, lexer_undef_token(), temp, type);

//   AST_Node *clos = ast_closure(m, lexer_undef_token(), r, cenv);
//   AST_Node *decl = ast_const_bind(m, lexer_undef_token(), bind, clos);
//   AST_Node *closure = ast_manager_push_decl(m, decl);

//   scope_push(conv->closures, decl);

//   return bind;
// }

AST_Node *fill_local_scope_expr(ClosureConverter *conv, Scope *local,
                                Scope *exterior, AST_Node *n) {
  AST_Manager *m = &conv->parser->ast_man;

  if (ast_is_null_node(n))
    return ast_node_null(m);

  if (n->kind == AST_I32_LITERAL || n->kind == AST_TYPE_BIND ||
      n->kind == AST_CLOS_DECL) {
    return ast_node_null(m);
  }

  if (n->kind == AST_HND_DECL) {
    AST_Node *effects = ast_manager_get_relative(m, n, n->right);

    while (!ast_is_null_node(effects)) {
      AST_Node *effect = ast_manager_get_relative(m, effects, effects->left);

      fill_local_scope_expr(conv, local, exterior, effect);

      effects = ast_manager_get_relative(m, effects, effects->left);
    }

    return ast_node_null(m);
  }

  if (n->kind == AST_SYMBOL_LITERAL) {

		printf("========\n");
		scope_print(local, conv->parser);
		printf("\n");
		
    AST_Node *decl = scope_find_local(local, conv->parser, n);

    if (ast_is_null_node(decl)) {
			// search environment
      AST_Node *extn = scope_find(local, conv->parser, n);

      if (ast_is_null_node(extn)) {
        i8 buff[512];
        i8 symb[512];

        token_get_id(&conv->parser->lex, n->tok, symb);

        sprintf(buff,
                "INTERNAL: Couldn't find '%s' definition on current scope",
                symb);

        parser_error(conv->parser, n->tok, buff);
      }
		 
      scope_push(exterior, extn);
    }

    return ast_node_null(m);
  }

  if (n->kind == AST_BIND_CONST || n->kind == AST_MUT_BIND) {
    AST_Node *l = ast_manager_get_relative(m, n, n->left);
    AST_Node *r = ast_manager_get_relative(m, n, n->right);

    scope_push(local, l);

    AST_Node *s = fill_local_scope_expr(conv, local, exterior, r);

    if (!ast_is_null_node(s)) {
      n->right = s->id;
    }

    return ast_node_null(m);
  }

  if (ast_is_binary_operation(n) || n->kind == AST_CTRL_FLOW_IF) {
    AST_Node *l = ast_manager_get_relative(m, n, n->left);
    AST_Node *r = ast_manager_get_relative(m, n, n->right);

    fill_local_scope_expr(conv, local, exterior, l);
    fill_local_scope_expr(conv, local, exterior, r);

    return ast_node_null(m);
  }

  if (n->kind == AST_CTRL_FLOW_RETURN) {
    AST_Node *expr = ast_manager_get_relative(m, n, n->left);

    if (ast_is_null_node(expr)) {
      return ast_node_null(m);
    }

    if (expr->kind == AST_FUNCTION_LITERAL) {
      AST_Node *bind = convert_anonymous_closure(conv, local, expr);

      // TODO: update function return type
      n->left = ast_type_bind_get_symbol(m, bind)->id;

      return ast_node_null(m);
    }

    if (expr->kind == AST_SYMBOL_LITERAL) {
      // TODO: convert to return a closure if symbol is function

      return ast_node_null(m);
    }

    return fill_local_scope_expr(conv, local, exterior, expr);
  }

  if (n->kind == AST_FUNCTION_LITERAL) {
    AST_Node *bind = convert_anonymous_closure(conv, local, n);
		
    if (ast_is_null_node(bind)) {
      return ast_node_null(m);
    }
		
    return ast_type_bind_get_symbol(m, bind);
  }

  if (n->kind == AST_FUNCTION_CALL) {
    // TODO
    // is call to closure ??
    return ast_node_null(m);
  }

  if (n->kind == AST_EFFECT_CALL) {
    // TODO
    return ast_node_null(m);
  }

  if (n->kind == AST_WITH_HND) {
    // TODO
    return ast_node_null(m);
  }

  i8 buff[512];

  sprintf(buff, "INTERNAL: Uncaught kind '%s' on fill_local_scope_expr",
          ast_kind_to_cstr(n->kind));
  parser_error(conv->parser, n->tok, buff);

  return ast_node_null(m);
}

void fill_local_scope_statment_list(ClosureConverter *conv, Scope *local,
                                    Scope *exterior, AST_Node *node) {
  if (ast_is_null_node(node))
    return;

  assert(node->kind == AST_PROGRAM_POINT);

  AST_Manager *m = &conv->parser->ast_man;

  AST_Node *statement = ast_program_point_get_decl(m, node);

  fill_local_scope_expr(conv, local, exterior, statement);

  AST_Node *tail = ast_program_point_get_tail(m, node);

  fill_local_scope_statment_list(conv, local, exterior, tail);
}

AST_Node *build_closure_environment_struct(ClosureConverter *conv, Scope *parent,
                             AST_Node *node) {
  AST_Manager *m = &conv->parser->ast_man;
  assert(node->kind == AST_FUNCTION_LITERAL);

  Scope *local = build_func_decl_scope(conv, parent, node);

  printf("%%%%%%%%%%%%%%\n");
  scope_print(local, conv->parser);
  printf("\n");

  Scope *exterior = scope_create(NULL);

  AST_Node *body = ast_function_literal_get_body(m, node);

  fill_local_scope_statment_list(conv, local, exterior, body);

  return context_to_struct(exterior->ctx, conv->parser);
}

void closure_convert_statment_list(ClosureConverter *c, Scope *local,
                                   Scope *exterior, AST_Node *n) {
  if (ast_is_null_node(n))
    return;

  assert(n->kind == AST_PROGRAM_POINT);

  AST_Manager *m = &c->parser->ast_man;

  AST_Node *l = ast_program_point_get_decl(m, n);

  fill_local_scope_expr(c, local, exterior, l);

  AST_Node *r = ast_program_point_get_tail(m, n);

  closure_convert_statment_list(c, local, exterior, r);
}

void closure_convert(Parser *p, AST_Node *n) {
  ClosureConverter c;

  converter_init(&c, p);

  Scope *global = scope_create(NULL);
  Scope *exterior = scope_create(NULL);

  closure_convert_statment_list(&c, global, exterior, n);
}
