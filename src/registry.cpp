#include "registry.hpp"

#include <cstddef>
#include <cstring>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "token.hpp"
#include "types.hpp"
#include "utils.hpp"
#include "crc64.hpp"

#include "ast.hpp"
#include "parser.hpp"

struct sym_node {
	// unique id of this sym_node
	u64 id;

	// the crc64 key of this sym_node
	u64 key;

	sym_node *left;
  sym_node *right;

  i64 height;

	b8 is_sym;
	
	// TODO: when we start to use a flat array for
	// storing ast sym_nodes, this refrence should be the
	// u64 id of the ast sym_node.
	// reference to a ast sym_node, it shouldn't be freed
	ast* ast;
};

int max(int a, int b) { return (a > b) ? a : b; }

int height(sym_node *N) {
  if (N == 0) {
    return 0;
	}

  return 1 + max(height(N->left), height(N->right));
}

sym_node *newsym_node(ast* ast, u64 id) {
  sym_node *n = (sym_node*)malloc(sizeof(sym_node));

	n->key = ast->tok.buf;
  n->left = 0;
  n->right = 0;
  n->height = 0;
	n->id = id;
	n->is_sym = ast->kind == AST_SYM_DECL;
	n->ast = ast;

	return n;
}

sym_node *rightRotate(sym_node *y) {
  sym_node *x = y->left;
  sym_node *T2 = x->right;

  x->right = y;
  y->left = T2;

  y->height = height(y);
  x->height = height(x);

  return x;
}

sym_node *leftRotate(sym_node *x) {
  sym_node *y = x->right;
  sym_node *T2 = y->left;

  y->left = x;
  x->right = T2;

  x->height = height(x);
  y->height = height(y);

  return y;
}

int getBalance(sym_node *N) {
  if (N == 0)
    return 0;
  return height(N->left) - height(N->right);
}

i32 compare_symbols(parser* p, ast* s, ast* h) {
	if(s->tok.size != h->tok.size) {
		return s->tok.size - h->tok.size;
	}

	const i8* c = lexer_get_token_file_buff_ptr(&p->lex, s->tok);
	const i8* d = lexer_get_token_file_buff_ptr(&p->lex, h->tok);
	
	for(u64 i = 0; i < s->tok.size; i++) {
		if(c[i] != d[i]) {
			return c[i] - d[i];
		}
	}

	return 0;
}

sym_node *insert_symbol(sym_node *n, parser* p, ast* sym, u64 *id, b8* inserted) {
  if (n == 0) {
		*inserted = true;
    return newsym_node(sym, *id);
	}
	
  if (sym->tok.buf < n->key) {
    n->left = insert_symbol(n->left, p, sym, id, inserted);
	}
  else if (sym->tok.buf > n->key) {
    n->right = insert_symbol(n->right, p, sym, id, inserted);
	}
  else {
		u64 r = compare_symbols(p, sym, n->ast);
		
		if(r != 0) {
			n->right = insert_symbol(n->left, p, sym, id, inserted);
		}
 
		*inserted = false;
		*id = n->id;
		
    return n;
	}
	
  n->height = height(n);

  int balance = getBalance(n);

  if (balance > 1 && sym->tok.buf < n->left->key)
    return rightRotate(n);

  if (balance < -1 && sym->tok.buf > n->right->key)
    return leftRotate(n);

  if (balance > 1 && sym->tok.buf > n->left->key) {
    n->left = leftRotate(n->left);
    return rightRotate(n);
  }

  if (balance < -1 && sym->tok.buf < n->right->key) {
    n->right = rightRotate(n->right);
    return leftRotate(n);
  }

  return n;
}


ast* find_alike_ast(sym_node* root, parser* p, ast* a) {
	if (root == 0) {
		// TODO: if we ise indexes on linear arrays,
		// this value should change to -1
		return 0;
	}

	u64 key = a->tok.buf;
	
	if(key > root->key) {
		return find_alike_ast(root->left, p, a);
	}

	if(key < root->key) {
		return find_alike_ast(root->right, p, a);
	}

	u64 r = compare_symbols(p, a, root->ast);

	if(r != 0) {
		return find_alike_ast(root->left, p, a);
	}
 
	return root->ast;
}

u64 find_alike_id(sym_node* root, parser* p, ast* a) {
	if (root == 0) {
		// TODO: if we ise indexes on linear arrays,
		// this value should change to -1
		return REGISTRY_NOT_FOUND;
	}

	u64 key = a->tok.buf;
	
	if(key > root->key) {
		return find_alike_id(root->left, p, a);
	}

	if(key < root->key) {
		return find_alike_id(root->right, p, a);
	}

	u64 r = compare_symbols(p, a, root->ast);

	if(r != 0) {
		return find_alike_id(root->left, p, a);
	}
 
	return root->id;
}


void sym_node_destroy(sym_node* n) {
	if(n == 0) return;

	sym_node_destroy(n->left);
	sym_node_destroy(n->right);

	free(n);
}

void registry_init(registry* a) {
	a->sym_reg_root = 0;
	a->count = 1;
}

void registry_destroy(registry* a) {
	sym_node_destroy(a->sym_reg_root);
}

u64 registry_insert_symbol(registry* t, parser* p, ast* sym) {
	assert(sym->kind == AST_SYM_DECL);

	b8 inserted = false;

	u64 id = t->count + 1;

	t->sym_reg_root = insert_symbol(t->sym_reg_root, p, sym, &id, &inserted);

	if(inserted == true) {
		t->count += 1;
	}

	return id;
}

ast* registry_find(registry* t, parser* p, ast* sym) {
	assert(sym->kind == AST_SYM_DECL);
	
	return find_alike_ast(t->sym_reg_root, p, sym);
}

u64 registry_find_id(registry* t, parser* p, ast* sym) {
	assert(sym->kind == AST_SYM_DECL);
	
	return find_alike_id(t->sym_reg_root, p, sym);
}
