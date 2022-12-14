#include "ast_manager.hpp"
#include "ast/ast_literals.hpp"
#include "ast_kind.hpp"
#include "compiler/compiler.hpp"
#include "compiler/symbol_table.hpp"
#include "../utils.hpp"

#include "assert.h"
#include "stdlib.h"
#include <iterator>

using namespace compiler;

namespace ast {

Manager* manager_create() {
  Manager* m = new Manager();

  m->size = 1;
  m->root = new Bucket();

  m->statements_list_root_id = 0;
  m->statements_list_tail_id = 0;
  m->statement_list_tail_ptr = &m->root->data[0];

  m->temp = __AST_KIND_END + 1;

  m->root->id   = 0;
  m->root->next = 0;
  m->root->prev = 0;

  m->tail = m->root;

  m->symbol_table = symbol::symbol_table_create();

  m->root->data[0].id     = 0;
  m->root->data[0].kind   = AST_NULL_NODE;
  m->root->data[0].left   = 0;
  m->root->data[0].right  = 0;
  m->root->data[0].bucket = m->root;
  // m->root->data[0].symbol = symbol::empty(compiler->parser->symbol_table);

  return m;
}

void manager_destroy(Manager* m) {

  while (m->root) {
    Bucket* tmp = m->root;
    m->root     = m->root->next;
    delete tmp;
  }

  symbol::symbol_table_destroy(m->symbol_table);

  delete m;
}

void manager_pop(Manager* m, Node* n) {
  assert(m->size - 1 == n->id);
  m->size -= 1;
}

void ast_init(Bucket* b, Node* a, u64 kind, Id id, Id l = 0, Id r = 0) {
  a->left  = l;
  a->right = r;
  a->id    = id;
  a->kind  = kind;
  // a->symbol = symbol;
  a->bucket = b;
}

Id manager_reserve(Manager* m, u32 kind, Id l, Id r) {
  Id id = m->size;

  if (id % AST_BUCKET_SIZE == 0) {
    m->tail->next       = new Bucket();
    m->tail->next->next = 0;
    m->tail->next->prev = m->tail;
    m->tail->next->id   = m->tail->id + 1;

    m->tail = m->tail->next;
  }

  ast_init(m->tail, &m->tail->data[id % AST_BUCKET_SIZE], kind, id, l, r);

  m->size = m->size + 1;

  return id;
}

Node* manager_alloc(Manager* m, u32 kind, Id l, Id r) {
  return manager_get(m, manager_reserve(m, kind, l, r));
}

Node* manager_get(Manager* m, Id id) {
  Bucket* b = m->root;

  for (u64 i = 0; i < id / AST_BUCKET_SIZE; i++) {
    b = b->next;
  }

  return &b->data[id % AST_BUCKET_SIZE];
}

Node* manager_get_relative(Manager* m, Node* root, Id child_id) {
  assert(root);
  Bucket* b = root->bucket;

  u32 w = round_down(root->id, AST_BUCKET_SIZE);
  u32 s = round_down(child_id, AST_BUCKET_SIZE);

  while (s > w) {
    w += AST_BUCKET_SIZE;
    b = b->next;
  }

  while (w > s) {
    w -= AST_BUCKET_SIZE;
    b = b->prev;
  }

  return &b->data[child_id % AST_BUCKET_SIZE];
}

Node* ast_manager_alloc(Manager* m, u32 kind, Id l, Id r) {
  return manager_get(m, manager_reserve(m, kind, l, r));
}

Node* left_of(Manager* m, Node* n) {
  if (n->kind == AST_SYMBOL_LITERAL || n->kind == AST_NATURAL_LITERAL) {
    return ast::create_node_literal_nothing(m);
  }
  return manager_get_relative(m, n, n->left);
}

Node* right_of(Manager* m, Node* n) {
  return manager_get_relative(m, n, n->right);
}

Id left_id_of(Manager* m, Node* n) {
  return n->left;
}

Id right_id_of(Manager* m, Node* n) {
  return n->right;
}

Node* deep_copy(Manager* m, Node* node) {
  if (node->kind == AST_NULL_NODE)
    return &m->root->data[0];

  if (node->kind == AST_SYMBOL_LITERAL || node->kind == AST_NATURAL_LITERAL) {
    return manager_alloc(m, node->kind, node->left, node->right);
  }

  Node* l = deep_copy(m, manager_get_relative(m, node, node->left));
  Node* r = deep_copy(m, manager_get_relative(m, node, node->right));

  return manager_alloc(m, node->kind, l->id, r->id);
}

Node* deep_copy_from(Manager* m, Manager* b, Node* node) {
  if (node->kind == AST_NULL_NODE) {
    return &m->root->data[0];
  }

  Node* l = deep_copy_from(m, b, manager_get_relative(b, node, node->left));
  Node* r = deep_copy_from(m, b, manager_get_relative(b, node, node->right));

  Node* c = manager_alloc(m, node->kind, l->id, r->id);

  return c;
}

Node* manager_push_decl(Manager* m, Node* decl) {
  Node* root = ast::manager_alloc(m, AST_PROGRAM_POINT, decl->id, 0);

  if (m->statements_list_root_id == 0) {
    m->statements_list_root_id = root->id;
    m->statements_list_tail_id = root->id;

    m->statement_list_tail_ptr = root;

    return root;
  }

  m->statement_list_tail_ptr->right = root->id;
  m->statement_list_tail_ptr        = root;

  return root;
}

void change_kind(Node* node, AST_Kind kind) {
  node->kind = kind;
}

void set_left(Manager* m, Node* l, Node* r) {
  l->left = get_id(r);
}

void set_right(Manager* m, Node* l, Node* r) {
  l->right = get_id(r);
}

void replace(Node* A, Node* B) {
  A->kind   = B->kind;
  A->left   = B->left;
  A->right  = B->right;
  A->bucket = B->bucket;
}

u32 get_id(Node* node) {
  return node ? node->id : 0;
}

// b8 is_temporary(ast::Node* node) {
//   return node->kind > __AST_KIND_END;
// }
} // namespace ast
