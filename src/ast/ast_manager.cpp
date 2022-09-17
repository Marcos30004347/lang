#include "ast_manager.hpp"
#include "ast_kind.hpp"
#include "compiler/compiler.hpp"
#include "compiler/symbol_table.hpp"
#include "utils.hpp"

#include "assert.h"
#include "stdlib.h"

using namespace compiler;

namespace ast {

Manager* manager_create() {
  Manager* m = new Manager();

  m->size = 1;
  m->root = (Bucket*)malloc(sizeof(Bucket));

  m->statements_list_root_id = 0;
  m->statements_list_tail_id = 0;
  m->statement_list_tail_ptr = &m->root->data[0];

  m->temp = __AST_KIND_END + 1;

  m->root->id   = 0;
  m->root->next = 0;
  m->root->prev = 0;

  m->tail = m->root;

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

  delete m;
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

  if (id % BUCKET_SIZE == 0) {
    m->tail->next       = (Bucket*)malloc(sizeof(Bucket));
    m->tail->next->prev = m->tail;
    m->tail->next->id   = m->tail->id + 1;
    m->tail             = m->tail->next;
  }

  ast_init(m->tail, &m->tail->data[id % BUCKET_SIZE], kind, id, l, r);

  m->size = m->size + 1;

  return id;
}

Node* manager_alloc(Manager* m, u32 kind, Id l, Id r) {
  return manager_get(m, manager_reserve(m, kind, l, r));
}

Node* manager_get(Manager* m, Id id) {
  Bucket* b = m->root;

  for (u64 i = 0; i < id / BUCKET_SIZE; i++) {
    b = b->next;
  }

  return &b->data[id % BUCKET_SIZE];
}

Node* manager_get_relative(Manager* m, Node* root, Id child_id) {
  assert(root);
  Bucket* b = root->bucket;

  u32 w = round_down(root->id, BUCKET_SIZE);
  u32 s = round_down(child_id, BUCKET_SIZE);

  while (s > w) {
    w += BUCKET_SIZE;
    b = b->next;
  }

  while (w > s) {
    w -= BUCKET_SIZE;
    b = b->prev;
  }

  return &b->data[child_id % BUCKET_SIZE];
}

Node* ast_manager_alloc(Manager* m, u32 kind, Id l, Id r) {
  return manager_get(m, manager_reserve(m, kind, l, r));
}

Node* left_of(Manager* m, Node* n) {
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

  Node* l = deep_copy(m, manager_get_relative(m, node, node->left));
  Node* r = deep_copy(m, manager_get_relative(m, node, node->right));
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

b8 is_temporary(ast::Node* node) {
  return node->kind > __AST_KIND_END;
}
} // namespace ast
