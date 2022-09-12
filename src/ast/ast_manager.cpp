#include "ast_manager.hpp"
#include "utils.hpp"

#include "assert.h"
#include "stdlib.h"

namespace ast {

void ast_init(Bucket* b, Node* a, Token tok, u64 kind, Id id, Id l = 0, Id r = 0) {
  a->left   = l;
  a->right  = r;
  a->id     = id;
  a->kind   = kind;
  a->tok    = tok;
  a->bucket = b;
}

Id manager_reserve(Manager* m, Token tok, u64 kind, Id l, Id r) {
  Id id = m->size;

  if (id % BUCKET_SIZE == 0) {
    m->tail->next       = (Bucket*)malloc(sizeof(Bucket));
    m->tail->next->prev = m->tail;
    m->tail->next->id   = m->tail->id + 1;
    m->tail             = m->tail->next;
  }

  ast_init(m->tail, &m->tail->data[id % BUCKET_SIZE], tok, kind, id, l, r);

  m->size = m->size + 1;

  return id;
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

Node* ast_manager_alloc(Manager* m, Token tok, u64 kind, Id l, Id r) {
  return manager_get(m, manager_reserve(m, tok, kind, l, r));
}

} // namespace ast
