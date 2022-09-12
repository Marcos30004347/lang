#pragma once

#include "lexer.hpp"
#include "types.hpp"

namespace ast {

// Unique id of each node
typedef u32 Id;

// Data Store for nodes
typedef struct Bucket Bucket;

#define BUCKET_SIZE 128

struct Node {
  Id id;

  u64 kind;

  // TODO(marcos): this node can use less bytes
  // if we take this token away and just store
  // a reference to it here, like a TokenId ref
  // as we do for the AST_Node childs.
  Token tok;

  Id left;
  Id right;

  Bucket* bucket;
};

struct Bucket {
  u64     id;
  Node    data[BUCKET_SIZE];
  Bucket* prev;
  Bucket* next;
};

struct Manager {
  u64 size;
  u64 temp;

  Id statements_list_root_id;
  Id statements_list_tail_id;

  Node* statement_list_tail_ptr;

  Bucket* root;
  Bucket* tail;
};

Node* manager_push_decl(Manager* m, Node* decl);

void manager_init(Manager* m);
void manager_free(Manager* m);

Node* manager_get(Manager* m, Id id);
Node* manager_get_relative(Manager* m, Node* from, Id id);
Node* manager_alloc(Manager* m, Token tok, u64 kind, Id l, Id r);

} // namespace ast
