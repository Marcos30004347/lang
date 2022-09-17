#pragma once

#include "types.hpp"

namespace ast {

// Unique id of each node
typedef u32 Id;

// Data Store for nodes
typedef struct Bucket Bucket;

#define AST_BUCKET_SIZE 128

struct Node {
  Id id;

  u64 kind;

  Id left;
  Id right;

  Bucket* bucket;
};

struct Bucket {
  u64     id;
  Node    data[AST_BUCKET_SIZE];
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

Manager* manager_create();

void manager_destroy(Manager* m);

Node* manager_get(Manager* m, Id id);

Node* manager_get_relative(Manager* m, Node* from, Id id);

Node* manager_alloc(Manager* m, u32 kind, Id l, Id r);

Node* left_of(Manager* m, Node* n);

Node* right_of(Manager* m, Node* n);

Id left_id_of(Manager* m, Node* n);

Id right_id_of(Manager* m, Node* n);

Node* deep_copy(Manager* m, Node* n);

Node* manager_push_decl(Manager* m, Node* decl);

template < typename A > A is_instance(Node* node);

template < typename A > A as(Node* node) {
  return (A)node;
}

// b8 is_temporary(ast::Node* node);

} // namespace ast
