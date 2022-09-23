#pragma once

#include "ast/ast_kind.hpp"
#include "compiler/symbol_table.hpp"
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

  compiler::symbol::Symbol_Table* symbol_table;

  Id statements_list_root_id;
  Id statements_list_tail_id;

  Node* statement_list_tail_ptr;

  Bucket* root;
  Bucket* tail;
};

Manager* manager_create();

void manager_destroy(Manager* m);

void manager_pop(Manager* m, Node* n);

Node* manager_get(Manager* m, Id id);

Node* manager_get_relative(Manager* m, Node* from, Id id);

Node* manager_alloc(Manager* m, u32 kind, Id l, Id r);

Node* left_of(Manager* m, Node* n);

Node* right_of(Manager* m, Node* n);

void set_left(Manager* m, Node* l, Node* r);
void set_right(Manager* m, Node* l, Node* r);

Id left_id_of(Manager* m, Node* n);

Id right_id_of(Manager* m, Node* n);

Node* deep_copy(Manager* m, Node* n);
Node* deep_copy_from(Manager* to, Manager* from, Node* n);

Node* manager_push_decl(Manager* m, Node* decl);

template < typename A > A is_instance(Node* node);

template < typename A > A as(Node* node) {
  return (A)node;
}

void change_kind(Node* node, AST_Kind kind);

void replace(Node* A, Node* B);

u32 get_id(Node* node);
// b8 is_temporary(ast::Node* node);

} // namespace ast
