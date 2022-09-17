#include "symbol_table.hpp"
#include "crc64.hpp"

#include "lexer.hpp"
#include "stdlib.h"
#include <assert.h>
#include <cstring>
#include <strings.h>

namespace compiler {

namespace symbol {

int max(int a, int b) {
  return (a > b) ? a : b;
}

int height(Id_To_CRC64_Map_Node* N) {
  if (N == 0) {
    return 0;
  }

  return N->height;
}

Id_To_CRC64_Map_Node* create_id_to_crc64_map_node(Id key, u64 crc) {
  Id_To_CRC64_Map_Node* node = new Id_To_CRC64_Map_Node();
  node->key                  = key;
  node->left                 = NULL;
  node->right                = NULL;
  node->height               = 1;
  node->crc                  = crc;
  return (node);
}

Id_To_CRC64_Map_Node* right_rotate(Id_To_CRC64_Map_Node* y) {
  Id_To_CRC64_Map_Node* x  = y->left;
  Id_To_CRC64_Map_Node* T2 = x->right;

  x->right = y;
  y->left  = T2;

  y->height = max(height(y->left), height(y->right)) + 1;
  x->height = max(height(x->left), height(x->right)) + 1;

  return x;
}

Id_To_CRC64_Map_Node* left_rotate(Id_To_CRC64_Map_Node* x) {
  Id_To_CRC64_Map_Node* y  = x->right;
  Id_To_CRC64_Map_Node* T2 = y->left;

  y->left  = x;
  x->right = T2;

  x->height = max(height(x->left), height(x->right)) + 1;
  y->height = max(height(y->left), height(y->right)) + 1;

  return y;
}

int getBalance(Id_To_CRC64_Map_Node* N) {
  if (N == NULL)
    return 0;
  return height(N->left) - height(N->right);
}

Id_To_CRC64_Map_Node* insert(Id_To_CRC64_Map_Node* node, Id key, u64 crc) {
  if (node == NULL)
    return (create_id_to_crc64_map_node(key, crc));

  if (key < node->key) {
    node->left = insert(node->left, key, crc);
  } else if (key > node->key) {
    node->right = insert(node->right, key, crc);
  } else {
    return node;
  }
  node->height = 1 + max(height(node->left), height(node->right));

  int balance = getBalance(node);

  if (balance > 1 && key < node->left->key) {
    return right_rotate(node);
  }
  if (balance < -1 && key > node->right->key) {
    return left_rotate(node);
  }

  if (balance > 1 && key > node->left->key) {
    node->left = left_rotate(node->left);
    return right_rotate(node);
  }

  if (balance < -1 && key < node->right->key) {
    node->right = right_rotate(node->right);
    return left_rotate(node);
  }

  return node;
}

Id_To_CRC64_Map_Node* search(struct Id_To_CRC64_Map_Node* root, Id key) {
  if (root == NULL || root->key == key) {
    return root;
  }

  if (root->key < key) {
    return search(root->right, key);
  }
  return search(root->left, key);
}

Manager* manager_create() {
  Manager* m = new Manager();

  m->used_chars_count      = 0;
  m->allocated_chars_count = BUCKET_SIZE;
  m->bucket_first          = new Bucket();

  m->bucket_first->id   = 0;
  m->bucket_first->next = 0;
  m->bucket_first->prev = 0;

  m->bucket_last = m->bucket_first;

  return m;
}

void manager_destroy(Manager* m) {
  while (m->bucket_first) {
    Bucket* tmp     = m->bucket_first;
    m->bucket_first = m->bucket_first->next;
    free(tmp);
  }

  delete m;
}

Symbol manager_alloc_symbol(Manager* m, const i8* c_str, u64 n, u64 crc) {
  u64     id = m->used_chars_count;
  Bucket* b  = m->bucket_last;

  for (u64 i = 0; i < n + 1; i++) {
    if (m->used_chars_count + i == m->allocated_chars_count) {
      m->bucket_last->next = new Bucket();
      m->bucket_last       = m->bucket_last->next;
      m->bucket_last->id   = m->allocated_chars_count;
      m->allocated_chars_count += BUCKET_SIZE;
    }

    m->bucket_last->buffer[id + i % BUCKET_SIZE] = c_str[i];

    m->used_chars_count += 1;
  }

  Symbol s = {.id = id, .size = n, .crc64 = crc, .bucket = b};

  return s;
}

i8 char_at(Symbol* it, u64 i) {
  assert(i < it->size);

  Bucket* b = it->bucket;

  for (u64 j = 0; j< it->id + i > b->id + BUCKET_SIZE; j++) {
    b = b->next;
  }

  return it->bucket->buffer[it->id + i % BUCKET_SIZE];
}

b8 is_equal(Manager* m, Symbol* a, const i8* b, u64 n) {
  if (a->size != n)
    return false;

  u64 crc = crc64(b, n);

  if (a->crc64 != crc)
    return false;

  for (u64 i = 0; i < a->size; i++) {
    if (char_at(a, i) != b[i]) {
      return false;
    }
  }

  return true;
}

b8 is_equal(Manager* m, Symbol* a, const i8* b) {
  u64 n = strlen(b);

  return is_equal(m, a, b, n);
}

b8 is_equal(Manager* m, Symbol* a, Symbol* b) {
  if (a->crc64 != b->crc64 || a->size != b->size)
    return false;

  for (u64 i = 0; i < a->size; i++) {
    if (char_at(a, i) - char_at(b, i) != 0) {
      return false;
    }
  }

  return true;
}

Symbol get_entry(Symbol_Table* table, const i8* c_str, u64 n) {
  u64                id   = crc64(c_str, n);
  Symbol_Table_Node* node = table->table[id % TABLE_SIZE];

  while (node) {
    if (is_equal(table->manager, &node->symbol, c_str, n)) {
      return node->symbol;
    }

    node = node->prev;
  }

  return empty(table);
}

Symbol get_entry(Symbol_Table* table, const i8* c_str) {
  return get_entry(table, c_str, strlen(c_str));
}

b8 exist_entry(Symbol_Table* table, const i8* c_str, u64 n) {
  u64                id   = crc64(c_str, n);
  Symbol_Table_Node* node = table->table[id % TABLE_SIZE];

  while (node) {
    if (is_equal(table->manager, &node->symbol, c_str, n)) {
      return true;
    }

    node = node->prev;
  }

  return false;
}

b8 exist_entry(Symbol_Table* table, const i8* c_str) {
  return exist_entry(table, c_str, strlen(c_str));
}

Symbol set_entry(Symbol_Table* table, const i8* str, u64 n) {
  Symbol symbol = get_entry(table, str, n);

  if (symbol.id != 0) {
    return symbol;
  }

  u64 crc = crc64(str, n);

  if (table->table[crc % TABLE_SIZE] == 0) {
    table->table[crc % TABLE_SIZE]         = new Symbol_Table_Node();
    table->table[crc % TABLE_SIZE]->symbol = manager_alloc_symbol(table->manager, str, n, crc);
    table->table[crc % TABLE_SIZE]->prev   = 0;

    table->crc64_map = insert(table->crc64_map, table->table[crc % TABLE_SIZE]->symbol.id, crc);

    return table->table[crc % TABLE_SIZE]->symbol;
  } else {
    Symbol_Table_Node* node        = new Symbol_Table_Node();
    node->prev                     = table->table[crc % TABLE_SIZE];
    table->table[crc % TABLE_SIZE] = node;

    node->symbol = manager_alloc_symbol(table->manager, str, n, crc);

    table->crc64_map = insert(table->crc64_map, table->table[crc % TABLE_SIZE]->symbol.id, crc);

    return node->symbol;
  }
}

Symbol set_entry(Symbol_Table* table, const i8* str) {
  return set_entry(table, str, strlen(str));
}

Symbol_Table* symbol_table_create() {
  Symbol_Table* table = new Symbol_Table();

  table->manager = manager_create();

  for (u64 i = 0; i < TABLE_SIZE; i++) {
    table->table[i] = 0;
  }

  table->empty = set_entry(table, "");

  return table;
}

void symbol_table_destroy(Symbol_Table* table) {
  for (u64 i = 0; i < TABLE_SIZE; i++) {
    while (table->table[i]) {
      Symbol_Table_Node* tmp = table->table[i]->prev;

      delete table->table[i];

      table->table[i] = tmp;
    }
  }

  manager_destroy(table->manager);

  delete table;
}

Symbol empty(Symbol_Table* table) {
  return table->empty;
}

Symbol get_symbol(Symbol_Table* table, Id id) {
  Id_To_CRC64_Map_Node* node = search(table->crc64_map, id);

  if (node == 0)
    return empty(table);

  Symbol_Table_Node* n = table->table[node->crc % TABLE_SIZE];

  while (n && n->symbol.id != id) {
    n = n->prev;
  }

  return n->symbol.id == id ? n->symbol : empty(table);
}

Symbol from_token(Symbol_Table* table, Lexer* lexer, Token token) {
  i8 buffer[token.size + 1];

  token_get_id(lexer, token, buffer);

  buffer[token.size] = '\0';

  return set_entry(table, buffer, token.size);
}

} // namespace symbol

} // namespace compiler
