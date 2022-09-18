#include "symbol_table.hpp"
#include "crc64.hpp"

#include "lexer.hpp"
#include "lib/table.hpp"
#include "stdlib.h"
#include <assert.h>
#include <cstring>
#include <strings.h>

namespace compiler {

namespace symbol {

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
  u64 crc = *lib::search(table->crc64_map, id);

  if (crc == 0) {
    return empty(table);
  }

  Symbol_Table_Node* n = table->table[crc % TABLE_SIZE];

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
