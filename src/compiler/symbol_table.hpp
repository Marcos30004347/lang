#pragma once

#include "lexer.hpp"
#include "lib/table.hpp"
#include "types.hpp"

namespace compiler {
namespace symbol {

typedef struct Bucket Bucket;
typedef u64           Id;

struct Symbol {
  Id  id;
  u64 size;
  u64 crc64;

  Bucket* bucket;
};

#define BUCKET_SIZE 4096
#define TABLE_SIZE  4096

struct Bucket {
  u64 id;

  i8 buffer[BUCKET_SIZE];

  Bucket* next;
  Bucket* prev;
};

struct Manager {
  u64 symbols_count;
  u64 used_chars_count;
  u64 allocated_chars_count;

  Bucket* bucket_first;
  Bucket* bucket_last;
};

struct Symbol_Table_Node {
  Symbol             symbol;
  Symbol_Table_Node* prev;
};

struct Symbol_Table {
  Symbol                  empty;
  Manager*                manager;
  lib::Table< u64, u64 >* crc64_map;
  Symbol_Table_Node*      table[TABLE_SIZE];
};

Manager* manager_create();
void     manager_destroy(Manager* m);

Symbol_Table* symbol_table_create();
void          symbol_table_destroy(Symbol_Table* table);

i8 char_at(Symbol* it, u64 i);

b8 is_equal(Symbol_Table* t, Symbol* a, Symbol* b);
b8 is_equal(Symbol_Table* m, Symbol* a, const i8* b, u64 n);
b8 is_equal(Symbol_Table* m, Symbol* a, const i8* b);

b8 exist_entry(Symbol_Table* table, const i8* c_str, u64 n);
b8 exist_entry(Symbol_Table* table, const i8* c_str);

Symbol get_entry(Symbol_Table* table, const i8* c_str, u64 n);
Symbol get_entry(Symbol_Table* table, const i8* c_str);

Symbol set_entry(Symbol_Table* table, const i8* c_str, u64 n);
Symbol set_entry(Symbol_Table* table, const i8* c_str);

Symbol from_token(Symbol_Table* table, Lexer* lexer, Token token);
Symbol empty(Symbol_Table* table);
Symbol get_symbol(Symbol_Table* table, Id id);

Symbol number_to_symbol(Symbol_Table* table, u64 num, const i8* prefix);

} // namespace symbol
} // namespace compiler
