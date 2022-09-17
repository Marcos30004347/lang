#include "tests.hpp"

#include "compiler/symbol_table.hpp"

using namespace compiler;
using namespace symbol;

void should_create_and_delete_symbol_table() {
  Symbol_Table* table = symbol_table_create();

  symbol_table_destroy(table);
}

void should_add_symbols_to_symbol_table() {
  Symbol_Table* table = symbol_table_create();

  Symbol s0 = set_entry(table, "symbol0");
  Symbol s1 = set_entry(table, "symbol1");
  Symbol s2 = set_entry(table, "symbol0");
  Symbol s3 = set_entry(table, "symbol3");

  assert(s0.id == s2.id);

  assert(exist_entry(table, "symbol0") == 1);
  assert(exist_entry(table, "symbol1") == 1);
  assert(exist_entry(table, "symbol3") == 1);
  assert(exist_entry(table, "symbol4") == 0);

  symbol_table_destroy(table);
}

int main() {
  TEST(should_create_and_delete_symbol_table);
  TEST(should_add_symbols_to_symbol_table);
}
