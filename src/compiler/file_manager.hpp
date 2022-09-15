#pragma once

#include "symbol_table.hpp"

namespace compiler {
namespace file {

i8* read_cstr_from_file(const i8* file);

// typedef u64 Id;

// struct Text_Data {
//   Id                    id;
//   i8*                   buffer;
//   symbol::Symbol        file_path;
//   symbol::Symbol_Table* table;
// };

// struct Text_Data_Node {
//   Text_Data       storage;
//   Text_Data_Node* prev;
// };

// struct Manager {
//   u64 file_count;

//   Text_Data_Node* files;
// };

// Manager* manager_create();

// void file_manager_destroy(Manager*);

// Text_Data* manager_load_file_into_text_data(Manager* m, const i8* path);

// Text_Data* manager_create_text_data(Manager* m);

// Text_Data* manager_get_text_data(Manager* m, Id idx);

// symbol::Symbol add_symbol_to_text_data_table(Manager* m, Text_Data* data, const i8* c_str, u64 n);
// symbol::Symbol add_symbol_to_text_data_table(Manager* m, Text_Data* data, const i8* c_str);

// b8 is_cstr_on_text_data_table(Manager* m, Text_Data* data, const i8* c_str, u64 n);
// b8 is_cstr_on_text_data_table(Manager* m, Text_Data* data, const i8* c_str);

// void destroy_file(Manager* m, Id idx);

} // namespace file
} // namespace compiler
