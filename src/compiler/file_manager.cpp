#include "file_manager.hpp"
#include "compiler/symbol_table.hpp"

#include "stdlib.h"
#include <cstdio>

namespace compiler {

namespace file {

i8* read_cstr_from_file(const i8* file) {
  FILE* fp = fopen(file, "r");

  i8* source = NULL;

  if (fp != NULL) {
    if (fseek(fp, 0L, SEEK_END) == 0) {
      u64 bufsize = ftell(fp);

      if (bufsize == -1) {
        printf("Error reading file");
        exit(1);
      }

      source = (i8*)malloc(sizeof(i8) * (bufsize + 1));

      if (fseek(fp, 0L, SEEK_SET) != 0) {
        printf("Error reading file");
        exit(1);
      }

      u64 newLen = fread(source, sizeof(i8), bufsize, fp);

      if (ferror(fp) != 0) {
        free(source);
        fputs("Error reading file", stderr);
        exit(1);
      } else {
        source[newLen++] = '\0';
      }
    }

    fclose(fp);
  }

  return source;
}

// File* allocate_file(Manager* m) {
//   File_Node* node = new File_Node();

//   node->file.id        = m->file_count;
//   node->file.buffer    = 0;
//   node->file.table     = symbol::symbol_table_create();
//   node->file.file_path = symbol::empty(node->file.table);
//   node->prev           = m->files;
//   m->file_count += 1;
//   m->files = node;

//   return &node->file;
// }

// File* load_file(Manager* m, const i8* path) {
//   File* file = allocate_file(m);

//   file->file_path = symbol::set_entry(file->table, path);
//   file->buffer    = read_from_file(path);

//   return file;
// }

// Manager* file_manager_create() {
//   Manager* m    = new Manager();
//   m->files      = 0;
//   m->file_count = 0;
//   return m;
// }

// void file_manager_destroy(Manager* m) {
//   File_Node* node = m->files;

//   while (node) {
//     File_Node* tmp = node->prev;
//     symbol::symbol_table_destroy(node->file.table);
//     if (node->file.buffer) delete node->file.buffer;
//     delete node;
//     node = tmp;
//   }

//   delete m;
// }

// File* get_file(Manager* m, Id idx) {
//   File_Node* node = m->files;
//   while (node && node->file.id != idx)
//     node = node->prev;
//   if (node) return &node->file;
//   return 0;
// }

// void destroy_file(Manager* m, Id idx) {
//   File_Node* prev = 0;
//   File_Node* node = m->files;
//   while (node && node->file.id != idx) {
//     prev = node;
//     node = node->prev;
//   }

//   if (node) {
//     if (prev) prev->prev = node->prev;
//     symbol::symbol_table_destroy(node->file.table);
//     if (node->file.buffer) delete node->file.buffer;
//     delete node;
//   }
// }
} // namespace file
} // namespace compiler
