#include "error.hpp"

#include "stdlib.h"
#include "stdio.h"

void parser_error(parser *p, token tok, const i8 *msg) {
  printf("Error: '%s' at line %lu and column %lu!\n", msg, tok.row, tok.col);
  exit(1);
}
