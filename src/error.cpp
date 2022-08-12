#include "error.hpp"

#include "stdlib.h"
#include "stdio.h"

void parser_error(Parser *p, Token tok, const i8 *msg) {
  printf("Error: '%s' at line %u and column %u!\n", msg, tok.row, tok.col);
  exit(1);
}
