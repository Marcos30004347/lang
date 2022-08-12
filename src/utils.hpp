#pragma once

#include "types.hpp"
#include "stdlib.h"
#include <cstdio>
#include <cstring>

i8* copy_str_to_buffer(const i8* buff, u64* len = 0) {
	u64 s = strlen(buff);
	
	i8* b = (i8*)malloc(sizeof(i8)*s);

	strcpy(b, buff);

	if(len) *len = s;

	return b;
}

i8 *read_from_file(const i8* file) {
  FILE *fp = fopen(file, "r");

  i8 *source = NULL;

	if (fp != NULL) {
    if (fseek(fp, 0L, SEEK_END) == 0) {
      u64 bufsize = ftell(fp);
		
      if (bufsize == -1) {
				printf("Error reading file");
				exit(1);
      }

      source = (i8 *)malloc(sizeof(i8) * (bufsize + 1));

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
