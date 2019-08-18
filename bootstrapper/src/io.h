#ifndef GAME_IO_H
#define GAME_IO_H 1

#include "util.h"
#include <stdio.h>

error_return read_file(string path, string* out);
int string_fputs(string, FILE*);

#endif
