#ifndef GAME_BUFFER_H
#define GAME_BUFFER_H 1

#include "util.h"

typedef struct {
	char* data;
	size_t cap;
	size_t len;
} buffer;

buffer make_buffer(size_t cap);
string buffer_to_string(buffer);
void buffer_append_char(buffer*, char);
void buffer_append_cstr(buffer*, const char*);
void buffer_append_string(buffer*, string);
void buffer_reserve(buffer*, size_t);

#endif
