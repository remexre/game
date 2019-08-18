#include "buffer.h"
#include <gc.h>
#include <string.h>
#include "common.h"

buffer make_buffer(size_t cap) {
	return (buffer) {
		.data = GC_malloc(cap),
		.cap = cap,
		.len = 0,
	};
}

string buffer_to_string(buffer buf) {
	return (string) {
		.data = buf.data,
		.len = buf.len,
	};
}

void buffer_append_char(buffer* buf, char ch) {
	buffer_reserve(buf, 1);
	buf->data[buf->len++] = ch;
}

void buffer_append_cstr(buffer* buf, const char* str) {
	size_t len = strlen(str);
	buffer_reserve(buf, len);
	memcpy(&buf->data[buf->len], str, len);
	buf->len += len;
}

void buffer_append_string(buffer* buf, string str) {
	buffer_reserve(buf, str.len);
	memcpy(&buf->data[buf->len], str.data, str.len);
	buf->len += str.len;
}

void buffer_reserve(buffer* buf, size_t n) {
	size_t new_cap = buf->cap;
	while(new_cap < buf->len + n)
		new_cap *= 2;
	buf->data = GC_realloc(buf->data, new_cap);
	buf->cap = new_cap;
}
