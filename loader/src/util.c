#include "util.h"

#include <gc.h>
#include <string.h>

#include "common.h"

hash djb2a(str str) {
	hash out = 5381;
	for(size_t i = 0; i < str_len(str); i++)
		out = (out * 33) ^ str_get(str, i);
	return out;
}

str str_from_cstr(const char* cstr) {
	size_t len = strlen(cstr);
	char* data = GC_malloc(len);
	for(size_t i = 0; i < len; i++)
		data[i] = cstr[i];
	str str;
	str.len = len;
	str.data = data;
	return str;
}

str str_from_static_cstr(const char* cstr) {
	size_t len = strlen(cstr);
	str str;
	str.len = len;
	str.data = cstr;
	return str;
}

str str_cat(str l, str r) {
	str out;
	out.len = l.len + r.len;
	char* data = GC_malloc(out.len);
	for(size_t i = 0; i < l.len; i++)
		data[i] = l.data[i];
	for(size_t i = 0; i < r.len; i++)
		data[l.len + i] = r.data[i];
	out.data = data;
	return out;
}

str str_drop(str str, size_t n) {
	expect(n <= str.len, "Tried to drop more characters than are in the string.");
	str.len -= n;
	return str;
}

str str_take(str str, size_t n) {
	expect(n <= str.len, "Tried to take more characters than are in the string.");
	str.len = n;
	return str;
}

str str_substr(str str, size_t start, size_t end) {
	expect(end >= start, "Tried to take a substring of negative length.");
	return str_take(str_drop(str, start), (end - start));
}

char str_get(str str, size_t i) {
	expect(i < str.len, "Tried to index out of bounds of a string!");
	return str.data[i];
}

size_t str_len(str str) {
	return str.len;
}

char* cstr_from_str(str str) {
	char* out = GC_malloc(str.len + 1);
	memcpy(out, str.data, str.len);
	out[str.len] = '\0';
	return out;
}
