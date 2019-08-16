#include "util.h"

#include <gc.h>
#include <string.h>

hash djb2a(str str) {
	hash out = 5381;
	for(size_t i = 0; i < str.len; i++)
		out = (out * 33) ^ str.data[i];
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
