#include "io.h"
#include <errno.h>
#include "common.h"

#define BUF_SIZE 4096

error_return read_file(string path, string* out) {
	char* cstr_path = cstr_from_string(path);
	FILE* file = fopen(cstr_path, "r");
	if(!file) {
		return error_errno(errno);
	}

	string s = string_empty;
	char buf[BUF_SIZE];
	while(1) {
		size_t len = fread(buf, 1, BUF_SIZE, file);
		s = string_cat(s, (string) { .len = len, .data = buf });
		if(len < BUF_SIZE) {
			if(ferror(file))
				return error_errno(errno);
			break;
		}
	}

	*out = s;
	return ok;
}

int string_fputs(string str, FILE* stream) {
	while(str.len) {
		size_t size = fwrite(str.data, 1, str.len, stream);
		if(size == str.len)
			break;
		str = string_drop(str, size);
	}
	return 0;
}
