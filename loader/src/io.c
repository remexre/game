#include "io.h"

#include <errno.h>
#include <stdio.h>

#include "common.h"

int read_file(str path, str* out_buf) {
	char* cstr_path = cstr_from_str(path);
	FILE* file = fopen(cstr_path, "r");
	if(!file)
		return errno;
	// TODO
	return 0;
}
