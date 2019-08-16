#ifndef GAME_UTIL_H
#define GAME_UTIL_H 1

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)
#define AT __FILE__ ":" TOSTRING(__LINE__)
#define expect(COND, MSG) \
	do { \
		if(!(COND)) { \
			fputs("At " AT ": " MSG "\nFailing condition: " STRINGIFY(COND) "\n", stderr); \
			abort(); \
		} \
	} while(0)

typedef struct {
	size_t len;
	const char* data;
} str;

str str_from_cstr(const char*);
str str_from_static_cstr(const char*);
str str_cat(str, str);
str str_drop(str, size_t);
str str_take(str, size_t);
str str_substr(str, size_t, size_t);
char str_get(str, size_t);
size_t str_len(str);
char* cstr_from_str(str);

typedef unsigned long long hash;

hash djb2a(str);

#endif
