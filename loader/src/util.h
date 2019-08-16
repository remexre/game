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

typedef unsigned long long hash;

hash djb2a(str);

str str_from_cstr(const char*);
str str_from_static_cstr(const char*);
str str_cat(str, str);

#endif
