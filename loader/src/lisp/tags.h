#ifndef GAME_LISP_TAGS_H
#define GAME_LISP_TAGS_H 1

#include "../util.h"
#include <stdint.h>

typedef uint8_t tag;
typedef struct { uint64_t n; } value;

compile_assert(sizeof(void*) == sizeof(uint64_t), pointer_is_64bit);

extern const tag TAG_CONS;
extern const tag TAG_FIXNUM;
extern const tag TAG_FUNCTION;
extern const tag TAG_FLOAT;
extern const tag TAG_OBJECT;
extern const tag TAG_SYMBOL;
extern const tag TAG_STRING;
extern const tag TAG_VECTOR;

string tag_name(tag);

#define check_type(EXPR, TAG) do { \
	value check_type__value = EXPR; \
	tag check_type__tag = TAG; \
	if(get_tag(check_type__value) != check_type__tag) { \
		return make_error(TYPE_ERROR, \
			string_cat(string_from_static_cstr("Expected "), \
				string_cat(tag_name(check_type__tag), \
					string_cat(string_from_static_cstr(", found "), \
						tag_name(get_tag(check_type__value)))))); \
	} \
} while(0)

extern const value NIL;

value add_tag(uint64_t, tag);
uint64_t del_tag(value);
tag get_tag(value);

#endif
