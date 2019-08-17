#ifndef GAME_LISP_TAGS_H
#define GAME_LISP_TAGS_H 1

#include "../util.h"
#include <stdint.h>

typedef uint8_t tag;
typedef struct { uint64_t n; } value;

compile_assert(sizeof(void*) == sizeof(uint64_t), pointer_is_64bit);

static const tag TAG_CONS = 0;
static const tag TAG_FIXNUM = 1;
static const tag TAG_FUNCTION = 2;
static const tag TAG_FLOAT = 3;
static const tag TAG_OBJECT = 4;
static const tag TAG_SYMBOL = 5;
static const tag TAG_STRING = 6;
static const tag TAG_VECTOR = 7;

string tag_name(tag);

extern const value NIL;

value add_tag(uint64_t, tag);
uint64_t del_tag(value);
tag get_tag(value);

#endif
