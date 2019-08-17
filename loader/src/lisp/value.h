#ifndef GAME_LISP_VALUE_H
#define GAME_LISP_VALUE_H 1

#include "context.h"
#include "symbol.h"
#include "../util.h"
#include <stdint.h>

typedef uint8_t tag;
typedef struct { uint64_t n; } value;

extern const tag TAG_CONS;
extern const tag TAG_FIXNUM;
extern const tag TAG_FUNCTION;
extern const tag TAG_FLOAT;
extern const tag TAG_OBJECT;
extern const tag TAG_SYMBOL;
extern const tag TAG_STRING;
extern const tag TAG_VECTOR;

string tag_name(tag);

extern const value NIL;

struct symbol_data {
	string name;
	hash name_hash;
	value class;
	value function;
	value global;
	value macro;
	package package;
	value place;
};

value add_tag(uint64_t, tag);
uint64_t del_tag(value);
tag get_tag(value);

value fixnum_to_value(int32_t);
value string_to_value(string);
value symbol_to_value(symbol);

value make_cons(value, value);

struct cons {
	value hd;
	value tl;
};

error as_cons(value, struct cons* out);
error as_cons_ref(value, struct cons** out);
error as_fixnum(value val, int32_t* out);
bool null(value);

#endif
