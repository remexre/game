#ifndef GAME_LISP_H
#define GAME_LISP_H 1

#include "util.h"

#include <stdint.h>

typedef uint8_t tag;
typedef struct { uint64_t n; } value;

extern const tag TAG_CONS;
extern const tag TAG_CLOSURE;
extern const tag TAG_FIXNUM;
extern const tag TAG_FLOAT;
extern const tag TAG_OBJECT;
extern const tag TAG_SYMBOL;
extern const tag TAG_STRING;
extern const tag TAG_VECTOR;

extern const value NIL;

typedef struct package_data* package;
typedef struct symbol_data* symbol;

struct package_data {
	string name;
	hash hash;
};

struct symbol_data {
	string name;
	string fq_name;
	hash hash;
	value class;
	value function;
	value global;
	package package;
	value place;
};

value add_tag(uint64_t, tag);
uint64_t del_tag(value);
tag get_tag(value);

package make_package(string name);
symbol make_symbol(package package, string name);

value value_of_string(string);
value value_of_symbol(symbol);

#endif
