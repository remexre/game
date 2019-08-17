#ifndef GAME_LISP_VALUE_H
#define GAME_LISP_VALUE_H 1

#include "context.h"
#include "symbol.h"
#include "tags.h"
#include "../util.h"
#include <stdint.h>

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

error_return as_cons(value, struct cons* out);
error_return as_cons_ref(value, struct cons** out);
error_return as_fixnum(value val, int32_t* out);
error_return as_symbol(value, symbol* out);
bool null(value);

string show_value(value);

#endif
