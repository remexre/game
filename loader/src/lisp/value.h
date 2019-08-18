#ifndef GAME_LISP_VALUE_H
#define GAME_LISP_VALUE_H 1

#include "context.h"
#include "env.h"
#include "symbol.h"
#include "tags.h"
#include "../util.h"
#include <stdint.h>

extern const value NIL;

struct optional_param {
	symbol name;
	value default_val;
};

struct closure {
	env env;
	value body;
	size_t num_required, num_optional;
	bool has_rest;
	symbol* requireds;
	struct optional_param* optionals;
	symbol rest;
};

struct func {
	string name;
	bool is_closure;
	union {
		struct closure closure;
		error (*native)(value, value*, context);
	};
};

#define native_func(NAME) error_return NAME(value args, value* out, context ctx)

struct cons {
	value hd;
	value tl;
};

union value_data {
	struct cons cons;
	struct func func;
	symbol sym;
};

enum symbol_flags {
	HAS_CLASS    = 1 << 0,
	HAS_FUNCTION = 1 << 1,
	HAS_GLOBAL   = 1 << 2,
	HAS_MACRO    = 1 << 3
};

struct symbol_data {
	string name;
	hash name_hash;
	string fq_name;
	hash fq_hash;
	package package;

	enum symbol_flags flags;
	value class;
	value function;
	value global;
	value macro;
};

value add_tag(uint64_t, tag);
uint64_t del_tag(value);
tag get_tag(value);

value closure_to_value(struct closure, string name);
value fixnum_to_value(int32_t);
value native_to_value(error (*)(value, value*, context), string name);
value string_to_value(string);
value symbol_to_value(symbol);

value make_cons(value, value);
value make_list(size_t, ...);

error_return as_cons(value, struct cons* out);
error_return as_cons_ref(value, struct cons** out);
error_return as_fixnum(value val, int32_t* out);
error_return as_function(value val, struct func* out);
error_return as_string(value, string* out);
error_return as_symbol(value, symbol* out);
bool null(value);

string show_value(value, bool newline);

#endif
