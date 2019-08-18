#ifndef GAME_LISP_VALUE_H
#define GAME_LISP_VALUE_H 1

#include "context.h"
#include "env.h"
#include "../util.h"
#include <stdint.h>

enum tag {
	TAG_CONS,
	TAG_FIXNUM,
	TAG_FUNCTION,
	TAG_FLOAT,
	TAG_OBJECT,
	TAG_SYMBOL,
	TAG_STRING,
	TAG_VECTOR
};

string tag_name(enum tag);

static const value NIL = NULL;

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
	symbol name;
	bool is_closure;
	union {
		struct closure closure;
		error (*native)(value, value*, context);
	};
};

#define native_func(NAME) error_return lisp_##NAME(value args, value* out, context ctx)

struct cons {
	value hd;
	value tl;
};

union value_data {
	struct cons cons;
	struct func func;
	int64_t fixnum;
	double float_;
	string string;
	symbol symbol;
};

struct value {
	enum tag tag;
	union value_data value;
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

value closure_to_value(struct closure, symbol name);
value fixnum_to_value(int64_t);
value float_to_value(double);
value native_to_value(error (*)(value, value*, context), symbol name);
value string_to_value(string);
value symbol_to_value(symbol);

value make_cons(value, value);
value make_list(size_t, ...);

error_return as_cons(value, struct cons* out);
error_return as_cons_ref(value, struct cons** out);
error_return as_fixnum(value val, int64_t* out);
error_return as_float(value val, double* out);
error_return as_function(value val, struct func* out);
error_return as_nil(value);
error_return as_string(value, string* out);
error_return as_symbol(value, symbol* out);

string show_value(value, bool newline);

#endif