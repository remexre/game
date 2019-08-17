#ifndef GAME_LISP_H
#define GAME_LISP_H 1

#include "util.h"
#include <stdint.h>

#define SYMTAB_BUCKETS 256

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

typedef struct package_data* package;
typedef struct symbol_data* symbol;

struct package_data {
	string name;
	hash hash;
	struct symtab_link* symtab[SYMTAB_BUCKETS];
};

struct symbol_data {
	string name;
	string fq_name;
	hash hash;
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

package make_package(string name);
symbol make_symbol(package pkg, string name);

symbol package_get_symbol(package pkg, string name);
symbol package_get_or_make_symbol(package pkg, string name);

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
