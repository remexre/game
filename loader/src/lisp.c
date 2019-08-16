#include "lisp.h"

#include <gc.h>

#include "common.h"

const tag TAG_CONS = 0;
const tag TAG_CLOSURE = 1;
const tag TAG_FIXNUM = 2;
const tag TAG_FLOAT = 3;
const tag TAG_OBJECT = 4;
const tag TAG_SYMBOL = 5;
const tag TAG_STRING = 6;
const tag TAG_VECTOR = 7;

const value NIL = 0;

static const uint64_t TAG_BIT_IDX = 60;
static const uint64_t TAG_MASK = 7;
static const uint64_t UNTAG_MASK = TAG_MASK << TAG_BIT_IDX;

value add_tag(uint64_t n, tag new_tag) {
	uint16_t high_bits = (n >> 48);
	expect(high_bits == 0 || high_bits == 0xffff, "Tried to tag a value that already had a tag!");
	return (n & ~UNTAG_MASK) | (((uint64_t) new_tag) << TAG_BIT_IDX);
}

uint64_t del_tag(value value) {
	if(value >> 63)
		return value | UNTAG_MASK;
	else
		return value & ~UNTAG_MASK;
}

tag get_tag(value value) {
	return (value >> TAG_BIT_IDX) & TAG_MASK;
}

package make_package(str name) {
	package package = GC_malloc(sizeof(*package));
	package->name = name;
	package->hash = djb2a(name);
	return package;
}

symbol make_symbol(package package, str name) {
	symbol symbol = GC_malloc(sizeof(*symbol));
	symbol->name = name;
	symbol->fq_name = str_cat(str_cat(package->name, str_from_static_cstr("::")), name);
	symbol->hash = djb2a(symbol->fq_name);
	symbol->class = NIL;
	symbol->function = NIL;
	symbol->global = NIL;
	symbol->package = package;
	return symbol;
}
