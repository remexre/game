#include "tags.h"
#include "../common.h"

string tag_name(tag t) {
	switch(t) {
	case TAG_CONS: return string_from_static_cstr("cons");
	case TAG_FUNCTION: return string_from_static_cstr("closure");
	case TAG_FIXNUM: return string_from_static_cstr("fixnum");
	case TAG_FLOAT: return string_from_static_cstr("float");
	case TAG_OBJECT: return string_from_static_cstr("object");
	case TAG_SYMBOL: return string_from_static_cstr("symbol");
	case TAG_STRING: return string_from_static_cstr("string");
	case TAG_VECTOR: return string_from_static_cstr("vector");
	default: return string_from_static_cstr("unknown tag");
	}
}

const value NIL = (value) { 0 };

static const uint64_t TAG_BIT_IDX = 60;
static const uint64_t TAG_MASK = 7;
static const uint64_t UNTAG_MASK = TAG_MASK << TAG_BIT_IDX;

value add_tag(uint64_t n, tag new_tag) {
	uint16_t high_bits = (n >> 48);
	expect(high_bits == 0 || high_bits == 0xffff, "Tried to tag a value that already had a tag!");
	return (value) { (n & ~UNTAG_MASK) | (((uint64_t) new_tag) << TAG_BIT_IDX) };
}

uint64_t del_tag(value val) {
	if(val.n >> 63)
		return val.n | UNTAG_MASK;
	else
		return val.n & ~UNTAG_MASK;
}

tag get_tag(value val) {
	return (val.n >> TAG_BIT_IDX) & TAG_MASK;
}
