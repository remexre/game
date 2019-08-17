#include "lisp.h"
#include <gc.h>
#include "common.h"

const tag TAG_CONS = 0;
const tag TAG_FIXNUM = 1;
const tag TAG_FUNCTION = 2;
const tag TAG_FLOAT = 3;
const tag TAG_OBJECT = 4;
const tag TAG_SYMBOL = 5;
const tag TAG_STRING = 6;
const tag TAG_VECTOR = 7;

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

compile_assert(sizeof(void*) == sizeof(uint64_t), pointer_is_64bit);

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

package make_package(string name) {
	package package = GC_malloc(sizeof(*package));
	package->name = name;
	package->hash = djb2a(name);
	for(size_t i = 0; i < SYMTAB_BUCKETS; i++)
		package->symtab[i] = NULL;
	return package;
}

symbol make_symbol(package package, string name) {
	symbol symbol = GC_malloc(sizeof(*symbol));
	symbol->name = name;
	symbol->fq_name = string_cat(string_cat(package->name, string_from_static_cstr("::")), name);
	symbol->hash = djb2a(symbol->fq_name);
	symbol->class = NIL;
	symbol->function = NIL;
	symbol->global = NIL;
	symbol->macro = NIL;
	symbol->package = package;
	symbol->place = NIL;
	return symbol;
}

struct symtab_link {
	symbol sym;
	struct symtab_link* next;
};

symbol package_get_symbol(package pkg, string name) {
	hash h = djb2a(name);
	struct symtab_link** entry = &pkg->symtab[h % SYMTAB_BUCKETS];
	struct symtab_link* iter = *entry;
	while(iter) {
		if(string_cmp(iter->sym->name, name) == 0)
			return iter->sym;
		iter = iter->next;
	}
	return NULL;
}

symbol package_get_or_make_symbol(package pkg, string name) {
	hash h = djb2a(name);
	struct symtab_link** entry = &pkg->symtab[h % SYMTAB_BUCKETS];
	struct symtab_link* iter = *entry;
	while(iter) {
		if(string_cmp(iter->sym->name, name) == 0)
			return iter->sym;
		iter = iter->next;
	}

	struct symtab_link* link = GC_malloc(sizeof(struct symtab_link));
	link->sym = make_symbol(pkg, name);
	link->next = *entry;
	*entry = link;
	return link->sym;
}

value fixnum_to_value(int32_t n) {
	return add_tag(n, TAG_FIXNUM);
}

value string_to_value(string str) {
	string* ptr = GC_malloc(sizeof(string));
	*ptr = str;
	return add_tag((uint64_t) ptr, TAG_STRING);
}

value symbol_to_value(symbol sym) {
	return add_tag((uint64_t) sym, TAG_SYMBOL);
}

value make_cons(value hd, value tl) {
	struct cons* cons = GC_malloc(sizeof(struct cons));
	cons->hd = hd;
	cons->tl = tl;
	uint64_t untagged = (uint64_t) cons;
	return add_tag(untagged, TAG_CONS);
}

error as_cons(value val, struct cons* out) {
	check_type(val, TAG_CONS);
	struct cons* cons = (struct cons*) del_tag(val);
	*out = *cons;
	return ok;
}

error as_cons_ref(value val, struct cons** out) {
	check_type(val, TAG_CONS);
	struct cons* cons = (struct cons*) del_tag(val);
	*out = cons;
	return ok;
}

error as_fixnum(value val, int32_t* out) {
	check_type(val, TAG_FIXNUM);
	*out = (int32_t) del_tag(val);
	return ok;
}

bool null(value val) { return !val.n; }
