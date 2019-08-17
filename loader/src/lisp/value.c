#include "value.h"
#include <gc.h>
#include "../common.h"

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
