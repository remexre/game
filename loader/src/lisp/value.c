#include "../buffer.h"
#include "value.h"
#include <gc.h>
#include "../common.h"

#define check_type(EXPR, TAG) do { \
	value check_type__value = EXPR; \
	tag check_type__tag = TAG; \
	string check_type__found = tag_name(get_tag(check_type__value)); \
	bool check_type__error = false; \
	if(null(check_type__value)) { \
		check_type__found = string_from_static_cstr("nil"); \
		check_type__error = true; \
	} else if(get_tag(check_type__value) != check_type__tag) { \
		check_type__error = true; \
	} \
	if(check_type__error) { \
		return make_error(TYPE_ERROR, \
			string_cat(string_from_static_cstr("Expected "), \
				string_cat(tag_name(check_type__tag), \
					string_cat(string_from_static_cstr(", found "), \
						check_type__found)))); \
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

error_return as_cons(value val, struct cons* out) {
	check_type(val, TAG_CONS);
	struct cons* cons = (struct cons*) del_tag(val);
	*out = *cons;
	return ok;
}

error_return as_cons_ref(value val, struct cons** out) {
	check_type(val, TAG_CONS);
	struct cons* cons = (struct cons*) del_tag(val);
	*out = cons;
	return ok;
}

error_return as_fixnum(value val, int32_t* out) {
	check_type(val, TAG_FIXNUM);
	*out = (int32_t) del_tag(val);
	return ok;
}

error_return as_symbol(value val, symbol* out) {
	check_type(val, TAG_SYMBOL);
	*out = (symbol) del_tag(val);
	return ok;
}

bool null(value val) { return !val.n; }

static void show_value_helper(buffer* buf, value val) {
	struct cons cons;
	symbol sym;
	switch(get_tag(val)) {
	case TAG_CONS:
		if(null(val)) {
			buffer_append_cstr(buf, "()");
		} else {
			expect_ok(as_cons(val, &cons), "inconsistent type-check");
			buffer_append_char(buf, '(');
			show_value_helper(buf, cons.hd);
			buffer_append_cstr(buf, " . ");
			show_value_helper(buf, cons.tl);
			buffer_append_char(buf, ')');
		}
		break;
	case TAG_FUNCTION:
		buffer_append_cstr(buf, "#<function>");
		break;
	case TAG_FIXNUM:
	case TAG_FLOAT:
	case TAG_OBJECT:
	case TAG_SYMBOL:
		expect_ok(as_symbol(val, &sym), "inconsistent type-check");
		buffer_append_string(buf, package_name(sym->package));
		buffer_append_cstr(buf, "::");
		buffer_append_string(buf, sym->name);
		break;
	case TAG_STRING:
	case TAG_VECTOR:
	default:
		buffer_append_string(buf, stringf("#<unknown-value %016lx>", val.n));
		break;
	}
}

string show_value(value val) {
	buffer buf = make_buffer(64);
	show_value_helper(&buf, val);
	buffer_append_char(&buf, '\n');
	return buffer_to_string(buf);
}
