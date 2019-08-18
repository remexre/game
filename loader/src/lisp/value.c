#include "../buffer.h"
#include "value.h"
#include <gc.h>
#include <stdio.h>
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

value closure_to_value(struct closure closure, string name) {
	struct func* func = GC_malloc(sizeof(struct func));
	func->name = name;
	func->is_closure = true;
	func->closure = closure;
	return add_tag((uint64_t) func, TAG_FUNCTION);
}

value fixnum_to_value(int32_t n) {
	return add_tag(n, TAG_FIXNUM);
}

value native_to_value(error (*native)(value, value*, context), string name) {
	struct func* func = GC_malloc(sizeof(struct func));
	func->name = name;
	func->is_closure = false;
	func->native = native;
	return add_tag((uint64_t) func, TAG_FUNCTION);
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

value make_list(size_t n, ...) {
	value out = NIL;
	dotimes(n) {
		out = make_cons(NIL, out);
	}

	va_list ap;
	va_start(ap, n);
	value iter = out;
	dotimes(n) {
		struct cons* cons;
		expect_ok(as_cons_ref(iter, &cons), "impossible");
		cons->hd = va_arg(ap, value);
		iter = cons->tl;
	}
	va_end(ap);

	return out;
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

error_return as_function(value val, struct func* out) {
	check_type(val, TAG_FUNCTION);
	struct func* func = (struct func*) del_tag(val);
	*out = *func;
	return ok;
}

error_return as_string(value val, string* out) {
	check_type(val, TAG_STRING);
	string* str = (string*) del_tag(val);
	*out = *str;
	return ok;
}

error_return as_symbol(value val, symbol* out) {
	check_type(val, TAG_SYMBOL);
	*out = (symbol) del_tag(val);
	return ok;
}

bool null(value val) { return !val.n; }

static void write_value_to_buffer(buffer* buf, value val) {
	union value_data data;
	switch(get_tag(val)) {
	case TAG_CONS:
		if(null(val)) {
			buffer_append_cstr(buf, "()");
		} else {
			expect_ok(as_cons(val, &data.cons), "inconsistent type-check");
			buffer_append_char(buf, '(');
			write_value_to_buffer(buf, data.cons.hd);
			buffer_append_cstr(buf, " . ");
			write_value_to_buffer(buf, data.cons.tl);
			buffer_append_char(buf, ')');
		}
		break;
	case TAG_FUNCTION:
		expect_ok(as_function(val, &data.func), "inconsistent type-check");
		buffer_append_cstr(buf, "#<function-");
		buffer_append_cstr(buf, data.func.is_closure ? "closure" : "native");
		if(data.func.name.len) {
			buffer_append_char(buf, ' ');
			buffer_append_string(buf, data.func.name);
		}
		buffer_append_char(buf, '>');
		break;
	// case TAG_FIXNUM:
	// case TAG_FLOAT:
	case TAG_OBJECT:
		buffer_append_cstr(buf, "#<object>");
		break;
	case TAG_SYMBOL:
		expect_ok(as_symbol(val, &data.sym), "inconsistent type-check");
		buffer_append_string(buf, data.sym->fq_name);
		break;
	// case TAG_STRING:
	// case TAG_VECTOR:
	default:
		buffer_append_string(buf, stringf("#<unknown-value %016lx>", val.n));
		break;
	}
}

string show_value(value val, bool newline) {
	buffer buf = make_buffer(64);
	write_value_to_buffer(&buf, val);
	if(newline)
		buffer_append_char(&buf, '\n');
	return buffer_to_string(buf);
}
