#include "../buffer.h"
#include "value.h"
#include <gc.h>
#include <stdio.h>
#include "../common.h"

#define check_type(EXPR, TAG) do { \
	value check_type__value = EXPR; \
	enum tag check_type__tag = TAG; \
	string check_type__found = get_tag_name(check_type__value); \
	bool check_type__error = false; \
	if(!check_type__value || check_type__value->tag != check_type__tag) { \
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

string get_tag_name(value val) {
	return val ? tag_name(val->tag) : string_from_static_cstr("nil");
}

string tag_name(enum tag tag) {
	switch(tag) {
	case TAG_CONS: return string_from_static_cstr("cons");
	case TAG_FIXNUM: return string_from_static_cstr("fixnum");
	case TAG_FUNCTION: return string_from_static_cstr("function");
	case TAG_FLOAT: return string_from_static_cstr("float");
	case TAG_OBJECT: return string_from_static_cstr("object");
	case TAG_SYMBOL: return string_from_static_cstr("symbol");
	case TAG_STRING: return string_from_static_cstr("string");
	case TAG_VECTOR: return string_from_static_cstr("vector");
	default: return string_from_static_cstr("unknown tag");
	}
}

value closure_to_value(struct closure closure, string name) {
	value val = GC_malloc(sizeof(struct value));
	val->tag = TAG_FUNCTION;
	val->value.func.name = name;
	val->value.func.is_closure = true;
	val->value.func.closure = closure;
	return val;
}

value fixnum_to_value(int64_t n) {
	value val = GC_malloc(sizeof(struct value));
	val->tag = TAG_FIXNUM;
	val->value.fixnum = n;
	return val;
}

value float_to_value(double n) {
	value val = GC_malloc(sizeof(struct value));
	val->tag = TAG_FLOAT;
	val->value.float_ = n;
	return val;
}

value native_to_value(error (*native)(value, value*, context), string name) {
	value val = GC_malloc(sizeof(struct value));
	val->tag = TAG_FUNCTION;
	val->value.func.name = name;
	val->value.func.is_closure = false;
	val->value.func.native = native;
	return val;
}

value string_to_value(string str) {
	value val = GC_malloc(sizeof(struct value));
	val->tag = TAG_STRING;
	val->value.string = str;
	return val;
}

value symbol_to_value(symbol sym) {
	value val = GC_malloc(sizeof(struct value));
	val->tag = TAG_SYMBOL;
	val->value.symbol = sym;
	return val;
}

value make_cons(value hd, value tl) {
	value val = GC_malloc(sizeof(struct value));
	val->tag = TAG_CONS;
	val->value.cons.hd = hd;
	val->value.cons.tl = tl;
	return val;
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
	*out = val->value.cons;
	return ok;
}

error_return as_cons_ref(value val, struct cons** out) {
	check_type(val, TAG_CONS);
	*out = &val->value.cons;
	return ok;
}

error_return as_fixnum(value val, int64_t* out) {
	check_type(val, TAG_FIXNUM);
	*out = val->value.fixnum;
	return ok;
}

error_return as_float(value val, double* out) {
	check_type(val, TAG_FLOAT);
	*out = val->value.float_;
	return ok;
}

error_return as_function(value val, struct func* out) {
	check_type(val, TAG_FUNCTION);
	*out = val->value.func;
	return ok;
}

error_return as_string(value val, string* out) {
	check_type(val, TAG_STRING);
	*out = val->value.string;
	return ok;
}

error_return as_symbol(value val, symbol* out) {
	check_type(val, TAG_SYMBOL);
	*out = val->value.symbol;
	return ok;
}

bool null(value val) { return !val; }

static void write_value_to_buffer(buffer* buf, value val) {
	if(!val) {
		buffer_append_cstr(buf, "()");
		return;
	}

	switch(val->tag) {
	case TAG_CONS:
		buffer_append_char(buf, '(');
		write_value_to_buffer(buf, val->value.cons.hd);
		buffer_append_cstr(buf, " . ");
		write_value_to_buffer(buf, val->value.cons.tl);
		buffer_append_char(buf, ')');
		break;
	case TAG_FUNCTION:
		buffer_append_cstr(buf, "#<function-");
		buffer_append_cstr(buf, val->value.func.is_closure ? "closure" : "native");
		if(val->value.func.name.len) {
			buffer_append_char(buf, ' ');
			buffer_append_string(buf, val->value.func.name);
		}
		buffer_append_char(buf, '>');
		break;
	case TAG_FIXNUM:
		buffer_append_string(buf, stringf("%ld", val->value.fixnum));
		break;
	case TAG_FLOAT:
		buffer_append_string(buf, stringf("%f", val->value.float_));
		break;
	case TAG_OBJECT:
		buffer_append_cstr(buf, "#<object>");
		break;
	case TAG_SYMBOL:
		buffer_append_string(buf, val->value.symbol->fq_name);
		break;
	// case TAG_STRING:
	// case TAG_VECTOR:
	default:
		buffer_append_string(buf, stringf("#<unknown-value %02x:%016lx>",
			val->tag, val->value.fixnum));
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
