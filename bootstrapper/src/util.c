#include "util.h"
#include "io.h"
#include <gc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "common.h"

#define MAKE_MIN(TY) TY min_##TY(TY x, TY y) { return x < y ? x : y; }
MAKE_MIN(int)
MAKE_MIN(size_t)
#undef MAKE_MIN

const string string_empty = { 0, NULL };

string stringf(const char* format, ...) {
	va_list ap;
	va_start(ap, format);
	string str = vstringf(format, ap);
	va_end(ap);
	return str;
}

string vstringf(const char* format, va_list ap) {
	va_list aq;
	va_copy(aq, ap);
	int size = vsnprintf(NULL, 0, format, aq);
	if(size < 0)
		return string_from_static_cstr("vstringf error");
	char* data = GC_malloc(size + 1);
	if(vsnprintf(data, size + 1, format, ap) < 0)
		return string_from_static_cstr("vstringf error");
	return (string) { .len = size, .data = data };
}

string string_from_cstr(const char* cstr) {
	size_t len = strlen(cstr);
	char* data = GC_malloc(len);
	upto(i, len)
		data[i] = cstr[i];
	string str;
	str.len = len;
	str.data = data;
	return str;
}

string string_from_static_cstr(const char* cstr) {
	size_t len = strlen(cstr);
	string str;
	str.len = len;
	str.data = cstr;
	return str;
}

string string_cat(string l, string r) {
	string out;
	out.len = l.len + r.len;
	char* data = GC_malloc(out.len);
	upto(i, l.len)
		data[i] = l.data[i];
	upto(i, r.len)
		data[l.len + i] = r.data[i];
	out.data = data;
	return out;
}

int string_cmp(string l, string r) {
	int c = memcmp(l.data, r.data, min(l.len, r.len));
	if(c == 0) {
		if(l.len < r.len)
			return -1;
		else if(l.len > r.len)
			return 1;
	}
	return c;
}

string string_drop(string str, size_t n) {
	expect(n <= str.len, "Tried to drop more characters than are in the string.");
	str.data += n;
	str.len -= n;
	return str;
}

string string_take(string str, size_t n) {
	expect(n <= str.len, "Tried to take more characters than are in the string.");
	str.len = n;
	return str;
}

string string_sub(string str, size_t start, size_t end) {
	expect(end >= start, "Tried to take a substring of negative length.");
	return string_drop(string_take(str, end), start);
}

char string_get(string str, size_t i) {
	expect(i < str.len, "Tried to index out of bounds of a string!");
	return str.data[i];
}

size_t string_len(string str) {
	return str.len;
}

char* cstr_from_string(string str) {
	char* out = GC_malloc(str.len + 1);
	memcpy(out, str.data, str.len);
	out[str.len] = '\0';
	return out;
}

bool string_contains_char(string str, char ch) {
	upto(i, str.len)
		if(string_get(str, i) == ch)
			return true;
	return false;
}

hash djb2a(string str) {
	hash out = 5381;
	upto(i, string_len(str))
		out = (out * 33) ^ string_get(str, i);
	return out;
}

const error ok = { OK, { 0, NULL }};

static string error_prefix(error_code code) {
	switch(code) {
	case OK: return string_from_static_cstr("Success: ");
	case ARGN_MISMATCH: return string_from_static_cstr("Argument number mismatch: ");
	case EXPECTATION_FAILED: return string_from_static_cstr("Expectation failed: ");
	case SYNTAX_ERROR: return string_from_static_cstr("Syntax error: ");
	case SYSCALL_FAILED: return string_from_static_cstr("System call failed: ");
	case TODO: return string_from_static_cstr("TODO: ");
	case TYPE_ERROR: return string_from_static_cstr("Type error: ");
	case UNBOUND_CLASS: return string_from_static_cstr("Unbound class: ");
	case UNBOUND_FUNC: return string_from_static_cstr("Unbound function: ");
	case UNBOUND_MACRO: return string_from_static_cstr("Unbound macro: ");
	case UNBOUND_VAR: return string_from_static_cstr("Unbound variable: ");
	default: return stringf("Unknown error (%d): ", code);
	}
}

error_return make_error(error_code code, string msg) {
	return (error) { .code = code, .msg = string_cat(error_prefix(code), msg), .data = NULL };
}

error_return errorf(error_code code, const char* format, ...) {
	va_list ap;
	va_start(ap, format);
	string msg = vstringf(format, ap);
	va_end(ap);
	return make_error(code, msg);
}

void fail_at_error(const char* at, error err) {
	if(err.code == OK)
		return;

	fputs("At ", stderr);
	fputs(at, stderr);
	fputs(": ", stderr);
	if(isatty(STDERR_FILENO))
		fputs("\x1b[1;31m", stderr);
	string_fputs(err.msg, stderr);
	if(isatty(STDERR_FILENO))
		fputs("\x1b[0m", stderr);
	fputs("\n", stderr);
	exit(err.code);
}

error_return error_add_msg(error err, string msg) {
	return (error) {
		.code = err.code,
		.msg = string_cat(msg, string_cat(string_from_static_cstr(": "), err.msg)),
		.data = NULL
	};
}

error_return error_errno(int err) {
	expect(err != 0, "error_errno should be called with a non-zero error");
	return make_error(SYSCALL_FAILED, string_from_cstr(strerror(err)));
}

error_return error_expect(bool cond, const char* expr) {
	if(cond)
		return ok;
	return make_error(EXPECTATION_FAILED, string_from_static_cstr(expr));
}

__attribute__((noinline))
void bp(void) {}
