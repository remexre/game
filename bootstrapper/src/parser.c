#include "parser.h"
#include "buffer.h"
#include "lisp/prims.h"
#include <stdlib.h>
#include "common.h"

#define parse_rule(NAME, TY) static error_return parse_##NAME(string* src, context ctx, TY* out)

static bool eof(string*);
static bool is_whitespace(char);
static error_return eat_ignored(string*);

static bool is_symbolish(char);

static error_return parse_fixnum(string, int64_t*);
static error_return parse_float(string, double*);
static error_return parse_prefix_rm(string* src, context ctx, const char* name, value* out);

parse_rule(lambda_list_keyword, value);
parse_rule(list_rest, value);
parse_rule(symbolish, string);
parse_rule(symbolish_value, value);
parse_rule(expr, value);
parse_rule(exprs, value);

error_return parse_one(string src, context ctx, value* out) {
	try(eat_ignored(&src));
	return parse_expr(&src, ctx, out);
}

error_return parse_all(string src, context ctx, value* out) {
	try(eat_ignored(&src));
	return parse_exprs(&src, ctx, out);
}

static bool eof(string* src) {
	return string_len(*src) == 0;
}

error_return peek(string* src, char* out) {
	if(!string_len(*src))
		return make_error(SYNTAX_ERROR, string_from_static_cstr("Unexpected EOF"));
	*out = string_get(*src, 0);
	return ok;
}

error_return advance(string* src, char* out) {
	if(!string_len(*src))
		return make_error(SYNTAX_ERROR, string_from_static_cstr("Unexpected EOF"));
	*out = string_get(*src, 0);
	*src = string_drop(*src, 1);
	return ok;
}

static bool is_whitespace(char c) {
	return c <= ' ';
}

static error_return eat_comment(string* src) {
	char peek_char;
	error err;
	while(1) {
		err = advance(src, &peek_char);
		if(err.code != OK || peek_char == '\n')
			break;
	}
	return ok;
}

static error_return eat_whitespace(string* src) {
	char peek_char;
	error err;
	while(1) {
		err = peek(src, &peek_char);
		if(err.code != OK)
			break;
		if(!is_whitespace(peek_char))
			break;
		try(advance(src, &peek_char));
	}
	return ok;
}

static error_return eat_ignored(string* src) {
	char peek_char;
	error err;
	while(1) {
		err = peek(src, &peek_char);
		if(err.code != OK)
			break;
		if(is_whitespace(peek_char))
			try(eat_whitespace(src));
		else if(peek_char == ';')
			try(eat_comment(src));
		else
			break;
	}
	return ok;
}

bool is_symbolish(char ch) {
	return ch == '*' || ch == '+' || ch == '-' || ch == '.' || ch == '/'
		|| ('0' <= ch && ch <= '9') || ch == ':' || ('<' <= ch && ch <= 'Z') || ch == '_'
		|| ('a' <= ch && ch <= 'z');
}

static error_return parse_fixnum(string str, int64_t* out) {
	*out = 0;
	upto(i, str.len) {
		*out *= 10;
		*out += string_get(str, i) - '0';
	}
	return ok;
}

static error_return parse_float(string str, double* out) { UNUSED(str); *out = 1234.0; todo; }

parse_rule(lambda_list_keyword, value) {
	string str;
	try(parse_symbolish(src, ctx, &str));

	if(string_contains_char(str, ':'))
		return make_error(SYNTAX_ERROR,
			string_cat(string_from_static_cstr("Invalid lambda list keyword: "), str));

	package pkg = context_def_package(ctx, string_from_static_cstr("lambda-list-keyword"));
	*out = symbol_to_value(package_intern_symbol(pkg, str));
	return ok;
}

parse_rule(list_rest, value) {
	char first;
	value head;
	while(1) {
		try(peek(src, &first));
		switch(first) {
		case ')':
			try(advance(src, &first));
			try(eat_ignored(src));
			return lisp_nreverse_list(make_list(1, *out), out, ctx);
		default:
			try(parse_expr(src, ctx, &head));
			*out = make_cons(head, *out);
			break;
		}
	}
}

parse_rule(symbolish, string) {
	UNUSED(ctx);

	buffer buf = make_buffer(32);
	char ch;
	while(1) {
		error err = peek(src, &ch);
		if(err.code != OK || !is_symbolish(ch))
			break;
		buffer_append_char(&buf, ch);
		try(advance(src, &ch));
	}

	*out = buffer_to_string(buf);
	return eat_ignored(src);
}

parse_rule(symbolish_value, value) {
	string str;
	try(parse_symbolish(src, ctx, &str));
	return symbolish_to_value(str, ctx, out);
}

error_return symbolish_to_value(string str, context ctx, value* out) {
	expect(str.len > 0, "symbolish_to_value should be called on non-empty strings");

	if(string_contains_char(str, ':')) {
		// TODO: Check if buf is namespaced (this handles keywords too).
		unreachable;
	}

	bool numberish = true;
	size_t decimals = 0;
	upto(i, str.len) {
		char ch = string_get(str, i);
		if(ch == '.') {
			decimals++;
		} else if(!('0' <= ch && ch <= '9')) {
			numberish = false;
			break;
		}
	}

	if(numberish) {
		if(decimals == 0) {
			int64_t n;
			try(parse_fixnum(str, &n));
			*out = fixnum_to_value(n);
			return ok;
		} else if(decimals == 1) {
			double n;
			try(parse_float(str, &n));
			*out = float_to_value(n);
			return ok;
		} else {
			return make_error(SYNTAX_ERROR,
				string_cat(string_from_static_cstr("Invalid number: "), str));
		}
	}

	*out = symbol_to_value(context_intern_symbol(ctx, str));
	return ok;
}

parse_rule(expr, value) {
	char first;
	try(peek(src, &first));
	switch(first) {
	case '&':
		try(advance(src, &first));
		return parse_lambda_list_keyword(src, ctx, out);
	case '\'':
		try(advance(src, &first));
		try(eat_ignored(src));
		return parse_prefix_rm(src, ctx, "quote", out);
	case '`':
		try(advance(src, &first));
		try(eat_ignored(src));
		return parse_prefix_rm(src, ctx, "quasiquote", out);
	case ',':
		try(advance(src, &first));
		try(peek(src, &first));
		if(first == '@')
			try(advance(src, &first));
		try(eat_ignored(src));
		return parse_prefix_rm(src, ctx, first == '@' ? "unquote-splicing" : "unquote", out);
	case '%':
		try(advance(src, &first));
		try(eat_ignored(src));
		return parse_prefix_rm(src, ctx, "print-id", out);
	case '(':
		try(advance(src, &first));
		try(eat_ignored(src));
		*out = NIL;
		return parse_list_rest(src, ctx, out);
	default:
		if(is_symbolish(first))
			return parse_symbolish_value(src, ctx, out);
		return errorf(SYNTAX_ERROR, "Unexpected character: 0x%02x", (int) first);
	}
}

parse_rule(exprs, value) {
	*out = NIL;
	while(!eof(src)) {
		value head;
		try(parse_expr(src, ctx, &head));
		*out = make_cons(head, *out);
	}
	try(lisp_nreverse_list(make_list(1, *out), out, ctx));
	return ok;
}

static error_return parse_prefix_rm(string* src, context ctx, const char* name, value* out) {
	value val;
	try(parse_expr(src, ctx, &val));
	symbol quote = context_intern_symbol(ctx, string_from_static_cstr(name));
	*out = make_cons(symbol_to_value(quote), make_cons(val, NIL));
	return ok;
}
