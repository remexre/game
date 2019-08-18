#include "parser.h"
#include "buffer.h"
#include "lisp/lists.h"
#include <stdlib.h>
#include "common.h"

#define parse_rule(NAME, TY) error_return parse_##NAME(string* src, context ctx, TY* out)

bool eof(string*);
parse_rule(peek, char);
parse_rule(advance, char);
bool is_whitespace(char);
error_return eat_whitespace(string*);

bool is_symbolish(char);

parse_rule(list_rest, value);
parse_rule(symbolish, value);
parse_rule(expr, value);
parse_rule(exprs, value);
parse_rule(quoted, value);

error_return parse_one(string src, context ctx, value* out) {
	try(eat_whitespace(&src));
	return parse_expr(&src, ctx, out);
}

error_return parse_all(string src, context ctx, value* out) {
	try(eat_whitespace(&src));
	return parse_exprs(&src, ctx, out);
}

bool eof(string* src) {
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

bool is_whitespace(char c) {
	return c <= ' ';
}

error_return eat_whitespace(string* src) {
	char peek_char;
	error err;
	while(1) {
		err = peek(src, &peek_char);
		if(err.code != 0)
			break;
		if(!is_whitespace(peek_char))
			break;
		try(advance(src, &peek_char));
	}
	return ok;
}

bool is_symbolish(char ch) {
	return ch == '&' || ch == '*' || ch == '+' || ch == '-' || ch == '.' || ch == '/'
		|| ('0' <= ch && ch <= '9') || ch == ':' || ('<' <= ch && ch <= 'Z') || ch == '_'
		|| ('a' <= ch && ch <= 'z');
}

parse_rule(list_rest, value) {
	char first;
	value head;
	while(1) {
		try(peek(src, &first));
		switch(first) {
		case ')':
			try(advance(src, &first));
			try(eat_whitespace(src));
			return nreverse_list(make_list(1, *out), out, ctx);
		default:
			try(parse_expr(src, ctx, &head));
			*out = make_cons(head, *out);
			break;
		}
	}
}

parse_rule(symbolish, value) {
	buffer buf = make_buffer(64);
	char ch;
	while(1) {
		try(peek(src, &ch));
		if(!is_symbolish(ch))
			break;
		buffer_append_char(&buf, ch);
		try(advance(src, &ch));
	}

	*out = symbolish_to_value(buffer_to_string(buf), ctx);
	return eat_whitespace(src);
}

value symbolish_to_value(string str, context ctx) {
	// TODO: Check if buf is a lambda-list keyword.
	// TODO: Check if buf is namespaced (this handles keywords too).
	// TODO: Check if buf represents a number.

	symbol sym = context_intern_symbol(ctx, str);
	return symbol_to_value(sym);
}

parse_rule(expr, value) {
	char first;
	try(peek(src, &first));
	switch(first) {
	case '\'':
		try(advance(src, &first));
		try(eat_whitespace(src));
		return parse_quoted(src, ctx, out);
	case '(':
		try(advance(src, &first));
		try(eat_whitespace(src));
		*out = NIL;
		return parse_list_rest(src, ctx, out);
	default:
		if(is_symbolish(first))
			return parse_symbolish(src, ctx, out);
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
	try(nreverse_list(make_list(1, *out), out, ctx));
	return ok;
}

parse_rule(quoted, value) {
	value val;
	try(parse_expr(src, ctx, &val));
	symbol quote = context_intern_symbol(ctx, string_from_static_cstr("quote"));
	*out = make_cons(symbol_to_value(quote), make_cons(val, NIL));
	return ok;
}
