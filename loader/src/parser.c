#include "parser.h"
#include "buffer.h"
#include "lisp/lists.h"
#include <stdlib.h>
#include "common.h"

#define parse_rule(NAME, TY) error parse_##NAME(string* src, package pkg, TY* out)

bool eof(string*);
parse_rule(peek, char);
parse_rule(advance, char);
bool is_whitespace(char);
error eat_whitespace(string*);

bool is_symbolish(char);

parse_rule(list_rest, value);
parse_rule(symbol, value);
parse_rule(expr, value);
parse_rule(exprs, value);

error parse_one(string src, package pkg, value* out) {
	try(eat_whitespace(&src));
	return parse_expr(&src, pkg, out);
}

error parse_all(string src, package pkg, value* out) {
	try(eat_whitespace(&src));
	return parse_exprs(&src, pkg, out);
}

bool eof(string* src) {
	return string_len(*src) == 0;
}

error peek(string* src, char* out) {
	if(!string_len(*src))
		return make_error(SYNTAX_ERROR, string_from_static_cstr("Unexpected EOF"));
	*out = string_get(*src, 0);
	return ok;
}

error advance(string* src, char* out) {
	if(!string_len(*src))
		return make_error(SYNTAX_ERROR, string_from_static_cstr("Unexpected EOF"));
	*out = string_get(*src, 0);
	*src = string_drop(*src, 1);
	return ok;
}

bool is_whitespace(char c) {
	return c <= ' ';
}

error eat_whitespace(string* src) {
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
	return ch == '*' || ch == '+' || ch == '-' || ch == '.' || ch == '/'
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
			return nreverse_list(*out, out);
		default:
			try(parse_expr(src, pkg, &head));
			*out = make_cons(head, *out);
			break;
		}
	}
}

parse_rule(symbol, value) {
	buffer buf = make_buffer(64);
	char ch;
	while(1) {
		try(peek(src, &ch));
		if(!is_symbolish(ch))
			break;
		buffer_append_char(&buf, ch);
		try(advance(src, &ch));
	}
	*out = symbol_to_value(package_get_or_make_symbol(pkg, buffer_to_string(buf)));
	return eat_whitespace(src);
}

parse_rule(expr, value) {
	char first;
	try(peek(src, &first));
	switch(first) {
	case '(':
		try(advance(src, &first));
		try(eat_whitespace(src));
		*out = NIL;
		return parse_list_rest(src, pkg, out);
	default:
		if(is_symbolish(first))
			return parse_symbol(src, pkg, out);
		return errorf(SYNTAX_ERROR, "Unexpected character: 0x%02x", (int) first);
	}
}

parse_rule(exprs, value) {
	*out = NIL;
	while(!eof(src)) {
		value head;
		try(parse_expr(src, pkg, &head));
		*out = make_cons(head, *out);
	}
	try(nreverse_list(*out, out));
	return ok;
}
