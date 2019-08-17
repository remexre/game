#include "lisp/lists.h"
#include <stdlib.h>
#include "common.h"

#define parse_rule(NAME, TY) error NAME(string* src, TY* out)

bool eof(string*);
parse_rule(peek, char);
parse_rule(advance, char);
error eat_whitespace(string*);

parse_rule(expr, value);
parse_rule(exprs, value);

error parse_one(string src, value* out) {
	return expr(&src, out);
}

error parse_all(string src, value* out) {
	return exprs(&src, out);
}

bool eof(string* src) {
	return string_len(*src) == 0;
}

parse_rule(peek, char) {
	if(!string_len(*src))
		return ERROR(SYNTAX_ERROR, string_from_static_cstr("Unexpected EOF"));
	*out = string_get(*src, 0);
	return ok;
}

parse_rule(advance, char) {
	if(!string_len(*src))
		return ERROR(SYNTAX_ERROR, string_from_static_cstr("Unexpected EOF"));
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
		err = advance(src, &peek_char);
		if(err.code != 0)
			return err;
	}
	return ok;
}

parse_rule(expr, value) { abort(); }

parse_rule(exprs, value) {
	*out = NIL;
	while(!eof(src)) {
		value head;
		try(expr(src, &head));
		*out = make_cons(head, *out);
	}
	try(nreverse_list(*out, out));
	return ok;
}
