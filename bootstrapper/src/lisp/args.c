#include "args.h"
#include <stdarg.h>
#include "../common.h"

error_return parse_args(string name, value args, size_t required, size_t optional, value* rest, ...) {
	va_list ap;
	va_start(ap, rest);

	dotimes(required) {
		value* out = va_arg(ap, value*);
		struct cons cons;
		if(as_cons(args, &cons).code != OK)
			return make_error(ARGN_MISMATCH,
				string_cat(string_from_static_cstr("Too few arguments to "), name));
		*out = cons.hd;
		args = cons.tl;
	}

	dotimes(optional) {
		value* out = va_arg(ap, value*);
		struct cons cons;
		if(as_cons(args, &cons).code != OK)
			continue;
		*out = cons.hd;
		args = cons.tl;
	}

	if(rest) {
		*rest = args;
	} else if(args) {
		return make_error(ARGN_MISMATCH,
			string_cat(string_from_static_cstr("Too many arguments to "), name));
	}

	va_end(ap);
	return ok;
}
