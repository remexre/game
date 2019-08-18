#include "prims.h"
#include "../io.h"
#include "args.h"
#include "eval.h"
#include <stdio.h>
#include "../common.h"

native_func(atom) {
	value val;
	try(parse_args(string_from_static_cstr("atom"), args, 1, 0, NULL, &val));

	if(null(val) || get_tag(val) != TAG_CONS)
		*out = context_t(ctx);
	else
		*out = NIL;

	return ok;
}

native_func(funcall) {
	value func_val, args_val;
	try(parse_args(string_from_static_cstr("funcall"), args, 1, 0, &args_val, &func_val));
	return apply(func_val, args_val, out, ctx);
}

native_func(print) {
	UNUSED(ctx);

	bool first = true;
	while(!null(args)) {
		if(first) first = false;
		else putc(' ', stdout);

		struct cons cons;
		try(as_cons(args, &cons));
		args = cons.tl;
		string_fputs(show_value(cons.hd, false), stdout);
	}
	putc('\n', stdout);

	*out = NIL;
	return ok;
}

native_func(set_symbol_function) {
	UNUSED(ctx);

	value sym_val;
	value func_val;
	try(parse_args(string_from_static_cstr("set-symbol-function"), args, 2, 0, NULL,
		&sym_val, &func_val));

	symbol sym;
	try(as_symbol(sym_val, &sym));

	sym->flags |= HAS_FUNCTION;
	sym->function = func_val;

	*out = NIL;
	return ok;
}
