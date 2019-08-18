#include "prims.h"
#include "../io.h"
#include "args.h"
#include "eval.h"
#include <stdio.h>
#include <stdlib.h>
#include "../common.h"

native_func(atom) {
	value val;
	try(parse_args(string_from_static_cstr("atom"), args, 1, 0, NULL, &val));
	*out = context_bool(ctx, !val || val->tag != TAG_CONS);
	return ok;
}

native_func(exit) {
	UNUSED(out);
	UNUSED(ctx);

	value arg = fixnum_to_value(0);
	int64_t code;
	try(parse_args(string_from_static_cstr("exit"), args, 0, 1, NULL, &arg));
	try(as_fixnum(arg, &code));
	exit(code);
}

native_func(funcall) {
	value func_val, args_val;
	try(parse_args(string_from_static_cstr("funcall"), args, 1, 0, &args_val, &func_val));
	return apply(func_val, args_val, out, ctx);
}

native_func(print) {
	UNUSED(ctx);

	bool first = true;
	while(args) {
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

native_func(set) {
	UNUSED(ctx);

	value sym_val, val;
	try(parse_args(string_from_static_cstr("set"), args, 2, 0, NULL,
		&sym_val, &val));

	symbol sym;
	try(as_symbol(sym_val, &sym));

	sym->flags |= HAS_GLOBAL;
	sym->global = val;

	*out = NIL;
	return ok;
}

native_func(set_class) {
	UNUSED(ctx);

	value sym_val, val;
	try(parse_args(string_from_static_cstr("set-class"), args, 2, 0, NULL,
		&sym_val, &val));

	symbol sym;
	try(as_symbol(sym_val, &sym));

	sym->flags |= HAS_CLASS;
	sym->class = val;

	*out = NIL;
	return ok;
}

native_func(set_function) {
	UNUSED(ctx);

	value sym_val, val;
	try(parse_args(string_from_static_cstr("set-function"), args, 2, 0, NULL,
		&sym_val, &val));

	symbol sym;
	try(as_symbol(sym_val, &sym));

	sym->flags |= HAS_FUNCTION;
	sym->function = val;

	*out = NIL;
	return ok;
}

native_func(set_macro) {
	UNUSED(ctx);

	value sym_val, val;
	try(parse_args(string_from_static_cstr("set-macro"), args, 2, 0, NULL,
		&sym_val, &val));

	symbol sym;
	try(as_symbol(sym_val, &sym));

	sym->flags |= HAS_MACRO;
	sym->macro = val;

	*out = NIL;
	return ok;
}
