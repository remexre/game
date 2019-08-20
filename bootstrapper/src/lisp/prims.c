#include "prims.h"
#include "../io.h"
#include "args.h"
#include "eval.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "../common.h"

native_func(apply_1) {
	value func_val, args_val;
	try(parse_args(string_from_static_cstr("apply-1"), args, 2, 0, NULL, &func_val, &args_val));
	return apply(func_val, args_val, out, ctx);
}

native_func(atom) {
	value val;
	try(parse_args(string_from_static_cstr("atom"), args, 1, 0, NULL, &val));
	*out = context_bool(ctx, !val || val->tag != TAG_CONS);
	return ok;
}

native_func(car) {
	UNUSED(ctx);

	value val;
	try(parse_args(string_from_static_cstr("car"), args, 1, 0, NULL, &val));

	struct cons cons;
	try(as_cons(val, &cons));

	*out = cons.hd;
	return ok;
}

native_func(cdr) {
	UNUSED(ctx);

	value val;
	try(parse_args(string_from_static_cstr("cdr"), args, 1, 0, NULL, &val));

	struct cons cons;
	try(as_cons(val, &cons));

	*out = cons.tl;
	return ok;
}

native_func(cons) {
	UNUSED(ctx);

	value car, cdr;
	try(parse_args(string_from_static_cstr("cons"), args, 2, 0, NULL, &car, &cdr));
	*out = make_cons(car, cdr);
	return ok;
}

native_func(define_package) {
	value pkg_val;
	try(parse_args(string_from_static_cstr("define-package"), args, 1, 0, NULL, &pkg_val));

	string pkg_name;
	try(as_string(ctx, pkg_val, &pkg_name));

	context_def_package(ctx, pkg_name);
	*out = NIL;
	return ok;
}

native_func(eq) {
	value l, r;
	try(parse_args(string_from_static_cstr("eq"), args, 2, 0, NULL, &l, &r));
	if(!l || !r) {
		*out = context_bool(ctx, l == r);
		return ok;
	} else if(l->tag != r->tag) {
		*out = NIL;
		return ok;
	} else {
		switch(l->tag) {
		case TAG_FIXNUM:
			*out = context_bool(ctx, l->value.fixnum == r->value.fixnum);
			return ok;
		case TAG_FLOAT:
			*out = context_bool(ctx, l->value.float_ == r->value.float_);
			return ok;
		case TAG_SYMBOL:
			*out = context_bool(ctx, l->value.symbol == r->value.symbol);
			return ok;
		case TAG_CONS:
		case TAG_FUNCTION:
		case TAG_OBJECT:
		case TAG_VECTOR:
		case TAG_HOST:
			*out = NIL;
			return ok;
		default: return errorf(TYPE_ERROR, "Can't eq unknown tag %02x", l->tag);
		}
	}
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

static void exports_of_helper(symbol sym, void* lst_voidp) {
	value* out = lst_voidp;
	*out = make_cons(symbol_to_value(sym), *out);
}

native_func(exports_of) {
	value pkg_val;
	try(parse_args(string_from_static_cstr("exports-of"), args, 1, 0, NULL, &pkg_val));

	string pkg_name;
	try(as_string(ctx, pkg_val, &pkg_name));

	package pkg = context_def_package(ctx, pkg_name);
	*out = NIL;
	package_get_exports(pkg, exports_of_helper, out);
	return ok;
}

native_func(funcall) {
	value func_val, args_val;
	try(parse_args(string_from_static_cstr("funcall"), args, 1, 0, &args_val, &func_val));
	return apply(func_val, args_val, out, ctx);
}

native_func(gensym) {
	try(parse_args(string_from_static_cstr("gensym"), args, 0, 0, NULL));
	*out = symbol_to_value(context_gensym(ctx));
	return ok;
}

native_func(get_class) {
	UNUSED(ctx);

	value val;
	try(parse_args(string_from_static_cstr("get-class"), args, 1, 0, NULL, &val));
	symbol sym;
	try(as_symbol(val, &sym));
	if(!(sym->flags & HAS_CLASS))
		return make_error(UNBOUND_CLASS, sym->fq_name);
	*out = sym->function;
	return ok;
}

native_func(get_function) {
	UNUSED(ctx);

	value val;
	try(parse_args(string_from_static_cstr("get-function"), args, 1, 0, NULL, &val));
	symbol sym;
	try(as_symbol(val, &sym));
	if(!(sym->flags & HAS_FUNCTION))
		return make_error(UNBOUND_FUNC, sym->fq_name);
	*out = sym->function;
	return ok;
}

native_func(get_global) {
	UNUSED(ctx);

	value val;
	try(parse_args(string_from_static_cstr("get-global"), args, 1, 0, NULL, &val));
	symbol sym;
	try(as_symbol(val, &sym));
	if(!(sym->flags & HAS_GLOBAL))
		return make_error(UNBOUND_VAR, sym->fq_name);
	*out = sym->global;
	return ok;
}

native_func(get_macro) {
	UNUSED(ctx);

	value val;
	try(parse_args(string_from_static_cstr("get-macro"), args, 1, 0, NULL, &val));
	symbol sym;
	try(as_symbol(val, &sym));
	if(!(sym->flags & HAS_MACRO))
		return make_error(UNBOUND_MACRO, sym->fq_name);
	*out = sym->macro;
	return ok;
}

native_func(import) {
	value sym_val;
	try(parse_args(string_from_static_cstr("import"), args, 1, 0, NULL, &sym_val));

	symbol sym;
	try(as_symbol(sym_val, &sym));

	context_import_symbol(ctx, sym);
	*out = NIL;
	return ok;
}

native_func(import_to) {
	value sym_val, pkg_val;
	try(parse_args(string_from_static_cstr("import-to"), args, 2, 0, NULL, &pkg_val, &sym_val));

	symbol sym;
	string pkg_name;
	try(as_symbol(sym_val, &sym));
	try(as_string(ctx, pkg_val, &pkg_name));

	package_import_symbol(context_def_package(ctx, pkg_name), sym);
	*out = NIL;
	return ok;
}

native_func(in_package) {
	value sym_val;
	try(parse_args(string_from_static_cstr("in-package"), args, 1, 0, NULL, &sym_val));

	symbol sym;
	try(as_symbol(sym_val, &sym));

	context_set_current_package(ctx, sym->name);
	*out = NIL;
	return ok;
}

native_func(nreverse_list) {
	UNUSED(ctx);

	value val;
	try(parse_args(string_from_static_cstr("nreverse"), args, 1, 0, NULL, &val));

	value prev = NIL;
	while(val) {
		struct cons* cons;
		try(as_cons_ref(val, &cons));
		value next = cons->tl;
		cons->tl = prev;
		prev = val;
		val = next;
	}
	*out = prev;
	return ok;
}

native_func(null) {
	value val;
	try(parse_args(string_from_static_cstr("null"), args, 1, 0, NULL, &val));
	*out = context_bool(ctx, !val);
	return ok;
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

native_func(set_global) {
	UNUSED(ctx);

	value sym_val, val;
	try(parse_args(string_from_static_cstr("set-global"), args, 2, 0, NULL,
		&sym_val, &val));

	symbol sym;
	try(as_symbol(sym_val, &sym));

	sym->flags |= HAS_GLOBAL;
	sym->global = val;

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

native_func(sleep) {
	UNUSED(ctx);

	value time_val;
	try(parse_args(string_from_static_cstr("sleep"), args, 1, 0, NULL, &time_val));

	int64_t time;
	try(as_fixnum(time_val, &time));

	usleep(time * 1000);

	*out = NIL;
	return ok;
}

native_func(symbol_name) {
	UNUSED(ctx);

	value sym_val;
	try(parse_args(string_from_static_cstr("symbol-name"), args, 1, 0, NULL, &sym_val));

	symbol sym;
	try(as_symbol(sym_val, &sym));

	*out = string_to_value(sym->name);
	return ok;
}

native_func(symbol_package) {
	UNUSED(ctx);

	value sym_val;
	try(parse_args(string_from_static_cstr("symbol-name"), args, 1, 0, NULL, &sym_val));

	symbol sym;
	try(as_symbol(sym_val, &sym));

	*out = string_to_value(package_name(sym->package));
	return ok;
}

native_func(unwind_protect_thunk) {
	value protected, cleanup;
	try(parse_args(string_from_static_cstr("unwind-protect-thunk"), args, 2, 0, NULL,
		&protected, &cleanup));

	value dummy;
	error err = apply(protected, NIL, out, ctx);
	try(apply(cleanup, NIL, &dummy, ctx));
	return err;
}
