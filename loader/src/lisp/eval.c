#include "eval.h"
#include "../io.h"
#include "args.h"
#include <stdio.h>
#include "../common.h"

static native_func(quote);

error_return apply(value func_val, value args, value* out, env env) {
	printf("APPLY:\n\tFUNC:");
	string_fputs(show_value(func_val), stdout);
	printf("\tARGS:");
	string_fputs(show_value(args), stdout);

	struct func func;
	try(as_function(func_val, &func));
	if(func.is_closure) {
		todo;
	} else {
		return func.native(args, out, env->ctx);
	}
}

error_return eval(value val, value* out, env env) {
	printf("EVAL: ");
	string_fputs(show_value(val), stdout);

	union value_data data;
	switch(get_tag(val)) {
	case TAG_CONS:
		if(null(val)) {
			*out = NIL;
			return ok;
		} else {
			expect_ok(as_cons(val, &data.cons), "inconsistent type-check");
			symbol func_sym;
			value args;

			try(as_symbol(data.cons.hd, &func_sym));
			if(func_sym == context_quote(env->ctx)) {
				return apply(native_to_value(quote), data.cons.tl, out, env);
			} else if(null(func_sym->function)) {
				return make_error(UNBOUND_FUNC, func_sym->fq_name);
			} else {
				try(eval_list(data.cons.tl, &args, env));
				return apply(func_sym->function, args, out, env);
			}
		}
	case TAG_SYMBOL:
		expect_ok(as_symbol(val, &data.sym), "inconsistent type-check");
		todo;
	case TAG_FUNCTION:
	case TAG_FIXNUM:
	case TAG_FLOAT:
	case TAG_OBJECT:
	case TAG_STRING:
	case TAG_VECTOR:
		*out = val;
		return ok;
	default:
		return errorf(TYPE_ERROR, "Can't eval unknown tag %d", get_tag(val));
	}
}

error_return eval_body(value val, value* out, env env) {
	struct cons cons;
	while(!null(val)) {
		try(as_cons(val, &cons));
		try(eval(cons.hd, out, env));
		val = cons.tl;
	}
	return ok;
}

// TODO: Make this much less hacky...
error_return eval_list(value args, value* out, env env) {
	if(null(args)) {
		*out = NIL;
		return ok;
	}

	*out = make_cons(NIL, NIL);

	struct cons args_cons;
	struct cons* out_cons;
	value iter = *out;

	while(!null(args)) {
		try(as_cons(args, &args_cons));
		try(as_cons_ref(iter, &out_cons));
		try(eval(args_cons.hd, &out_cons->hd, env));

		args = args_cons.tl;
		iter = out_cons->tl = make_cons(NIL, NIL);
	}

	out_cons->tl = NIL;

	return ok;
}

// NOTE: This is run as a fexpr.
static native_func(quote) {
	value val;
	try(parse_args(string_from_static_cstr("quote"), args, 1, 0, false, &val));
	*out = val;
	return ok;
}
