#include "eval.h"
#include "../io.h"
#include "args.h"
#include <gc.h>
#include <stdio.h>
#include "../common.h"

static error_return make_lambda(string name, value, value* out, env);
static error_return parse_lambda_list(value, struct closure* out);

error_return apply(value func_val, value args, value* out, context ctx) {
#ifndef NDEBUG
	printf("APPLY:\n\tFUNC:");
	string_fputs(show_value(func_val, true), stdout);
	printf("\tARGS:");
	string_fputs(show_value(args, true), stdout);
#endif

	struct func func;
	try(as_function(func_val, &func));
	if(func.is_closure) {
		string name = show_value(func_val, false);
		env env = env_clone(func.closure.env);

		upto(i, func.closure.num_required) {
			struct cons cons;
			if(as_cons(args, &cons).code != OK)
				return make_error(ARGN_MISMATCH,
					string_cat(string_from_static_cstr("Too few arguments to "), name));
			env_add(env, func.closure.requireds[i], cons.hd);
			args = cons.tl;
		}

		expect(func.closure.num_optional == 0, "TODO: parse args");

		if(func.closure.has_rest)
			env_add(env, func.closure.rest, args);
		else if(!null(args))
			return make_error(ARGN_MISMATCH,
				string_cat(string_from_static_cstr("Too many arguments to "), name));
		return eval_body(func.closure.body, out, env);
	} else {
		return func.native(args, out, ctx);
	}
}

error_return eval(value val, value* out, env env) {
#ifndef NDEBUG
	printf("EVAL: ");
	string_fputs(show_value(val, true), stdout);
#endif

	union value_data data;
	switch(val->tag) {
	case TAG_CONS:
		if(null(val)) {
			*out = NIL;
			return ok;
		} else {
			expect_ok(as_cons(val, &data.cons), "inconsistent type-check");
			symbol func_sym;
			value args;

			try(as_symbol(data.cons.hd, &func_sym));
			if(func_sym == context_lang(env->ctx, "lambda")) {
				return make_lambda(string_empty, data.cons.tl, out, env);
			} else if(func_sym == context_lang(env->ctx, "named-lambda")) {
				string name;
				struct cons lambda_cons;
				try(as_cons(data.cons.tl, &lambda_cons));
				try(as_string(lambda_cons.hd, &name));
				return make_lambda(name, lambda_cons.tl, out, env);
			} else if(func_sym == context_lang(env->ctx, "quote")) {
				return parse_args(string_from_static_cstr("quote"), data.cons.tl, 1, 0, NULL, out);
			} else if(!(func_sym->flags & HAS_FUNCTION)) {
				return make_error(UNBOUND_FUNC, func_sym->fq_name);
			} else {
				try(eval_list(data.cons.tl, &args, env));
				return apply(func_sym->function, args, out, env->ctx);
			}
		}
	case TAG_SYMBOL:
		expect_ok(as_symbol(val, &data.symbol), "inconsistent type-check");
		return env_get(env, data.symbol, out);
	case TAG_FUNCTION:
	case TAG_FIXNUM:
	case TAG_FLOAT:
	case TAG_OBJECT:
	case TAG_STRING:
	case TAG_VECTOR:
		*out = val;
		return ok;
	default:
		return errorf(TYPE_ERROR, "Can't eval unknown tag %02x", val->tag);
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

static error_return make_lambda(string name, value val, value* out, env env) {
	struct cons cons;
	try(as_cons(val, &cons));

	struct closure closure;
	closure.num_required = 0;
	closure.num_optional = 0;
	closure.has_rest = false;
	closure.requireds = GC_malloc(0);
	closure.optionals = GC_malloc(0);
	try(parse_lambda_list(cons.hd, &closure));

	closure.env = env;
	closure.body = cons.tl;
	*out = closure_to_value(closure, name);
	return ok;
}

static error_return parse_lambda_list(value val, struct closure* out) {
	// 0 -> required, 1 -> optional, 2 -> rest, 3 -> done
	int mode = 0;

	struct cons cons;
	symbol sym;
	while(1) {
		if(mode == 0) {
			if(null(val))
				return ok;
			try(as_cons(val, &cons));
			try(as_symbol(cons.hd, &sym));
			val = cons.tl;
			out->requireds = GC_realloc(out->requireds, sizeof(symbol) * (out->num_required + 1));
			out->requireds[out->num_required++] = sym;
		} else if(mode == 1) {
			todo;
		} else if(mode == 2) {
			todo;
		} else if(mode == 3) {
			todo;
		} else {
			unreachable;
		}
	}
}
