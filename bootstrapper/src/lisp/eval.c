#include "eval.h"
#include "../io.h"
#include "args.h"
#include "../parser.h"
#include <gc.h>
#include <stdio.h>
#include "../common.h"

static error_return do_cond(value, value* out, env);
static error_return do_let(value clauses, value body, value* out, env);
static error_return make_lambda(symbol name, value, value* out, env);
static error_return parse_lambda_list(value, struct closure* out, context ctx);

error_return eval_string(string src, value* out, env env) {
	value forms, result;
	try(parse_all(src, env->ctx, &forms));
	try(eval_body(forms, &result, env));
	if(out) *out = result;
	return ok;
}

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
		else if(args)
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

	if(!val) {
		*out = NIL;
		return ok;
	}

	symbol func_sym;
	value args;
	switch(val->tag) {
	case TAG_CONS:
		try(as_symbol(val->value.cons.hd, &func_sym));
		if(func_sym == context_lang(env->ctx, "cond")) {

			return do_cond(val->value.cons.tl, out, env);

		} else if(func_sym == context_lang(env->ctx, "function")) {

			value sym_val;
			try(parse_args(string_from_static_cstr("function"), val->value.cons.tl,
				1, 0, NULL, &sym_val));

			symbol sym;
			try(as_symbol(sym_val, &sym));

			if(!(sym->flags & HAS_FUNCTION))
				return make_error(UNBOUND_FUNC, sym->fq_name);
			*out = sym->function;
			return ok;

		} else if(func_sym == context_lang(env->ctx, "lambda")) {

			return make_lambda(NULL, val->value.cons.tl, out, env);

		} else if(func_sym == context_lang(env->ctx, "let")) {

			struct cons clause_cons;
			try(as_cons(val->value.cons.tl, &clause_cons));
			return do_let(clause_cons.hd, clause_cons.tl, out, env);

		} else if(func_sym == context_lang(env->ctx, "named-lambda")) {

			symbol name;
			struct cons lambda_cons;
			try(as_cons(val->value.cons.tl, &lambda_cons));
			try(as_symbol(lambda_cons.hd, &name));
			return make_lambda(name, lambda_cons.tl, out, env);

		} else if(func_sym == context_lang(env->ctx, "quote")) {

			return parse_args(string_from_static_cstr("quote"), val->value.cons.tl,
				1, 0, NULL, out);

		} else if(func_sym->flags & HAS_MACRO) {

			value src;
			try(apply(func_sym->macro, val->value.cons.tl, &src, env->ctx));
			return eval(src, out, env);

		} else if(!(func_sym->flags & HAS_FUNCTION)) {

			return make_error(UNBOUND_FUNC, func_sym->fq_name);

		} else {

			try(eval_list(val->value.cons.tl, &args, env));
			return apply(func_sym->function, args, out, env->ctx);

		}
	case TAG_SYMBOL:
		return env_get(env, val->value.symbol, out);
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
	*out = NIL;
	struct cons cons;
	while(val) {
		try(as_cons(val, &cons));
		try(eval(cons.hd, out, env));
		val = cons.tl;
	}
	return ok;
}

// TODO: Make this much less hacky...
error_return eval_list(value args, value* out, env env) {
	if(!args) {
		*out = NIL;
		return ok;
	}

	*out = make_cons(NIL, NIL);

	struct cons args_cons;
	struct cons* out_cons;
	value iter = *out;

	while(args) {
		try(as_cons(args, &args_cons));
		try(as_cons_ref(iter, &out_cons));
		try(eval(args_cons.hd, &out_cons->hd, env));

		args = args_cons.tl;
		iter = out_cons->tl = make_cons(NIL, NIL);
	}

	out_cons->tl = NIL;

	return ok;
}

static error_return do_cond(value val, value* out, env env) {
	while(val) {
		struct cons cons, clause;
		try(as_cons(val, &cons));
		try(as_cons(cons.hd, &clause));

		value taken;
		try(eval(clause.hd, &taken, env));
		if(taken)
			return eval_body(clause.tl, out, env);

		val = cons.tl;
	}

	*out = NIL;
	return ok;
}

static error_return do_let(value clauses, value body, value* out, env e) {
	env local_env = env_clone(e);

	while(clauses) {
		struct cons clauses_cons, clause_cons;
		symbol name;
		value val;
		try(as_cons(clauses, &clauses_cons));
		clauses = clauses_cons.tl;
		try(as_cons(clauses_cons.hd, &clause_cons));
		try(as_symbol(clause_cons.hd, &name));

		try(eval_body(clause_cons.tl, &val, e));
		env_add(local_env, name, val);
	}

	return eval_body(body, out, local_env);
}

static error_return make_lambda(symbol name, value val, value* out, env env) {
	struct cons cons;
	try(as_cons(val, &cons));

	struct closure closure;
	closure.num_required = 0;
	closure.num_optional = 0;
	closure.has_rest = false;
	closure.requireds = GC_malloc(0);
	closure.optionals = GC_malloc(0);
	try(parse_lambda_list(cons.hd, &closure, env->ctx));

	closure.env = env;
	closure.body = cons.tl;
	*out = closure_to_value(closure, name);
	return ok;
}

static error_return parse_lambda_list(value val, struct closure* out, context ctx) {
	// 0 -> required, 1 -> optional, 2 -> rest, 3 -> done
	int mode = 0;

	struct cons cons;
	symbol sym;
	while(1) {
		if(mode == 0) {
			if(!val)
				return ok;
			try(as_cons(val, &cons));
			try(as_symbol(cons.hd, &sym));
			val = cons.tl;

			if(is_lambda_list_keyword(ctx, sym)) {
				if(string_cmp(sym->name, string_from_static_cstr("optional")) == 0)
					mode = 1;
				else if(string_cmp(sym->name, string_from_static_cstr("body")) == 0)
					mode = 2;
				else if(string_cmp(sym->name, string_from_static_cstr("rest")) == 0)
					mode = 2;
				else
					return make_error(SYNTAX_ERROR, string_cat(
						string_from_static_cstr("Invalid lambda list keyword: &"),
						sym->name));
				continue;
			}


			out->requireds = GC_realloc(out->requireds, sizeof(symbol) * (out->num_required + 1));
			out->requireds[out->num_required++] = sym;
		} else if(mode == 1) {
			todo;
		} else if(mode == 2) {
			try(as_cons(val, &cons));
			try(as_symbol(cons.hd, &sym));
			val = cons.tl;
			mode = 3;

			out->has_rest = true;
			out->rest = sym;
		} else if(mode == 3) {
			try(as_nil(val));
			return ok;
		} else {
			unreachable;
		}
	}
}
