#include "../io.h"
#include "eval.h"
#include <stdio.h>
#include "../common.h"

error_return eval_all(value val, context ctx) {
	env env = make_env(ctx);
	struct cons cons;
	value eval_tmp;
	while(!null(val)) {
		try(as_cons(val, &cons));
		try(eval(cons.hd, &eval_tmp, env));
		val = cons.tl;
	}
	return ok;
}

error_return eval(value val, value* out, env env) {
	string_fputs(show_value(val), stdout);
	todo;
}
