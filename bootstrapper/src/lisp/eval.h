#ifndef GAME_EVAL_H
#define GAME_EVAL_H 1

#include "env.h"
#include "value.h"

error_return apply(value func, value args, value* out, context ctx);
error_return eval(value, value* out, env);
error_return eval_body(value, value* out, env);
error_return eval_list(value, value* out, env);

#endif
