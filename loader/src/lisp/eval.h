#ifndef GAME_EVAL_H
#define GAME_EVAL_H 1

#include "env.h"
#include "value.h"

error_return eval(value, value* out, env);
error_return eval_all(value, context);

#endif
