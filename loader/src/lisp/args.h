#ifndef GAME_LISP_ARGS_H
#define GAME_LISP_ARGS_H 1

#include "value.h"
#include "../util.h"

error_return parse_args(string name, value args, size_t required, size_t optional, value* rest, ...);

#endif
