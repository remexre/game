#ifndef GAME_LISP_PRIMS_H
#define GAME_LISP_PRIMS_H 1

#include "value.h"
#include "../util.h"

native_func(atom);
native_func(funcall);
native_func(print);
native_func(set);
native_func(set_symbol_function);

#endif
