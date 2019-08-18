#ifndef GAME_LISP_PRIMS_H
#define GAME_LISP_PRIMS_H 1

#include "value.h"
#include "../util.h"

native_func(atom);
native_func(exit);
native_func(funcall);
native_func(function);
native_func(print);
native_func(set);
native_func(set_class);
native_func(set_function);
native_func(set_macro);

#endif
