#ifndef GAME_LISP_PRIMS_H
#define GAME_LISP_PRIMS_H 1

#include "value.h"
#include "../util.h"

native_func(apply_1);
native_func(atom);
native_func(car);
native_func(cdr);
native_func(class_of);
native_func(cons);
native_func(define_package);
native_func(eq);
native_func(exit);
native_func(exports_of);
native_func(funcall);
native_func(function);
native_func(gensym);
native_func(get_class);
native_func(get_function);
native_func(get_global);
native_func(get_macro);
native_func(get_slot);
native_func(import);
native_func(import_to);
native_func(in_package);
native_func(make_instance);
native_func(nreverse_list);
native_func(null);
native_func(print);
native_func(set_class);
native_func(set_function);
native_func(set_global);
native_func(set_macro);
native_func(set_slot);
native_func(sleep);
native_func(symbol_name);
native_func(symbol_package);
native_func(unwind_protect_thunk);

#endif
