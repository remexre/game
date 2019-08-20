#ifndef GAME_LISP_GL_H
#define GAME_LISP_GL_H 1

#include "value.h"
#include "../util.h"

native_func(gl_make_window);
native_func(gl_get_constant);

native_func(gl_compile_shader);
native_func(gl_get_shader);
native_func(gl_attach_shader);
native_func(gl_detach_shader);
native_func(gl_delete_shader);

native_func(gl_create_program);
native_func(gl_delete_program);
native_func(gl_link_program);
native_func(gl_use_program);

#endif
