#ifndef GAME_LISP_GL_H
#define GAME_LISP_GL_H 1

#include "value.h"
#include "../util.h"

void glfwPreinit(void);

native_func(glfw_init);
native_func(glfw_terminate);
native_func(glfw_create_window);
native_func(glfw_destroy_window);
native_func(glfw_get_error);

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
