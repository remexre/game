#include "gl.h"
#include "args.h"
#include <GLFW/glfw3.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "../common.h"

static const char* glfwLastError = NULL;
static void glfwErrorCallback(int errorCode, const char* description) {
	UNUSED(errorCode);
	if(isatty(STDERR_FILENO))
		fputs("\x1b[1;31m", stderr);
	fputs(description, stderr);
	if(isatty(STDERR_FILENO))
		fputs("\x1b[0m", stderr);
	glfwLastError = description;
}

native_func(glfw_init) {
	try(parse_args(string_from_static_cstr("glfwInit"), args, 0, 0, NULL));
	glfwSetErrorCallback(glfwErrorCallback);
	if(!glfwInit())
		*out = symbol_to_value(context_intern_static(ctx, "gl-raw", "error-glfw-init-failed"));
	else
		*out = NIL;
	return ok;
}

native_func(glfw_terminate) {
	UNUSED(ctx);

	try(parse_args(string_from_static_cstr("glfwTerminate"), args, 0, 0, NULL));
	glfwTerminate();
	*out = NIL;
	return ok;
}

native_func(glfw_create_window) {
	value width_val, height_val, title_val;
	value full_screen = NIL;
	try(parse_args(string_from_static_cstr("glfwCreateWindow"), args, 3, 1, NULL,
		&width_val, &height_val, &title_val, &full_screen));

	int64_t width, height;
	string title;
	try(as_fixnum(width_val, &width));
	try(as_fixnum(height_val, &height));
	try(as_string(ctx, title_val, &title));

	GLFWwindow* window = glfwCreateWindow((int) width, (int) height, cstr_from_string(title),
		full_screen ? glfwGetPrimaryMonitor() : NULL, NULL);
	if(!window) {
		*out = symbol_to_value(context_intern_static(ctx, "gl-raw",
			"error-glfw-create-window-failed"));
		return ok;
	}

	// TODO
	UNUSED(window);
	*out = NIL;
	return ok;
}

native_func(glfw_destroy_window) {
	UNUSED(ctx);

	value window_obj_val;
	try(parse_args(string_from_static_cstr("glfwDestroyWindow"), args, 1, 0, NULL,
		&window_obj_val));

	*out = NIL;
	return ok;
}
