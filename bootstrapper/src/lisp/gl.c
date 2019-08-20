#include "gl.h"
#include "args.h"
#include "platform.h"
#include <EGL/egl.h>
#include <GLES/gl.h>
#include <stdlib.h>
#include "../common.h"

static EGLint const attribute_list[] = {
	EGL_RED_SIZE, 1,
	EGL_GREEN_SIZE, 1,
	EGL_BLUE_SIZE, 1,
	EGL_NONE
};

native_func(gl_make_window) {
	value display_name_value = NIL;
	try(parse_args(string_from_static_cstr("glMakeWindow"), args, 0, 1, NULL, &display_name_value));

	string display_name;
	if(display_name_value) {
		try(as_string(display_name_value, &display_name));
	} else if(getenv("DISPLAY")) {
		display_name = string_from_static_cstr(getenv("DISPLAY"));
	} else {
		*out = symbol_to_value(context_intern_static(ctx, "gl-raw", "error-x11-no-display"));
		return ok;
	}

	EGLDisplay display = eglGetDisplay(EGL_DEFAULT_DISPLAY);
	if(display == EGL_NO_DISPLAY) {
		*out = symbol_to_value(context_intern_static(ctx, "gl-raw", "error-egl-no-display"));
		return ok;
	}
	if(eglInitialize(display, NULL, NULL) == EGL_FALSE) {
		*out = symbol_to_value(context_intern_static(ctx, "gl-raw", "error-egl-init-failed"));
		return ok;
	}

	EGLConfig config;
	EGLint num_config;
	if(eglChooseConfig(display, attribute_list, &config, 1, &num_config) == EGL_FALSE) {
		*out = symbol_to_value(context_intern_static(ctx, "gl-raw", "error-egl-no-config"));
		return ok;
	}

	EGLContext egl_ctx = eglCreateContext(display, config, EGL_NO_CONTEXT, NULL);
	if(egl_ctx == EGL_NO_CONTEXT) {
		*out = symbol_to_value(context_intern_static(ctx, "gl-raw", "error-egl-create-egl_ctx"));
		return ok;
	}

	EGLNativeWindowType native_window;
	try(createNativeWindow(display_name, &native_window));

	EGLSurface surface = eglCreateWindowSurface(display, config, native_window, NULL);
	if(surface == EGL_NO_SURFACE) {
		*out = symbol_to_value(context_intern_static(ctx, "gl-raw", "error-egl-create-window-surface"));
		return ok;
	}

	if(eglMakeCurrent(display, surface, surface, egl_ctx) == EGL_FALSE) {
		*out = symbol_to_value(context_intern_static(ctx, "gl-raw", "error-egl-make-current"));
		return ok;
	}

	// TODO
	*out = NULL;
	return ok;
}

native_func(gl_get_constant);
