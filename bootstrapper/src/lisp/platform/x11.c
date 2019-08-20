#include "x11.h"
#include <stdio.h>
#include <unistd.h>
#include <X11/Xlib.h>
#include "../../common.h"

static int error_handler(Display*, XErrorEvent*);

error_return createNativeWindow(string displayName, EGLNativeWindowType* out) {
	XSetErrorHandler(error_handler);

	Display* display = XOpenDisplay(cstr_from_string(displayName));
	if(!display) {
		todo; // TODO: error
	}

	Window rootWindow = XDefaultRootWindow(display);
	Window window = XCreateSimpleWindow(display, rootWindow, 0, 0, 0, 0, 0, 0, 0);

	*out = window;
	return ok;
}

static int error_handler(Display* display, XErrorEvent* err) {
	UNUSED(display);

	// TODO: This should probably call out to a user-specified handler.
	if(isatty(STDERR_FILENO))
		fputs("\x1b[1;31m", stderr);
	fprintf(stderr, "X Error: Request %lu failed, opcode %d:%d, error %d\n",
		err->serial, err->request_code, err->minor_code, err->error_code);
	if(isatty(STDERR_FILENO))
		fputs("\x1b[0m", stderr);

	return 0;
}
