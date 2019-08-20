#ifndef GAME_LISP_PLATFORM_X11_H
#define GAME_LISP_PLATFORM_X11_H 1

#include "../../util.h"
#include <EGL/egl.h>

error_return createNativeWindow(string displayName, EGLNativeWindowType* out);

#endif
