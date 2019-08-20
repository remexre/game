#if defined(__unix__) || defined(__APPLE__)
# if !defined(MESA_EGL_NO_X11_HEADERS)
#  include "platform/x11.c"
# else
#  error "Platform not yet supported"
# endif
#else
# error "Platform not yet supported"
#endif
