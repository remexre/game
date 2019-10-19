//! The interface to C and Lisp.

use std::os::raw::{c_char, c_void};

/// Attempts to set up a renderer.
///
/// ## Arguments
///
/// `app_name`: The name of the application as a null-terminated string, which will not be mutated,
/// and will not be referenced after this function returns (i.e., it's fine for it to be a
/// stack-allocated string or a string constant).
///
/// `vert_path`: The path to a SPIR-V vertex shader which will initially be used by the renderer.
/// The string must be null-terminated string, and will not be mutated, nor will it be referenced
/// after this function returns (i.e., it's fine for it to be a stack-allocated string or a string
/// constant).
///
/// `frag_path`: The path to a SPIR-V fragment shader which will initially be used by the renderer.
/// The string must be null-terminated string, and will not be mutated, nor will it be referenced
/// after this function returns (i.e., it's fine for it to be a stack-allocated string or a string
/// constant).
///
/// `out_renderer`: A non-null pointer to the location where the renderer pointer will be stored.
/// If the renderer is successfully initialized, a pointer to it will be stored here. If an error
/// occurs, the pointer will not be written to. This pointer will not be referenced after this
/// function returns (i.e., it's fine for it to be a stack location).
///
/// ## Return Value
///
/// On success, returns `NULL`. On error, returns a non-null pointer to a null-terminated string
/// describing the error. This string must be freed with `renderer_free_error`.
///
/// ## Example
///
/// ```c
/// char* renderer_init(const char* app_name, const char* vert_path, const char* frag_path,
///     void** out_renderer);
/// void renderer_free_error(char* error);
///
/// int main(void) {
///     void* renderer = NULL;
///     char* error = renderer_init("Vulkan Example", "./vert.spv", "./frag.spv", &renderer);
///     if(error) {
///         fprintf(stderr, "failed to init renderer: %s", error);
///         renderer_free_error(error);
///         exit(1);
///     }
/// }
/// ```
#[no_mangle]
pub unsafe extern "C" fn renderer_init(
    app_name: *const c_char,
    vert_path: *const c_char,
    frag_path: *const c_char,
    out_renderer: *mut *const c_void,
) -> *mut c_char {
    unimplemented!()
}

/// Frees a string returned as an error.
#[no_mangle]
pub extern "C" fn renderer_free_error(error: *mut c_char) {
    unimplemented!()
}
