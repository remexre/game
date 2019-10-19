//! The interface to C and Lisp.
//!
//! ## Example
//!
//! ```c
//! struct Renderer;
//! char* renderer_init(const char* app_name, const char* vert_path, const char* frag_path,
//!     Renderer** out_renderer);
//! void renderer_free(Renderer* renderer);
//! void renderer_free_error(char* error);
//!
//! int main(void) {
//!     void* renderer = NULL;
//!     char* error = renderer_init("Vulkan Example", "./vert.spv", "./frag.spv", &renderer);
//!     if(error) {
//!         fprintf(stderr, "failed to init renderer: %s", error);
//!         renderer_free_error(error);
//!         exit(1);
//!     }
//!
//!     renderer_free(renderer);
//!     return 0;
//! }
//! ```

use crate::Renderer;
use anyhow::Result;
use std::{
    ffi::{CStr, CString},
    os::raw::c_char,
    panic::{catch_unwind, AssertUnwindSafe},
    process::abort,
    ptr,
};

fn catch<F: FnOnce() -> Result<()>>(body: F) -> *mut c_char {
    match catch_unwind(AssertUnwindSafe(body)) {
        Ok(Ok(())) => ptr::null_mut(),
        Ok(Err(err)) => renderer_alloc_error(format!("{:?}", err)),
        Err(panic) => {
            // TODO: Process the panic in some way.
            renderer_alloc_error(format!("Uncaught panic: {:?}", panic))
        }
    }
}

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
#[no_mangle]
pub unsafe extern "C" fn renderer_init(
    app_name: *const c_char,
    vert_path: *const c_char,
    frag_path: *const c_char,
    out_renderer: *mut *mut Renderer,
) -> *mut c_char {
    let app_name = CStr::from_ptr(app_name).to_string_lossy();
    let vert_path = CStr::from_ptr(vert_path).to_string_lossy();
    let frag_path = CStr::from_ptr(frag_path).to_string_lossy();

    catch(|| {
        let r = Renderer::new(&app_name, &*vert_path, &*frag_path)?;
        let r = Box::into_raw(Box::new(r));
        ptr::write(out_renderer, r);
        Ok(())
    })
}

/// Frees a renderer.
///
/// ## Arguments
///
/// `renderer`: The renderer pointer, as returned by `renderer_init`. This pointer must not be used
/// after calling this function.
///
/// ## Return Value
///
/// On success, returns `NULL`. On error, returns a non-null pointer to a null-terminated string
/// describing the error. This string must be freed with `renderer_free_error`.
#[no_mangle]
pub unsafe extern "C" fn renderer_free(renderer: *mut Renderer) -> *mut c_char {
    catch(|| {
        drop(Box::from_raw(renderer));
        Ok(())
    })
}

fn renderer_alloc_error(s: String) -> *mut c_char {
    match CString::new(s) {
        Ok(cstr) => cstr.into_raw(),
        Err(err) => {
            // We must not panic at this point.
            eprintln!("Failed to create CString in renderer_alloc_error: {}", err);
            eprintln!("This ought to be impossible...");
            abort();
        }
    }
}

/// Frees a string returned as an error.
///
/// ## Arguments
///
/// `error`: A pointer returned from another function in this library as an error. This pointer
/// must not be used after calling this function.
#[no_mangle]
pub unsafe extern "C" fn renderer_free_error(error: *mut c_char) {
    drop(CString::from_raw(error))
}
