//! The interface to C and Lisp.
//!
//! ## Example
//!
//! See `example.c`

use crate::{ImmutableBuffer, Renderer, Uniforms, Vertex, WindowEvent};
use anyhow::Result;
use std::{
    ffi::{CStr, CString},
    os::raw::{c_char, c_int, c_uint, c_void},
    panic::{catch_unwind, AssertUnwindSafe},
    process::abort,
    ptr, slice,
};

fn catch<F: FnOnce() -> Result<()>>(body: F) -> *mut c_char {
    match catch_unwind(AssertUnwindSafe(body)) {
        Ok(Ok(())) => ptr::null_mut(),
        Ok(Err(err)) => nova_alloc_error(format!("{:?}", err)),
        Err(panic) => {
            // TODO: Process the panic in some way.
            nova_alloc_error(format!("Uncaught panic: {:?}", panic))
        }
    }
}

fn nova_alloc_error(s: String) -> *mut c_char {
    match CString::new(s) {
        Ok(cstr) => cstr.into_raw(),
        Err(err) => {
            // We must not panic at this point.
            eprintln!("Failed to create CString in nova_alloc_error: {}", err);
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
pub unsafe extern "C" fn nova_free_error(error: *mut c_char) {
    if !error.is_null() {
        drop(CString::from_raw(error))
    }
}

/// Attempts to set up Nova.
///
/// ## Arguments
///
/// `app_name`: The name of the application as a null-terminated string, which will not be mutated,
/// and will not be referenced after this function returns (i.e., it's fine for it to be a
/// stack-allocated string or a string constant).
///
/// `debug`: A flag for whether debugging should be enabled.
///
/// `vert_path`: The path to a SPIR-V vertex shader which will initially be used by Nova. The
/// string must be null-terminated string, and will not be mutated, nor will it be referenced after
/// this function returns (i.e., it's fine for it to be a stack-allocated string or a string
/// constant).
///
/// `frag_path`: The path to a SPIR-V fragment shader which will initially be used by Nova. The
/// string must be null-terminated string, and will not be mutated, nor will it be referenced after
/// this function returns (i.e., it's fine for it to be a stack-allocated string or a string
/// constant).
///
/// `out_renderer`: A non-null pointer to the location where the Renderer pointer will be stored.
/// If Nova is successfully initialized, a pointer to it will be stored here. If an error occurs,
/// the pointer will not be written to. This pointer will not be referenced after this function
/// returns (i.e., it's fine for it to be a stack location).
///
/// ## Return Value
///
/// On success, returns `NULL`. On error, returns a non-null pointer to a null-terminated string
/// describing the error. This string must be freed with `nova_free_error`.
#[no_mangle]
pub unsafe extern "C" fn nova_init(
    app_name: *const c_char,
    debug: c_int,
    vert_path: *const c_char,
    frag_path: *const c_char,
    out_renderer: *mut *mut Renderer,
) -> *mut c_char {
    let app_name = CStr::from_ptr(app_name).to_string_lossy();
    let vert_path = CStr::from_ptr(vert_path).to_string_lossy();
    let frag_path = CStr::from_ptr(frag_path).to_string_lossy();

    catch(|| {
        let debug = if debug == 0 { false } else { true };
        let r = Renderer::new(&app_name, debug, &*vert_path, &*frag_path)?;
        let r = Box::into_raw(Box::new(r));
        ptr::write(out_renderer, r);
        Ok(())
    })
}

/// Frees a Nova instance.
///
/// ## Arguments
///
/// `renderer`: The renderer pointer, as obtained from `nova_init`. This pointer must not be used
/// after calling this function.
///
/// ## Return Value
///
/// On success, returns `NULL`. On error, returns a non-null pointer to a null-terminated string
/// describing the error. This string must be freed with `nova_free_error`.
#[no_mangle]
pub unsafe extern "C" fn nova_free(renderer: *mut Renderer) -> *mut c_char {
    catch(|| {
        drop(Box::from_raw(renderer));
        Ok(())
    })
}

/// Finishes rendering the current frame and starts rendering the next one. Also checks for events,
/// returning them.
///
/// ## Arguments
///
/// `renderer`: The renderer pointer, as obtained from `nova_init`.
///
/// `on_event`: The function called for each event.
///
/// `on_event_clo`: Extra data passed to `on_event`. This function will not dereference this
/// pointer, nor store it after this function returns (i.e., it's fine for it to be NULL, a stack
/// location, or a `size_t` cast to a `void*`).
///
/// `out_should_close`: A non-null pointer to the location where the close flag will be stored. If
/// an error occurs, the pointer will not be written to. Otherwise, if the window should be closed,
/// stores 1. In any other case, stores 0. This pointer will not be referenced after this function
/// returns (i.e., it's fine for it to be a stack location).
///
/// ## Return Value
///
/// On success, returns `NULL`. On error, returns a non-null pointer to a null-terminated string
/// describing the error. This string must be freed with `nova_free_error`.
#[no_mangle]
pub unsafe extern "C" fn nova_flip(
    renderer: *mut Renderer,
    on_event: extern "C" fn(*const c_char, *const c_void),
    on_event_clo: *const c_void,
    out_should_close: *mut c_int,
) -> *mut c_char {
    catch(|| {
        let renderer = renderer.as_mut().expect("Got null pointer for renderer");

        for (t, ev) in renderer.flip()? {
            let s = match ev {
                WindowEvent::Key(key, _code, action, _mods) => {
                    format!("({} :key {:?} {:?})", t, key, action)
                }
                ev => {
                    eprintln!("Unsupported event: {:?}", ev);
                    format!("({} :unsupported-event)", t)
                }
            };
            let s = CString::new(s).expect("Invalid CString");
            on_event(s.as_ptr(), on_event_clo);
        }

        let should_close = if renderer.should_close() { 1 } else { 0 };
        ptr::write(out_should_close, should_close);

        Ok(())
    })
}

/// Allocates a VBO.
///
/// ## Arguments
///
/// `renderer`: The renderer pointer, as obtained from `nova_init`. This pointer must not be used
/// after calling this function.
///
/// `vertices`: The vertex data, which will not be mutated, and will not be referenced after this
/// function returns (i.e., it's fine for it to be stack-allocated or a constant).
///
/// `vertex_count`: The number of vertices to read from `vertices`.
///
/// `out_vbo`: A non-null pointer to the location where the VBO pointer will be stored. If the VBO
/// is successfully allocated, a pointer to it will be stored here. If an error occurs, the pointer
/// will not be written to. This pointer will not be referenced after this function returns (i.e.,
/// it's fine for it to be a stack location).
///
/// ## Return Value
///
/// On success, returns `NULL`. On error, returns a non-null pointer to a null-terminated string
/// describing the error. This string must be freed with `nova_free_error`.
#[no_mangle]
pub unsafe extern "C" fn nova_new_vbo(
    renderer: *mut Renderer,
    vertices: *const Vertex,
    vertex_count: c_uint,
    out_vbo: *mut *mut ImmutableBuffer,
) -> *mut c_char {
    catch(|| {
        let renderer = renderer.as_mut().expect("Got null pointer for renderer");
        let vertices = slice::from_raw_parts(vertices, vertex_count as usize);
        let vbo = renderer.new_vbo(&vertices)?;
        ptr::write(out_vbo, Box::into_raw(Box::new(vbo)));
        Ok(())
    })
}

/// Frees a Nova instance.
///
/// ## Arguments
///
/// `vbo`: The VBO pointer, as obtained from `nova_init`. This pointer must not be used
/// after calling this function.
///
/// ## Return Value
///
/// On success, returns `NULL`. On error, returns a non-null pointer to a null-terminated string
/// describing the error. This string must be freed with `nova_free_error`.
#[no_mangle]
pub unsafe extern "C" fn nova_free_vbo(
    _renderer: *mut Renderer,
    vbo: *mut ImmutableBuffer,
) -> *mut c_char {
    catch(|| {
        drop(Box::from_raw(vbo));
        Ok(())
    })
}

/// Sets the title of the window.
///
/// ## Arguments
///
/// `renderer`: The renderer pointer, as obtained from `nova_init`.
///
/// `title`: The name of the application as a null-terminated string, which will not be mutated,
/// and will not be referenced after this function returns (i.e., it's fine for it to be a
/// stack-allocated string or a string constant).
///
/// ## Return Value
///
/// On success, returns `NULL`. On error, returns a non-null pointer to a null-terminated string
/// describing the error. This string must be freed with `nova_free_error`.
#[no_mangle]
pub unsafe extern "C" fn nova_set_title(
    renderer: *mut Renderer,
    title: *const c_char,
) -> *mut c_char {
    catch(|| {
        let renderer = renderer.as_mut().expect("Got null pointer for renderer");

        let title = CStr::from_ptr(title).to_string_lossy();
        renderer.set_title(&title);

        Ok(())
    })
}

/// Draws a VBO to the screen with the given uniforms.
///
/// ## Arguments
///
/// `renderer`: The renderer pointer, as obtained from `nova_init`.
///
/// `vbo`: A VBO handle, as obtained from `nova_new_vbo`.
///
/// `uniforms`: The uniforms to use for this draw call, which will not be mutated, and will not be
/// referenced after this function returns (i.e., it's fine for it to be a stack-allocated value
/// or a constant).
///
/// ## Return Value
///
/// On success, returns `NULL`. On error, returns a non-null pointer to a null-terminated string
/// describing the error. This string must be freed with `nova_free_error`.
#[no_mangle]
pub unsafe extern "C" fn nova_draw_vbo(
    renderer: *mut Renderer,
    vbo: *const ImmutableBuffer,
    uniforms: *const Uniforms,
) -> *mut c_char {
    catch(|| {
        let renderer = renderer.as_mut().expect("Got null pointer for renderer");
        let vbo = vbo.as_ref().expect("Got null pointer for vbo");
        let uniforms = uniforms.as_ref().expect("Got null pointer for uniforms");

        renderer.with_draw(|mut ctx| ctx.draw(vbo, uniforms))
    })
}
