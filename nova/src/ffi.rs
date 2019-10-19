//! The interface to C and Lisp.
//!
//! ## Example
//!
//! See `example.c`

use crate::{bufs::VBO, draw::DrawTarget, Renderer};
use anyhow::Result;
use derivative::Derivative;
use glfw::WindowEvent;
use log::debug;
use std::{
    ffi::{CStr, CString},
    mem::transmute,
    os::raw::{c_char, c_int, c_void},
    panic::{catch_unwind, AssertUnwindSafe},
    process::abort,
    ptr,
};

/// The data carried across the FFI boundary.
///
/// Treat this struct as opaque: its size and contents may change.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Nova {
    renderer: Renderer,
    in_loop: bool,
    #[derivative(Debug = "ignore")]
    on_draw: Box<dyn for<'a> FnMut(DrawTarget<'a>)>,
    #[derivative(Debug = "ignore")]
    on_event: Box<dyn FnMut(f64, WindowEvent)>,
}

/// The data carried across the FFI boundary during drawing.
///
/// Treat this struct as opaque: its size and contents may change.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct NovaDraw {
    target: DrawTarget<'static>,
    valid: bool,
}

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

/// Attempts to set up Nova.
///
/// ## Arguments
///
/// `app_name`: The name of the application as a null-terminated string, which will not be mutated,
/// and will not be referenced after this function returns (i.e., it's fine for it to be a
/// stack-allocated string or a string constant).
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
/// `out_nova`: A non-null pointer to the location where the Nova pointer will be stored. If Nova
/// is successfully initialized, a pointer to it will be stored here. If an error occurs, the
/// pointer will not be written to. This pointer will not be referenced after this function returns
/// (i.e., it's fine for it to be a stack location).
///
/// ## Return Value
///
/// On success, returns `NULL`. On error, returns a non-null pointer to a null-terminated string
/// describing the error. This string must be freed with `nova_free_error`.
#[no_mangle]
pub unsafe extern "C" fn nova_init(
    app_name: *const c_char,
    vert_path: *const c_char,
    frag_path: *const c_char,
    out_nova: *mut *mut Nova,
) -> *mut c_char {
    let app_name = CStr::from_ptr(app_name).to_string_lossy();
    let vert_path = CStr::from_ptr(vert_path).to_string_lossy();
    let frag_path = CStr::from_ptr(frag_path).to_string_lossy();

    catch(|| {
        let r = Nova {
            renderer: Renderer::new(&app_name, &*vert_path, &*frag_path)?,
            in_loop: false,
            on_draw: Box::new(|_| {}),
            on_event: Box::new(|t, ev| debug!("At {}, got {:?}", t, ev)),
        };
        let r = Box::into_raw(Box::new(r));
        ptr::write(out_nova, r);
        Ok(())
    })
}

/// Frees a Nova instance.
///
/// ## Arguments
///
/// `nova`: The Nova pointer, as obtained from `nova_init`. This pointer must not be used after
/// calling this function.
///
/// ## Return Value
///
/// On success, returns `NULL`. On error, returns a non-null pointer to a null-terminated string
/// describing the error. This string must be freed with `nova_free_error`.
#[no_mangle]
pub unsafe extern "C" fn nova_free(nova: *mut Nova) -> *mut c_char {
    catch(|| {
        drop(Box::from_raw(nova));
        Ok(())
    })
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

/// Sets the title of the window.
///
/// ## Arguments
///
/// `nova`: The Nova pointer, as obtained from `nova_init`.
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
pub unsafe extern "C" fn nova_set_title(nova: *mut Nova, title: *const c_char) -> *mut c_char {
    catch(|| {
        let nova = nova.as_mut().expect("Got null pointer for nova");

        let title = CStr::from_ptr(title).to_string_lossy();
        nova.renderer.window.set_title(&title);

        Ok(())
    })
}

/// Registers a function to be called for drawing.
///
/// ## Arguments
///
/// `nova`: The Nova pointer, as obtained from `nova_init`.
///
/// `func`: The function that does the drawing.
///
/// `ctx`: An argument passed to `func` every time it's called.
///
/// ## Return Value
///
/// On success, returns `NULL`. On error, returns a non-null pointer to a null-terminated string
/// describing the error. This string must be freed with `nova_free_error`.
#[no_mangle]
pub unsafe extern "C" fn nova_on_draw(
    nova: *mut Nova,
    func: extern "C" fn(draw: *mut NovaDraw, ctx: *mut c_void),
    ctx: *mut c_void,
) -> *mut c_char {
    catch(|| {
        let nova = nova.as_mut().expect("Got null pointer for nova");
        assert!(!nova.in_loop);
        nova.on_draw = Box::new(move |target| {
            let mut draw = NovaDraw {
                target: transmute::<DrawTarget<'_>, DrawTarget<'static>>(target),
                valid: true,
            };
            func(&mut draw, ctx);
            draw.valid = false;
        });
        Ok(())
    })
}

/// Polls and processes events, then starts a draw operation.
///
/// Calls the function registered by `nova_on_event` with each event.
///
/// Next, calls the function registered by `nova_on_draw`.
///
/// ## Arguments
///
/// `nova`: The Nova pointer, as obtained from `nova_init`.
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
pub unsafe extern "C" fn nova_loop(nova: *mut Nova, out_should_close: *mut c_int) -> *mut c_char {
    catch(|| {
        let nova = nova.as_mut().expect("Got null pointer for nova");

        assert!(!nova.in_loop);
        nova.in_loop = true;

        let on_event = &mut nova.on_event;
        let on_draw = &mut nova.on_draw;

        nova.renderer
            .poll_events()
            .for_each(|(t, ev)| (on_event)(t, ev));

        nova.renderer.draw(|target| {
            (on_draw)(target);
            Ok(())
        })?;

        nova.in_loop = false;

        ptr::write(
            out_should_close,
            if nova.renderer.should_close() { 1 } else { 0 },
        );
        Ok(())
    })
}

/// Draws a VBO to the G-buffer. **TODO**: Not actually true; we're doing forward rendering rn.
///
/// This must only be called from the `on_draw` function called by `nova_loop`.
///
/// ## Arguments
///
/// `draw`: The NovaDraw pointer.
///
/// ## Return Value
///
/// On success, returns `NULL`. On error, returns a non-null pointer to a null-terminated string
/// describing the error. This string must be freed with `nova_free_error`.
#[no_mangle]
pub unsafe extern "C" fn nova_draw_vbo<'a>(
    draw: *mut NovaDraw,
    vbo: *mut VBO,
    model: *const [f32; 16],
    view: *const [f32; 16],
    proj: *const [f32; 16],
    specularity: f32,
) -> *mut c_char {
    static IDENTITY_MAT4: [f32; 16] = [
        1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0,
    ];

    catch(|| {
        let draw = draw.as_mut().expect("Got null pointer for draw");
        assert!(draw.valid);

        let vbo = vbo.as_mut().expect("Got null pointer for vbo");
        // TODO: Bind

        let model = model.as_ref().unwrap_or(&IDENTITY_MAT4);
        let view = view.as_ref().unwrap_or(&IDENTITY_MAT4);
        let proj = proj.as_ref().unwrap_or(&IDENTITY_MAT4);
        // TODO: Uniforms

        draw.target.draw(vbo.num_vertices);

        Ok(())
    })
}
