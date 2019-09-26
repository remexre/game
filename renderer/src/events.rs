use crate::RendererState;
use std::{
    fmt::Write,
    panic::{catch_unwind, AssertUnwindSafe},
    process::exit,
    slice,
};
use winit::{ElementState, Event, WindowEvent};

#[no_mangle]
pub extern "C" fn renderer_poll_events(state: &mut RendererState) -> usize {
    catch_unwind(AssertUnwindSafe(|| {
        let mut events = String::new();
        writeln!(&mut events, "(").unwrap();
        state.event_loop.poll_events(|ev| lispify(&mut events, ev));
        writeln!(&mut events, ")").unwrap();
        state.events = events;
        state.events.len() + 1
    }))
    .unwrap_or_else(|e| {
        eprintln!("Caught panic: {:?}", e);
        exit(1);
    })
}

#[no_mangle]
pub extern "C" fn renderer_get_events(state: &mut RendererState, ptr: *mut u8, len: usize) {
    catch_unwind(AssertUnwindSafe(|| {
        let buf = state.events.as_bytes();
        assert!(buf.len() < len);
        let out = unsafe { slice::from_raw_parts_mut(ptr, len) };
        for i in 0..len {
            out[i] = if i < buf.len() { buf[i] } else { 0 };
        }
    }))
    .unwrap_or_else(|e| {
        eprintln!("Caught panic: {:?}", e);
        exit(1);
    })
}

fn lispify(out: &mut String, ev: Event) {
    match ev {
        Event::WindowEvent {
            window_id: _,
            event,
        } => match event {
            WindowEvent::CloseRequested => writeln!(out, ":close-requested").unwrap(),
            WindowEvent::KeyboardInput {
                device_id: _,
                input,
            } => {
                write!(out, "(:keyboard {} ", input.scancode).unwrap();
                *out += match input.state {
                    ElementState::Pressed => ":pressed",
                    ElementState::Released => ":released",
                };
                *out += " (";
                if input.modifiers.shift {
                    *out += " :shift";
                }
                if input.modifiers.ctrl {
                    *out += " :ctrl";
                }
                if input.modifiers.alt {
                    *out += " :alt";
                }
                if input.modifiers.logo {
                    *out += " :logo";
                }
                *out += "))\n";
            }
            _ => {}
        },
        _ => {}
    }
}
