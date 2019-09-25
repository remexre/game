mod bufs;
mod draw;
mod events;
mod flip;
mod init;
mod pass;

use crate::pass::GameRenderPass;
use derivative::Derivative;
use std::{
    ffi::CStr,
    panic::{catch_unwind, AssertUnwindSafe},
    process::exit,
    sync::Arc,
};
use vulkano::{
    command_buffer::AutoCommandBufferBuilder,
    device::{Device, Queue},
    framebuffer::RenderPass,
    image::SwapchainImage,
    instance::Instance,
    swapchain::{Surface, Swapchain},
    sync::GpuFuture,
};
use winit::{EventsLoop, Window};

#[derive(Derivative)]
#[derivative(Debug)]
pub struct RendererState {
    device: Arc<Device>,
    event_loop: EventsLoop,
    events: String,
    #[derivative(Debug = "ignore")]
    images: Vec<Arc<SwapchainImage<Window>>>,
    instance: Arc<Instance>,
    queue: Arc<Queue>,
    surface: Arc<Surface<Window>>,
    swapchain: Arc<Swapchain<Window>>,
    render_pass: Arc<RenderPass<GameRenderPass>>,
    recreate_swapchain: bool,

    #[derivative(Debug = "ignore")]
    command_buffer_builder: Option<AutoCommandBufferBuilder>,
    image_num: usize,

    #[derivative(Debug = "ignore")]
    cleanup_future: Option<Box<dyn GpuFuture + Send>>,
}

#[no_mangle]
pub extern "C" fn renderer_init() -> Box<RendererState> {
    catch_unwind(|| Box::new(RendererState::new(None, None))).unwrap_or_else(|e| {
        eprintln!("Caught panic: {:?}", e);
        exit(1);
    })
}

#[no_mangle]
pub extern "C" fn renderer_exit(state: Box<RendererState>) {
    catch_unwind(AssertUnwindSafe(|| drop(state))).unwrap_or_else(|e| {
        eprintln!("Caught panic: {:?}", e);
        exit(1);
    })
}

#[no_mangle]
pub extern "C" fn renderer_draw(state: &mut RendererState, r: f32, g: f32, b: f32, a: f32) {
    catch_unwind(AssertUnwindSafe(|| state.draw([r, g, b, a]))).unwrap_or_else(|e| {
        eprintln!("Caught panic: {:?}", e);
        exit(1);
    })
}

#[no_mangle]
pub extern "C" fn renderer_set_title(state: &mut RendererState, ptr: *const i8) {
    catch_unwind(AssertUnwindSafe(|| {
        let title = unsafe { CStr::from_ptr(ptr) }.to_string_lossy();
        state.surface.window().set_title(&title)
    }))
    .unwrap_or_else(|e| {
        eprintln!("Caught panic: {:?}", e);
        exit(1);
    })
}
