mod draw;
mod flip;
mod initialize;

use derivative::Derivative;
use std::{
    panic::{catch_unwind, AssertUnwindSafe},
    process::exit,
    sync::Arc,
};
use vulkano::{
    device::{Device, Queue},
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
    #[derivative(Debug = "ignore")]
    images: Vec<Arc<SwapchainImage<Window>>>,
    instance: Arc<Instance>,
    queue: Arc<Queue>,
    surface: Arc<Surface<Window>>,
    swapchain: Arc<Swapchain<Window>>,
    recreate_swapchain: bool,

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
    drop(state);
}

#[no_mangle]
pub extern "C" fn renderer_flip(state: &mut RendererState) {
    catch_unwind(AssertUnwindSafe(|| state.flip())).unwrap_or_else(|e| {
        eprintln!("Caught panic: {:?}", e);
        exit(1);
    })
}
