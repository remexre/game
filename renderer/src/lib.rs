mod util;

use std::sync::Arc;
use vulkano::{instance::Instance, swapchain::Surface};
use vulkano_win::VkSurfaceBuild;
use winit::{EventsLoop, Window, WindowBuilder};

#[derive(Debug)]
pub struct RendererState {
    event_loop: EventsLoop,
    instance: Arc<Instance>,
    surface: Arc<Surface<Window>>,
}

#[no_mangle]
pub extern "C" fn renderer_init() -> Box<RendererState> {
    let instance_extensions = vulkano_win::required_extensions();
    let instance = must!(Instance::new(None, &instance_extensions, None));
    let event_loop = EventsLoop::new();
    let surface = must!(WindowBuilder::new().build_vk_surface(&event_loop, instance.clone()));
    Box::new(RendererState {
        event_loop,
        instance,
        surface,
    })
}

#[no_mangle]
pub extern "C" fn renderer_exit(state: Box<RendererState>) {
    drop(state);
}
