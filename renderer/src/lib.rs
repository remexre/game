mod util;

use std::sync::Arc;
use vulkano::instance::{Instance, InstanceExtensions};

#[derive(Debug)]
pub struct RendererState {
    pub instance: Arc<Instance>,
}

#[no_mangle]
pub extern "C" fn renderer_init() -> Box<RendererState> {
    let instance = must!(Instance::new(None, &InstanceExtensions::none(), None));
    eprintln!("Got instance!");
    Box::new(RendererState { instance })
}

#[no_mangle]
pub extern "C" fn renderer_exit(state: Box<RendererState>) {
    eprintln!("Freeing renderer state!");
    drop(state);
}
