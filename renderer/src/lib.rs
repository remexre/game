use ash::{
    extensions::khr::{Surface, Swapchain},
    Entry,
};
use derivative::Derivative;
use glfw::{Glfw, Window, WindowEvent};
use libremexre::errors::Result;
use std::sync::mpsc::Receiver;

#[macro_use]
mod utils;

pub mod imgs;
pub mod init;
pub mod pipeline;
pub mod shaders;

#[derive(Derivative)]
#[derivative(Debug)]
pub struct Renderer {
    events: Receiver<(f64, WindowEvent)>,

    #[derivative(Debug = "ignore")]
    glfw: Glfw,

    #[derivative(Debug = "ignore")]
    window: Window,
}

impl Renderer {
    /// Creates a new Renderer with the given window name.
    pub fn new(name: &str) -> Result<Renderer> {
        let (mut glfw, window, events) = init::create_window(name)?;
        let entry = Entry::new()?;
        let instance = init::create_instance(&glfw, &entry, true)?;
        let surface = init::create_surface(&instance, &window)?;

        let surface_ext = Surface::new(&entry, &instance);
        let (pd, qf) =
            init::choose_physical_device_and_queue_family(&instance, &surface_ext, surface)?;
        let (dev, queue) = init::create_device(&instance, pd, qf)?;
        let swapchain_ext = Swapchain::new(&instance, &dev);
        let (swapchain, images) =
            init::create_swapchain(&surface_ext, &swapchain_ext, surface, pd)?;

        Ok(Renderer {
            events,
            glfw,
            window,
        })
    }

    pub fn poll_events<'a>(&'a mut self) -> impl 'a + Iterator<Item = (f64, WindowEvent)> {
        self.glfw.poll_events();
        self.events.try_iter()
    }

    pub fn should_close(&self) -> bool {
        self.window.should_close()
    }
}
