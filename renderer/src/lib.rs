use ash::{
    extensions::khr::{Surface, Swapchain},
    vk::ShaderStageFlags,
    Entry,
};
use derivative::Derivative;
use glfw::{Glfw, Window, WindowEvent};
use libremexre::errors::Result;
use log::info;
use std::{path::Path, sync::mpsc::Receiver};

#[macro_use]
pub mod utils;

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
    pub fn new(
        name: &str,
        vert_path: impl AsRef<Path>,
        frag_path: impl AsRef<Path>,
    ) -> Result<Renderer> {
        let (glfw, window, events) = init::create_window(name)?;
        let entry = Entry::new()?;
        let instance = init::create_instance(&glfw, &entry, true)?;
        let surface = init::create_surface(&instance, &window)?;

        let surface_ext = Surface::new(&entry, &instance);
        let (pd, qf) =
            init::choose_physical_device_and_queue_family(&instance, &surface_ext, surface)?;
        let (dev, queue, has_rtx) = init::create_device(&instance, pd, qf)?;
        if has_rtx {
            info!("RTX found!");
        }
        let swapchain_ext = Swapchain::new(&instance, &dev);
        let (swapchain, images, image_views, format, dims) =
            init::create_swapchain(&surface_ext, &swapchain_ext, surface, pd, &dev)?;

        let (_, vert_stage) = shaders::load_shader(&dev, vert_path, ShaderStageFlags::VERTEX)?;
        let (_, frag_stage) = shaders::load_shader(&dev, frag_path, ShaderStageFlags::FRAGMENT)?;

        let pipeline =
            pipeline::create_graphics_pipeline(&dev, format, dims, vert_stage, frag_stage)?;

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
