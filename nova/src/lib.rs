//! A Vulkan-based renderer.
//!
//! See `nova/examples/nova.rs` for an example of usage.
#![deny(missing_docs)]

// pub mod ffi;

use anyhow::{Context, Result};
use lye::*;
use std::{path::Path, sync::Arc};

/// A convenient renderer object wrapping up `lye`.
///
/// Eventually, this renderer will perform deferred rendering, with optional raytraced reflections
/// and shadows.
///
/// Currently, it performs simple forward rendering.
#[derive(Debug)]
pub struct Renderer {
    window: Window,
    instance: Arc<Instance>,
    device: Arc<Device>,
    command_manager: CommandManager<ForwardPipeline>,
}

impl Renderer {
    /// Creates a new Renderer with the given window name, and shaders corresponding to the given
    /// paths.
    pub fn new(
        name: &str,
        debug: bool,
        vert_path: impl AsRef<Path>,
        frag_path: impl AsRef<Path>,
    ) -> Result<Renderer> {
        let window = Window::new(name).context("Failed to create Window")?;
        let instance = Instance::new(&window, debug).context("Failed to create Instance")?;
        let device = Device::new(&window, instance.clone()).context("Failed to create Device")?;

        let vert = Shader::load_from_path(device.clone(), vert_path, ShaderStageFlags::VERTEX)
            .context("Failed to load vertex shader")?;
        let frag = Shader::load_from_path(device.clone(), frag_path, ShaderStageFlags::FRAGMENT)
            .context("Failed to load fragment shader")?;

        let swapchain = Swapchain::new(device.clone())?;
        let pipeline = ForwardPipeline::new(swapchain, vert, frag)?;
        let command_manager = CommandManager::new(pipeline)?;

        Ok(Renderer {
            window,
            instance,
            device,
            command_manager,
        })
    }

    /// Finishes rendering the current frame and starts rendering the next one. Also checks for
    /// events, returning them.
    pub fn flip(&mut self) -> Result<Vec<(f64, WindowEvent)>> {
        // Try to flip.
        let mut recreate = false;
        if let Err(err) = self.command_manager.flip() {
            if err.downcast_ref() == Some(&VkResult::ERROR_OUT_OF_DATE_KHR) {
                recreate = true;
            } else {
                return Err(err);
            }
        }

        // Snag the events, checking if we need to resize.
        let events = self
            .window
            .poll_events()
            .drain(..)
            .filter(|(_, ev)| match ev {
                WindowEvent::Size(_, _) => {
                    recreate = true;
                    false
                }
                _ => true,
            })
            .collect();

        // If we need to resize, do so.
        if recreate {
            self.command_manager.recreate()?;
        }

        Ok(events)
    }

    /*
    pub fn draw<F: for<'a> FnOnce(DrawTarget<'a>) -> Result<()>>(&mut self, body: F) -> Result<()> {
        let recreate = match self.draw_inner(body) {
            Ok(()) => false,
            Err(ref err) if err.downcast_ref() == Some(&VkResult::ERROR_OUT_OF_DATE_KHR) => true,
            Err(err) => return Err(err),
        };
        if recreate || self.resized {
            self.resized = false;
            self.recreate_framebuffer()?;
        }
        Ok(())
    }

    fn draw_inner<'a, F: FnOnce(DrawTarget<'a>) -> Result<()>>(
        &'a mut self,
        body: F,
    ) -> Result<()> {
        let i = cmds::draw_start(
            &self.swapchain_ext,
            self.swapchain,
            image_available_semaphore,
        )?;

        let framebuffer = self.framebuffers[i as usize];

        cmds::begin_command_buffer(&self.dev, command_buffer, &render_finished_fence)?;
        cmds::begin_render_pass(
            &self.dev,
            command_buffer,
            self.render_pass,
            framebuffer,
            self.dims,
        );
        cmds::bind_pipeline(&self.dev, command_buffer, self.pipeline);

        body(DrawTarget {
            dev: &self.dev,
            command_buffer: command_buffer,
        })?;
    */

    /// Sets the title of the window.
    pub fn set_title(&mut self, title: &str) {
        self.window.set_title(title);
    }

    /// Returns whether the window should close.
    pub fn should_close(&self) -> bool {
        self.window.should_close()
    }
}
