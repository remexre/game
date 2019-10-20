//! A Vulkan-based renderer.
//!
//! See `examples/lye.rs` for an example of usage.

#[macro_use]
mod utils;

// mod bufs;
// mod cmds;
// mod draw;
// mod imgs;
// mod init;
// mod pipeline;
// mod shaders;
// mod sync;

// pub mod ffi;
pub mod lye;

use crate::lye::*;
use anyhow::{Context, Result};
use std::{path::Path, sync::Arc};

/*
pub use crate::{
    bufs::{Vertex, VBO},
    draw::DrawTarget,
};
use ash::{
    extensions::khr::{Surface, Swapchain},
    version::DeviceV1_0,
    vk::{
        CommandBuffer, CommandPool, Extent2D, Fence, Format, Framebuffers, Image, ImageView,
        PhysicalDevice, Pipeline, PipelineShaderStageCreateInfo, Queue, RenderPass,
        Result as VkResult, Semaphore, ShaderStageFlags, SurfaceKHR, SwapchainKHR,
    },
    Device, Entry, Instance,
};
use derivative::Derivative;
use glfw::{Glfw, Window, WindowEvent};
use log::info;
use std::sync::mpsc::Receiver;
*/

/// A convenient renderer object wrapping up `lye`, and the interface exposed to FFI.
///
/// Eventually, this renderer will perform deferred rendering, with optional raytraced reflections
/// and shadows.
///
/// Currently, it performs simple forward rendering, but with slightly less driver overhead.
#[derive(Debug)]
pub struct Renderer {
    window: Arc<Window>,
    instance: Arc<Instance>,
    device: Arc<Device>,

    vert: Arc<Shader>,
    frag: Arc<Shader>,

    framebuffers: Framebuffers<ForwardPipeline>,
    /*
    command_pool: CommandPool,
    command_buffers: Vec<CommandBuffer>,
    frame_num: usize,
    image_available_semaphores: Vec<Semaphore>,
    render_finished_semaphores: Vec<Semaphore>,
    render_finished_fences: Vec<Fence>,
    */
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
        let pipeline = ForwardPipeline::new(swapchain, vert.clone(), frag.clone())?;
        let framebuffers = Framebuffers::new(pipeline)?;

        /*
        let command_pool = cmds::create_command_pool(&dev, qf)?;
        let command_buffers = cmds::create_command_buffers(&dev, command_pool, num_images)?;

        let image_available_semaphores = sync::create_semaphores(&dev, num_images)?;
        let render_finished_semaphores = sync::create_semaphores(&dev, num_images)?;
        let render_finished_fences = sync::create_fences(&dev, true, num_images)?;
        */

        Ok(Renderer {
            window,
            instance,
            device,

            vert,
            frag,

            framebuffers,
            /*
            command_pool,
            command_buffers,
            frame_num: 0,
            image_available_semaphores,
            render_finished_semaphores,
            render_finished_fences,
            */
        })
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
        let image_available_semaphore = self.image_available_semaphores[self.frame_num];
        let render_finished_semaphore = self.render_finished_semaphores[self.frame_num];
        let render_finished_fence = self.render_finished_fences[self.frame_num];
        let command_buffer = self.command_buffers[self.frame_num];
        self.frame_num = (self.frame_num + 1) % self.images.len();

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

        cmds::end_render_pass(&self.dev, command_buffer);
        cmds::end_command_buffer(&self.dev, command_buffer)?;
        cmds::submit_command_buffer(
            &self.dev,
            self.queue,
            &command_buffer,
            &image_available_semaphore,
            &render_finished_semaphore,
            render_finished_fence,
        )?;
        cmds::present(
            &self.swapchain_ext,
            self.queue,
            &self.swapchain,
            i,
            &render_finished_semaphore,
        )?;
        Ok(())
    }
    */

    fn recreate_framebuffer(&mut self) -> Result<()> {
        let swapchain = Swapchain::new(self.device.clone())?;
        let pipeline = ForwardPipeline::new(swapchain, self.vert.clone(), self.frag.clone())?;
        let framebuffers = Framebuffers::new(pipeline)?;

        unimplemented!()

        /*
            let command_pool = cmds::create_command_pool(&self.dev, self.qf)?;
            let command_buffers = cmds::create_command_buffers(&self.dev, command_pool, num_images)?;

            Ok(())
        */
    }

    /// Returns a reference to the Window inside the Renderer.
    pub fn window(&self) -> &Window {
        &self.window
    }
}
