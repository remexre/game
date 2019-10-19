use anyhow::{Context, Result};
use ash::{
    extensions::khr::{Surface, Swapchain},
    version::DeviceV1_0,
    vk::{
        CommandBuffer, CommandPool, Extent2D, Fence, Format, Framebuffer, Image, ImageView,
        PhysicalDevice, Pipeline, Queue, QueueFamily, RenderPass, Result as VkResult, Semaphore,
        ShaderStageFlags, SurfaceKHR, SwapchainKHR,
    },
    Device, Entry, Instance,
};
use derivative::Derivative;
use glfw::{Glfw, Window, WindowEvent};
use log::info;
use std::{path::Path, sync::mpsc::Receiver};

#[macro_use]
pub mod utils;

pub mod cmds;
pub mod imgs;
pub mod init;
pub mod pipeline;
pub mod shaders;
pub mod sync;

#[derive(Derivative)]
#[derivative(Debug)]
pub struct Renderer {
    #[derivative(Debug = "ignore")]
    glfw: Glfw,
    #[derivative(Debug = "ignore")]
    window: Window,
    events: Receiver<(f64, WindowEvent)>,
    #[derivative(Debug = "ignore")]
    entry: Entry,
    #[derivative(Debug = "ignore")]
    instance: Instance,
    surface: SurfaceKHR,
    #[derivative(Debug = "ignore")]
    surface_ext: Surface,
    pd: PhysicalDevice,
    qf: QueueFamily,
    #[derivative(Debug = "ignore")]
    dev: Device,
    queue: Queue,
    has_rtx: bool,
    vert_stage: (),
    frag_stage: (),
    #[derivative(Debug = "ignore")]
    swapchain_ext: Swapchain,
    swapchain: SwapchainKHR,
    images: Vec<Image>,
    image_views: Vec<ImageView>,
    format: Format,
    dims: Extent2D,
    render_pass: RenderPass,
    framebuffers: Vec<Framebuffer>,
    pipeline: Pipeline,
    command_pool: CommandPool,
    command_buffers: Vec<CommandBuffer>,
    frame_num: usize,
    image_available_semaphores: Vec<Semaphore>,
    render_finished_semaphores: Vec<Semaphore>,
    render_finished_fences: Vec<Fence>,
}

impl Renderer {
    /// Creates a new Renderer with the given window name.
    pub fn new(
        name: &str,
        vert_path: impl AsRef<Path>,
        frag_path: impl AsRef<Path>,
    ) -> Result<Renderer> {
        let (glfw, window, events) =
            init::create_window(name).context("Failed to create window")?;
        let entry = Entry::new().context("Failed to get a Vulkan entrypoint")?;
        let instance = init::create_instance(&glfw, &entry, true)
            .context("Failed to create a Vulkan instance")?;
        let surface = init::create_surface(&instance, &window)
            .context("Failed to create a Vulkan surface")?;

        let surface_ext = Surface::new(&entry, &instance);
        let (pd, qf) =
            init::choose_physical_device_and_queue_family(&instance, &surface_ext, surface)
                .context("Failed to choose a physical device and queue family")?;
        let (dev, queue, has_rtx) =
            init::create_device(&instance, pd, qf).context("Failed to create a Vulkan device")?;
        if has_rtx {
            info!("RTX found!");
        }

        let (_, vert_stage) = shaders::load_shader(&dev, vert_path, ShaderStageFlags::VERTEX)
            .context("Failed to load vertex shader")?;
        let (_, frag_stage) = shaders::load_shader(&dev, frag_path, ShaderStageFlags::FRAGMENT)
            .context("Failed to load fragment shader")?;

        let swapchain_ext = Swapchain::new(&instance, &dev);
        let (swapchain, images, image_views, format, dims) =
            init::create_swapchain(&surface_ext, &swapchain_ext, surface, pd, &dev)
                .context("Failed to create swapchain")?;
        let num_images = images.len() as u32;

        let (render_pass, pipeline) =
            pipeline::create_graphics_pipeline(&dev, format, dims, vert_stage, frag_stage)
                .context("Failed to create graphics pipeline")?;

        let framebuffers = image_views
            .iter()
            .map(|image_view| init::create_framebuffer(&dev, image_view, dims, render_pass))
            .collect::<Result<Vec<_>>>()?;

        let command_pool = cmds::create_command_pool(&dev, qf)?;
        let command_buffers = cmds::create_command_buffers(&dev, command_pool, num_images)?;

        let image_available_semaphores = sync::create_semaphores(&dev, num_images)?;
        let render_finished_semaphores = sync::create_semaphores(&dev, num_images)?;
        let render_finished_fences = sync::create_fences(&dev, true, num_images)?;

        Ok(Renderer {
            glfw,
            window,
            events,
            entry,
            instance,
            surface,
            surface_ext,
            pd,
            qf,
            dev,
            queue,
            has_rtx,
            vert_stage,
            frag_stage,
            swapchain_ext,
            swapchain,
            images,
            image_views,
            format,
            dims,
            render_pass,
            pipeline,
            framebuffers,
            command_pool,
            command_buffers,
            frame_num: 0,
            image_available_semaphores,
            render_finished_semaphores,
            render_finished_fences,
        })
    }

    pub fn draw<F: FnOnce() -> Result<()>>(&mut self, f: F) -> Result<()> {
        match self.draw_inner(f) {
            Ok(()) => Ok(()),
            Err(ref err) if err.downcast_ref() == Some(&VkResult::ERROR_OUT_OF_DATE_KHR) => {
                self.recreate_framebuffer()
            }
            Err(err) => Err(err),
        }
    }

    fn draw_inner<F: FnOnce() -> Result<()>>(&mut self, f: F) -> Result<()> {
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

        // TODO Call f

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

    fn recreate_framebuffer(&mut self) -> Result<()> {
        let (swapchain, images, image_views, format, dims) = init::create_swapchain(
            &self.surface_ext,
            &self.swapchain_ext,
            self.surface,
            self.pd,
            &self.dev,
        )
        .context("Failed to create swapchain")?;
        let num_images = images.len() as u32;

        let (render_pass, pipeline) = pipeline::create_graphics_pipeline(
            &self.dev,
            format,
            dims,
            self.vert_stage,
            self.frag_stage,
        )
        .context("Failed to create graphics pipeline")?;

        let framebuffers = image_views
            .iter()
            .map(|image_view| init::create_framebuffer(&self.dev, image_view, dims, render_pass))
            .collect::<Result<Vec<_>>>()?;

        let command_pool = cmds::create_command_pool(&self.dev, self.qf)?;
        let command_buffers = cmds::create_command_buffers(&self.dev, command_pool, num_images)?;
        unimplemented!()
    }

    pub fn poll_events<'a>(&'a mut self) -> impl 'a + Iterator<Item = (f64, WindowEvent)> {
        self.glfw.poll_events();
        self.events.try_iter()
    }

    pub fn should_close(&self) -> bool {
        self.window.should_close()
    }

    pub fn wait_idle(&self) -> Result<()> {
        let () = unsafe { self.dev.device_wait_idle() }?;
        Ok(())
    }
}
