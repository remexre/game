use crate::{Pipeline, Swapchain};
use anyhow::{Context, Result};
use ash::{
    version::DeviceV1_0,
    vk::{
        CommandBuffer, CommandBufferAllocateInfo, CommandBufferLevel, CommandPool,
        CommandPoolCreateFlags, CommandPoolCreateInfo, Fence, FenceCreateFlags, FenceCreateInfo,
        Framebuffer, FramebufferCreateInfo, RenderPass, Semaphore, SemaphoreCreateInfo,
    },
};
use log::debug;
use std::slice;

/// A wrapper around a bunch of Vulkan state that handles drawing commands to the screen.
///
/// In total, this encapsulates a command pool, and per frame a framebuffer, command buffer, and
/// whatever per-frame state the pipeline wishes to have.
#[derive(Debug)]
pub struct CommandManager<P: Pipeline> {
    pool: CommandPool,
    per_image: Vec<PerImage>,

    current_frame: u32,
    first_frame: bool,

    // pipeline holds the device alive
    pipeline: P,
}

#[derive(Debug)]
struct PerImage {
    framebuffer: Framebuffer,
    cmd_buffer: CommandBuffer,
    image_available_semaphore: Semaphore,
    render_finished_semaphore: Semaphore,
    render_finished_fence: Fence,
}

impl<P: Pipeline> CommandManager<P> {
    /// Creates a new CommandManager wrapping the given Framebuffers, creating a command pool and
    /// corresponding command buffers.
    pub fn new(pipeline: P) -> Result<CommandManager<P>> {
        let swapchain = pipeline.swapchain();

        // Create the command pool.
        let create_info = CommandPoolCreateInfo::builder()
            .flags(CommandPoolCreateFlags::RESET_COMMAND_BUFFER)
            .queue_family_index(swapchain.device.qf);
        let pool = unsafe { swapchain.device.create_command_pool(&create_info, None)? };

        // Create all the per-image stuff.
        let render_pass = unsafe { pipeline.render_pass() };
        let per_image = create_per_image(render_pass, &swapchain, pool)?;

        Ok(CommandManager {
            pool,
            per_image,

            current_frame: 0,
            first_frame: true,

            pipeline,
        })
    }

    fn destroy_per_image(&mut self) {
        unsafe {
            for per_image in self.per_image.drain(..) {
                self.pipeline
                    .swapchain()
                    .device
                    .destroy_fence(per_image.render_finished_fence, None);

                self.pipeline
                    .swapchain()
                    .device
                    .destroy_semaphore(per_image.render_finished_semaphore, None);

                self.pipeline
                    .swapchain()
                    .device
                    .destroy_semaphore(per_image.image_available_semaphore, None);

                self.pipeline
                    .swapchain()
                    .device
                    .destroy_framebuffer(per_image.framebuffer, None);

                // Not ideal that we free them one by one... It'd be shocking if this even showed
                // up in the profiler though, I suppose.
                self.pipeline
                    .swapchain()
                    .device
                    .free_command_buffers(self.pool, slice::from_ref(&per_image.cmd_buffer));
            }
        }
    }

    /// Finishes drawing a frame and starts drawing the next frame.
    pub fn flip(&mut self) -> Result<()> {
        unimplemented!()
    }

    /// Recreates the swapchain, framebuffers, etc.
    pub fn recreate(&mut self) -> Result<()> {
        debug!("CommandManager.recreate() start");

        let device = self.pipeline.swapchain().device.clone();
        let swapchain = Swapchain::new(device)?;
        self.pipeline.recreate(swapchain.clone())?;
        let render_pass = unsafe { self.pipeline.render_pass() };

        self.destroy_per_image();
        self.per_image = create_per_image(render_pass, &swapchain, self.pool)?;

        self.current_frame = 0;
        self.first_frame = true;

        debug!("CommandManager.recreate() end");
        Ok(())
    }
}

impl<P: Pipeline> Drop for CommandManager<P> {
    fn drop(&mut self) {
        unsafe {
            self.destroy_per_image();

            self.pipeline
                .swapchain()
                .device
                .destroy_command_pool(self.pool, None);
        }
    }
}

fn create_per_image(
    render_pass: RenderPass,
    swapchain: &Swapchain,
    pool: CommandPool,
) -> Result<Vec<PerImage>> {
    let num_images = swapchain.images.len() as u32;

    // Create all the command buffers.
    let create_info = CommandBufferAllocateInfo::builder()
        .command_pool(pool)
        .level(CommandBufferLevel::PRIMARY)
        .command_buffer_count(num_images);
    let bufs = unsafe { swapchain.device.allocate_command_buffers(&create_info) }
        .context("Failed to allocate command buffers")?;

    // Create the semaphore and fence's create info outside the loop.
    let sema_create_info = SemaphoreCreateInfo::builder();
    let fence_create_info = FenceCreateInfo::builder().flags(FenceCreateFlags::SIGNALED);

    // Create closures for creating the semaphores and fences. This is mainly to make it nicer to
    // read :P
    let create_sema = || unsafe { swapchain.device.create_semaphore(&sema_create_info, None) };
    let create_fence = || unsafe { swapchain.device.create_fence(&fence_create_info, None) };

    (0..bufs.len())
        .map(|i| {
            let cmd_buffer = bufs[i];
            let image_view = &swapchain.images[i].1;

            // Create the framebuffer.
            let create_info = FramebufferCreateInfo::builder()
                .render_pass(render_pass)
                .attachments(slice::from_ref(image_view))
                .width(swapchain.extent.width)
                .height(swapchain.extent.height)
                .layers(1);
            let framebuffer = unsafe { swapchain.device.create_framebuffer(&create_info, None) }
                .context("Failed to create framebuffer")?;

            // Create the synchronization values.
            let image_available_semaphore = create_sema()?;
            let render_finished_semaphore = create_sema()?;
            let render_finished_fence = create_fence()?;

            Ok(PerImage {
                framebuffer,
                cmd_buffer,
                image_available_semaphore,
                render_finished_semaphore,
                render_finished_fence,
            })
        })
        .collect()
}
