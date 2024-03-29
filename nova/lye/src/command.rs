use crate::{DrawContext, MutableBuffer, Pipeline, Swapchain, Uniforms};
use anyhow::{Context, Result};
use ash::{
    version::DeviceV1_0,
    vk::{
        BufferUsageFlags, ClearColorValue, ClearValue, CommandBuffer, CommandBufferAllocateInfo,
        CommandBufferBeginInfo, CommandBufferLevel, CommandBufferResetFlags,
        CommandBufferUsageFlags, CommandPool, CommandPoolCreateFlags, CommandPoolCreateInfo,
        DescriptorBufferInfo, DescriptorPoolCreateInfo, DescriptorPoolSize, DescriptorSet,
        DescriptorSetAllocateInfo, DescriptorSetLayout, DescriptorType, Fence, FenceCreateFlags,
        FenceCreateInfo, Framebuffer, FramebufferCreateInfo, Offset2D, PipelineBindPoint,
        PipelineStageFlags, PresentInfoKHR, Rect2D, RenderPass, RenderPassBeginInfo,
        Result as VkResult, Semaphore, SemaphoreCreateInfo, SubmitInfo, SubpassContents,
        WriteDescriptorSet,
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
    framebuffers: Vec<Framebuffer>,
    per_image: Vec<PerImage>,

    current_frame: usize,
    image_index: Option<u32>,

    // pipeline holds the device alive
    pipeline: P,
}

#[derive(Debug)]
struct PerImage {
    cmd_buffer: CommandBuffer,
    image_available_semaphore: Semaphore,
    render_finished_semaphore: Semaphore,
    render_finished_fence: Fence,

    descriptor_set: DescriptorSet,
    ubo: MutableBuffer<Uniforms>,
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
        let framebuffers = create_framebuffers(pipeline.render_pass(), &swapchain)?;
        let descriptor_set_layout = pipeline.uniform_descriptor_set_layout();
        let per_image = create_per_image(&swapchain, descriptor_set_layout, pool)?;

        Ok(CommandManager {
            pool,
            framebuffers,
            per_image,

            current_frame: 0,
            image_index: None,

            pipeline,
        })
    }

    fn destroy_framebuffers(&mut self) {
        for framebuffer in self.framebuffers.drain(..) {
            unsafe {
                self.pipeline
                    .swapchain()
                    .device
                    .destroy_framebuffer(framebuffer, None);
            }
        }
    }

    fn destroy_per_image(&mut self) {
        for per_image in self.per_image.drain(..) {
            unsafe {
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
        let swapchain = self.pipeline.swapchain();
        let device = &swapchain.device;

        // If it's the first frame, there's no previous frame to finish.
        if let Some(image_index) = self.image_index.take() {
            let per_image = &self.per_image[self.current_frame];

            // End the render pass and command buffer.
            unsafe {
                device.cmd_end_render_pass(per_image.cmd_buffer);
                device.end_command_buffer(per_image.cmd_buffer)?;
            }

            // Submit the command buffer to the GPU.
            let wait_dst_stage_mask = [PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT];
            let submit_info = SubmitInfo::builder()
                .wait_semaphores(slice::from_ref(&per_image.image_available_semaphore))
                .wait_dst_stage_mask(&wait_dst_stage_mask)
                .command_buffers(slice::from_ref(&per_image.cmd_buffer))
                .signal_semaphores(slice::from_ref(&per_image.render_finished_semaphore));
            unsafe {
                device.queue_submit(
                    device.queue,
                    slice::from_ref(&submit_info),
                    per_image.render_finished_fence,
                )?;
            }

            // Present the framebuffer to the screen.
            let image_indices = [image_index];
            let present_info = PresentInfoKHR::builder()
                .wait_semaphores(slice::from_ref(&per_image.render_finished_semaphore))
                .swapchains(slice::from_ref(&swapchain.swapchain))
                .image_indices(&image_indices);
            let suboptimal = unsafe {
                device
                    .swapchain_ext
                    .queue_present(device.queue, &present_info)?
            };
            if suboptimal {
                return Err(VkResult::ERROR_OUT_OF_DATE_KHR.into());
            }

            // Advance to the next image.
            self.current_frame = (self.current_frame + 1) % self.per_image.len();
        }

        let per_image = &self.per_image[self.current_frame];

        // Get an image.
        let (image_index, suboptimal) = unsafe {
            device.swapchain_ext.acquire_next_image(
                swapchain.swapchain,
                std::u64::MAX,
                per_image.image_available_semaphore,
                Fence::null(),
            )?
        };
        if suboptimal {
            return Err(VkResult::ERROR_OUT_OF_DATE_KHR.into());
        }
        let framebuffer = self.framebuffers[image_index as usize];

        // Begin the command buffer.
        let begin_info =
            CommandBufferBeginInfo::builder().flags(CommandBufferUsageFlags::SIMULTANEOUS_USE);
        unsafe {
            device.wait_for_fences(
                slice::from_ref(&per_image.render_finished_fence),
                true,
                std::u64::MAX,
            )?;
            device.reset_fences(slice::from_ref(&per_image.render_finished_fence))?;

            device.reset_command_buffer(per_image.cmd_buffer, CommandBufferResetFlags::empty())?;
            device.begin_command_buffer(per_image.cmd_buffer, &begin_info)?;
        }

        // Bind the pipeline and descriptor set to the command buffer.
        unsafe {
            device.cmd_bind_pipeline(
                per_image.cmd_buffer,
                PipelineBindPoint::GRAPHICS,
                self.pipeline.handle(),
            );
            device.cmd_bind_descriptor_sets(
                per_image.cmd_buffer,
                PipelineBindPoint::GRAPHICS,
                self.pipeline.layout(),
                0,
                &[per_image.descriptor_set],
                &[],
            );
        }

        // Begin the render pass.
        let clear_values = [ClearValue {
            color: ClearColorValue {
                float32: [0.0, 0.0, 0.0, 1.0],
            },
        }];
        let begin_info = RenderPassBeginInfo::builder()
            .render_pass(self.pipeline.render_pass())
            .framebuffer(framebuffer)
            .render_area(Rect2D {
                extent: swapchain.extent,
                offset: Offset2D { x: 0, y: 0 },
            })
            .clear_values(&clear_values);
        unsafe {
            device.cmd_begin_render_pass(
                per_image.cmd_buffer,
                &begin_info,
                SubpassContents::INLINE,
            );
        }

        self.image_index = Some(image_index);
        Ok(())
    }

    /// Recreates the swapchain, framebuffers, etc.
    pub fn recreate(&mut self) -> Result<()> {
        debug!("CommandManager.recreate() start");

        let device = self.pipeline.swapchain().device.clone();
        device.wait_idle()?;

        let swapchain = Swapchain::new(device)?;
        self.pipeline.recreate(swapchain.clone())?;

        self.destroy_framebuffers();
        self.framebuffers = create_framebuffers(self.pipeline.render_pass(), &swapchain)?;

        self.destroy_per_image();
        let descriptor_set_layout = self.pipeline.uniform_descriptor_set_layout();
        self.per_image = create_per_image(&swapchain, descriptor_set_layout, self.pool)?;

        self.current_frame = 0;
        self.image_index = None;

        debug!("CommandManager.recreate() end");
        Ok(())
    }

    /// Calls the closure with the current frame's command buffer. `flip` must have been called
    /// before this method.
    pub fn with_draw_context_and_pipeline<'a, F: FnOnce(DrawContext<'a>, &'a P) -> Result<()>>(
        &'a mut self,
        body: F,
    ) -> Result<()> {
        if self.image_index.is_none() {
            debug!("Skipping draw (no available image, we might've just resized?)");
            return Ok(());
        }

        let per_image = &mut self.per_image[self.current_frame];

        body(
            DrawContext {
                cmd_buffer: per_image.cmd_buffer,
                ubo: &mut per_image.ubo,
            },
            &self.pipeline,
        )
    }
}

impl<P: Pipeline> Drop for CommandManager<P> {
    fn drop(&mut self) {
        self.pipeline.swapchain().device.wait_idle().ok();

        unsafe {
            self.destroy_framebuffers();
            self.destroy_per_image();

            self.pipeline
                .swapchain()
                .device
                .destroy_command_pool(self.pool, None);
        }
    }
}

fn create_framebuffers(render_pass: RenderPass, swapchain: &Swapchain) -> Result<Vec<Framebuffer>> {
    swapchain
        .images
        .iter()
        .map(|(_, image_view)| {
            let create_info = FramebufferCreateInfo::builder()
                .render_pass(render_pass)
                .attachments(slice::from_ref(image_view))
                .width(swapchain.extent.width)
                .height(swapchain.extent.height)
                .layers(1);
            unsafe { swapchain.device.create_framebuffer(&create_info, None) }
                .context("Failed to create framebuffer")
        })
        .collect()
}

fn create_per_image(
    swapchain: &Swapchain,
    descriptor_set_layout: DescriptorSetLayout,
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

    // Create the descriptor pool. TODO: When does this get destroyed...
    let pool_size = DescriptorPoolSize::builder()
        .ty(DescriptorType::UNIFORM_BUFFER)
        .descriptor_count(num_images);
    let create_info = DescriptorPoolCreateInfo::builder()
        .max_sets(num_images)
        .pool_sizes(slice::from_ref(&pool_size));
    let descriptor_pool = unsafe {
        swapchain
            .device
            .create_descriptor_pool(&create_info, None)?
    };

    // Create the descriptor sets.
    let set_layouts = vec![descriptor_set_layout; num_images as usize];
    let create_info = DescriptorSetAllocateInfo::builder()
        .descriptor_pool(descriptor_pool)
        .set_layouts(&set_layouts);
    let descriptor_sets = unsafe { swapchain.device.allocate_descriptor_sets(&create_info)? };

    // Create the semaphore and fence's create info outside the loop.
    let sema_create_info = SemaphoreCreateInfo::builder();
    let fence_create_info = FenceCreateInfo::builder().flags(FenceCreateFlags::SIGNALED);

    // Create closures for creating the semaphores and fences. This is mainly to make it nicer to
    // read :P
    let create_sema = || unsafe { swapchain.device.create_semaphore(&sema_create_info, None) };
    let create_fence = || unsafe { swapchain.device.create_fence(&fence_create_info, None) };

    bufs.into_iter()
        .zip(descriptor_sets)
        .map(|(cmd_buffer, descriptor_set)| {
            // Create the synchronization values.
            let image_available_semaphore = create_sema()?;
            let render_finished_semaphore = create_sema()?;
            let render_finished_fence = create_fence()?;

            // Create the UBO.
            let ubo = MutableBuffer::new(
                swapchain.device.clone(),
                1,
                BufferUsageFlags::UNIFORM_BUFFER,
            )?;

            // Initialize the descriptor set.
            let buffer_info = DescriptorBufferInfo::builder()
                .buffer(ubo.buffer)
                .offset(0)
                .range(ubo.size);
            let write_info = WriteDescriptorSet::builder()
                .dst_set(descriptor_set)
                .dst_binding(0)
                .dst_array_element(0)
                .descriptor_type(DescriptorType::UNIFORM_BUFFER)
                .buffer_info(slice::from_ref(&buffer_info));
            unsafe {
                swapchain
                    .device
                    .update_descriptor_sets(slice::from_ref(&write_info), &[]);
            }

            Ok(PerImage {
                cmd_buffer,
                image_available_semaphore,
                render_finished_semaphore,
                render_finished_fence,

                descriptor_set,
                ubo,
            })
        })
        .collect()
}
