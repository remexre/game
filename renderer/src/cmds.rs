use anyhow::Result;
use ash::{
    extensions::khr::Swapchain,
    version::DeviceV1_0,
    vk::{
        ClearColorValue, ClearValue, CommandBuffer, CommandBufferAllocateInfo,
        CommandBufferBeginInfo, CommandBufferLevel, CommandBufferResetFlags,
        CommandBufferUsageFlags, CommandPool, CommandPoolCreateFlags, CommandPoolCreateInfo,
        Extent2D, Fence, Framebuffer, Offset2D, Pipeline, PipelineBindPoint, PipelineStageFlags,
        PresentInfoKHR, Queue, Rect2D, RenderPass, RenderPassBeginInfo, Semaphore, SubmitInfo,
        SubpassContents, SwapchainKHR,
    },
    Device,
};
use std::slice;

pub fn create_command_pool(dev: &Device, qf: u32) -> Result<CommandPool> {
    let create_info = CommandPoolCreateInfo::builder()
        .flags(CommandPoolCreateFlags::RESET_COMMAND_BUFFER)
        .queue_family_index(qf);
    let command_pool = unsafe { dev.create_command_pool(&create_info, None)? };
    Ok(command_pool)
}

pub fn create_command_buffers(
    dev: &Device,
    pool: CommandPool,
    count: u32,
) -> Result<Vec<CommandBuffer>> {
    let create_info = CommandBufferAllocateInfo::builder()
        .command_pool(pool)
        .level(CommandBufferLevel::PRIMARY)
        .command_buffer_count(count);
    let command_buffers = unsafe { dev.allocate_command_buffers(&create_info)? };
    Ok(command_buffers)
}

pub fn begin_command_buffer(
    dev: &Device,
    command_buffer: CommandBuffer,
    render_finished_fence: &Fence,
) -> Result<()> {
    unsafe {
        dev.wait_for_fences(slice::from_ref(render_finished_fence), true, std::u64::MAX)?;
        dev.reset_fences(slice::from_ref(render_finished_fence))?;

        dev.reset_command_buffer(command_buffer, CommandBufferResetFlags::empty())?;
    }

    let begin_info =
        CommandBufferBeginInfo::builder().flags(CommandBufferUsageFlags::SIMULTANEOUS_USE);
    let () = unsafe { dev.begin_command_buffer(command_buffer, &begin_info)? };
    Ok(())
}

pub fn end_command_buffer(dev: &Device, command_buffer: CommandBuffer) -> Result<()> {
    let () = unsafe { dev.end_command_buffer(command_buffer)? };
    Ok(())
}

pub fn begin_render_pass(
    dev: &Device,
    command_buffer: CommandBuffer,
    render_pass: RenderPass,
    framebuffer: Framebuffer,
    dims: Extent2D,
) {
    let render_area = Rect2D {
        extent: dims,
        offset: Offset2D { x: 0, y: 0 },
    };

    let clear_black = [ClearValue {
        color: ClearColorValue {
            float32: [1.0, 0.0, 1.0, 1.0],
        },
    }];

    let begin_info = RenderPassBeginInfo::builder()
        .render_pass(render_pass)
        .framebuffer(framebuffer)
        .render_area(render_area)
        .clear_values(&clear_black);

    unsafe { dev.cmd_begin_render_pass(command_buffer, &begin_info, SubpassContents::INLINE) }
}

pub fn bind_pipeline(dev: &Device, command_buffer: CommandBuffer, pipeline: Pipeline) {
    unsafe { dev.cmd_bind_pipeline(command_buffer, PipelineBindPoint::GRAPHICS, pipeline) }
}

pub fn draw(dev: &Device, command_buffer: CommandBuffer, vert_count: u32) {
    unsafe { dev.cmd_draw(command_buffer, vert_count, 1, 0, 0) }
}

pub fn end_render_pass(dev: &Device, command_buffer: CommandBuffer) {
    unsafe { dev.cmd_end_render_pass(command_buffer) }
}

pub fn draw_start(
    swapchain_ext: &Swapchain,
    swapchain: SwapchainKHR,
    image_available_semaphore: Semaphore,
) -> Result<(u32, bool)> {
    let (i, suboptimal) = unsafe {
        swapchain_ext.acquire_next_image(
            swapchain,
            std::u64::MAX,
            image_available_semaphore,
            Fence::null(),
        )?
    };
    Ok((i, suboptimal))
}

pub fn submit_command_buffer(
    dev: &Device,
    queue: Queue,
    command_buffer: &CommandBuffer,
    image_available_semaphore: &Semaphore,
    render_finished_semaphore: &Semaphore,
    render_finished_fence: Fence,
) -> Result<()> {
    let wait_dst_stage_mask = [PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT];
    let submit_info = SubmitInfo::builder()
        .wait_semaphores(slice::from_ref(image_available_semaphore))
        .wait_dst_stage_mask(&wait_dst_stage_mask)
        .command_buffers(slice::from_ref(command_buffer))
        .signal_semaphores(slice::from_ref(render_finished_semaphore));
    let () =
        unsafe { dev.queue_submit(queue, slice::from_ref(&submit_info), render_finished_fence)? };
    Ok(())
}

pub fn present(
    swapchain_ext: &Swapchain,
    queue: Queue,
    swapchain: &SwapchainKHR,
    image_index: u32,
    render_finished_semaphore: &Semaphore,
) -> Result<bool> {
    let image_indices = [image_index];
    let create_info = PresentInfoKHR::builder()
        .wait_semaphores(slice::from_ref(render_finished_semaphore))
        .swapchains(slice::from_ref(swapchain))
        .image_indices(&image_indices);

    let suboptimal = unsafe { swapchain_ext.queue_present(queue, &create_info)? };
    Ok(suboptimal)
}
