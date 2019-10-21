use anyhow::Result;
use ash::{
    extensions::khr::Swapchain,
    version::DeviceV1_0,
    vk::{
        ClearColorValue, ClearValue, CommandBuffer, CommandBufferAllocateInfo,
        CommandBufferBeginInfo, CommandBufferLevel, CommandBufferResetFlags,
        CommandBufferUsageFlags, CommandPool, CommandPoolCreateFlags, CommandPoolCreateInfo,
        Extent2D, Fence, Framebuffer, Offset2D, Pipeline, PipelineBindPoint, PipelineStageFlags,
        PresentInfoKHR, Queue, Rect2D, RenderPass, RenderPassBeginInfo, Result as VkResult,
        Semaphore, SubmitInfo, SubpassContents, SwapchainKHR,
    },
    Device,
};
use std::slice;

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
