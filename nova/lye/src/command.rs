use crate::Device;
use anyhow::Result;
use ash::{
    version::DeviceV1_0,
    vk::{
        CommandBuffer, CommandBufferAllocateInfo, CommandBufferLevel, CommandPool,
        CommandPoolCreateFlags, CommandPoolCreateInfo,
    },
};
use std::sync::Arc;

/// A wrapper around a CommandPool and some number of CommandBuffers.
#[derive(Debug)]
pub struct CommandBuffers {
    pool: CommandPool,
    bufs: Vec<CommandBuffer>,

    // The device must outlive us.
    device: Arc<Device>,
}

impl CommandBuffers {
    /// Creates a new CommandBuffers wrapping `count` buffers.
    pub fn new(device: Arc<Device>, count: u32) -> Result<CommandBuffers> {
        let create_info = CommandPoolCreateInfo::builder()
            .flags(CommandPoolCreateFlags::RESET_COMMAND_BUFFER)
            .queue_family_index(device.qf);
        let pool = unsafe { device.create_command_pool(&create_info, None)? };
        let create_info = CommandBufferAllocateInfo::builder()
            .command_pool(pool)
            .level(CommandBufferLevel::PRIMARY)
            .command_buffer_count(count);
        let bufs = unsafe { device.allocate_command_buffers(&create_info)? };
        Ok(CommandBuffers { pool, bufs, device })
    }
}

impl Drop for CommandBuffers {
    fn drop(&mut self) {
        unsafe {
            self.device.free_command_buffers(self.pool, &self.bufs);
            self.device.destroy_command_pool(self.pool, None);
        }
    }
}
