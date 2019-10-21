use crate::Device;
use anyhow::Result;
use ash::{
    version::DeviceV1_0,
    vk::{
        Buffer, BufferCreateInfo, BufferUsageFlags, DeviceMemory, MappedMemoryRange,
        MemoryMapFlags, MemoryPropertyFlags, SharingMode, WHOLE_SIZE,
    },
};
use std::{mem::size_of_val, ptr::copy_nonoverlapping, slice, sync::Arc};

/// A Vulkan buffer.
#[derive(Debug)]
pub struct ImmutableBuffer {
    pub(crate) buffer: Buffer,
    memory: DeviceMemory,
    pub(crate) size: u64,

    // The device must outlive us.
    device: Arc<Device>,
}

impl ImmutableBuffer {
    /// Creates a new buffer, initialized with the given data.
    ///
    /// Note that the buffer's contents are indeterminate until the next call to
    /// `CommandManager::flip`.
    pub fn new<T: Copy + ?Sized>(
        device: Arc<Device>,
        data: &[T],
        usage: BufferUsageFlags,
    ) -> Result<ImmutableBuffer> {
        let size = size_of_val(data) as u64;
        let src = &data[0] as *const T as *const u8;

        let create_info = BufferCreateInfo::builder()
            .size(size)
            .usage(usage)
            .sharing_mode(SharingMode::EXCLUSIVE)
            .queue_family_indices(slice::from_ref(&device.qf));
        let buffer = unsafe { device.create_buffer(&create_info, None)? };

        let reqs = unsafe { device.get_buffer_memory_requirements(buffer) };

        // We don't require conherency, so we need to flush after writing!
        let memory = device.alloc(reqs, MemoryPropertyFlags::HOST_VISIBLE)?;
        unsafe {
            let dst = device.map_memory(memory, 0, size, MemoryMapFlags::empty())?;
            let dst = dst as *mut u8;

            copy_nonoverlapping(src, dst, size as usize);

            let range = MappedMemoryRange::builder()
                .memory(memory)
                .offset(0)
                .size(WHOLE_SIZE);
            device.flush_mapped_memory_ranges(slice::from_ref(&range))?;

            device.unmap_memory(memory);
        }

        // Bind the memory to the buffer.
        unsafe { device.bind_buffer_memory(buffer, memory, 0)? };

        Ok(ImmutableBuffer {
            buffer,
            memory,
            size,

            device,
        })
    }
}

impl Drop for ImmutableBuffer {
    fn drop(&mut self) {
        unsafe {
            self.device.free_memory(self.memory, None);
            self.device.destroy_buffer(self.buffer, None);
        }
    }
}
