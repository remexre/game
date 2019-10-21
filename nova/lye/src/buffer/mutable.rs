use crate::Device;
use anyhow::Result;
use ash::{
    version::DeviceV1_0,
    vk::{
        Buffer, BufferCreateInfo, BufferUsageFlags, DeviceMemory, MemoryMapFlags,
        MemoryPropertyFlags, SharingMode,
    },
};
use std::{
    mem::size_of,
    ops::{Deref, DerefMut},
    ptr, slice,
    sync::Arc,
};

/// A Vulkan buffer.
#[derive(Debug)]
pub struct MutableBuffer<T> {
    pub(crate) buffer: Buffer,
    memory: DeviceMemory,
    ptr: *mut T,
    length: usize,

    // The device must outlive us.
    device: Arc<Device>,
}

impl<T: Copy + Default> MutableBuffer<T> {
    /// Creates a new buffer, initialized with the default value for the type. This is likely to be
    /// very slow if said value isn't all zeroes!
    ///
    /// Note that the buffer's contents are indeterminate until the next call to
    /// `CommandManager::flip`.
    pub fn new(
        device: Arc<Device>,
        length: usize,
        usage: BufferUsageFlags,
    ) -> Result<MutableBuffer<T>> {
        let size = (size_of::<T>() * length) as u64;

        let create_info = BufferCreateInfo::builder()
            .size(size)
            .usage(usage)
            .sharing_mode(SharingMode::EXCLUSIVE)
            .queue_family_indices(slice::from_ref(&device.qf));
        let buffer = unsafe { device.create_buffer(&create_info, None)? };

        let reqs = unsafe { device.get_buffer_memory_requirements(buffer) };

        let memory = device.alloc(
            reqs,
            MemoryPropertyFlags::HOST_VISIBLE | MemoryPropertyFlags::HOST_COHERENT,
        )?;
        let ptr = unsafe { device.map_memory(memory, 0, size, MemoryMapFlags::empty())? };
        let ptr = ptr as *mut T;

        // Initialize the buffer.
        for i in 0..length {
            unsafe {
                ptr::write(ptr.add(i), T::default());
            }
        }

        // Bind the memory to the buffer.
        unsafe { device.bind_buffer_memory(buffer, memory, 0)? };

        Ok(MutableBuffer {
            buffer,
            memory,
            ptr,
            length,

            device,
        })
    }
}

impl<T> Drop for MutableBuffer<T> {
    fn drop(&mut self) {
        unsafe {
            self.device.unmap_memory(self.memory);
            self.device.free_memory(self.memory, None);
            self.device.destroy_buffer(self.buffer, None);
        }
    }
}

impl<T> Deref for MutableBuffer<T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        unsafe { slice::from_raw_parts(self.ptr, self.length) }
    }
}

impl<T> DerefMut for MutableBuffer<T> {
    fn deref_mut(&mut self) -> &mut [T] {
        unsafe { slice::from_raw_parts_mut(self.ptr, self.length) }
    }
}
