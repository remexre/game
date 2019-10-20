use anyhow::Result;
use ash::{
    version::DeviceV1_0,
    vk::{BufferCreateInfo, BufferUsageFlags, SharingMode},
    Device,
};
use derivative::Derivative;
use std::{
    mem::transmute,
    ops::{Deref, DerefMut},
    sync::Arc,
};

#[derive(Derivative)]
#[derivative(Debug)]
pub struct Buffer {
    buffer: ash::vk::Buffer,
    #[derivative(Debug = "ignore")]
    device: Arc<Device>,
}

impl Buffer {
    pub fn new(device: Arc<Device>, data: &[u8], usage: BufferUsageFlags) -> Result<Buffer> {
        let create_info = BufferCreateInfo::builder()
            .size(data.len() as u64)
            .usage(usage)
            .sharing_mode(SharingMode::EXCLUSIVE);
        let buffer = unsafe { device.create_buffer(&create_info, None)? };

        let mem_reqs = unsafe { device.get_buffer_memory_requirements(buffer) };

        // TODO: Copy in data.
        Ok(Buffer { buffer, device })
    }

    pub fn buffer(&self) -> ash::vk::Buffer {
        self.buffer
    }
}

impl Drop for Buffer {
    fn drop(&mut self) {
        unsafe {
            self.device.destroy_buffer(self.buffer, None);
        }
    }
}

#[derive(Debug)]
pub struct VBO {
    pub buffer: Buffer,
    pub num_vertices: u32,
}

impl VBO {
    pub fn new(device: Arc<Device>, data: &[Vertex]) -> Result<VBO> {
        let data_bytes = unsafe { transmute::<&[Vertex], &[u8]>(data) };
        let buffer = Buffer::new(device, data_bytes, BufferUsageFlags::VERTEX_BUFFER)?;
        Ok(VBO {
            buffer,
            num_vertices: data.len() as u32,
        })
    }
}

impl Deref for VBO {
    type Target = Buffer;

    fn deref(&self) -> &Buffer {
        &self.buffer
    }
}

impl DerefMut for VBO {
    fn deref_mut(&mut self) -> &mut Buffer {
        &mut self.buffer
    }
}
