use anyhow::Result;
use ash::{
    version::DeviceV1_0,
    vk::{Fence, FenceCreateFlags, FenceCreateInfo, Semaphore, SemaphoreCreateInfo},
    Device,
};

pub fn create_semaphore(dev: &Device) -> Result<Semaphore> {
    let create_info = SemaphoreCreateInfo::builder();
    let semaphore = unsafe { dev.create_semaphore(&create_info, None) }?;
    Ok(semaphore)
}

pub fn create_fence(dev: &Device, signaled: bool) -> Result<Fence> {
    let mut create_info = FenceCreateInfo::default();
    if signaled {
        create_info.flags |= FenceCreateFlags::SIGNALED;
    }
    let semaphore = unsafe { dev.create_fence(&create_info, None) }?;
    Ok(semaphore)
}
