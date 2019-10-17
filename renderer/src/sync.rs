use anyhow::Result;
use ash::{
    version::DeviceV1_0,
    vk::{Semaphore, SemaphoreCreateInfo},
    Device,
};

pub fn create_semaphore(dev: &Device) -> Result<Semaphore> {
    let create_info = SemaphoreCreateInfo::builder();
    let semaphore = unsafe { dev.create_semaphore(&create_info, None) }?;
    Ok(semaphore)
}
