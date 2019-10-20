use crate::{lye::Device, utils::read_u32s};
use anyhow::{anyhow, Context, Result};
use ash::{
    extensions::{khr::Swapchain, nv::RayTracing},
    version::{DeviceV1_0, InstanceV1_0},
    vk::{
        DeviceCreateInfo, DeviceQueueCreateInfo, KhrGetMemoryRequirements2Fn, PhysicalDevice,
        PhysicalDeviceType, Queue, QueueFlags, ShaderModule, ShaderModuleCreateInfo, SurfaceKHR,
    },
    Device as AshDevice,
};
use derivative::Derivative;
use lazy_static::lazy_static;
use std::{ffi::CString, path::Path, sync::Arc};

lazy_static! {
    static ref REQUIRED_EXTS: Vec<CString> = {
        extensions![
            Swapchain,
        ]
    };

    static ref WANTED_EXTS: Vec<CString> = {
        extensions![
            KhrGetMemoryRequirements2Fn, // for RayTracing
            RayTracing,
        ]
    };
}

/// A Vulkan device and queue.
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct Shader {
    module: ShaderModule,

    // The device must outlive us.
    device: Arc<Device>,
}

impl Shader {
    pub fn load_from_path<P: AsRef<Path>>(device: Arc<Device>, path: P) -> Result<Arc<Shader>> {
        let src = read_u32s(path)?;
        let create_info = ShaderModuleCreateInfo::builder().code(&src);
        let module = unsafe { device.device.create_shader_module(&create_info, None)? };
        Ok(Arc::new(Shader { module, device }))
    }

    /*
    pub fn shader_info(&self) -> PipelineShaderStageCreateInfo {
        let stage_create_info = PipelineShaderStageCreateInfo::builder()
            .stage(kind)
            .module(shader)
            .name(CStr::from_bytes_with_nul(b"main\0").unwrap())
            .build();
    }
    */
}

impl Drop for Shader {
    fn drop(&mut self) {
        unsafe {
            self.device.device.destroy_shader_module(self.module, None);
        }
    }
}
