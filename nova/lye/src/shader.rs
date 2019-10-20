use crate::{utils::read_u32s, Device};
use anyhow::Result;
use ash::{
    version::DeviceV1_0,
    vk::{PipelineShaderStageCreateInfo, ShaderModule, ShaderModuleCreateInfo, ShaderStageFlags},
};
use derivative::Derivative;
use std::{ffi::CStr, path::Path, sync::Arc};

/// A Vulkan device and queue.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Shader {
    module: ShaderModule,
    stage: ShaderStageFlags,

    // The device must outlive us.
    device: Arc<Device>,
}

impl Shader {
    pub fn load_from_path<P: AsRef<Path>>(
        device: Arc<Device>,
        path: P,
        stage: ShaderStageFlags,
    ) -> Result<Arc<Shader>> {
        let src = read_u32s(path)?;
        let create_info = ShaderModuleCreateInfo::builder().code(&src);
        let module = unsafe { device.create_shader_module(&create_info, None)? };

        // TODO: Validate shader stage

        Ok(Arc::new(Shader {
            module,
            stage,

            device,
        }))
    }

    pub(crate) fn stage_create_info(&self) -> PipelineShaderStageCreateInfo {
        PipelineShaderStageCreateInfo::builder()
            .stage(self.stage)
            .module(self.module)
            .name(CStr::from_bytes_with_nul(b"main\0").unwrap())
            .build()
    }
}

impl Drop for Shader {
    fn drop(&mut self) {
        unsafe {
            self.device.destroy_shader_module(self.module, None);
        }
    }
}
