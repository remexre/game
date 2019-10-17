//! Loading of compiled shaders.

use crate::utils::read_u32s;
use ash::{
    version::DeviceV1_0,
    vk::{PipelineShaderStageCreateInfo, ShaderModule, ShaderModuleCreateInfo, ShaderStageFlags},
    Device,
};
use libremexre::errors::Result;
use std::path::Path;

pub fn load_shader<P: AsRef<Path>>(
    dev: &Device,
    path: P,
    kind: ShaderStageFlags,
) -> Result<(ShaderModule, PipelineShaderStageCreateInfo)> {
    let src = read_u32s(path)?;
    let shader_info = ShaderModuleCreateInfo::builder().code(&src);
    let shader = unsafe { dev.create_shader_module(&shader_info, None)? };
    drop(src);
    let stage_create_info = PipelineShaderStageCreateInfo::builder()
        .stage(kind)
        .module(shader)
        .build();
    Ok((shader, stage_create_info))
}
