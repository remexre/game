//! The graphics and raytracing pipelines.

use crate::shaders::load_shader;
use ash::{
    version::DeviceV1_0,
    vk::{
        BlendFactor, BlendOp, ColorComponentFlags, CullModeFlags, Extent2D, FrontFace,
        GraphicsPipelineCreateInfo, Offset2D, PipelineColorBlendAttachmentState,
        PipelineColorBlendStateCreateInfo, PipelineInputAssemblyStateCreateInfo,
        PipelineLayoutCreateInfo, PipelineMultisampleStateCreateInfo,
        PipelineRasterizationStateCreateInfo, PipelineVertexInputStateCreateInfo,
        PipelineViewportStateCreateInfo, PolygonMode, PrimitiveTopology, Rect2D, SampleCountFlags,
        ShaderStageFlags, Viewport,
    },
    Device,
};
use libremexre::errors::Result;
use std::{path::Path, slice};

pub fn create_graphics_pipeline_from_paths<P1: AsRef<Path>, P2: AsRef<Path>>(
    dev: &Device,
    vert: P1,
    frag: P2,
    width: u32,
    height: u32,
) -> Result<()> {
    let (vert, vert_stage) = load_shader(dev, vert, ShaderStageFlags::VERTEX)?;
    let (frag, frag_stage) = load_shader(dev, frag, ShaderStageFlags::FRAGMENT)?;

    let shader_stages = vec![vert_stage, frag_stage];

    let vertex_input_state = PipelineVertexInputStateCreateInfo::builder();

    let input_assembly_state = PipelineInputAssemblyStateCreateInfo::builder()
        .topology(PrimitiveTopology::TRIANGLE_LIST)
        .primitive_restart_enable(false);

    let viewport = Viewport::builder()
        .max_depth(1.0)
        .width(width as f32)
        .height(height as f32);

    let scissor = Rect2D {
        extent: Extent2D { width, height },
        offset: Offset2D { x: 0, y: 0 },
    };

    let viewport_state = PipelineViewportStateCreateInfo::builder()
        .viewports(slice::from_ref(&viewport))
        .scissors(slice::from_ref(&scissor));

    let rasterization_state = PipelineRasterizationStateCreateInfo::builder()
        .depth_clamp_enable(false)
        .rasterizer_discard_enable(false)
        .polygon_mode(PolygonMode::FILL)
        .line_width(1.0)
        .cull_mode(CullModeFlags::BACK)
        .front_face(FrontFace::CLOCKWISE);

    let multisample_state = PipelineMultisampleStateCreateInfo::builder()
        .sample_shading_enable(false)
        .rasterization_samples(SampleCountFlags::TYPE_1)
        .min_sample_shading(1.0);

    // TODO: Depth buffer

    let color_blend_attachment = PipelineColorBlendAttachmentState::builder()
        .color_write_mask(
            ColorComponentFlags::R
                | ColorComponentFlags::G
                | ColorComponentFlags::B
                | ColorComponentFlags::A,
        )
        .blend_enable(true)
        .src_color_blend_factor(BlendFactor::SRC_ALPHA)
        .dst_color_blend_factor(BlendFactor::ONE_MINUS_SRC_ALPHA)
        .color_blend_op(BlendOp::ADD)
        .src_alpha_blend_factor(BlendFactor::ONE)
        .dst_alpha_blend_factor(BlendFactor::ZERO)
        .alpha_blend_op(BlendOp::ADD);

    let color_blend_state = PipelineColorBlendStateCreateInfo::builder()
        .attachments(slice::from_ref(&color_blend_attachment));

    let layout_create_info = PipelineLayoutCreateInfo::builder();

    let layout = unsafe { dev.create_pipeline_layout(&layout_create_info, None)? };

    let create_info = GraphicsPipelineCreateInfo::builder()
        .stages(&shader_stages)
        .vertex_input_state(&vertex_input_state)
        .input_assembly_state(&input_assembly_state)
        .viewport_state(&viewport_state)
        .rasterization_state(&rasterization_state)
        .multisample_state(&multisample_state)
        .color_blend_state(&color_blend_state)
        .layout(layout);

    unimplemented!()
}
