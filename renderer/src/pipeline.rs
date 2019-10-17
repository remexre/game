//! The graphics and raytracing pipelines.

use ash::{
    version::DeviceV1_0,
    vk::{
        AttachmentDescription, AttachmentLoadOp, AttachmentReference, AttachmentStoreOp,
        BlendFactor, BlendOp, ColorComponentFlags, CullModeFlags, Extent2D, Format, FrontFace,
        GraphicsPipelineCreateInfo, ImageLayout, Offset2D, Pipeline, PipelineBindPoint,
        PipelineCache, PipelineColorBlendAttachmentState, PipelineColorBlendStateCreateInfo,
        PipelineInputAssemblyStateCreateInfo, PipelineLayoutCreateInfo,
        PipelineMultisampleStateCreateInfo, PipelineRasterizationStateCreateInfo,
        PipelineShaderStageCreateInfo, PipelineVertexInputStateCreateInfo,
        PipelineViewportStateCreateInfo, PolygonMode, PrimitiveTopology, Rect2D, RenderPass,
        RenderPassCreateInfo, SampleCountFlags, SubpassDescription, Viewport,
    },
    Device,
};
use libremexre::errors::Result;
use std::slice;

pub fn create_graphics_pipeline(
    dev: &Device,
    format: Format,
    dims: Extent2D,
    vert_stage: PipelineShaderStageCreateInfo,
    frag_stage: PipelineShaderStageCreateInfo,
) -> Result<Pipeline> {
    let shader_stages = vec![vert_stage, frag_stage];

    let vertex_input_state = PipelineVertexInputStateCreateInfo::builder();

    let input_assembly_state = PipelineInputAssemblyStateCreateInfo::builder()
        .topology(PrimitiveTopology::TRIANGLE_LIST)
        .primitive_restart_enable(false);

    let viewport = Viewport::builder()
        .max_depth(1.0)
        .width(dims.width as f32)
        .height(dims.height as f32);

    let scissor = Rect2D {
        extent: dims,
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

    let render_pass = create_graphics_render_pass(dev, format)?;

    let create_info = GraphicsPipelineCreateInfo::builder()
        .stages(&shader_stages)
        .vertex_input_state(&vertex_input_state)
        .input_assembly_state(&input_assembly_state)
        .viewport_state(&viewport_state)
        .rasterization_state(&rasterization_state)
        .multisample_state(&multisample_state)
        .color_blend_state(&color_blend_state)
        .layout(layout)
        .render_pass(render_pass)
        .subpass(0);

    let pipeline_result = unsafe {
        dev.create_graphics_pipelines(PipelineCache::null(), slice::from_ref(&create_info), None)
    };
    match pipeline_result {
        Ok(mut pipelines) => {
            let pipeline = pipelines.remove(0);
            Ok(pipeline)
        }
        Err((_, err)) => Err(err.into()),
    }
}

pub fn create_graphics_render_pass(dev: &Device, format: Format) -> Result<RenderPass> {
    let color_attachment = AttachmentDescription::builder()
        .format(format)
        .samples(SampleCountFlags::TYPE_1)
        .load_op(AttachmentLoadOp::CLEAR)
        .store_op(AttachmentStoreOp::STORE)
        .stencil_load_op(AttachmentLoadOp::DONT_CARE)
        .stencil_store_op(AttachmentStoreOp::DONT_CARE)
        .initial_layout(ImageLayout::UNDEFINED)
        .final_layout(ImageLayout::PRESENT_SRC_KHR);

    let color_attachment_ref = AttachmentReference::builder()
        .attachment(0)
        .layout(ImageLayout::COLOR_ATTACHMENT_OPTIMAL);

    let subpass = SubpassDescription::builder()
        .pipeline_bind_point(PipelineBindPoint::GRAPHICS)
        .color_attachments(slice::from_ref(&color_attachment_ref));

    let create_info = RenderPassCreateInfo::builder()
        .attachments(slice::from_ref(&color_attachment))
        .subpasses(slice::from_ref(&subpass));

    let render_pass = unsafe { dev.create_render_pass(&create_info, None)? };
    Ok(render_pass)
}
