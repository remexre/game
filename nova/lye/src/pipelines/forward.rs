use crate::{pipelines::Pipeline, Device, Shader, Swapchain, Vertex};
use anyhow::Result;
use ash::{
    version::DeviceV1_0,
    vk::{
        AttachmentDescription, AttachmentLoadOp, AttachmentReference, AttachmentStoreOp,
        BlendFactor, BlendOp, ColorComponentFlags, CullModeFlags, Format, FrontFace,
        GraphicsPipelineCreateInfo, ImageLayout, Offset2D, Pipeline as VkPipeline,
        PipelineBindPoint, PipelineCache, PipelineColorBlendAttachmentState,
        PipelineColorBlendStateCreateInfo, PipelineInputAssemblyStateCreateInfo, PipelineLayout,
        PipelineLayoutCreateInfo, PipelineMultisampleStateCreateInfo,
        PipelineRasterizationStateCreateInfo, PipelineVertexInputStateCreateInfo,
        PipelineViewportStateCreateInfo, PolygonMode, PrimitiveTopology, Rect2D, RenderPass,
        RenderPassCreateInfo, SampleCountFlags, SubpassDescription,
        VertexInputAttributeDescription, VertexInputBindingDescription, VertexInputRate, Viewport,
    },
};
use derivative::Derivative;
use memoffset::offset_of;
use std::{mem::size_of, slice, sync::Arc};

/// The graphics pipeline used for forward rendering.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct ForwardPipeline {
    layout: PipelineLayout,
    render_pass: RenderPass,
    pipeline: VkPipeline,

    vert: Arc<Shader>,
    frag: Arc<Shader>,
    swapchain: Arc<Swapchain>,
}

impl ForwardPipeline {
    /// Creates a ForwardPipeline with the given shaders, rendering to the given Swapchain.
    ///
    /// **TODO**: Much of this could probably be genericized.
    ///
    /// **TODO**: Shader parameters should be made configurable. `unsafe trait Vertex`?
    pub fn new(
        swapchain: Arc<Swapchain>,
        vert: Arc<Shader>,
        frag: Arc<Shader>,
    ) -> Result<ForwardPipeline> {
        let shader_stages = vec![vert.stage_create_info(), frag.stage_create_info()];

        let vertex_attribute_descriptions = [
            VertexInputAttributeDescription::builder()
                .location(0)
                .binding(0)
                .format(Format::R32G32B32_SFLOAT)
                .offset(offset_of!(Vertex, position) as u32)
                .build(),
            VertexInputAttributeDescription::builder()
                .location(1)
                .binding(0)
                .format(Format::R32G32_SFLOAT)
                .offset(offset_of!(Vertex, texcoords) as u32)
                .build(),
            VertexInputAttributeDescription::builder()
                .location(2)
                .binding(0)
                .format(Format::R32G32B32_SFLOAT)
                .offset(offset_of!(Vertex, normal) as u32)
                .build(),
        ];
        let vertex_binding_description = VertexInputBindingDescription::builder()
            .binding(0)
            .stride(size_of::<Vertex>() as u32)
            .input_rate(VertexInputRate::VERTEX);

        let vertex_input_state = PipelineVertexInputStateCreateInfo::builder()
            .vertex_attribute_descriptions(&vertex_attribute_descriptions)
            .vertex_binding_descriptions(slice::from_ref(&vertex_binding_description));

        let input_assembly_state = PipelineInputAssemblyStateCreateInfo::builder()
            .topology(PrimitiveTopology::TRIANGLE_LIST)
            .primitive_restart_enable(false);

        let viewport = Viewport::builder()
            .max_depth(1.0)
            .width(swapchain.extent.width as f32)
            .height(swapchain.extent.height as f32);

        let scissor = Rect2D {
            extent: swapchain.extent,
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
            .color_write_mask(ColorComponentFlags::all())
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

        let layout = unsafe {
            swapchain
                .device
                .create_pipeline_layout(&layout_create_info, None)?
        };

        let render_pass = create_graphics_render_pass(&swapchain.device, swapchain.format)?;

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
            swapchain.device.create_graphics_pipelines(
                PipelineCache::null(),
                slice::from_ref(&create_info),
                None,
            )
        };
        match pipeline_result {
            Ok(mut pipelines) => {
                assert_eq!(pipelines.len(), 1);
                let pipeline = pipelines.remove(0);
                Ok(ForwardPipeline {
                    layout,
                    render_pass,
                    pipeline,

                    vert,
                    frag,
                    swapchain,
                })
            }
            Err((_, err)) => Err(err.into()),
        }
    }
}

impl Drop for ForwardPipeline {
    fn drop(&mut self) {
        unsafe {
            self.swapchain
                .device
                .destroy_pipeline_layout(self.layout, None);
            self.swapchain
                .device
                .destroy_render_pass(self.render_pass, None);
            self.swapchain.device.destroy_pipeline(self.pipeline, None);
        }
    }
}

impl Pipeline for ForwardPipeline {
    unsafe fn render_pass(&self) -> RenderPass {
        self.render_pass
    }

    fn swapchain(&self) -> &Swapchain {
        &self.swapchain
    }
}

fn create_graphics_render_pass(device: &Arc<Device>, format: Format) -> Result<RenderPass> {
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

    let render_pass = unsafe { device.create_render_pass(&create_info, None)? };
    Ok(render_pass)
}
