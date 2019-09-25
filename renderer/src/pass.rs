use vulkano::{
    format::{ClearValue, Format},
    framebuffer::{
        AttachmentDescription, LoadOp, PassDependencyDescription, PassDescription, RenderPassDesc,
        RenderPassDescClearValues, StoreOp,
    },
    image::ImageLayout,
};

#[derive(Debug)]
pub struct GameRenderPass {
    pub color_format: Format,
}

unsafe impl RenderPassDesc for GameRenderPass {
    fn num_attachments(&self) -> usize {
        2
    }

    fn attachment_desc(&self, num: usize) -> Option<AttachmentDescription> {
        if num == 0 {
            Some(AttachmentDescription {
                format: self.color_format,
                samples: 1,
                initial_layout: ImageLayout::ColorAttachmentOptimal,
                final_layout: ImageLayout::ColorAttachmentOptimal,

                load: LoadOp::Clear,
                store: StoreOp::Store,
                stencil_load: LoadOp::Clear,
                stencil_store: StoreOp::Store,
            })
        } else if num == 1 {
            Some(AttachmentDescription {
                format: Format::D16Unorm,
                samples: 1,
                initial_layout: ImageLayout::DepthStencilAttachmentOptimal,
                final_layout: ImageLayout::DepthStencilAttachmentOptimal,

                load: LoadOp::Clear,
                store: StoreOp::DontCare,
                stencil_load: LoadOp::Clear,
                stencil_store: StoreOp::DontCare,
            })
        } else {
            None
        }
    }

    fn num_subpasses(&self) -> usize {
        1
    }

    fn subpass_desc(&self, num: usize) -> Option<PassDescription> {
        if num == 0 {
            Some(PassDescription {
                color_attachments: vec![(0, ImageLayout::ColorAttachmentOptimal)],
                depth_stencil: Some((1, ImageLayout::DepthStencilAttachmentOptimal)),
                input_attachments: vec![],
                resolve_attachments: vec![],
                preserve_attachments: vec![],
            })
        } else {
            None
        }
    }

    fn num_dependencies(&self) -> usize {
        0
    }

    fn dependency_desc(&self, _num: usize) -> Option<PassDependencyDescription> {
        None
    }
}

unsafe impl RenderPassDescClearValues<Vec<ClearValue>> for GameRenderPass {
    fn convert_clear_values(&self, vals: Vec<ClearValue>) -> Box<dyn Iterator<Item = ClearValue>> {
        Box::new(vals.into_iter())
    }
}
