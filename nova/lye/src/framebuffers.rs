use crate::Pipeline;
use anyhow::{Context, Result};
use ash::{
    version::DeviceV1_0,
    vk::{Framebuffer, FramebufferCreateInfo},
};
use derivative::Derivative;
use std::slice;

/// A wrapper for the framebuffers used as render targets.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Framebuffers<P: Pipeline> {
    pub(crate) framebuffers: Vec<Framebuffer>,

    pipeline: P,
}

impl<P: Pipeline> Framebuffers<P> {
    /// Creates a new Framebuffers.
    ///
    /// Note that one Vulkan framebuffer object will be created per image in the swapchain, rather
    /// than a single framebuffer for all images.
    ///
    /// **TODO**: Comments, context for errors.
    pub fn new(pipeline: P) -> Result<Framebuffers<P>> {
        let swapchain = pipeline.swapchain();
        let framebuffers = swapchain
            .images
            .iter()
            .map(|(_, image_view)| {
                let create_info = FramebufferCreateInfo::builder()
                    .render_pass(unsafe { pipeline.render_pass() })
                    .attachments(slice::from_ref(image_view))
                    .width(swapchain.extent.width)
                    .height(swapchain.extent.height)
                    .layers(1);

                unsafe { swapchain.device.create_framebuffer(&create_info, None) }
                    .context("Failed to create framebuffer")
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(Framebuffers {
            framebuffers,

            pipeline,
        })
    }
}

impl<P: Pipeline> Drop for Framebuffers<P> {
    fn drop(&mut self) {
        unsafe {
            for framebuffer in self.framebuffers.drain(..) {
                self.pipeline
                    .swapchain()
                    .device
                    .destroy_framebuffer(framebuffer, None);
            }
        }
    }
}
