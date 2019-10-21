//! Different pipelines.

pub mod forward;

use crate::{MutableBuffer, Swapchain, Uniforms};
use anyhow::Result;
use ash::vk::{CommandBuffer, Pipeline as VkPipeline, RenderPass};
use std::sync::Arc;

// TODO: Set up some shared pipeline cache. Sounds like making a pipeline can be pretty damn
// expensive. As with anything, profile it first though...
//
// If we do that, we also might as well go the full nine yards and get enough info to pass an
// ApplicationInfo when making the instance, then store the cache to an XDG-approved cache
// location. That gets weird though, since the engine proper needs to be sure to replicate the
// behavior for its own file management.
//
// I suppose we could expose the file management we do here to the engine, but... ew.

/// The state required to know how and where to draw.
#[derive(Debug)]
pub struct DrawContext<'a> {
    pub(crate) cmd_buffer: CommandBuffer,
    pub(crate) ubo: &'a mut MutableBuffer<Uniforms>,
}

/// A single Vulkan pipeline.
pub trait Pipeline {
    /// Returns the pipeline handle.
    fn handle(&self) -> VkPipeline;

    /// Recreates the pipeline for a different swapchain.
    fn recreate(&mut self, swapchain: Arc<Swapchain>) -> Result<()>;

    /// Returns the render pass that draws to the framebuffer.
    fn render_pass(&self) -> RenderPass;

    /// Returns a reference to the contained swapchain.
    fn swapchain(&self) -> &Swapchain;
}
