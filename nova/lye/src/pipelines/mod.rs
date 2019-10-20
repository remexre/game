//! Different pipelines.

pub mod forward;

use crate::Swapchain;
use ash::vk::RenderPass;

// TODO: Set up some shared pipeline cache. Sounds like making a pipeline can be pretty damn
// expensive. As with anything, profile it first though...
//
// If we do that, we also might as well go the full nine yards and get enough info to pass an
// ApplicationInfo when making the instance, then store the cache to an XDG-approved cache
// location. That gets weird though, since the engine proper needs to be sure to replicate the
// behavior for its own file management.
//
// I suppose we could expose the file management we do here to the engine, but... ew.

/// A single Vulkan pipeline.
pub trait Pipeline {
    /// Returns the render pass that draws to the framebuffer.
    ///
    /// Unsafe because arbitrary bad things can occur by messing with the render pass object.
    /// However, this method should generally just be a field access and a copy.
    unsafe fn render_pass(&self) -> RenderPass;

    /// Returns a reference to the contained swapchain.
    fn swapchain(&self) -> &Swapchain;
}
