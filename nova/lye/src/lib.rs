//! A wrapper around `ash` that makes it more ergonomic (largely via RAII).
//!
//! See `nova/examples/lye.rs` for an example of usage.
//!
//! **TODO**: Go over the spec's "Externally Synchronized Parameters" and slap some `&mut`s and
//! `antidote::Mutex<>`es around.

#[macro_use]
mod utils;

mod command;
mod device;
mod framebuffers;
mod instance;
mod pipelines;
mod shader;
mod swapchain;
mod vertex;
mod window;

pub use ash::vk::ShaderStageFlags;
pub use command::CommandBuffers;
pub use device::Device;
pub use framebuffers::Framebuffers;
pub use instance::Instance;
pub use pipelines::{forward::ForwardPipeline, Pipeline};
pub use shader::Shader;
pub use swapchain::Swapchain;
pub use vertex::Vertex;
pub use window::Window;
