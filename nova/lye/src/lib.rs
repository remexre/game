//! A higher-level wrapper around `ash`. Probably unsound if you stray off the happy path.
//!
//! See `nova/examples/lye.rs` for an example of usage.
//!
//! **TODO**: Go over the spec's "Externally Synchronized Parameters" and slap some `&mut`s and
//! `antidote::Mutex<>`es around; the happy path is not yet threaded.
//!
//! **TODO**: Go over all the `new`-shaped functions and be sure resources are freed appropriately
//! on error.
#![deny(missing_docs)]

#[macro_use]
mod utils;

mod buffer;
mod command;
mod device;
mod instance;
mod layout;
mod pipelines;
mod shader;
mod swapchain;
mod window;

pub use ash::vk::{BufferUsageFlags, Result as VkResult, ShaderStageFlags};
pub use glfw::WindowEvent;

pub use buffer::{immutable::ImmutableBuffer, mutable::MutableBuffer};
pub use command::CommandManager;
pub use device::Device;
pub use instance::Instance;
pub use layout::{Uniforms, Vertex};
pub use pipelines::{forward::ForwardPipeline, DrawContext, Pipeline};
pub use shader::Shader;
pub use swapchain::Swapchain;
pub use window::Window;
