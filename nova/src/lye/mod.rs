//! A wrapper around `ash` that makes it more ergonomic (largely via RAII).

mod device;
mod instance;
mod shader;
mod window;

pub use device::Device;
pub use instance::Instance;
pub use shader::Shader;
pub use window::Window;
