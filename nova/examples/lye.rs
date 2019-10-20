use anyhow::Result;
use nova::lye::*;

fn main() -> Result<()> {
    stderrlog::new().verbosity(3).init()?;

    let window = Window::new("Vulkan Example")?;
    let instance = Instance::new(&window, true)?;
    let device = Device::new(&window, instance)?;

    let vert_shader = Shader::load_from_path(device.clone(), "assets/shaders/tutorial.vert.spv")?;
    let frag_shader = Shader::load_from_path(device.clone(), "assets/shaders/tutorial.frag.spv")?;

    Ok(())
}
