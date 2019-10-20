use anyhow::Result;
use nova::lye::*;

fn main() -> Result<()> {
    stderrlog::new().verbosity(3).init()?;

    let window = Window::new("Vulkan Example")?;
    let instance = Instance::new(&window, true)?;
    let device = Device::new(instance, &window)?;

    Ok(())
}
