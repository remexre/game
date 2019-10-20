use anyhow::Result;
use lye::*;

fn main() -> Result<()> {
    stderrlog::new().verbosity(3).init()?;
    run()?;
    Instance::assert_no_instances_exist();
    Ok(())
}

fn run() -> Result<()> {
    let window = Window::new("Vulkan Example")?;
    let instance = Instance::new(&window, true)?;
    let device = Device::new(&window, instance)?;

    let swapchain = Swapchain::new(device.clone())?;
    let vert = Shader::load_from_path(
        device.clone(),
        "assets/shaders/tutorial.vert.spv",
        ShaderStageFlags::VERTEX,
    )?;
    let frag = Shader::load_from_path(
        device.clone(),
        "assets/shaders/tutorial.frag.spv",
        ShaderStageFlags::FRAGMENT,
    )?;
    let pipeline = ForwardPipeline::new(swapchain, vert, frag)?;
    let command_manager = CommandManager::new(pipeline)?;

    Ok(())
}