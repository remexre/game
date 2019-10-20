use anyhow::Result;
use log::info;
use lye::*;

fn main() -> Result<()> {
    stderrlog::new().verbosity(3).init()?;
    run()?;
    Instance::assert_no_instances_exist();
    Ok(())
}

fn run() -> Result<()> {
    let mut window = Window::new_fixed_size("Vulkan Example", 800, 600)?;
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
    let mut command_manager = CommandManager::new(pipeline)?;

    while !window.should_close() {
        // This doesn't do resizing.
        command_manager.flip()?;

        for ev in window.poll_events() {
            info!("{:?}", ev);
        }
    }

    Ok(())
}
