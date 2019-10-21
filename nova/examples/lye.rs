use anyhow::Result;
use log::info;
use lye::*;

const VERTICES: &[Vertex] = &[
    Vertex {
        position: [-1.0, -1.0, 0.0],
        normal: [1.0, 0.0, 0.0],
        texcoords: [0.0, 0.0],
    },
    Vertex {
        position: [1.0, -1.0, 0.0],
        normal: [0.0, 1.0, 0.0],
        texcoords: [0.0, 0.0],
    },
    Vertex {
        position: [0.0, 1.0, 0.0],
        normal: [0.0, 0.0, 1.0],
        texcoords: [0.0, 0.0],
    },
];

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

    let vbo = ImmutableBuffer::new(device.clone(), VERTICES, BufferUsageFlags::VERTEX_BUFFER)?;

    while !window.should_close() {
        // This doesn't do resizing.
        command_manager.flip()?;

        command_manager.with_draw_context_and_pipeline(|ctx, pipeline| {
            pipeline.draw(ctx, &vbo)?;
            Ok(())
        })?;

        for ev in window.poll_events() {
            info!("{:?}", ev);
        }
    }

    Ok(())
}
