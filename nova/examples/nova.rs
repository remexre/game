use anyhow::Result;
use nova::{lye::Instance, Renderer};

fn main() -> Result<()> {
    stderrlog::new().verbosity(3).init()?;
    run()?;
    Instance::assert_no_instances_exist();
    Ok(())
}

fn run() -> Result<()> {
    let renderer = Renderer::new(
        "Vulkan Example",
        true,
        "assets/shaders/tutorial.vert.spv",
        "assets/shaders/tutorial.frag.spv",
    )?;

    Ok(())
}
