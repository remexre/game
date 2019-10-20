use anyhow::Result;
use nova::Renderer;

fn main() -> Result<()> {
    stderrlog::new().verbosity(3).init()?;

    let renderer = Renderer::new(
        "Vulkan Example",
        true,
        "assets/shaders/tutorial.vert.spv",
        "assets/shaders/tutorial.frag.spv",
    )?;

    Ok(())
}
