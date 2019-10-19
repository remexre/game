use anyhow::{Context, Result};
use nova::Renderer;

fn main() -> Result<()> {
    stderrlog::new().verbosity(3).init().unwrap();

    let mut renderer = Renderer::new(
        "Vulkan Test",
        "../assets/shaders/tutorial.vert.spv",
        "../assets/shaders/tutorial.frag.spv",
    )
    .context("new")?;
    while !renderer.should_close() {
        for event in renderer.poll_events() {
            println!("{:?}", event);
        }

        renderer
            .draw(|mut target| {
                target.draw(3);
                Ok(())
            })
            .context("draw")?;
    }
    renderer.wait_idle().context("wait_idle")?;
    Ok(())
}
