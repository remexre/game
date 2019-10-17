use anyhow::Result;
use log::error;
use renderer::Renderer;
use std::process::exit;

fn main() {
    stderrlog::new().verbosity(3).init().unwrap();

    if let Err(err) = run() {
        for err in err.chain() {
            error!("{}", err);
        }
        exit(1);
    }
}

fn run() -> Result<()> {
    let mut renderer = Renderer::new(
        "Vulkan Test",
        "../assets/shaders/tutorial.vert.spv",
        "../assets/shaders/tutorial.frag.spv",
    )?;
    while !renderer.should_close() {
        for event in renderer.poll_events() {
            println!("{:?}", event);
        }

        renderer.draw(|| {
            println!("osu");
            Ok(())
        })?;
    }
    renderer.wait_idle()?;
    Ok(())
}
