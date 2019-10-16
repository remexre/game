use libremexre::errors::Result;
use renderer::Renderer;

fn main() {
    run().unwrap();
}

fn run() -> Result<()> {
    stderrlog::new().verbosity(3).init()?;

    let mut renderer = Renderer::new("Vulkan Test")?;
    while !renderer.should_close() {
        for event in renderer.poll_events() {
            println!("{:?}", event);
        }
    }
    Ok(())
}
