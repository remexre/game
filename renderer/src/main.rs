use libremexre::errors::Result;

fn main() {
    run().unwrap();
}

fn run() -> Result<()> {
    let instance = renderer::init::create_instance(true)?;
    let pd = renderer::init::choose_physical_device(&instance)?;
    let (events_loop, surface) = renderer::init::create_window(instance.clone())?;
    let qf = renderer::init::choose_queue_family(pd, &surface)?;
    let (dev, queue) = renderer::init::create_device(qf)?;
    let (swapchain, images) = renderer::init::create_swapchain(dev, &queue, surface)?;
    dbg!((events_loop, swapchain, images.len()));
    Ok(())
}
