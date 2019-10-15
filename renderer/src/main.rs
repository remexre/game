use libremexre::errors::Result;

fn main() {
    run().unwrap();
}

fn run() -> Result<()> {
    let instance = renderer::init::create_instance(true)?;
    let pd = renderer::init::choose_physical_device(&instance)?;
    let qf = renderer::init::choose_queue_family(pd)?;
    let (dev, queue) = renderer::init::create_device(pd, qf)?;
    dbg!(dev);
    Ok(())
}
