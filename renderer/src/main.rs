use libremexre::errors::Result;

fn main() {
    run().unwrap();
}

fn run() -> Result<()> {
    let (instance, init_flags) = renderer::init::create_instance()?;
    let pd = renderer::init::choose_physical_device(&instance)?;
    dbg!(pd);
    Ok(())
}
