use glfw::{ClientApiHint, WindowHint, WindowMode};
use libremexre::errors::Result;

fn main() {
    run().unwrap();
}

fn run() -> Result<()> {
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS)?;
    glfw.window_hint(WindowHint::ClientApi(ClientApiHint::NoApi));
    glfw.window_hint(WindowHint::Resizable(false));
    let (window, events) = glfw
        .create_window(800, 600, "Vulkan", WindowMode::Windowed)
        .ok_or_else(|| "Failed to create window")?;

    let entry = ash::Entry::new()?;
    let instance = renderer::init::create_instance(&glfw, &entry, true)?;
    let (pd, qf) = renderer::init::choose_physical_device_and_queue_family(&instance)?;
    let (dev, queue) = renderer::init::create_device(&instance, pd, qf)?;

    while !window.should_close() {
        for event in events.try_iter() {
            println!("{:?}", event);
        }

        glfw.poll_events();
    }
    /*
    let (swapchain, images) = renderer::init::create_swapchain(dev, &queue, surface)?;
    dbg!((events_loop, swapchain, images.len()));
    */
    Ok(())
}
