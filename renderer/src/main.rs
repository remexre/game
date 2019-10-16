use glfw::{ClientApiHint, WindowHint, WindowMode};
use libremexre::errors::Result;

fn main() {
    run().unwrap();
}

fn run() -> Result<()> {
    stderrlog::new().verbosity(3).init()?;

    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS)?;
    glfw.window_hint(WindowHint::ClientApi(ClientApiHint::NoApi));
    glfw.window_hint(WindowHint::Resizable(false));
    let (mut window, events) = glfw
        .create_window(800, 600, "Vulkan", WindowMode::Windowed)
        .ok_or_else(|| "Failed to create window")?;
    window.set_key_polling(true);

    let entry = ash::Entry::new()?;
    let instance = renderer::init::create_instance(&glfw, &entry, true)?;
    let (pd, qf) = renderer::init::choose_physical_device_and_queue_family(&instance)?;
    let (dev, queue) = renderer::init::create_device(&instance, pd, qf)?;
    let (swapchain, images) =
        renderer::init::create_swapchain(&entry, &instance, pd, qf, &dev, &window)?;

    while !window.should_close() {
        for event in events.try_iter() {
            println!("{:?}", event);
        }

        glfw.poll_events();
    }
    Ok(())
}
