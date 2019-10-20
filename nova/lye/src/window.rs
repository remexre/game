use crate::Instance;
use anyhow::{anyhow, Context, Result};
use ash::{
    prelude::VkResult,
    version::InstanceV1_0,
    vk::{Handle, SurfaceKHR},
};
use derivative::Derivative;
use glfw::{
    ffi::glfwCreateWindowSurface, ClientApiHint, Context as GlfwContext, Glfw, WindowEvent,
    WindowHint, WindowMode,
};
use std::{
    ffi::CString,
    mem::MaybeUninit,
    sync::{mpsc::Receiver, Arc},
};

/// A GLFW-based window.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Window {
    #[derivative(Debug = "ignore")]
    glfw: Glfw,
    #[derivative(Debug = "ignore")]
    window: glfw::Window,
    events: Receiver<(f64, WindowEvent)>,
}

impl Window {
    /// Creates a new window, not set up for rendering.
    pub fn new(name: &str) -> Result<Window> {
        let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).context("Failed to initialize GLFW")?;
        glfw.window_hint(WindowHint::ClientApi(ClientApiHint::NoApi));
        let (mut window, events) = glfw
            .create_window(800, 600, name, WindowMode::Windowed)
            .ok_or_else(|| anyhow!("Failed to create window"))?;
        window.set_key_polling(true);
        window.set_size_polling(true);
        Ok(Window {
            glfw,
            window,
            events,
        })
    }

    /// Polls for events.
    pub fn poll_events(&mut self) -> Vec<(f64, WindowEvent)> {
        self.glfw.poll_events();
        self.events.try_iter().collect()
    }

    /// Sets the title of the window.
    pub fn set_title(&mut self, title: &str) {
        self.window.set_title(title);
    }

    /// Returns whether the window should close.
    pub fn should_close(&self) -> bool {
        self.window.should_close()
    }
}

impl Window {
    /// Creates a `SurfaceKHR` corresponding to the window.
    pub(crate) fn create_surface(&self, instance: &Arc<Instance>) -> VkResult<SurfaceKHR> {
        let mut surface = MaybeUninit::uninit();
        let result = ash::vk::Result::from_raw(unsafe {
            glfwCreateWindowSurface(
                instance.handle().as_raw() as usize,
                self.window.window_ptr(),
                std::ptr::null(),
                surface.as_mut_ptr(),
            ) as i32
        });

        if result == ash::vk::Result::SUCCESS {
            Ok(unsafe { SurfaceKHR::from_raw(surface.assume_init()) })
        } else {
            Err(result)
        }
    }

    /// Returns Vulkan extensions needed to render to this window.
    pub(crate) fn required_vulkan_extensions(&self) -> Result<Vec<CString>> {
        let exts = self
            .glfw
            .get_required_instance_extensions()
            .ok_or_else(|| anyhow!("GLFW doesn't support Vulkan"))?;
        exts.into_iter()
            .map(|cstr| {
                CString::new(cstr.into_bytes())
                    .context("A GLFW-required extension contained a null byte???")
            })
            .collect()
    }
}
