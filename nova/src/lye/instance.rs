use crate::{lye::Window, utils::char_array_to_cstring};
use anyhow::{Context as AnyhowContext, Result};
use ash::{
    extensions::khr::Surface,
    version::{EntryV1_0, InstanceV1_0},
    vk::{InstanceCreateInfo, KhrGetPhysicalDeviceProperties2Fn},
    Entry, Instance as AshInstance,
};
use derivative::Derivative;
use lazy_static::lazy_static;
use log::trace;
use std::{ffi::CString, sync::Arc};

lazy_static! {
    static ref REQUIRED_EXTS: Vec<CString> = {
        extensions![
            Surface,
        ]
    };

    static ref WANTED_EXTS: Vec<CString> = {
        extensions![
            KhrGetPhysicalDeviceProperties2Fn, // for RayTracing
        ]
    };
}

/// A Vulkan instance, and some extensions we depend on.
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct Instance {
    #[derivative(Debug = "ignore")]
    pub(crate) entry: Entry,

    #[derivative(Debug = "ignore")]
    pub(crate) instance: AshInstance,

    #[derivative(Debug = "ignore")]
    pub(crate) surface_ext: Surface,
}

impl Instance {
    /// Creates a new `Instance`.
    pub fn new(window: &Arc<Window>, debug: bool) -> Result<Arc<Instance>> {
        // Get all the Vulkan pointers.
        let entry = Entry::new().context("Failed to get a Vulkan entrypoint")?;

        // Collect all the extensions we want and the instance supports.
        let mut exts = REQUIRED_EXTS.clone();
        exts.extend(window.required_vulkan_extensions()?);
        trace!("Instance Extensions:");
        for ext in entry
            .enumerate_instance_extension_properties()
            .context("Failed to get instance extensions")?
        {
            let name = char_array_to_cstring(&ext.extension_name);
            let flag = if exts.contains(&name) {
                '+'
            } else if WANTED_EXTS.contains(&name) {
                exts.push(name.to_owned());
                '+'
            } else {
                '-'
            };

            trace!("{} {}", flag, name.to_string_lossy());
        }

        // Collect all the layers we want and the instance supports.
        let mut layers = Vec::new();
        trace!("Layers:");
        for layer in entry
            .enumerate_instance_layer_properties()
            .context("Failed to list available instance layers")?
        {
            let name = char_array_to_cstring(&layer.layer_name);
            let flag = if debug && name.to_bytes() == b"VK_LAYER_KHRONOS_validation" {
                layers.push(name.to_owned());
                '+'
            } else {
                '-'
            };

            trace!("{} {}", flag, name.to_string_lossy());
        }

        // Create the instance.
        let ext_ptrs = exts.iter().map(|name| name.as_ptr()).collect::<Vec<_>>();
        let layer_ptrs = layers.iter().map(|name| name.as_ptr()).collect::<Vec<_>>();
        let create_info = InstanceCreateInfo::builder()
            .enabled_layer_names(&layer_ptrs)
            .enabled_extension_names(&ext_ptrs);
        let instance = unsafe {
            entry
                .create_instance(&create_info, None)
                .context("Failed to create instance")?
        };

        // Create the extensions.
        let surface_ext = Surface::new(&entry, &instance);

        Ok(Arc::new(Instance {
            entry,
            instance,
            surface_ext,
        }))
    }
}

impl Drop for Instance {
    fn drop(&mut self) {
        unsafe {
            self.instance.destroy_instance(None);
        }
    }
}
