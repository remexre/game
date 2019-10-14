use libremexre::errors::Result;
use std::{ffi::CString, sync::Arc};
use vulkano::instance::{
    layers_list, Instance, PhysicalDevice, PhysicalDeviceType, RawInstanceExtensions,
};
use winit::{EventsLoop, WindowBuilder};

#[derive(Clone, Copy, Debug, Default)]
pub struct InitFlags {
    debug: bool,
    rtx: bool,
}

pub fn create_instance() -> Result<(Arc<Instance>, InitFlags)> {
    let win_exts = vulkano_win::required_extensions();
    let mut exts: RawInstanceExtensions = (&win_exts).into();
    let mut flags = InitFlags::default();

    println!("Extensions:");
    for ext in RawInstanceExtensions::supported_by_core()?.iter() {
        let enabled = if ext.as_bytes() == b"VK_NV_ray_tracing" {
            flags.rtx = true;
            exts.insert(ext.clone());
            true
        } else {
            false
        };

        println!(
            "{} {}",
            if enabled { '+' } else { '-' },
            ext.to_string_lossy()
        );
    }

    println!("Layers:");
    let mut layers = Vec::new();
    for layer in layers_list()? {
        let name = layer.name();

        let enabled = if name == "VK_LAYER_KHRONOS_validation" {
            flags.debug = true;
            layers.push(name.to_string());
            true
        } else {
            false
        };

        println!("{} {}", if enabled { '+' } else { '-' }, name);
    }

    let instance = Instance::new(None, exts, layers.iter().map(|s| s as &str))?;
    Ok((instance, flags))
}

pub fn choose_physical_device(instance: &Arc<Instance>) -> Result<PhysicalDevice> {
    let dev = PhysicalDevice::enumerate(instance).max_by_key(|dev| match dev.ty() {
        PhysicalDeviceType::IntegratedGpu => 1,
        PhysicalDeviceType::DiscreteGpu => 2,
        _ => 0,
    });
    let dev = dev.ok_or_else(|| "No Vulkan devices found")?;
    println!("Choosing physical device {:?}", dev.name());
    Ok(dev)
}

pub fn create_window(instance: Arc<Instance>) -> Result<()> {
    let events_loop = EventsLoop::new();
    unimplemented!()
}
