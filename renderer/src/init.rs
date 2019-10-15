//! Basic initialization of a Vulkan-supporting window.

use libremexre::errors::Result;
use std::{ffi::CString, sync::Arc};
use vulkano::{
    device::{Device, Features, Queue, RawDeviceExtensions},
    image::SwapchainImage,
    instance::{
        layers_list, Instance, PhysicalDevice, PhysicalDeviceType, QueueFamily,
        RawInstanceExtensions,
    },
    swapchain::{PresentMode, Surface, SurfaceTransform, Swapchain},
};
use vulkano_win::VkSurfaceBuild;
use winit::{EventsLoop, Window, WindowBuilder};

const WANTED_INSTANCE_EXTS: &[&[u8]] = &[
    b"VK_KHR_get_physical_device_properties2", // for VK_NV_ray_tracing
];

const WANTED_DEVICE_EXTS: &[&[u8]] = &[
    b"VK_KHR_get_memory_requirements2", // for VK_NV_ray_tracing
    b"VK_NV_ray_tracing",
];

lazy_static::lazy_static! {
    static ref REQUIRED_DEVICE_EXTS: RawDeviceExtensions = {
        ["VK_KHR_swapchain"]
            .iter()
            .map(|s| CString::new(*s).unwrap())
            .collect::<RawDeviceExtensions>()
    };

    static ref REQUIRED_INSTANCE_EXTS: RawInstanceExtensions = {
        RawInstanceExtensions::from(&vulkano_win::required_extensions())
    };
}

pub fn create_instance(debug: bool) -> Result<Arc<Instance>> {
    let mut exts = REQUIRED_INSTANCE_EXTS.clone();
    println!("Device Extensions:");
    for ext in RawInstanceExtensions::supported_by_core()?.iter() {
        let flag = if WANTED_INSTANCE_EXTS.contains(&ext.as_bytes()) {
            exts.insert(ext.clone());
            '+'
        } else {
            '-'
        };

        println!("{} {}", flag, ext.to_string_lossy());
    }

    let mut layers = Vec::new();
    println!("Layers:");
    for layer in layers_list()? {
        let name = layer.name();

        let flag = if debug && name == "VK_LAYER_KHRONOS_validation" {
            layers.push(name.to_string());
            '+'
        } else {
            '-'
        };

        println!("{} {}", flag, name);
    }

    let instance = Instance::new(None, exts, layers.iter().map(|s| s as &str))?;
    Ok(instance)
}

pub fn choose_physical_device(instance: &Arc<Instance>) -> Result<PhysicalDevice> {
    let dev = PhysicalDevice::enumerate(instance)
        .filter(|dev| dev.queue_families().any(|qf| qf.supports_graphics()))
        .filter(|&dev| {
            let unsupported_exts =
                REQUIRED_DEVICE_EXTS.difference(&RawDeviceExtensions::supported_by_device(dev));
            let unsupported_ext_count = unsupported_exts.iter().count();
            unsupported_ext_count == 0
        })
        .max_by_key(|dev| match dev.ty() {
            PhysicalDeviceType::IntegratedGpu => 1,
            PhysicalDeviceType::DiscreteGpu => 2,
            _ => 0,
        });
    let dev = dev.ok_or("No suitable Vulkan devices found")?;
    println!("Choosing physical device {:?}", dev.name());
    Ok(dev)
}

pub fn choose_queue_family<'a>(
    dev: PhysicalDevice<'a>,
    surface: &Surface<Window>,
) -> Result<QueueFamily<'a>> {
    let qf = dev
        .queue_families()
        .filter(|qf| qf.supports_graphics())
        .filter(|&qf| surface.is_supported(qf).unwrap_or(false))
        .max_by_key(|qf| qf.queues_count())
        .ok_or("No graphics-supporting queue families found")?;
    Ok(qf)
}

pub fn create_device<'a>(qf: QueueFamily<'a>) -> Result<(Arc<Device>, Arc<Queue>)> {
    let mut exts = REQUIRED_DEVICE_EXTS.clone();
    println!("Device Extensions:");
    for ext in RawDeviceExtensions::supported_by_device(qf.physical_device()).iter() {
        let flag = if WANTED_DEVICE_EXTS.contains(&ext.as_bytes()) {
            exts.insert(ext.clone());
            '+'
        } else {
            '-'
        };

        println!("{} {}", flag, ext.to_string_lossy());
    }

    let (dev, mut queues) = Device::new(
        qf.physical_device(),
        &Features::none(),
        exts,
        [(qf, 1.0)].iter().cloned(),
    )?;
    let queue = queues.next().ok_or("Device had no queues")?;
    Ok((dev, queue))
}

pub fn create_window(instance: Arc<Instance>) -> Result<(EventsLoop, Arc<Surface<Window>>)> {
    let events_loop = EventsLoop::new();
    let surface = WindowBuilder::new().build_vk_surface(&events_loop, instance)?;
    Ok((events_loop, surface))
}

pub fn create_swapchain(
    dev: Arc<Device>,
    queue: &Arc<Queue>,
    surface: Arc<Surface<Window>>,
) -> Result<(Arc<Swapchain<Window>>, Vec<Arc<SwapchainImage<Window>>>)> {
    let caps = surface.capabilities(dev.physical_device())?;

    let format = if caps.supported_formats.is_empty() {
        return Err("No supported formats")?;
    } else {
        caps.supported_formats[0].0
    };

    let dimensions = caps.current_extent.unwrap_or([1920, 1080]);

    let alpha = caps
        .supported_composite_alpha
        .iter()
        .next()
        .ok_or("No supported composite alpha modes")?;

    let (swapchain, images) = Swapchain::new(
        dev,                        // device
        surface,                    // surface
        caps.min_image_count,       // num_images
        format,                     // format
        dimensions,                 // dimensions
        1,                          // layers
        caps.supported_usage_flags, // usage
        queue,                      // sharing
        SurfaceTransform::Identity, // transform
        alpha,                      // alpha
        PresentMode::Fifo,          // mode
        true,                       // clipped
        None,                       // old_swapchain
    )?;

    Ok((swapchain, images))
}
