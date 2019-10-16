//! Basic initialization of a Vulkan-supporting window.

use crate::utils::char_array_to_cstring;
use ash::{
    version::{DeviceV1_0, EntryV1_0, InstanceV1_0},
    vk::{
        DeviceCreateInfo, DeviceQueueCreateInfo, InstanceCreateInfo, PhysicalDevice,
        PhysicalDeviceFeatures, PhysicalDeviceType, Queue, QueueFlags,
    },
    Device, Entry, Instance,
};
use glfw::Glfw;
use lazy_static::lazy_static;
use libremexre::errors::Result;
use std::ffi::CString;

lazy_static! {
    static ref REQUIRED_INSTANCE_EXTS: Vec<CString> = {
        Vec::new() // TODO
    };

    static ref WANTED_INSTANCE_EXTS: Vec<CString> = {
        cstrings![
            "VK_KHR_get_physical_device_properties2", // for VK_NV_ray_tracing
        ]
    };

    static ref REQUIRED_DEVICE_EXTS: Vec<CString> = {
        cstrings!["VK_KHR_swapchain"]
    };

    static ref WANTED_DEVICE_EXTS: Vec<CString> = {
        cstrings![
            "VK_KHR_get_memory_requirements2", // for VK_NV_ray_tracing
            "VK_NV_ray_tracing",
        ]
    };
}

pub fn create_instance(glfw: &Glfw, entry: &Entry, debug: bool) -> Result<Instance> {
    let mut exts = REQUIRED_INSTANCE_EXTS.clone();
    exts.extend(
        glfw.get_required_instance_extensions()
            .ok_or("GLFW doesn't support Vulkan")?
            .into_iter()
            .map(|cstr| CString::new(cstr.into_bytes()).unwrap()),
    );
    println!("Instance Extensions:");
    for ext in entry.enumerate_instance_extension_properties()? {
        let name = char_array_to_cstring(&ext.extension_name);
        let flag = if WANTED_INSTANCE_EXTS.contains(&name) {
            exts.push(name.to_owned());
            '+'
        } else if exts.contains(&name) {
            '+'
        } else {
            '-'
        };

        println!("{} {}", flag, name.to_string_lossy());
    }

    let mut layers = Vec::new();
    println!("Layers:");
    for layer in entry.enumerate_instance_layer_properties()? {
        let name = char_array_to_cstring(&layer.layer_name);
        let flag = if debug && name.to_bytes() == b"VK_LAYER_KHRONOS_validation" {
            layers.push(name.to_owned());
            '+'
        } else {
            '-'
        };

        println!("{} {}", flag, name.to_string_lossy());
    }

    let ext_ptrs = exts.iter().map(|name| name.as_ptr()).collect::<Vec<_>>();
    let layer_ptrs = layers.iter().map(|name| name.as_ptr()).collect::<Vec<_>>();
    let create_info = InstanceCreateInfo::builder()
        .enabled_layer_names(&layer_ptrs)
        .enabled_extension_names(&ext_ptrs);

    let instance = unsafe { entry.create_instance(&create_info, None)? };
    Ok(instance)
}

pub fn choose_physical_device_and_queue_family(
    instance: &Instance,
) -> Result<(PhysicalDevice, u32)> {
    let pds = unsafe { instance.enumerate_physical_devices()? }
        .into_iter()
        .map(|pd| {
            let props = unsafe { instance.get_physical_device_properties(pd) };
            let name = char_array_to_cstring(&props.device_name);
            let exts = unsafe { instance.enumerate_device_extension_properties(pd)? }
                .into_iter()
                .map(|ext| char_array_to_cstring(&ext.extension_name))
                .collect::<Vec<_>>();
            unsafe { instance.get_physical_device_queue_family_properties(pd) }
                .into_iter()
                .enumerate()
                .filter(|(_, props)| props.queue_flags.contains(QueueFlags::GRAPHICS))
                .next()
                .map(|(qf, _)| Ok((pd, qf as u32, name, props.device_type, exts)))
                .transpose()
        })
        .collect::<Result<Vec<_>>>()?;
    let pd_info = pds
        .into_iter()
        .filter_map(|o| o)
        .filter(|(_, _, _, _, exts)| REQUIRED_DEVICE_EXTS.iter().all(|ext| exts.contains(ext)))
        .max_by_key(|(_, _, _, device_type, exts)| {
            let exts_points = if WANTED_DEVICE_EXTS.iter().all(|ext| exts.contains(ext)) {
                10
            } else {
                0
            };

            let type_points = match *device_type {
                PhysicalDeviceType::DISCRETE_GPU => 2,
                PhysicalDeviceType::INTEGRATED_GPU => 1,
                _ => 0,
            };

            exts_points + type_points
        })
        .ok_or("No suitable Vulkan device found")?;

    let (pd, qf, device_name, _, _) = pd_info;
    println!("Choosing physical device {:?}", device_name);
    Ok((pd, qf))
}

pub fn create_device(instance: &Instance, pd: PhysicalDevice, qf: u32) -> Result<(Device, Queue)> {
    let mut exts = REQUIRED_DEVICE_EXTS.clone();
    println!("Device Extensions:");
    for ext in unsafe { instance.enumerate_device_extension_properties(pd)? } {
        let name = char_array_to_cstring(&ext.extension_name);
        let flag = if WANTED_DEVICE_EXTS.contains(&name) {
            exts.push(name.to_owned());
            '+'
        } else if exts.contains(&name) {
            '+'
        } else {
            '-'
        };

        println!("{} {}", flag, name.to_string_lossy());
    }

    let queue_create_info = DeviceQueueCreateInfo::builder()
        .queue_family_index(qf)
        .queue_priorities(&[1.0]);
    let queue_create_infos: Vec<DeviceQueueCreateInfo> = vec![*queue_create_info];
    let exts = exts.into_iter().map(|ext| ext.as_ptr()).collect::<Vec<_>>();
    let features = PhysicalDeviceFeatures::default();
    let create_info = DeviceCreateInfo::builder()
        .queue_create_infos(&queue_create_infos)
        .enabled_extension_names(&exts)
        .enabled_features(&features);
    let dev = unsafe { instance.create_device(pd, &create_info, None)? };
    let queue = unsafe { dev.get_device_queue(qf, 0) };
    Ok((dev, queue))
}

/*
pub fn create_swapchain(
    dev: Arc<Device>,
    queue: &Arc<Queue>,
    surface: Arc<Surface<Window>>,
) -> Result<(Arc<Swapchain<Window>>, Vec<Arc<SwapchainImage<Window>>>)> {
    /*
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
    */
}

*/
