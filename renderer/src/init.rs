//! Basic initialization of a Vulkan-supporting window.

use crate::utils::char_array_to_cstring;
use ash::{
    extensions::khr::{Surface, Swapchain},
    version::{DeviceV1_0, EntryV1_0, InstanceV1_0},
    vk::{
        ColorSpaceKHR, CompositeAlphaFlagsKHR, DeviceCreateInfo, DeviceQueueCreateInfo, Format,
        Handle, Image, ImageUsageFlags, InstanceCreateInfo, PhysicalDevice, PhysicalDeviceFeatures,
        PhysicalDeviceType, PresentModeKHR, Queue, QueueFlags, SharingMode, SurfaceKHR,
        SwapchainCreateInfoKHR, SwapchainKHR,
    },
    Device, Entry, Instance,
};
use glfw::{ffi::glfwCreateWindowSurface, Context, Glfw, Window};
use lazy_static::lazy_static;
use libremexre::errors::Result;
use log::debug;
use std::{ffi::CString, mem::MaybeUninit};

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
    debug!("Instance Extensions:");
    for ext in entry.enumerate_instance_extension_properties()? {
        let name = char_array_to_cstring(&ext.extension_name);
        let flag = if exts.contains(&name) {
            '+'
        } else if WANTED_INSTANCE_EXTS.contains(&name) {
            exts.push(name.to_owned());
            '+'
        } else {
            '-'
        };

        debug!("{} {}", flag, name.to_string_lossy());
    }

    let mut layers = Vec::new();
    debug!("Layers:");
    for layer in entry.enumerate_instance_layer_properties()? {
        let name = char_array_to_cstring(&layer.layer_name);
        let flag = if debug && name.to_bytes() == b"VK_LAYER_KHRONOS_validation" {
            layers.push(name.to_owned());
            '+'
        } else {
            '-'
        };

        debug!("{} {}", flag, name.to_string_lossy());
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
                // TODO: Filter for presentation support
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
    debug!("Choosing physical device {:?}", device_name);
    Ok((pd, qf))
}

pub fn create_device(instance: &Instance, pd: PhysicalDevice, qf: u32) -> Result<(Device, Queue)> {
    let mut exts = REQUIRED_DEVICE_EXTS.clone();
    debug!("Device Extensions:");
    for ext in unsafe { instance.enumerate_device_extension_properties(pd)? } {
        let name = char_array_to_cstring(&ext.extension_name);
        let flag = if exts.contains(&name) {
            '+'
        } else if WANTED_DEVICE_EXTS.contains(&name) {
            exts.push(name.to_owned());
            '+'
        } else {
            '-'
        };

        debug!("{} {}", flag, name.to_string_lossy());
    }

    let queue_create_info = DeviceQueueCreateInfo::builder()
        .queue_family_index(qf)
        .queue_priorities(&[1.0]);
    let queue_create_infos: Vec<DeviceQueueCreateInfo> = vec![*queue_create_info];
    let exts = exts.iter().map(|ext| ext.as_ptr()).collect::<Vec<_>>();
    let create_info = DeviceCreateInfo::builder()
        .queue_create_infos(&queue_create_infos)
        .enabled_extension_names(&exts);
    let dev = unsafe { instance.create_device(pd, &create_info, None)? };
    let queue = unsafe { dev.get_device_queue(qf, 0) };
    Ok((dev, queue))
}

pub fn create_swapchain(
    entry: &Entry,
    instance: &Instance,
    pd: PhysicalDevice,
    qf: u32,
    dev: &Device,
    window: &Window,
) -> Result<(SwapchainKHR, Vec<Image>)> {
    // Bind to the extensions.
    let surface = Surface::new(entry, instance);
    let swapchain = Swapchain::new(instance, dev);

    // Create the actual surface.
    let mut surface_khr = MaybeUninit::uninit();
    let result = ash::vk::Result::from_raw(unsafe {
        glfwCreateWindowSurface(
            instance.handle().as_raw() as usize,
            window.window_ptr(),
            std::ptr::null(),
            surface_khr.as_mut_ptr(),
        ) as i32
    });
    if result != ash::vk::Result::SUCCESS {
        Err(result)?
    }
    let surface_khr = unsafe { SurfaceKHR::from_raw(surface_khr.assume_init()) };

    let caps = unsafe { surface.get_physical_device_surface_capabilities(pd, surface_khr)? };
    let formats = unsafe { surface.get_physical_device_surface_formats(pd, surface_khr)? };

    let present_modes =
        unsafe { surface.get_physical_device_surface_present_modes(pd, surface_khr)? };

    let format = formats
        .iter()
        .max_by_key(|f| {
            let format = match f.format {
                Format::B8G8R8A8_UNORM => 1,
                _ => 0,
            };
            let space = match f.color_space {
                ColorSpaceKHR::SRGB_NONLINEAR => 1,
                _ => 0,
            };
            format + space
        })
        .ok_or("Surface doesn't support any formats")?;

    let present_mode = present_modes
        .into_iter()
        .max_by_key(|&m| match m {
            PresentModeKHR::MAILBOX => 2,
            PresentModeKHR::FIFO => 1,
            _ => 0,
        })
        .ok_or("Surface doesn't support any present modes")?;

    debug!("Current extent: {:?}", caps.current_extent);

    let num_images = caps.min_image_count
        + if caps.min_image_count == caps.max_image_count {
            0
        } else {
            1
        };

    let qf_indices = [];
    let create_info = SwapchainCreateInfoKHR::builder()
        .min_image_count(num_images)
        .image_format(format.format)
        .image_color_space(format.color_space)
        .image_extent(caps.current_extent)
        .image_array_layers(1)
        .image_usage(ImageUsageFlags::COLOR_ATTACHMENT)
        .image_sharing_mode(SharingMode::EXCLUSIVE)
        .queue_family_indices(&qf_indices)
        .pre_transform(caps.current_transform)
        .composite_alpha(CompositeAlphaFlagsKHR::OPAQUE)
        .present_mode(present_mode)
        .clipped(true);

    let swapchain_khr = unsafe { swapchain.create_swapchain(&create_info, None)? };
    let images = unsafe { swapchain.get_swapchain_images(swapchain_khr)? };
    Ok((swapchain_khr, images))
}
