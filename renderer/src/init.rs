//! Basic initialization of a Vulkan-supporting window.

use crate::utils::char_array_to_cstring;
use anyhow::{anyhow, Result};
use ash::{
    extensions::khr::{Surface, Swapchain},
    version::{DeviceV1_0, EntryV1_0, InstanceV1_0},
    vk::{
        ColorSpaceKHR, ComponentMapping, CompositeAlphaFlagsKHR, DeviceCreateInfo,
        DeviceQueueCreateInfo, Extent2D, Format, Framebuffer, FramebufferCreateInfo, Handle, Image,
        ImageAspectFlags, ImageSubresourceRange, ImageUsageFlags, ImageView, ImageViewCreateInfo,
        ImageViewType, InstanceCreateInfo, PhysicalDevice, PhysicalDeviceType, PresentModeKHR,
        Queue, QueueFlags, RenderPass, SharingMode, SurfaceKHR, SwapchainCreateInfoKHR,
        SwapchainKHR,
    },
    Device, Entry, Instance,
};
use glfw::{
    ffi::glfwCreateWindowSurface, ClientApiHint, Context, Glfw, Window, WindowEvent, WindowHint,
    WindowMode,
};
use lazy_static::lazy_static;
use log::debug;
use std::{ffi::CString, mem::MaybeUninit, slice, sync::mpsc::Receiver};

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

pub fn create_window(name: &str) -> Result<(Glfw, Window, Receiver<(f64, WindowEvent)>)> {
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS)?;
    glfw.window_hint(WindowHint::ClientApi(ClientApiHint::NoApi));
    // glfw.window_hint(WindowHint::Resizable(false));
    let (mut window, events) = glfw
        .create_window(800, 600, name, WindowMode::Windowed)
        .ok_or_else(|| anyhow!("Failed to create window"))?;
    window.set_key_polling(true);
    Ok((glfw, window, events))
}

pub fn create_instance(glfw: &Glfw, entry: &Entry, debug: bool) -> Result<Instance> {
    let mut exts = REQUIRED_INSTANCE_EXTS.clone();
    exts.extend(
        glfw.get_required_instance_extensions()
            .ok_or_else(|| anyhow!("GLFW doesn't support Vulkan"))?
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
    surface_ext: &Surface,
    surface: SurfaceKHR,
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
                .map(|(i, props)| (i as u32, props))
                .filter(|(_, props)| props.queue_flags.contains(QueueFlags::GRAPHICS))
                .filter(|(qf, _)| unsafe {
                    surface_ext.get_physical_device_surface_support(pd, *qf, surface)
                })
                // TODO: Filter for presentation support
                .next()
                .map(|(qf, _)| Ok((pd, qf, name, props.device_type, exts)))
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
        .ok_or_else(|| anyhow!("No suitable Vulkan device found"))?;

    let (pd, qf, device_name, _, _) = pd_info;
    debug!("Choosing physical device {:?}", device_name);
    Ok((pd, qf))
}

pub fn create_device(
    instance: &Instance,
    pd: PhysicalDevice,
    qf: u32,
) -> Result<(Device, Queue, bool)> {
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
    let ext_ptrs = exts.iter().map(|ext| ext.as_ptr()).collect::<Vec<_>>();
    let create_info = DeviceCreateInfo::builder()
        .queue_create_infos(&queue_create_infos)
        .enabled_extension_names(&ext_ptrs);
    let dev = unsafe { instance.create_device(pd, &create_info, None)? };
    let queue = unsafe { dev.get_device_queue(qf, 0) };
    let has_rtx = exts.iter().any(|e| e.as_bytes() == b"VK_NV_ray_tracing");
    Ok((dev, queue, has_rtx))
}

pub fn create_surface(instance: &Instance, window: &Window) -> Result<SurfaceKHR> {
    // Create the actual surface.
    let mut surface = MaybeUninit::uninit();
    let result = ash::vk::Result::from_raw(unsafe {
        glfwCreateWindowSurface(
            instance.handle().as_raw() as usize,
            window.window_ptr(),
            std::ptr::null(),
            surface.as_mut_ptr(),
        ) as i32
    });
    if result != ash::vk::Result::SUCCESS {
        Err(result)?
    }
    Ok(unsafe { SurfaceKHR::from_raw(surface.assume_init()) })
}

pub fn create_swapchain(
    surface_ext: &Surface,
    swapchain_ext: &Swapchain,
    surface: SurfaceKHR,
    pd: PhysicalDevice,
    dev: &Device,
) -> Result<(SwapchainKHR, Vec<Image>, Vec<ImageView>, Format, Extent2D)> {
    let caps = unsafe { surface_ext.get_physical_device_surface_capabilities(pd, surface)? };
    let formats = unsafe { surface_ext.get_physical_device_surface_formats(pd, surface)? };

    let present_modes =
        unsafe { surface_ext.get_physical_device_surface_present_modes(pd, surface)? };

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
        .ok_or_else(|| anyhow!("Surface doesn't support any formats"))?;

    let present_mode = present_modes
        .into_iter()
        .max_by_key(|&m| match m {
            PresentModeKHR::MAILBOX => 2,
            PresentModeKHR::FIFO => 1,
            _ => 0,
        })
        .ok_or_else(|| anyhow!("Surface doesn't support any present modes"))?;

    debug!("Current extent: {:?}", caps.current_extent);

    let num_images = caps.min_image_count
        + if caps.min_image_count == caps.max_image_count {
            0
        } else {
            1
        };

    let qf_indices = [];
    let create_info = SwapchainCreateInfoKHR::builder()
        .surface(surface)
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

    let swapchain_khr = unsafe { swapchain_ext.create_swapchain(&create_info, None)? };
    let images = unsafe { swapchain_ext.get_swapchain_images(swapchain_khr)? };

    let image_views = images
        .iter()
        .cloned()
        .map(|image| {
            let create_info = ImageViewCreateInfo::builder()
                .image(image)
                .view_type(ImageViewType::TYPE_2D)
                .format(format.format)
                .components(ComponentMapping::default())
                .subresource_range(
                    ImageSubresourceRange::builder()
                        .aspect_mask(ImageAspectFlags::COLOR)
                        .base_mip_level(0)
                        .level_count(1)
                        .base_array_layer(0)
                        .layer_count(1)
                        .build(),
                );
            let image_view = unsafe { dev.create_image_view(&create_info, None)? };
            Ok(image_view)
        })
        .collect::<Result<_>>()?;

    Ok((
        swapchain_khr,
        images,
        image_views,
        format.format,
        caps.current_extent,
    ))
}

pub fn create_framebuffer(
    dev: &Device,
    image_view: &ImageView,
    dims: Extent2D,
    render_pass: RenderPass,
) -> Result<Framebuffer> {
    let create_info = FramebufferCreateInfo::builder()
        .render_pass(render_pass)
        .attachments(slice::from_ref(image_view))
        .width(dims.width)
        .height(dims.height)
        .layers(1);

    let framebuffer = unsafe { dev.create_framebuffer(&create_info, None)? };
    Ok(framebuffer)
}
