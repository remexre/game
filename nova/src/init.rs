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
