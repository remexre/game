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
