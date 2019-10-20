use crate::lye::Device;
use anyhow::{anyhow, Result};
use ash::{
    version::DeviceV1_0,
    vk::{
        ColorSpaceKHR, ComponentMapping, CompositeAlphaFlagsKHR, Extent2D, Format, Image,
        ImageAspectFlags, ImageSubresourceRange, ImageUsageFlags, ImageView, ImageViewCreateInfo,
        ImageViewType, PresentModeKHR, SharingMode, SwapchainCreateInfoKHR, SwapchainKHR,
    },
};
use derivative::Derivative;
use log::debug;
use std::sync::Arc;

#[derive(Derivative)]
#[derivative(Debug)]
pub struct Swapchain {
    swapchain: SwapchainKHR,
    images: Vec<(Image, ImageView)>,
    pub(crate) format: Format,
    pub(crate) extent: Extent2D,

    // The device must outlive us.
    pub(crate) device: Arc<Device>,
}

impl Swapchain {
    /// Creates a new Swapchain.
    ///
    /// **TODO**: Comments, context for errors.
    pub fn new(device: Arc<Device>) -> Result<Swapchain> {
        let formats = unsafe {
            device
                .instance
                .surface_ext
                .get_physical_device_surface_formats(device.pd, device.surface)?
        };

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

        let present_modes = unsafe {
            device
                .instance
                .surface_ext
                .get_physical_device_surface_present_modes(device.pd, device.surface)?
        };

        let present_mode = present_modes
            .into_iter()
            .max_by_key(|&m| match m {
                PresentModeKHR::MAILBOX => 2,
                PresentModeKHR::FIFO => 1,
                _ => 0,
            })
            .ok_or_else(|| anyhow!("Surface doesn't support any present modes"))?;

        let caps = unsafe {
            device
                .instance
                .surface_ext
                .get_physical_device_surface_capabilities(device.pd, device.surface)?
        };
        let extent = caps.current_extent;
        debug!("Current extent: {:?}", extent);

        let num_images = caps.min_image_count
            + if caps.min_image_count == caps.max_image_count {
                0
            } else {
                1
            };

        let qf_indices = [];
        let create_info = SwapchainCreateInfoKHR::builder()
            .surface(device.surface)
            .min_image_count(num_images)
            .image_format(format.format)
            .image_color_space(format.color_space)
            .image_extent(extent)
            .image_array_layers(1)
            .image_usage(ImageUsageFlags::COLOR_ATTACHMENT)
            .image_sharing_mode(SharingMode::EXCLUSIVE)
            .queue_family_indices(&qf_indices)
            .pre_transform(caps.current_transform)
            .composite_alpha(CompositeAlphaFlagsKHR::OPAQUE)
            .present_mode(present_mode)
            .clipped(true);

        let swapchain = unsafe { device.swapchain_ext.create_swapchain(&create_info, None)? };

        let images = unsafe { device.swapchain_ext.get_swapchain_images(swapchain)? };
        let images = images
            .into_iter()
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
                let image_view = unsafe { device.device.create_image_view(&create_info, None)? };
                Ok((image, image_view))
            })
            .collect::<Result<_>>()?;

        Ok(Swapchain {
            swapchain,
            images,
            format: format.format,
            extent,

            device,
        })
    }
}

impl Drop for Swapchain {
    fn drop(&mut self) {
        unsafe {
            // The format and extent are plain data, so don't need destroying.

            for (_, image_view) in self.images.drain(..) {
                // Apparently we don't destroy the actual images; I think the intuition might be
                // that they're "part of the swapchain"?
                // TODO: Verify that this is indeed the case.
                self.device.destroy_image_view(image_view, None);
            }

            self.device
                .swapchain_ext
                .destroy_swapchain(self.swapchain, None);
        }
    }
}
