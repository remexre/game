use crate::{pass::GameRenderPass, RendererState};
use std::{borrow::Cow, sync::Arc};
use vulkano::{
    device::{Device, DeviceExtensions, Features},
    framebuffer::RenderPass,
    instance::{
        ApplicationInfo, Instance, PhysicalDevice, PhysicalDeviceType, QueueFamily, Version,
    },
    swapchain::{PresentMode, SurfaceTransform, Swapchain},
    sync::now,
};
use vulkano_win::VkSurfaceBuild;
use winit::{EventsLoop, WindowBuilder};

impl RendererState {
    /// Creates a new `RendererState` with the given application name and version.
    pub fn new(name: Option<&str>, version: Option<(u16, u16, u16)>) -> RendererState {
        let app_info = ApplicationInfo {
            application_name: name.map(Cow::Borrowed),
            application_version: version.map(|(major, minor, patch)| Version {
                major,
                minor,
                patch,
            }),
            engine_name: Some(Cow::Borrowed("ia-renderer")),
            engine_version: Some(Version {
                major: env!("CARGO_PKG_VERSION_MAJOR").parse().unwrap(),
                minor: env!("CARGO_PKG_VERSION_MINOR").parse().unwrap(),
                patch: env!("CARGO_PKG_VERSION_PATCH").parse().unwrap(),
            }),
        };
        let instance = Instance::new(Some(&app_info), &vulkano_win::required_extensions(), None)
            .expect("Failed to create instance");

        let mut pds = PhysicalDevice::enumerate(&instance)
            .map(|pd| {
                let mem_bytes: usize = pd.memory_heaps().map(|h| h.size()).sum();
                let mem_gb = mem_bytes >> 30;
                let qf_count: usize = pd
                    .queue_families()
                    .filter(QueueFamily::supports_graphics)
                    .map(|qf| qf.queues_count())
                    .sum();
                let type_priority = match pd.ty() {
                    PhysicalDeviceType::DiscreteGpu => 5,
                    PhysicalDeviceType::IntegratedGpu => 4,
                    PhysicalDeviceType::VirtualGpu => 3,
                    PhysicalDeviceType::Cpu => 2,
                    PhysicalDeviceType::Other => 1,
                };
                let priority = if qf_count == 0 {
                    0
                } else {
                    (100 * type_priority) + (10 * mem_gb) + qf_count
                };
                (pd, priority)
            })
            .collect::<Vec<_>>();
        pds.sort_by_key(|&(_, priority)| priority);

        if pds.is_empty() {
            panic!("No Vulkan devices found");
        }

        println!("Found {} Vulkan devices:", pds.len());
        for (n, (pd, priority)) in pds.iter().enumerate() {
            println!("#{} (p={}): {}", n, priority, pd.name());
        }
        let pd = pds.remove(0).0;
        drop(pds);

        let mut qfs = pd
            .queue_families()
            .filter(|&qf| qf.supports_graphics())
            .collect::<Vec<_>>();
        qfs.sort_by_key(QueueFamily::queues_count);

        if qfs.is_empty() {
            panic!("Primary Vulkan device had no queue families");
        }

        let qf = qfs.remove(0);
        drop(qfs);

        let device_exts = DeviceExtensions {
            khr_swapchain: true,
            ..DeviceExtensions::none()
        };
        let (device, queues) = Device::new(pd, &Features::none(), &device_exts, Some((qf, 0.5)))
            .expect("Failed to create device");

        let mut queues = queues.collect::<Vec<_>>();
        if queues.is_empty() {
            panic!("Primary Vulkan device had no queues");
        }
        let queue = queues.remove(0);
        drop(queues);

        let event_loop = EventsLoop::new();
        let surface = WindowBuilder::new()
            .with_title("game")
            .build_vk_surface(&event_loop, instance.clone())
            .expect("Failed to create window");

        let window = surface.window();
        let caps = surface.capabilities(device.physical_device()).unwrap();
        let dims = window
            .get_inner_size()
            .map(|dims| dims.to_physical(window.get_hidpi_factor()).into())
            .map(|(w, h)| [w, h])
            .or_else(|| caps.current_extent)
            .unwrap_or_else(|| {
                panic!("Window was closed immediately?");
            });
        let alpha = caps.supported_composite_alpha.iter().next().unwrap();
        let format = caps.supported_formats[0].0;
        let (swapchain, images) = Swapchain::new(
            device.clone(),
            surface.clone(),
            caps.min_image_count,
            format,
            dims,
            1,
            caps.supported_usage_flags,
            &queue,
            SurfaceTransform::Identity,
            alpha,
            PresentMode::Fifo,
            true,
            None,
        )
        .expect("Failed to create swapchain");

        let render_pass = Arc::new(
            RenderPass::new(
                device.clone(),
                GameRenderPass {
                    color_format: swapchain.format(),
                },
            )
            .expect("Failed to create render pass"),
        );

        let cleanup_future = now(device.clone());

        RendererState {
            device,
            event_loop,
            images,
            instance,
            queue,
            surface,
            swapchain,
            render_pass,
            recreate_swapchain: false,

            command_buffer_builder: None,
            image_num: std::usize::MAX,

            cleanup_future: Some(Box::new(cleanup_future)),
        }
    }
}
