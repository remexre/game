use crate::{utils::char_array_to_cstring, Instance, Window};
use anyhow::{anyhow, Context, Result};
use ash::{
    extensions::{khr::Swapchain, nv::RayTracing},
    version::{DeviceV1_0, InstanceV1_0},
    vk::{
        DeviceCreateInfo, DeviceQueueCreateInfo, KhrGetMemoryRequirements2Fn, PhysicalDevice,
        PhysicalDeviceType, Queue, QueueFlags, SurfaceKHR,
    },
    Device as AshDevice,
};
use derivative::Derivative;
use lazy_static::lazy_static;
use log::{debug, trace};
use std::{ffi::CString, sync::Arc};

lazy_static! {
    static ref REQUIRED_EXTS: Vec<CString> = {
        extensions![
            Swapchain,
        ]
    };

    static ref WANTED_EXTS: Vec<CString> = {
        extensions![
            KhrGetMemoryRequirements2Fn, // for RayTracing
            RayTracing,
        ]
    };
}

/// A Vulkan device and queue.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Device {
    pub(crate) surface: SurfaceKHR,
    pub(crate) pd: PhysicalDevice,
    pub(crate) qf: u32,
    #[derivative(Debug = "ignore")]
    device: AshDevice,
    pub(crate) queue: Queue,

    #[derivative(Debug = "ignore")]
    pub(crate) raytracing_ext: Option<RayTracing>,
    #[derivative(Debug = "ignore")]
    pub(crate) swapchain_ext: Swapchain,

    // The instance must outlive us.
    pub(crate) instance: Arc<Instance>,
}

deref_field!(Device, device: ash::Device);

impl Device {
    /// Creates a Vulkan device for the given instance and window.
    pub fn new(window: &Arc<Window>, instance: Arc<Instance>) -> Result<Arc<Device>> {
        // Create a surface.
        let surface = window
            .create_surface(&instance)
            .context("Failed to get a surface from GLFW")?;

        // Get information about all the physical devices on the system.
        let pd_infos = unsafe { instance.enumerate_physical_devices() }
            .context("Failed to list physical devices")?
            .into_iter()
            .map(|pd| {
                let props = unsafe { instance.get_physical_device_properties(pd) };
                let name = char_array_to_cstring(&props.device_name);
                let exts = unsafe { instance.enumerate_device_extension_properties(pd) }
                    .context("Failed to get device extensions")?
                    .into_iter()
                    .map(|ext| char_array_to_cstring(&ext.extension_name))
                    .collect::<Vec<_>>();

                let qf_props = unsafe {
                    instance
                        .get_physical_device_queue_family_properties(pd)
                };

                let qf = qf_props
                    .into_iter()
                    .enumerate()
                    .map(|(i, props)| (i as u32, props))
                    .filter(|(_, props)| props.queue_flags.contains(QueueFlags::GRAPHICS))
                    .filter(|(qf, _)| unsafe {
                        instance
                            .surface_ext
                            .get_physical_device_surface_support(pd, *qf, surface)
                    })
                    // TODO: Filter for presentation support
                    .map(|(qf, _)| qf)
                    .next();

                if let Some(qf) = qf {
                    Ok(Some((pd, qf, name, props.device_type, exts)))
                } else {
                    debug!("Rejecting physical device {:?}: no queue family supports displaying to the window", name);
                    Ok(None)
                }
            })
            .collect::<Result<Vec<_>>>()?;

        // Choose a physical device.
        let pd_info = pd_infos
            .into_iter()
            .filter_map(|o| o)
            .filter(|(_, _, name, _, exts)| {
                let ok = REQUIRED_EXTS.iter().all(|ext| exts.contains(ext));
                if !ok {
                    debug!("Rejecting physical device {:?}: not all required extensions were supported.", name);
                    debug!("Required: {:?}", *REQUIRED_EXTS);
                    debug!("Found: {:?}", exts);
                }
                ok
            })
            .max_by_key(|(_, _, _, device_type, exts)| {
                let exts_points = if WANTED_EXTS.iter().all(|ext| exts.contains(ext)) {
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

        // Log the device we chose.
        let (pd, qf, device_name, _, _) = pd_info;
        debug!("Choosing physical device {:?}", device_name);

        // Collect all the extensions we want and the device supports.
        let mut exts = REQUIRED_EXTS.clone();
        trace!("Device Extensions:");
        for ext in unsafe {
            instance
                .enumerate_device_extension_properties(pd)
                .context("Failed to get device extensions for chosen device")?
        } {
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

        // Create the device and queue.
        let queue_create_info = DeviceQueueCreateInfo::builder()
            .queue_family_index(qf)
            .queue_priorities(&[1.0]);
        let queue_create_infos: Vec<DeviceQueueCreateInfo> = vec![*queue_create_info];
        let ext_ptrs = exts.iter().map(|ext| ext.as_ptr()).collect::<Vec<_>>();
        let create_info = DeviceCreateInfo::builder()
            .queue_create_infos(&queue_create_infos)
            .enabled_extension_names(&ext_ptrs);
        let device = unsafe { instance.create_device(pd, &create_info, None) }
            .context("Failed to create device")?;
        let queue = unsafe { device.get_device_queue(qf, 0) };

        // Create the extensions.
        let swapchain_ext = Swapchain::new(&**instance, &device);

        // Snag the raytracing extension, if it exists.
        let raytracing_ext = if exts.iter().any(|e| RayTracing::name() == e.as_ref()) {
            Some(RayTracing::new(&**instance, &device))
        } else {
            None
        };

        Ok(Arc::new(Device {
            surface,
            pd,
            qf,
            device,
            queue,

            raytracing_ext,
            swapchain_ext,

            instance,
        }))
    }
}

impl Drop for Device {
    fn drop(&mut self) {
        unsafe {
            // The queue gets destroyed along with the device.
            self.device.destroy_device(None);
            // The queue family and physical device are plain data, so don't need destroying.
            self.instance
                .surface_ext
                .destroy_surface(self.surface, None);
        }
    }
}
