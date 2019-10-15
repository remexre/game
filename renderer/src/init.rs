use libremexre::errors::Result;
use std::sync::Arc;
use vulkano::{
    device::{Device, Features, Queue, RawDeviceExtensions},
    instance::{
        layers_list, Instance, PhysicalDevice, PhysicalDeviceType, QueueFamily,
        RawInstanceExtensions,
    },
};
use winit::{EventsLoop, WindowBuilder};

const WANTED_INSTANCE_EXTS: &[&[u8]] = &[
    b"VK_KHR_get_physical_device_properties2", // for VK_NV_ray_tracing
];

const WANTED_DEVICE_EXTS: &[&[u8]] = &[
    b"VK_KHR_get_memory_requirements2", // for VK_NV_ray_tracing
    b"VK_NV_ray_tracing",
];

pub fn create_instance(debug: bool) -> Result<Arc<Instance>> {
    let mut exts = RawInstanceExtensions::from(&vulkano_win::required_extensions());
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
        .max_by_key(|dev| match dev.ty() {
            PhysicalDeviceType::IntegratedGpu => 1,
            PhysicalDeviceType::DiscreteGpu => 2,
            _ => 0,
        });
    let dev = dev.ok_or("No Vulkan devices found")?;
    println!("Choosing physical device {:?}", dev.name());
    Ok(dev)
}

pub fn choose_queue_family<'a>(dev: PhysicalDevice<'a>) -> Result<QueueFamily<'a>> {
    let qf = dev
        .queue_families()
        .filter(|qf| qf.supports_graphics())
        .max_by_key(|qf| qf.queues_count())
        .ok_or("No graphics-supporting queue families found")?;
    Ok(qf)
}

pub fn create_device<'a>(
    dev: PhysicalDevice,
    qf: QueueFamily<'a>,
) -> Result<(Arc<Device>, Arc<Queue>)> {
    let mut exts = RawDeviceExtensions::none();
    println!("Device Extensions:");
    for ext in RawDeviceExtensions::supported_by_device(dev).iter() {
        let flag = if WANTED_DEVICE_EXTS.contains(&ext.as_bytes()) {
            exts.insert(ext.clone());
            '+'
        } else {
            '-'
        };

        println!("{} {}", flag, ext.to_string_lossy());
    }

    let (dev, mut queues) = Device::new(dev, &Features::none(), exts, [(qf, 1.0)].iter().cloned())?;
    let queue = queues.next().ok_or("Device had no queues")?;
    Ok((dev, queue))
}

pub fn create_window(instance: Arc<Instance>) -> Result<()> {
    let events_loop = EventsLoop::new();
    unimplemented!()
}
