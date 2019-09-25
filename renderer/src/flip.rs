use crate::RendererState;
use vulkano::{
    command_buffer::AutoCommandBufferBuilder, format::ClearValue, swapchain::SwapchainCreationError,
};

impl RendererState {
    pub fn flip(&mut self) {
        if let Some(cleanup_future) = self.cleanup_future.as_mut() {
            cleanup_future.cleanup_finished();
        }

        if self.recreate_swapchain {
            let window = self.surface.window();
            let dims = if let Some(dims) = window.get_inner_size() {
                let dims: (u32, u32) = dims.to_physical(window.get_hidpi_factor()).into();
                [dims.0, dims.1]
            } else {
                eprintln!(
                "Could not get window inner size when recreating swapchain; was the window closed?"
            );
                return;
            };
            let (swapchain, images) = match self.swapchain.recreate_with_dimension(dims) {
                Ok(r) => r,
                Err(SwapchainCreationError::UnsupportedDimensions) => return,
                Err(err) => {
                    eprintln!("Failed to recreate swapchain: {:?} ({})", err, err);
                    return;
                }
            };
            self.swapchain = swapchain;
            self.images = images;
            self.recreate_swapchain = false;
        }

        let device = self.device.clone();
        let queue = self.queue.clone();
        let queue_family = queue.family();
        self.draw(|image| {
            AutoCommandBufferBuilder::primary_one_time_submit(device, queue_family)?
                .clear_color_image(image.clone(), ClearValue::Float([0.0, 0.0, 0.5, 1.0]))?
                .build()
                .map_err(From::from)
        })
    }
}
