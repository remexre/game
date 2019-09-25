use crate::RendererState;
use libremexre::errors::Result;
use std::{mem::replace, sync::Arc};
use vulkano::{
    command_buffer::AutoCommandBufferBuilder,
    format::{ClearValue, Format},
    framebuffer::Framebuffer,
    image::attachment::AttachmentImage,
    swapchain::{acquire_next_image, AcquireError, SwapchainCreationError},
    sync::{FlushError, GpuFuture},
};

impl RendererState {
    pub fn draw(&mut self, clear: [f32; 4]) {
        self.draw_inner(clear)
            .unwrap_or_else(|err| eprintln!("Draw error: {}\n{:#?}", err, err));
    }

    pub fn draw_inner(&mut self, clear: [f32; 4]) -> Result<()> {
        let r = if let Some(command_buffer_builder) =
            replace(&mut self.command_buffer_builder, None)
        {
            let command_buffer = command_buffer_builder.end_render_pass()?.build()?;
            self.cleanup_future
                .take()
                .unwrap()
                .then_execute(self.queue.clone(), command_buffer)?
                .then_swapchain_present(self.queue.clone(), self.swapchain.clone(), self.image_num)
                .then_signal_fence_and_flush()
                .map(Some)
        } else {
            Ok(None)
        };

        let (image_num, acquire_future) = loop {
            match acquire_next_image(self.swapchain.clone(), None) {
                Ok(r) => break r,
                Err(AcquireError::OutOfDate) => {
                    self.recreate_swapchain = true;
                }
                Err(err) => return Err(Box::new(err)),
            }
        };
        self.image_num = image_num;

        match r {
            Ok(Some(future)) => {
                self.cleanup_future = Some(Box::new(future.join(acquire_future)));
            }
            Ok(None) => {
                self.cleanup_future = Some(Box::new(acquire_future));
            }
            Err(FlushError::OutOfDate) => {
                self.cleanup_future = Some(Box::new(acquire_future));
                self.recreate_swapchain = true;
            }
            Err(e) => {
                self.cleanup_future = Some(Box::new(acquire_future));
                return Err(Box::new(e));
            }
        }

        if let Some(cleanup_future) = self.cleanup_future.as_mut() {
            cleanup_future.cleanup_finished();
        }

        let window = self.surface.window();
        let dims = if let Some(dims) = window.get_inner_size() {
            let dims: (u32, u32) = dims.to_physical(window.get_hidpi_factor()).into();
            [dims.0, dims.1]
        } else {
            eprintln!("Could not get window inner size; was the window closed?");
            return Ok(());
        };

        if self.recreate_swapchain {
            let (swapchain, images) = match self.swapchain.recreate_with_dimension(dims) {
                Ok(r) => r,
                Err(SwapchainCreationError::UnsupportedDimensions) => return Ok(()),
                Err(err) => {
                    eprintln!("Failed to recreate swapchain: {:?} ({})", err, err);
                    return Ok(());
                }
            };
            self.swapchain = swapchain;
            self.images = images;
            self.recreate_swapchain = false;
        }

        let depth_buffer =
            AttachmentImage::transient(self.device.clone(), dims, Format::D16Unorm).unwrap();
        let framebuffer = Arc::new(
            Framebuffer::start(self.render_pass.clone())
                .add(self.images[image_num].clone())
                .unwrap()
                .add(depth_buffer)
                .unwrap()
                .build()
                .unwrap(),
        );

        self.command_buffer_builder = Some(
            AutoCommandBufferBuilder::primary_one_time_submit(
                self.device.clone(),
                self.queue.family(),
            )
            .unwrap()
            .begin_render_pass(
                framebuffer,
                false,
                vec![clear.into(), ClearValue::Depth(0.0)],
            )
            .unwrap(),
        );
        Ok(())
    }
}
