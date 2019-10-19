use ash::{vk::CommandBuffer, Device};
use derivative::Derivative;

#[derive(Derivative)]
#[derivative(Debug)]
pub struct DrawTarget<'a> {
    #[derivative(Debug = "ignore")]
    pub(crate) dev: &'a Device,

    pub(crate) command_buffer: CommandBuffer,
}

impl<'a> DrawTarget<'a> {
    pub fn draw(&mut self, n: u32) {
        crate::cmds::draw(self.dev, self.command_buffer, n)
    }
}
