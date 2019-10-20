use derivative::Derivative;

#[derive(Derivative)]
#[derivative(Debug)]
pub struct Framebuffer {}

impl Drop for Framebuffer {
    fn drop(&mut self) {
        unimplemented!()
    }
}
