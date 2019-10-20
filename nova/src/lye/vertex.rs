/// A single vertex in a VBO.
#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct Vertex {
    pub position: [f32; 3],
    pub texcoords: [f32; 2],
    pub normal: [f32; 3],
}
