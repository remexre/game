/// A single vertex in a VBO.
#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct Vertex {
    /// The position of the vertex in model-space.
    pub position: [f32; 3],
    /// The uv texture coordinates.
    pub texcoords: [f32; 2],
    /// The normal vector of the vertex in model-space.
    pub normal: [f32; 3],
}
