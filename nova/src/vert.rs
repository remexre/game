#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct Vertex {
    position: [f32; 3],
    normals: [f32; 3],
    texcoords: [f32; 2],
}
