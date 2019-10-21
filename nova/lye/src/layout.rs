/// The uniforms expected for rendering.
#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct Uniforms {
    /// The model matrix.
    pub model: [f32; 16],

    /// The view matrix.
    pub view: [f32; 16],

    /// The projection matrix.
    pub proj: [f32; 16],

    /// The ambient color.
    pub ambient: [f32; 3],

    /// The default diffuse color.
    pub diffuse: [f32; 3],

    /// The specular exponent.
    pub specular: f32,
}

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
