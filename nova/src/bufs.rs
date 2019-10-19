use derivative::Derivative;

#[derive(Derivative)]
#[derivative(Debug)]
pub struct VBO {
    pub num_vertices: u32,
}
