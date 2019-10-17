mod font;
mod shader;

fn main() {
    font::bdf("ter-u32n");
    shader::glsl("default.frag");
    shader::glsl("default.vert");
    shader::glsl("tutorial.frag");
    shader::glsl("tutorial.vert");
}
