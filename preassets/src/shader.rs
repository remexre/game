use shaderc::{Compiler, ShaderKind};
use std::fs::{create_dir_all, read_to_string, write};

pub fn glsl(name: &str) {
    create_dir_all("assets/shaders").expect("Couldn't create shaders directory");

    let path = format!("preassets/shaders/{}", name);
    println!("Preprocessing shader {:?}...", path);

    let mut compiler = Compiler::new().expect("Couldn't start shader compiler");
    let source = read_to_string(&path).expect("Couldn't open shader");
    let binary = compiler
        .compile_into_spirv(&source, ShaderKind::InferFromSource, &path, "main", None)
        .unwrap_or_else(|err| match err {
            _ => panic!("Failed to compile shader: {}", err),
        });
    write(
        format!("assets/shaders/{}.spv", name),
        binary.as_binary_u8(),
    )
    .expect("Failed to write compiled shader");
}
