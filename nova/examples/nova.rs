use anyhow::Result;
use log::info;
use lye::Instance;
use nova::{Renderer, Uniforms, Vertex};

static MAT4_IDENTITY: [f32; 16] = [
    1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0,
];

static UNIFORMS: Uniforms = Uniforms {
    model: MAT4_IDENTITY,
    view: MAT4_IDENTITY,
    proj: MAT4_IDENTITY,

    ambient: [0.2, 0.0, 0.0],
    diffuse: [0.0, 0.0, 0.0],
    specular: 1.0,
};

const VERTICES: &[Vertex] = &[
    Vertex {
        position: [-1.0, -1.0, 0.0],
        normal: [1.0, 0.0, 0.0],
        texcoords: [0.0, 0.0],
    },
    Vertex {
        position: [1.0, -1.0, 0.0],
        normal: [0.0, 1.0, 0.0],
        texcoords: [0.0, 0.0],
    },
    Vertex {
        position: [0.0, 1.0, 0.0],
        normal: [0.0, 0.0, 1.0],
        texcoords: [0.0, 0.0],
    },
];

fn main() -> Result<()> {
    stderrlog::new().verbosity(3).init()?;
    run()?;
    Instance::assert_no_instances_exist();
    Ok(())
}

fn run() -> Result<()> {
    let mut renderer = Renderer::new(
        "Vulkan Example",
        true,
        "assets/shaders/tutorial.vert.spv",
        "assets/shaders/tutorial.frag.spv",
    )?;

    let vbo = renderer.new_vbo(VERTICES)?;

    while !renderer.should_close() {
        for ev in renderer.flip()? {
            info!("{:?}", ev);
        }

        renderer.with_draw(|mut ctx| {
            ctx.draw(&vbo, &UNIFORMS)?;
            Ok(())
        })?;
    }

    Ok(())
}
