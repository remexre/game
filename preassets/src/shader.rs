use image::{ImageBuffer, Luma};
use std::fs::create_dir_all;

pub fn glsl(name: &str) {
    create_dir_all("assets/shaders").expect("Couldn't create shaders directory");

    println!("Preprocessing shader {:?}...", name);

    let bdf = bdf::open(format!("preassets/fonts/{}.glsl", name)).expect("Couldn't load font");
    let bounds = bdf.bounds();

    let iter = (b'!'..=b'~').map(char::from);
    let mut texture = ImageBuffer::from_pixel(
        bounds.width * iter.len() as u32,
        bounds.height,
        Luma([127u8]),
    );

    let glyphs = bdf.glyphs();
    let mut left = 0;
    for ch in iter {
        for ((x, y), px) in glyphs[&ch].pixels() {
            let gs = if px { 0 } else { 255 };
            texture[(left + x, y)] = Luma([gs]);
        }
        left += bounds.width;
    }

    texture
        .save(format!("assets/shaders/{}.png", name))
        .expect("Couldn't write texture");
}
