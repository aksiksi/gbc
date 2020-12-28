#![allow(dead_code)]

use gbc::{Gameboy, Result};

fn gui() {
    use sdl2::rect::Rect;
    use sdl2::render::TextureAccess;
    use sdl2::pixels::Color;
    use sdl2::event::Event;
    use sdl2::keyboard::Keycode;
    use std::time::Duration;

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let window = video_subsystem.window("rust-sdl2 demo", 800, 600)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();
    let texture_creator = canvas.texture_creator();


    canvas.set_draw_color(Color::RGB(0, 255, 255));
    canvas.clear();
    canvas.present();

    let mut event_pump = sdl_context.event_pump().unwrap();
    let mut i = 0;
    let mut j = 0;
    'running: loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit {..} |
                Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                    break 'running
                },
                _ => (),
            }
        }

        // Draw a 100x100 rect on a 800x600 texture at a varying position
        canvas.clear();
        let mut texture = texture_creator.create_texture(None,
                                                         TextureAccess::Streaming,
                                                         800,
                                                         600).unwrap();
        let rect = Rect::new(i * 100, i * 100, 100, 100);
        let mut pixels = Vec::new();
        let c = Color::MAGENTA.rgba();
        for i in 0..100 {
            for j in 0..100 {
                pixels.extend(&[c.0, c.1, c.2, c.3]);
            }
        }

        texture.update(Some(rect), &pixels, 100 * 4).unwrap();

        canvas.copy(&texture, None, None).unwrap();
        canvas.present();

        i = (i + 1) % 8;
        j = (j + 1) % 6;


        // The rest of the game loop goes here...

        // canvas.present();
        ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
    }
}

fn main() -> Result<()> {
    // let mut gameboy = Gameboy::init("samples/pokemon_gold.gbc")?;
    // gameboy.run();

    gui();

    // let cpu = gameboy.cpu();
    // cpu.step();
    // cpu.step();
    // cpu.step();
    // cpu.step();
    // cpu.step();
    // cpu.step();
    // cpu.step();
    // cpu.step();
    // cpu.step();
    // cpu.step();
    // cpu.step();

    Ok(())
}
