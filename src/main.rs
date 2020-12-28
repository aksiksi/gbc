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

    // Setup an SDL2 Window
    let mut window = video_subsystem.window("gbc", 800, 600)
        .position_centered()
        .allow_highdpi()
        .build()
        .unwrap();

    // Convert the Window into a Canvas
    // This is what we will use to render content in the Window
    let mut canvas = window.into_canvas().build().unwrap();

    // Get a handle to the Canvas texture creator
    let texture_creator = canvas.texture_creator();

    // Create a Texture
    // We write raw pixel data here and copy it to the Canvas for rendering
    let mut texture = texture_creator.create_texture(None,
                                                     TextureAccess::Target,
                                                     800,
                                                     600).unwrap();

    let mut i = 0;
    let mut j = 0;

    let gameboy = Gameboy::init("samples/pokemon_gold.gbc").unwrap();

    // Start the event loop
    let mut event_pump = sdl_context.event_pump().unwrap();
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

        // Remember the texture we created above? That is basically a buffer in VRAM
        // With the following, we are setting that texture as a render target for
        // our main canvas. This allows us to use regular canvas drawing functions -
        // e.g., rect, point - to update the GPU texture buffer.
        //
        // Once this closure ends, the canvas target is reset back for us.
        //
        // Helpful C example: https://wiki.libsdl.org/SDL_CreateTexture
        canvas.with_texture_canvas(&mut texture, |canvas| {
            let rect = Rect::new(i * 100, i * 100, 100, 100);
            canvas.set_draw_color(Color::YELLOW);
            canvas.clear();
            canvas.draw_rect(rect).unwrap();
            canvas.set_draw_color(Color::MAGENTA);
            canvas.fill_rect(Some(rect)).unwrap();

            for i in 300..500 {
                canvas.draw_point((i, 100)).unwrap();
            }
        }).unwrap();

        // Once we've completed our texture operations, we need to copy the texture
        // back to the canvas and present it.
        canvas.copy(&texture, None, None).unwrap();
        canvas.present();

        i = (i + 1) % 8;
        j = (j + 1) % 6;

        // The rest of the game loop goes here...

        std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
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
