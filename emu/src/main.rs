#![allow(dead_code)]
use std::path::Path;
use std::time::{Instant, Duration};

use gbc::{Gameboy, Result};
use gbc::joypad::{JoypadEvent, JoypadInput};

use sdl2::render::TextureAccess;
use sdl2::pixels::Color;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;

fn keycode_to_joypad_input(keycode: Option<Keycode>) -> Option<JoypadInput> {
    match keycode.unwrap() {
        // TODO: Make key mapping configurable
        Keycode::X => Some(JoypadInput::A),
        Keycode::Z => Some(JoypadInput::B),
        Keycode::A => Some(JoypadInput::Select),
        Keycode::S => Some(JoypadInput::Start),
        Keycode::Up => Some(JoypadInput::Up),
        Keycode::Down => Some(JoypadInput::Down),
        Keycode::Left => Some(JoypadInput::Left),
        Keycode::Right => Some(JoypadInput::Right),
        _ => None,
    }
}

fn event_to_joypad(event: Event) -> Option<JoypadEvent> {
    match event {
        Event::KeyDown { keycode, .. } => {
            if let Some(event) = keycode_to_joypad_input(keycode) {
                Some(JoypadEvent::Down(event))
            } else {
                None
            }
        }
        Event::KeyUp { keycode, .. } => {
            if let Some(event) = keycode_to_joypad_input(keycode) {
                Some(JoypadEvent::Up(event))
            } else {
                None
            }
        }
        _ => unreachable!(),
    }
}

fn gui() {
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    // Setup an SDL2 Window
    let window = video_subsystem.window("gbc", 640, 576)
        .position_centered()
        .allow_highdpi()
        .build()
        .unwrap();

    // Convert the Window into a Canvas
    // This is what we will use to render content in the Window
    let mut canvas = window.into_canvas()
                           .build()
                           .unwrap();

    // Get a handle to the Canvas texture creator
    let texture_creator = canvas.texture_creator();

    // Create a Texture
    // We write raw pixel data here and copy it to the Canvas for rendering
    let mut texture = texture_creator.create_texture(None,
                                                     TextureAccess::Target,
                                                     160,
                                                     144).unwrap();

    let rom_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("../samples/cpu_instrs.gb");
    let mut gameboy = Gameboy::init(rom_path).unwrap();
    let frame_duration = Duration::new(0, Gameboy::FRAME_DURATION);

    // Start the event loop
    let mut event_pump = sdl_context.event_pump().unwrap();
    'running: loop {
        // List of pending key events
        let mut latest_event = None;

        for event in event_pump.poll_iter() {
            match event {
                Event::Quit {..} |
                Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                    break 'running
                },
                Event::KeyDown { .. } | Event::KeyUp { .. } => {
                    if latest_event.is_none() {
                        latest_event = Some(event.clone());
                    }
                }
                _ => (),
            }
        }

        // Take the latest event and map it to a JoypadEvent
        // TODO: Does this make sense?
        let event = match latest_event {
            Some(e) => event_to_joypad(e),
            None => None,
        };

        // Run the Gameboy for a single frame and return the frame data
        let frame_buffer = gameboy.frame(event);

        let start = Instant::now();

        // Remember the texture we created above? That is basically a buffer in VRAM
        // With the following, we are setting that texture as a render target for
        // our main canvas. This allows us to use regular canvas drawing functions -
        // e.g., rect, point - to update the GPU texture buffer.
        //
        // Once this closure ends, the canvas target is reset back for us.
        //
        // Helpful C example: https://wiki.libsdl.org/SDL_CreateTexture
        canvas.with_texture_canvas(&mut texture, |canvas| {
            canvas.clear();

            for row in 0..144 {
                for col in 0..160 {
                    let pixel = &frame_buffer.data[row][col];
                    let color = Color::RGBA(pixel.red, pixel.green, pixel.blue, pixel.alpha);
                    canvas.set_draw_color(color);
                    canvas.draw_point((col as i32, row as i32)).unwrap();
                }
            }
        }).unwrap();

        // Once we've completed our texture operations, we need to copy the texture
        // back to the canvas and present it.
        canvas.copy(&texture, None, None).unwrap();
        canvas.present();

        println!("{:?}", start.elapsed());

        std::thread::sleep(frame_duration);
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
