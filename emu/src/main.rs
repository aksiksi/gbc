#![allow(dead_code)]
use std::path::PathBuf;
use std::time::{Instant, Duration};

use gbc::{Gameboy, Result};
use gbc::joypad::{JoypadEvent, JoypadInput};

use sdl2::render::TextureAccess;
use sdl2::pixels::Color;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;

use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct Cli {
    #[structopt(parse(from_os_str))]
    rom_file: PathBuf,

    #[structopt(long)]
    headless: bool,
}

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

fn gui(rom_path: Option<PathBuf>) {
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
    // TODO: Add flag for software vs. GPU
    let mut canvas = window.into_canvas()
                           .software()
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

    let mut gameboy = Gameboy::init(rom_path).unwrap();
    let frame_duration = Duration::new(0, Gameboy::FRAME_DURATION);

    // Start the event loop
    let mut event_pump = sdl_context.event_pump().unwrap();
    'running: loop {
        let frame_start = Instant::now();

        // List of pending key events
        let mut latest_event = None;

        for event in event_pump.poll_iter() {
            match event {
                Event::Quit {..} |
                Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                    break 'running
                },
                Event::KeyDown { keycode: Some(Keycode::R), .. } => {
                    // Reset the emulator
                    gameboy.reset().unwrap();
                }
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

        // With the following, we are setting the texture as a render target for
        // our main canvas. This allows us to use regular canvas drawing functions -
        // e.g., rect, point - to update the underlyinh texture. Note that the texture
        // will be updated only when all canvas operations are complete.
        //
        // Note that, if GPU rendering is enabled, the texture lives in GPU VRAM. If
        // this is the case, updates are fairly expensive, as we need to round-trip
        // to GPU VRAM on every frame (?).
        //
        // Once this closure ends, the canvas target is reset back for us.
        //
        // Helpful C example: https://wiki.libsdl.org/SDL_CreateTexture
        canvas.with_texture_canvas(&mut texture, |canvas| {
            canvas.clear();
            for x in 0..160 {
                for y in 0..144 {
                    let color = frame_buffer.data[y * 160 + x];
                    canvas.set_draw_color(Color::RGBA(color.red, color.green, color.blue, color.alpha));
                    canvas.draw_point((x as i32, y as i32)).unwrap();
                }
            }
        }).unwrap();

        // Once we've completed our texture operations, we need to copy the texture
        // back to the canvas to be able to present it.
        canvas.copy(&texture, None, None).unwrap();
        canvas.present();

        let elapsed = frame_start.elapsed();

        if elapsed < frame_duration {
            std::thread::sleep(frame_duration - elapsed);
        }
    }
}

fn main() -> Result<()> {
    let cli = Cli::from_args();

    if !cli.headless {
        gui(Some(cli.rom_file));
    } else {
        let mut gameboy = Gameboy::init(Some(cli.rom_file))?;
        loop {
            // TODO: Perhaps allow user to provide joypad input file?
            // e.g., list of (input, time)
            gameboy.frame(None);
            std::thread::sleep(Duration::from_nanos(Gameboy::FRAME_DURATION as u64))
        }
    }

    Ok(())
}
