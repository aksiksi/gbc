use std::path::PathBuf;
use std::time::{Instant, Duration};

use gbc::Gameboy;
use gbc::cartridge::Cartridge;
use gbc::joypad::{JoypadEvent, JoypadInput};
use gbc::ppu::{FrameBuffer, GameboyRgba, LCD_WIDTH, LCD_HEIGHT};

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::render::{Canvas, Texture, TextureAccess};
use sdl2::pixels::Color;
use sdl2::video::Window;

use structopt::StructOpt;

struct FpsCounter {
    start_time: Instant,
    last_elapsed: Duration,
    frame_count: u64,
}

impl FpsCounter {
    /// The weight for older frames - current frame gets 1 - WEIGHT
    const WEIGHT: f32 = 0.1;

    pub fn new() -> Self {
        Self {
            start_time: Instant::now(),
            last_elapsed: Duration::default(),
            frame_count: 0,
        }
    }

    /// Records a new frame and outputs the current FPS
    pub fn frame(&mut self) -> f32 {
        self.frame_count += 1;

        let elapsed = self.start_time.elapsed().as_millis() as f32;
        let last_elapsed = self.last_elapsed.as_millis() as f32;
        let weighted_duration = elapsed * (1.0 - Self::WEIGHT) + last_elapsed * Self::WEIGHT;
        let fps = self.frame_count as f32 / (weighted_duration / 1000.0);

        self.last_elapsed = self.start_time.elapsed();

        fps
    }
}

#[derive(Debug, StructOpt)]
#[structopt(about = "A simple GBC emulator")]
enum Args {
    #[structopt(about = "Run a ROM on the emulator")]
    Run {
        #[structopt(parse(from_os_str), help = "Path to ROM file")]
        rom_file: PathBuf,

        #[structopt(default_value = "4", long, help = "Emulation resolution multiplier")]
        scale: u32,

        #[structopt(default_value = "1", long, help = "Emulation speed multiplier")]
        speed: u8,

        #[structopt(long, help = "Boot into the DMG boot ROM")]
        boot_rom: bool,

        #[structopt(long, help = "Trace all instructions to a file in the current directory")]
        trace: bool,
    },
    #[structopt(about = "Inspect a ROM")]
    Inspect {
        #[structopt(parse(from_os_str))]
        rom_file: Vec<PathBuf>,
    }
}

fn keycode_to_joypad_input(keycode: Option<Keycode>) -> Option<JoypadInput> {
    match keycode.unwrap() {
        // TODO: Make the key mapping configurable
        Keycode::A => Some(JoypadInput::B),
        Keycode::S => Some(JoypadInput::A),
        Keycode::Return | Keycode::KpEnter => Some(JoypadInput::Start),
        Keycode::RShift | Keycode::LShift => Some(JoypadInput::Select),
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

/// Renders a single Gameboy frame to the SDL canvas using a texture as the render target.
///
/// Once the texture is ready, it is copied back to the canvas and presented.
fn render_frame(frame_buffer: &FrameBuffer, canvas: &mut Canvas<Window>, texture: &mut Texture,
                outline: bool) {
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
    canvas.with_texture_canvas(texture, |canvas| {
        canvas.clear();
        canvas.set_draw_color(Color::BLACK);

        // Draw the rendered frame
        for x in 0..LCD_WIDTH {
            for y in 0..LCD_HEIGHT {
                let GameboyRgba { red, green, blue, alpha } = frame_buffer.read(x, y);
                canvas.set_draw_color(Color::RGBA(red, green, blue, alpha));
                canvas.draw_point((x as i32, y as i32)).unwrap();
            }
        }

        if outline {
            // Draw an outline showing the tiles in the frame
            canvas.set_draw_color(Color::GRAY);

            for row in (0i32..LCD_HEIGHT as i32).step_by(8) {
                canvas.draw_line((0, row), (LCD_WIDTH as i32 - 1, row)).unwrap();
            }

            for col in (0i32..LCD_WIDTH as i32).step_by(8) {
                canvas.draw_line((col, 0), (col, LCD_HEIGHT as i32 - 1)).unwrap();
            }
        }
    }).unwrap();

    // Once we've completed our texture operations, we need to copy the texture
    // back to the canvas and then present to the screen.
    canvas.copy(&texture, None, None).unwrap();
    canvas.present();
}

/// Handles a single Gameboy frame.
///
/// This advances the Gameboy for the number of CPU cycles in a single frame. Once the
/// underlying frame buffer is ready (i.e., on VBLANK), the frame is picked up and rendered
/// to an SDL texture.
///
/// At the end of the frame, any input joypad events are passed on to the Gameboy to be
/// picked up in the next frame.
fn handle_frame(gameboy: &mut Gameboy, canvas: &mut Canvas<Window>, texture: &mut Texture,
                joypad_events: &mut Vec<JoypadEvent>, outline: bool) {
    // Run the Gameboy until the next frame is ready (i.e., start of VBLANK).
    //
    // This means we run from VBLANK to VBLANK. From the rendering side, it doesn't
    // really matter: as long as the frame is ready, we can render. The emulator
    // will catch up & process the VBLANK in the next call to this function.
    loop {
        // Runs the CPU and all peripherals in lock step for a single "step". The size
        // of this step is controlled by the CPU based on the last executed instruction.
        let (frame_buffer, _) = gameboy.step();

        if let Some(frame_buffer) = frame_buffer {
            render_frame(frame_buffer, canvas, texture, outline);
            break;
        }
    }

    gameboy.update_joypad(Some(joypad_events));

    // Clear out all processed input events
    joypad_events.clear();
}

fn gui(rom_file: PathBuf, scale: u32, speed: u8, boot_rom: bool, trace: bool) {
    let rom_name = match rom_file.file_name() {
        None => None,
        Some(n) => Some(n.to_str().unwrap()),
    }.unwrap_or("Unknown ROM");

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let width = LCD_WIDTH as u32 * scale;
    let height = LCD_HEIGHT as u32 * scale;

    // Setup an SDL2 Window
    let window = video_subsystem
        .window(rom_name, width, height)
        .position_centered()
        .allow_highdpi()
        .resizable()
        .build()
        .unwrap();

    // Convert the Window into a Canvas
    // This is what we will use to render content in the Window
    // TODO: Add flag for software vs. GPU
    let mut canvas = window
        .into_canvas()
        .software()
        .build()
        .unwrap();

    // Fix aspect ratio of canvas
    canvas.set_logical_size(width, height).unwrap();

    // Get a handle to the Canvas texture creator
    let texture_creator = canvas.texture_creator();

    // Create a Texture
    // We write raw pixel data here and copy it to the Canvas for rendering
    let mut texture = texture_creator.create_texture(None,
                                                     TextureAccess::Target,
                                                     LCD_WIDTH as u32,
                                                     LCD_HEIGHT as u32).unwrap();

    let mut gameboy = Gameboy::init(&rom_file, boot_rom, trace).unwrap();

    let mut paused = false;
    let mut outline = false;

    // List of joypad events to push to the Gameboy
    let mut joypad_events = Vec::new();

    // More accurate sleep, especially on Windows
    let sleeper = spin_sleep::SpinSleeper::default();

    let frame_time_ns = Gameboy::FRAME_DURATION / speed as u64;
    let frame_duration = Duration::from_nanos(frame_time_ns);

    let mut fps_counter = FpsCounter::new();

    // Start the event loop
    let mut event_pump = sdl_context.event_pump().unwrap();
    'running: loop {
        let frame_start = Instant::now();

        for event in event_pump.poll_iter() {
            match event {
                Event::Quit {..} |
                Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                    break 'running
                },
                Event::KeyDown { keycode: Some(Keycode::KpMultiply), .. } => {
                    // Reset the emulator
                    gameboy.reset();
                }
                Event::KeyDown { keycode: Some(Keycode::P), .. } => {
                    paused = !paused;
                }
                Event::KeyDown { keycode: Some(Keycode::O), .. } => {
                    outline = !outline;
                }
                Event::KeyDown { keycode: Some(Keycode::K), .. } => {
                    // Save this Gameboy to disk
                    gameboy.dump("save.state").unwrap();
                }
                Event::KeyDown { keycode: Some(Keycode::L), .. } => {
                    // Load a Gameboy from disk
                    gameboy = Gameboy::load(&rom_file, "save.state").unwrap();
                }
                Event::KeyDown { .. } | Event::KeyUp { .. } => {
                    if let Some(e) = event_to_joypad(event) {
                        joypad_events.push(e);
                    }
                }
                _ => (),
            }
        }

        if !paused {
            // Render a single frame
            handle_frame(&mut gameboy, &mut canvas, &mut texture, &mut joypad_events, outline);
        }

        let elapsed = frame_start.elapsed();

        // Sleep for the rest of the frame
        //
        // TODO: Evaluate if we need VSYNC to avoid tearing on higher Hz displays
        if elapsed < frame_duration {
            sleeper.sleep(frame_duration - elapsed);
        }

        // Update FPS counter in window title
        let fps = fps_counter.frame();
        let title = format!("{} - {:.2} fps", rom_name, fps);
        canvas.window_mut().set_title(&title).unwrap();
    }
}

fn main() {
    env_logger::init();

    let cli = Args::from_args();

    match cli {
        Args::Run { rom_file, scale, speed, boot_rom, trace } => {
            if speed == 0 || speed > 5 {
                eprintln!("Error: Maximum supported emulator speed is 5x!");
                return;
            }

            gui(rom_file, scale, speed, boot_rom, trace);
        }
        Args::Inspect { rom_file } => {
            for f in &rom_file {
                let cartridge = match Cartridge::from_file(f, false) {
                    Err(e) => {
                        eprintln!("Error reading cartridge: {}", e);
                        return;
                    }
                    Ok(c) => c,
                };

                println!("\nTitle: {}", cartridge.title().unwrap_or("N/A"));
                println!("Manufacturer: {}", cartridge.manufacturer_code().unwrap_or("N/A"));
                println!("GBC support: {}", cartridge.cgb());
                println!("Cartridge type: {:?}", cartridge.cartridge_type().unwrap());
                println!("ROM size: {:?}", cartridge.rom_size().unwrap());
                println!("RAM size: {:?}\n", cartridge.ram_size().unwrap());
            }
        }
    }
}
