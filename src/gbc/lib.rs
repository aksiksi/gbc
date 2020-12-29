use std::path::Path;
use std::time::Instant;

pub mod cartridge;
pub mod cpu;
pub mod error;
pub mod instructions;
pub mod joypad;
pub mod memory;
pub mod ppu;
pub mod registers;

#[cfg(feature = "debug")]
pub mod debug;

pub use cpu::Cpu;
use cpu::Interrupt;
use cartridge::Cartridge;
pub use error::{Error, Result};
use joypad::JoypadEvent;

/// Gameboy
pub struct Gameboy {
    cpu: Cpu,
}

impl Gameboy {
    pub const FRAME_DURATION: u32 = 16_666_666; // in ns

    pub fn init<P: AsRef<Path>>(rom_path: P) -> Result<Self> {
        let cartridge = Cartridge::from_file(rom_path).unwrap();
        let memory = memory::MemoryBus::from_cartridge(cartridge)?;
        let cpu = Cpu::new(memory);

        Ok(Self {
            cpu,
        })
    }

    /// Run Gameboy for a single frame.
    ///
    /// The frame takes in an optional joypad event as input.
    pub fn frame(&mut self, joypad_event: Option<JoypadEvent>) {
        let now = Instant::now();

        // Figure out the number of cycles we can execute in a single frame
        let num_cycles = Self::FRAME_DURATION / self.cpu.cycle_time();

        // Execute next instruction
        let mut cycle = 0;
        while cycle < num_cycles {
            cycle += self.cpu.step() as u32;
        }

        // TODO: Update PPU

        // TODO: Update sound

        // Sleep until the next frame
        // std::thread::sleep(sleep);

        // Trigger a VBLANK at the end of each frame
        self.cpu.trigger_interrupt(Interrupt::Vblank);

        // Update joypad, if needed
        if let Some(event) = joypad_event {
            self.cpu.memory.joypad().handle_event(event);
        }

        println!("Done in {:?}", now.elapsed());
    }

    pub fn cpu(&mut self) -> &mut Cpu {
        &mut self.cpu
    }
}
