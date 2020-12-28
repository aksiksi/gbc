use std::path::Path;
use std::time::Instant;

pub mod cartridge;
pub mod cpu;
pub mod error;
pub mod instructions;
pub mod joypad;
pub mod memory;
pub mod registers;

#[cfg(feature = "debug")]
pub mod debug;

pub use cpu::Cpu;
use cartridge::Cartridge;
pub use error::{Error, Result};

/// Gameboy
pub struct Gameboy {
    cpu: Cpu,
}

impl Gameboy {
    const FRAME_DURATION: u64 = 16_666_666; // in ns

    pub fn init<P: AsRef<Path>>(rom_path: P) -> Result<Self> {
        let cartridge = Cartridge::from_file(rom_path).unwrap();
        let memory = memory::MemoryBus::from_cartridge(cartridge)?;
        let cpu = Cpu::new(memory);

        Ok(Self {
            cpu,
        })
    }

    pub fn run(&mut self) {
        // TODO: Display logo, then jump to cartridge entry point
        // Once that's done, revert the current default CPU PC
        loop {
            let now = Instant::now();

            // Figure out the number of cycles we can execute in a single frame
            // let num_cycles = Self::FRAME_DURATION / self.cpu.cycle_time();
            let num_cycles = 60;

            // Execute next instruction
            let mut cycle = 0;
            while cycle < num_cycles {
                cycle += self.cpu.step() as u64;
            }

            // TODO: Update PPU

            // TODO: Update sound

            // Sleep until the next frame
            // std::thread::sleep(sleep);
            println!("Done in {:?}", now.elapsed());
        }
    }

    pub fn cpu(&mut self) -> &mut Cpu {
        &mut self.cpu
    }
}
