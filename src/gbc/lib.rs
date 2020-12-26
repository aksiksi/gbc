use std::path::Path;

mod cartridge;
mod cpu;
mod error;
mod instructions;
mod joypad;
mod memory;
mod registers;

pub use cpu::Cpu;
use cartridge::Cartridge;
pub use error::{Error, Result};
use memory::MemoryBus;

/// Gameboy
pub struct Gameboy {
    cpu: Cpu,
}

impl Gameboy {
    pub fn init<P: AsRef<Path>>(rom_path: P) -> Result<Self> {
        let mut cartridge = Cartridge::from_file(rom_path).unwrap();
        let memory = MemoryBus::from_cartridge(&mut cartridge)?;
        let cpu = Cpu::new(memory);

        Ok(Self {
            cpu,
        })
    }

    pub fn run(&mut self) {
        // TODO: Display logo, then jump to cartridge entry point
        // Once that's done, revert the current default CPU PC
    }

    pub fn cpu(&mut self) -> &mut Cpu {
        &mut self.cpu
    }
}
