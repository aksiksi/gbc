use std::path::Path;

pub mod cartridge;
pub mod cpu;
pub mod error;
pub mod instructions;
pub mod joypad;
pub mod memory;
pub mod registers;

pub use cpu::Cpu;
use cartridge::Cartridge;
pub use error::{Error, Result};

/// Gameboy
pub struct Gameboy {
    cpu: Cpu,
}

impl Gameboy {
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
    }

    pub fn cpu(&mut self) -> &mut Cpu {
        &mut self.cpu
    }
}
