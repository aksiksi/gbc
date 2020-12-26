use std::path::Path;

mod cartridge;
mod cpu;
mod error;
mod instructions;
mod memory;
mod registers;

pub use cpu::Cpu;
use cartridge::Cartridge;
pub use error::{Error, Result};
use memory::MemoryBus;

pub struct Gameboy {
    pub cpu: Cpu,
}

impl Gameboy {
    pub fn new<P: AsRef<Path>>(rom_path: P) -> Result<Self> {
        let mut cartridge = Cartridge::from_file(rom_path).unwrap();
        let memory = MemoryBus::from_cartridge(&mut cartridge)?;
        let cpu = Cpu::new(memory);

        Ok(Self {
            cpu,
        })
    }
}
