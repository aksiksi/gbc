mod cartridge;
mod cpu;
mod error;
mod memory;
mod registers;

use cartridge::Cartridge;
use cpu::Cpu;
use error::Result;
use memory::Memory;

struct Gameboy {
    cpu: Cpu,
    cartridge: Cartridge,
}

fn main() -> Result<()> {
    let mut cartridge = Cartridge::from_file("samples/pokemon_gold.gbc").unwrap();
    let memory = Memory::from_cartridge(&mut cartridge)?;
    let cpu = Cpu::new(memory);
    let memory = cpu.memory();

    dbg!(&memory);

    let gameboy = Gameboy { cpu, cartridge };

    Ok(())
}
