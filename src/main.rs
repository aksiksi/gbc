#![allow(dead_code)]

use gbc::{Gameboy, Result};

fn main() -> Result<()> {
    let mut gameboy = Gameboy::init("samples/pokemon_gold.gbc")?;
    let cpu = gameboy.cpu();

    cpu.step();
    cpu.step();
    cpu.step();
    cpu.step();
    cpu.step();
    cpu.step();
    cpu.step();
    cpu.step();
    cpu.step();
    cpu.step();
    cpu.step();

    dbg!(cpu);

    Ok(())
}
