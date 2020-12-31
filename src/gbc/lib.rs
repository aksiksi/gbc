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
use memory::{MemoryRead, MemoryWrite};

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

        // Figure out the number of clock cycles we can execute in a single frame
        let speed = self.cpu.speed();
        let cycle_time = self.cpu.cycle_time();
        let num_cycles = Self::FRAME_DURATION / cycle_time;

        let mut vblank = false;

        // Execute next instruction
        let mut cycle = 0;
        while cycle < num_cycles {
            // Update internal PPU state based on current cycle and trigger any required interrupts
            let (trigger_vblank, trigger_stat) = self.cpu.memory.ppu_mut().step(cycle, speed);
            if trigger_vblank {
                vblank = true;
                self.cpu.trigger_interrupt(Interrupt::Vblank);
            }
            if trigger_stat {
                self.cpu.trigger_interrupt(Interrupt::LcdStat);
            }

            if vblank {
                dbg!(self.cpu.registers.PC);
            }

            let (cycles_taken, inst) = self.cpu.step();

            if vblank {
                dbg!(inst);
            }

            // Check if a serial interrupt needs to be triggered
            //
            // TODO: This does not happen every cycle, right?
            if self.cpu.memory.io_mut().serial_interrupt() {
                self.cpu.trigger_interrupt(Interrupt::Serial);
            }

            cycle += cycles_taken as u32;
        }

        // TODO: Update PPU

        // TODO: Update sound

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
