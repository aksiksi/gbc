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
use ppu::FrameBuffer;

/// Gameboy
pub struct Gameboy {
    cpu: Cpu,

    #[cfg(feature = "debug")]
    debugger: debug::Debugger,
}

impl Gameboy {
    pub const FRAME_DURATION: u32 = 16_666_666; // in ns

    pub fn init<P: AsRef<Path>>(rom_path: P) -> Result<Self> {
        let cartridge = Cartridge::from_file(rom_path).unwrap();
        let memory = memory::MemoryBus::from_cartridge(cartridge)?;
        let cpu = Cpu::new(memory);

        #[cfg(feature = "debug")]
        let gameboy = Ok(Self {
            cpu,
            // Dump all instructions to a file
            debugger: debug::Debugger::new(false),
        });

        #[cfg(not(feature = "debug"))]
        let gameboy = Ok(Self {
            cpu,
        });

        gameboy
    }

    /// Run Gameboy for a single frame.
    ///
    /// The frame takes in an optional joypad event as input.
    pub fn frame(&mut self, joypad_event: Option<JoypadEvent>) -> &FrameBuffer {

        // Figure out the number of clock cycles we can execute in a single frame
        let speed = self.cpu.speed();
        let cycle_time = self.cpu.cycle_time();
        let num_cycles = Self::FRAME_DURATION / cycle_time;

        // let now = Instant::now();
        // let mut iters = 0;

        // Execute next instruction
        let mut cycle = 0;
        while cycle < num_cycles {
            #[cfg(feature = "debug")]
            // If the debugger is triggered, step into the REPL.
            if self.debugger.triggered(&self.cpu) {
                self.debugger.repl(&mut self.cpu);
            }

            // Execute a step of the CPU
            let (cycles_taken, _) = self.cpu.step();

            // Execute a step of the PPU.
            //
            // The PPU will "catch up" based on what happened in the CPU.
            let (trigger_vblank, trigger_stat) =
                self.cpu.memory.ppu_mut().step(cycle + cycles_taken as u32, speed);
            if trigger_vblank {
                self.cpu.trigger_interrupt(Interrupt::Vblank);
            }
            if trigger_stat {
                self.cpu.trigger_interrupt(Interrupt::LcdStat);
            }

            // if vblank {
            //     dbg!(self.cpu.registers.PC);
            // }

            // if vblank {
            //     dbg!(inst);
            // }

            // Check if a serial interrupt needs to be triggered
            //
            // TODO: This does not happen every cycle, right?
            if self.cpu.memory.io_mut().serial_interrupt() {
                self.cpu.trigger_interrupt(Interrupt::Serial);
            }

            cycle += cycles_taken as u32;
            // iters += 1;
        }

        // println!("Done: {:?}, Iters: {}", now.elapsed(), iters);

        // TODO: Update PPU

        // TODO: Update sound

        // Update joypad, if needed
        if let Some(event) = joypad_event {
            self.cpu.memory.joypad().handle_event(event);
        }

        // println!("Done in {:?}", now.elapsed());

        // Return the rendered frame as a frame buffer
        self.cpu.memory.ppu().frame_buffer()
    }

    pub fn cpu(&mut self) -> &mut Cpu {
        &mut self.cpu
    }
}
