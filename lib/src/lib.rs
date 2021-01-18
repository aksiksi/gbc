use std::path::Path;

pub mod cartridge;
pub mod cpu;
pub mod error;
pub mod instructions;
pub mod joypad;
pub mod memory;
pub mod ppu;
pub mod registers;
pub mod timer;

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

    /// Initialize the emulator with an optional ROM.
    ///
    /// If no ROM is provided, the emulator will boot into the CGB BIOS ROM. You can
    /// use `Self::insert` to load a cartridge later.
    pub fn init<P: AsRef<Path>>(rom_path: Option<P>) -> Result<Self> {
        let cartridge = match rom_path {
            Some(p) => Some(Cartridge::from_file(p)?),
            None => None,
        };

        let cpu = Cpu::new(cartridge)?;

        #[cfg(feature = "debug")]
        let gameboy = Self {
            cpu,
            debugger: debug::Debugger::new(),
        };

        #[cfg(not(feature = "debug"))]
        let gameboy = Self {
            cpu,
        };

        Ok(gameboy)
    }

    /// Run Gameboy for a single frame.
    ///
    /// The frame takes in an optional joypad event as input.
    pub fn frame(&mut self, joypad_event: Option<JoypadEvent>) -> &FrameBuffer {
        // Figure out the number of clock cycles we can execute in a single frame
        let speed = self.cpu.speed();
        let cycle_time = self.cpu.cycle_time();
        let num_cycles = Self::FRAME_DURATION / cycle_time;

        // Execute next instruction
        let mut cycle = 0;
        while cycle < num_cycles {
            #[cfg(feature = "debug")]
            // If the debugger is triggered, step into the REPL.
            if self.debugger.triggered(&self.cpu) {
                self.debugger.repl(&mut self.cpu);
            }

            // Execute a step of the CPU
            let (cycles_taken, _inst) = self.cpu.step();

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

            // Check if a serial interrupt needs to be triggered
            //
            // TODO: This does not happen every cycle, right?
            if self.cpu.memory.io_mut().serial_interrupt() {
                self.cpu.trigger_interrupt(Interrupt::Serial);
            }

            // Update the internal timer and trigger an interrupt, if needed
            // Note that the timer may tick multiple times for a single instruction
            if self.cpu.memory.timer().step(cycles_taken) {
                self.cpu.trigger_interrupt(Interrupt::Timer);
            }

            cycle += cycles_taken as u32;
        }

        // Update joypad, if needed
        if let Some(event) = joypad_event {
            self.cpu.memory.joypad().handle_event(event);
        }

        // Return the rendered frame as a frame buffer
        self.cpu.memory.ppu().frame_buffer()
    }

    /// Insert a new cartridge and reset the emulator
    pub fn insert<P: AsRef<Path>>(&mut self, rom_path: P) -> Result<()> {
        let cartridge = Some(Cartridge::from_file(rom_path)?);
        self.cpu = Cpu::new(cartridge)?;
        Ok(())
    }

    /// Eject the inserted cartridge, if any, and reset the CPU
    pub fn eject(&mut self) {
        self.cpu = Cpu::new(None).unwrap();
    }

    /// Reset the emulator
    pub fn reset(&mut self) -> Result<()> {
        // Reset the CPU
        self.cpu.reset()
    }

    pub fn cpu(&mut self) -> &mut Cpu {
        &mut self.cpu
    }
}
