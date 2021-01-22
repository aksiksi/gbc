use std::path::Path;

pub mod cartridge;
pub mod cpu;
pub mod dma;
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

    /// Number of frames executed
    frame_counter: u64,

    #[cfg(feature = "debug")]
    debugger: debug::Debugger,
}

impl Gameboy {
    pub const FRAME_DURATION: u32 = 16_666_666; // in ns

    /// Initialize the emulator with an optional ROM.
    ///
    /// If no ROM is provided, the emulator will boot into the CGB BIOS ROM. You can
    /// use `Self::insert` to load a cartridge later.
    pub fn init<P: AsRef<Path>>(rom_path: P, boot_rom: bool, trace: bool) -> Result<Self> {
        let cartridge = Cartridge::from_file(rom_path, boot_rom)?;
        let cpu = Cpu::from_cartridge(cartridge, trace)?;

        #[cfg(feature = "debug")]
        let gameboy = Self {
            cpu,
            frame_counter: 0,
            debugger: debug::Debugger::new(),
        };

        #[cfg(not(feature = "debug"))]
        let gameboy = Self {
            cpu,
            frame_counter: 0,
        };

        Ok(gameboy)
    }

    /// Figure out the number of clock cycles we can execute in a single frame
    #[inline]
    pub fn cycles_per_frame(&self) -> u32 {
        let cycle_time = self.cpu.cycle_time();
        Self::FRAME_DURATION / cycle_time
    }

    /// Run the Gameboy for a single step.
    ///
    /// Returns a tuple of: (cycles consumed, pointer to `FrameBuffer`)
    pub fn step(&mut self, cycle: u32) -> (u32, &FrameBuffer) {
        let speed = self.cpu.speed();

        #[cfg(feature = "debug")]
        // If the debugger is triggered, step into the REPL.
        if self.debugger.triggered(&self.cpu) {
            self.debugger.repl(&mut self.cpu);
        }

        // Execute a step of the CPU
        let (cycles_taken, _inst) = self.cpu.step();

        let mut interrupts = Vec::new();

        // Execute a step of the PPU.
        //
        // The PPU will "catch up" based on what happened in the CPU.
        self.cpu.memory.ppu_mut().step(cycle + cycles_taken as u32, speed, &mut interrupts);

        // Check if a serial interrupt needs to be triggered
        //
        // TODO: This does not happen every cycle, right?
        if self.cpu.memory.io_mut().serial_interrupt() {
            // TODO: Implement correct timing for serial interrupts
            //interrupts.push(Interrupt::Serial);
        }

        self.cpu.dma_step(cycles_taken);

        // Update the internal timer and trigger an interrupt, if needed
        // Note that the timer may tick multiple times for a single instruction
        if self.cpu.memory.timer().step(cycles_taken) {
            interrupts.push(Interrupt::Timer);
        }

        for interrupt in interrupts {
            self.cpu.trigger_interrupt(interrupt);
        }

        (cycles_taken as u32, self.cpu.memory.ppu().frame_buffer())
    }

    /// Run a Gameboy for a single frame.
    ///
    /// The frame takes in an list of joypad events as input, and returns
    /// a `FrameBuffer`.
    pub fn frame(&mut self, mut joypad_events: Vec<Option<JoypadEvent>>) -> &FrameBuffer {
        // Execute next instruction
        let mut cycle = 0;
        let num_cycles = self.cycles_per_frame();

        while cycle < num_cycles {
            let (cycles_taken, _) = self.step(cycle);
            cycle += cycles_taken;
        }

        // Update joypad, if needed
        for event in &mut joypad_events {
            if let Some(event) = event.take() {
                if self.cpu.memory.joypad().handle_event(event) {
                    self.cpu.trigger_interrupt(Interrupt::Joypad);
                }
            }
        }

        self.frame_counter += 1;

        // Return the rendered frame as a frame buffer
        self.cpu.memory.ppu().frame_buffer()
    }

    /// Insert a new cartridge and reset the emulator
    pub fn insert<P: AsRef<Path>>(&mut self, rom_path: P, boot_rom: bool) -> Result<()> {
        let cartridge = Cartridge::from_file(rom_path, boot_rom)?;
        self.cpu = Cpu::from_cartridge(cartridge, false)?;
        self.frame_counter = 0;
        Ok(())
    }

    /// Reset the emulator
    pub fn reset(&mut self) {
        self.frame_counter = 0;

        // Reset the CPU
        self.cpu.reset();
    }

    pub fn cpu(&mut self) -> &mut Cpu {
        &mut self.cpu
    }
}
