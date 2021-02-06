pub mod cartridge;
mod cpu;
mod dma;
pub mod error;
mod instructions;
pub mod joypad;
mod memory;
pub mod ppu;
mod registers;
mod rtc;
mod timer;

#[cfg(feature = "debug")]
pub mod debug;

pub use cpu::Cpu;
use cpu::Interrupt;
use cartridge::{Cartridge, Controller};
pub use error::{Error, Result};
use joypad::JoypadEvent;
use memory::MemoryWrite;
use ppu::FrameBuffer;

#[derive(Default)]
pub struct GameboyState<'a> {
    pub ram: Option<&'a [u8]>,
    pub rtc: Option<Vec<u8>>,
}

#[cfg_attr(feature = "save", derive(serde::Serialize), derive(serde::Deserialize))]
/// Gameboy
pub struct Gameboy {
    cpu: Cpu,

    #[cfg(feature = "debug")]
    #[cfg_attr(feature = "save", serde(skip))]
    debugger: debug::Debugger,
}

impl Gameboy {
    const FRAME_FREQUENCY: f64 = 59.7; // Hz

    /// Frame duration, in ns
    pub const FRAME_DURATION: u64 = ((1f64 / Self::FRAME_FREQUENCY) * 1e9) as u64;

    /// Initialize the emulator from a `Cartridge`.
    pub fn init(cartridge: Cartridge, trace: bool) -> Result<Self> {
        let cpu = Cpu::from_cartridge(cartridge, trace)?;

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

    /// Run the Gameboy for a single step.
    ///
    /// Returns the number of cycles consumed by the CPU.
    pub fn step(&mut self) -> u32 {
        let speed = self.cpu.speed;

        #[cfg(feature = "debug")]
        // If the debugger is triggered, step into the REPL.
        if self.debugger.triggered(&self.cpu) {
            self.debugger.repl(&mut self.cpu);
        }

        // Execute a step of the CPU
        //
        // This handles interrupt processing and DMA internally.
        let (cycles_taken, _inst) = self.cpu.step();

        let mut interrupts = Vec::new();

        // Update the memory bus
        //
        // Internally, this executes a step for each of:
        //
        // 1. PPU
        // 2. Timer
        // 3. Serial
        // 4. RTC (if present)
        self.cpu.memory.step(cycles_taken, speed, &mut interrupts);

        // Trigger any pending interrupts
        for interrupt in interrupts {
            self.cpu.trigger_interrupt(interrupt);
        }

        if self.cpu.stopped {
            // Reset DIV on speed switch
            self.cpu.memory.write(0xFF04u16, 0u8);
            self.cpu.stopped = false;
        }

        cycles_taken as u32
    }

    /// Run the Gameboy until a frame is ready (i.e., start of VBLANK).
    ///
    /// Returns a pointer to the frame buffer.
    pub fn frame(&mut self, joypad_events: Option<&[JoypadEvent]>) -> &FrameBuffer {
        while !self.cpu.memory.ppu().is_frame_ready() {
            self.step();
        }

        self.update_joypad(joypad_events);

        // This is a clear-on-read operation. That is, the frame will be marked as
        // "not ready" within this method.
        self.cpu.memory.ppu_mut().frame_buffer().unwrap()
    }

    pub fn update_joypad(&mut self, joypad_events: Option<&[JoypadEvent]>) {
        if let Some(events) = joypad_events {
            for event in events {
                if self.cpu.memory.joypad().handle_event(event) {
                    self.cpu.trigger_interrupt(Interrupt::Joypad);
                }
            }
        }
    }

    /// Insert a new cartridge and reset the emulator
    pub fn insert(&mut self, cartridge: Cartridge) -> Result<()> {
        self.cpu = Cpu::from_cartridge(cartridge, false)?;
        Ok(())
    }

    /// Load a Gameboy from a save state and a `Cartridge`.
    #[cfg(feature = "save")]
    pub fn load(save_data: &[u8], cartridge: Cartridge) -> Result<Self> {
        let mut gameboy: Self = bincode::deserialize_from(save_data)?;

        // Load ROM and any other cartridge-related info
        gameboy.cpu.memory.controller_mut().load_rom(cartridge.data);

        Ok(gameboy)
    }

    /// Dump the current state of this Gameboy to a byte `Vec`.
    #[cfg(feature = "save")]
    pub fn dump(&self) -> Result<Vec<u8>> {
        let mut data = Vec::new();
        bincode::serialize_into(&mut data, &self)?;
        Ok(data)
    }

    /// Reset the emulator
    pub fn reset(&mut self) {
        // Reset the CPU
        self.cpu.reset();
    }

    pub fn cpu(&mut self) -> &mut Cpu {
        &mut self.cpu
    }

    pub fn controller(&mut self) -> &mut Controller {
        self.cpu.memory.controller_mut()
    }

    /// Returns raw cartridge RAM and RTC state
    ///
    /// This data can be used by the emulator to "persist" state across runs
    /// of a ROM. Note that it should be sufficient to call this method once
    /// per frame.
    pub fn state(&self) -> GameboyState {
        let (mut ram_data, mut rtc_data) = (None, None);

        if let Some(ram) = &self.cpu.memory.controller().ram {
            ram_data = Some(ram.data());
        }

        if let Some(rtc) = &self.cpu.memory.controller().rtc {
            rtc_data = Some(rtc.dump());
        }

        GameboyState {
            ram: ram_data,
            rtc: rtc_data,
        }
    }

    #[inline]
    pub fn is_persist_required(&self) -> bool {
        let ram = &self.cpu.memory.controller().ram;
        let rtc = &self.cpu.memory.controller().rtc;
        ram.is_some() || rtc.is_some()
    }

    /// If this `Gameboy` requires state to be persisted, the provided callback
    /// will be triggered with the current state.
    pub fn persist(&self, persist_fn: impl Fn(GameboyState) -> Result<()>) -> Result<()>{
        if self.is_persist_required() {
            persist_fn(self.state())?;
        }

        Ok(())
    }

    pub fn unpersist(&mut self, state: GameboyState) -> Result<()> {
        if let Some(ram) = state.ram {
            self.cpu.memory.controller_mut().load_ram(ram)?;
        }

        if let Some(rtc) = state.rtc {
            self.cpu.memory.controller_mut().load_rtc(&rtc)?;
        }

        Ok(())
    }

    /// Returns a String containing the serial output of this Gameboy _so far_.
    ///
    /// In other words, this output is cumulative and contains every character
    /// logged to serial since the start of Gameboy.
    pub fn serial_output(&self) -> String {
        self.cpu.memory.io().serial_buffer().into_iter().collect()
    }
}
