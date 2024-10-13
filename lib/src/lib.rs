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

use cartridge::{Cartridge, Controller};
pub use cpu::Cpu;
use cpu::Interrupt;
pub use error::{Error, Result};
use joypad::JoypadEvent;
use memory::MemoryWrite;
use ppu::FrameBuffer;

#[derive(serde::Deserialize, serde::Serialize)]
pub struct GameboyState<'a> {
    pub ram: Option<&'a [u8]>,
    pub rtc: Option<Vec<u8>>,
}

#[derive(Clone, Copy, Debug)]
#[cfg_attr(feature = "save", derive(serde::Serialize), derive(serde::Deserialize))]
pub enum CpuAction {
    None,
    Fetch,
    Execute(instructions::Instruction),
    MemWait,
    ServiceInterrupts,
}

#[derive(Clone, Copy, Debug)]
#[cfg_attr(feature = "save", derive(serde::Serialize), derive(serde::Deserialize))]
pub enum Action {
    None,
    Cpu(CpuAction),
    PpuRenderDot,
    ProcessDma,
    ProcessTimer,
    ProcessRtc,
}

// Action ring buffer for next C M-cycles. Each cycle can hold up to A actions.
//
// 1 M-cycle = 4 CPU cycles
#[cfg_attr(feature = "save", derive(serde::Serialize), derive(serde::Deserialize))]
pub struct ActionQueue {
    idx: u8,
    buf: Box<[[Action; Self::A]; Self::C]>,
    counts: [u8; Self::C],
}

impl ActionQueue {
    // Number of M-cycles
    const C: usize = 16;
    // Number of actions
    const A: usize = 16;

    fn new() -> Self {
        let mut q = Self {
            idx: 0,
            buf: Box::new([[Action::None; Self::A]; Self::C]),
            counts: [0; Self::C],
        };
        q.buf[0][0] = Action::Cpu(CpuAction::Fetch);
        q.counts[0] = 1;
        q
    }

    // Enqueue an action to be executed a number of M-cycles in the future.
    fn enqueue(&mut self, cycles: u8, action: Action) {
        let idx = (self.idx + cycles) as usize % Self::C;
        assert!(self.counts[idx] <= Self::A as u8);
        let pos = self.counts[idx] as usize;
        self.buf[idx][pos] = action;
        self.counts[idx] += 1;
    }

    // Returns actions for current M-cycle and advances the buffer to the next
    // valid M-cycle.
    fn dequeue(&mut self) -> Option<[Action; Self::A]> {
        let idx = self.idx as usize;

        // Advance the queue position.
        self.idx = (self.idx + 1) % Self::C as u8;

        // Fast path: nothing to do for this M-cycle.
        if self.counts[idx] == 0 {
            return None;
        }

        let actions = self.buf[idx];

        // Clear out the current slot in the buffer.
        self.buf[idx] = [Action::None; Self::A];
        self.counts[idx] = 0;

        Some(actions)
    }
}

#[cfg_attr(feature = "save", derive(serde::Serialize), derive(serde::Deserialize))]
/// Gameboy
pub struct Gameboy {
    cpu: Cpu,
    action_queue: ActionQueue,

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
            action_queue: ActionQueue::new(),
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
        let (cycles_taken, _inst) = self.cpu.step(&mut self.action_queue);

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

    /// Save the current state of this Gameboy to a byte `Vec`.
    #[cfg(feature = "save")]
    pub fn save(&self) -> Result<Vec<u8>> {
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

    #[inline]
    pub fn is_persist_required(&self) -> bool {
        let controller = &self.cpu.memory.controller();
        let ram = &controller.ram;
        let rtc = &controller.rtc;
        ram.is_some() || rtc.is_some()
    }

    #[inline]
    pub fn is_persist_ram(&self) -> bool {
        let controller = &self.cpu.memory.controller();
        let ram = &controller.ram;
        ram.is_some()
    }

    #[inline]
    pub fn is_persist_rtc(&self) -> bool {
        let controller = &self.cpu.memory.controller();
        let rtc = &controller.rtc;
        rtc.is_some()
    }

    /// Returns raw persisted state for this Gameboy (i.e., RAM and/or RTC).
    ///
    /// This data can be used by the emulator to "persist" state across runs
    /// of a ROM. Note that it should be sufficient to call this method once
    /// per frame.
    ///
    /// For cartridge RAM, the contents of the RAM will only be returned if
    /// a write has occurred since the last frame.
    pub fn persist(&mut self) -> Option<GameboyState> {
        if !self.is_persist_required() {
            return None;
        }

        let (mut ram_data, mut rtc_data) = (None, None);
        let controller = self.cpu.memory.controller_mut();

        if let Some(ram) = &mut controller.ram {
            if ram.is_dirty {
                ram.is_dirty = false;
                ram_data = Some(ram.data());
            }
        }

        if let Some(rtc) = &controller.rtc {
            rtc_data = Some(rtc.dump());
        }

        let state = GameboyState {
            ram: ram_data,
            rtc: rtc_data,
        };

        Some(state)
    }

    /// Load persisted state into this `Gameboy`.
    pub fn unpersist<T, U>(&mut self, ram: Option<T>, rtc: Option<U>) -> Result<()>
    where
        T: AsRef<[u8]>,
        U: AsRef<[u8]>,
    {
        if !self.is_persist_required() {
            return Ok(());
        }

        if let Some(ram) = ram {
            self.cpu.memory.controller_mut().load_ram(ram.as_ref())?;
        }

        if let Some(rtc) = rtc {
            self.cpu.memory.controller_mut().load_rtc(rtc.as_ref())?;
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
