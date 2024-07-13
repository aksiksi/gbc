use crate::cartridge::{BootRom, Cartridge, Controller, Ram as CartridgeRam, Rom};
use crate::cpu::Interrupt;
use crate::error::Result;
use crate::joypad::Joypad;
use crate::ppu::{Ppu, Vram};
use crate::timer::Timer;

/// Generic traits that provide access to some memory.
///
/// `A` is the address size, and `V` is the value size.
pub trait MemoryRead<A, V> {
    /// Read a single value from an address.
    fn read(&self, addr: A) -> V;
}

pub trait MemoryWrite<A, V> {
    /// Write a single value to an address.
    fn write(&mut self, addr: A, value: V);
}

/// Internal console work RAM
///
/// * 0xC000 - 0xCFFF: Bank 0,   4K, static
/// * 0xD000 - 0xDFFF: Bank 1,   4K  (non-CGB mode)
/// * 0xD000 - 0xDFFF: Bank 1-7, 4K, switchable (CGB mode)
#[cfg_attr(feature = "save", derive(serde::Serialize), derive(serde::Deserialize))]
pub struct Ram {
    data: Vec<u8>,
    active_bank: u8,
    num_banks: u8,
    cgb: bool,
}

impl Ram {
    const BANK_SIZE: usize = 4 * 1024; // 4K
    pub const BASE_ADDR: u16 = 0xC000;
    pub const LAST_ADDR: u16 = 0xDFFF;
    pub const BANK_SELECT_ADDR: u16 = 0xFF70;

    pub fn new(cgb: bool) -> Self {
        let num_banks = if cgb {
            8u8
        } else {
            2u8
        };

        Self {
            data: vec![0u8; Self::BANK_SIZE * num_banks as usize],
            active_bank: 1,
            num_banks,
            cgb,
        }
    }

    /// Update the active RAM bank
    pub fn update_bank(&mut self, bank: u8) {
        if self.cgb {
            self.active_bank = if bank == 0 {
                1
            } else {
                bank & 0b0111
            };

            assert!(self.active_bank < self.num_banks);
        } else {
            panic!("Received RAM bank change request on unbanked RAM");
        }
    }
}

impl MemoryRead<u16, u8> for Ram {
    #[inline]
    fn read(&self, addr: u16) -> u8 {
        let addr = (addr - Self::BASE_ADDR) as usize;

        match addr {
            0x0000..=0x0FFF => {
                // Read from the first bank
                self.data[addr]
            }
            _ => {
                // Read from the switchable bank
                let addr = addr - 0x1000;
                let bank_offset = self.active_bank as usize * Self::BANK_SIZE;
                self.data[bank_offset + addr]
            }
        }
    }
}

impl MemoryWrite<u16, u8> for Ram {
    #[inline]
    fn write(&mut self, addr: u16, value: u8) {
        let addr = (addr - Self::BASE_ADDR) as usize;

        match addr {
            0x0000..=0x0FFF => {
                // Write to the first bank
                self.data[addr] = value;
            }
            _ => {
                // Write to the switchable bank
                let addr = addr - 0x1000;
                let bank_offset = self.active_bank as usize * Self::BANK_SIZE;
                self.data[bank_offset + addr] = value;
            }
        }
    }
}

/// Memory-mapped I/O registers and buffers
///
/// TODO: Move some stuff to PPU
#[cfg_attr(feature = "save", derive(serde::Serialize), derive(serde::Deserialize))]
pub struct Io {
    /// Joypad register: 0xFF00
    joypad: Joypad,

    // Serial port (SB) and control (SC) (0xFF01, 0xFF02)
    serial: [u8; 2],
    serial_buffer: Vec<char>,

    /// Timer: 0xFF04 - 0xFF07
    timer: Timer,

    /// Interrupt flags (IF) 0xFF0F
    pub int_flags: u8,

    /// Range: 0xFF10 - 0xFF26
    sound: [u8; 23],

    /// Range: 0xFF30 - 0xFF3F
    waveform_ram: [u8; 16],

    // KEY1: 0xFF4D
    pub prep_speed_switch: u8,

    /// Range: 0xFF50
    pub disable_boot_rom: u8,

    /// HDMA1-HDMA5 (0xFF51-0xFF55)
    hdma: [u8; 5],
    pub hdma_active: bool,
    pub hdma_stopped: bool,

    /// Infrared comm. register (0xFF56)
    rp: u8,
}

impl Io {
    pub const BASE_ADDR: u16 = 0xFF00;
    pub const LAST_ADDR: u16 = 0xFF7F;

    pub const SC_ADDR: u16 = 0xFF02;
    pub const SC_REQUEST_MASK: u8 = 1 << 7;

    pub fn new() -> Self {
        Self {
            joypad: Joypad::new(),
            serial: [0; 2],
            serial_buffer: Vec::new(),
            timer: Timer::new(),
            int_flags: 0,
            sound: [0; 23],
            waveform_ram: [0; 16],
            prep_speed_switch: 0,
            disable_boot_rom: 0,
            hdma: [0; 5],
            hdma_active: false,
            hdma_stopped: false,
            rp: 0,
        }
    }

    /// Returns current CGB speed
    ///
    /// `false`: regular, `true`: double
    pub fn speed(&self) -> bool {
        if self.prep_speed_switch & (1 << 7) != 0 {
            true
        } else {
            false
        }
    }

    /// Return a reference to the joypad
    pub fn joypad(&mut self) -> &mut Joypad {
        &mut self.joypad
    }

    pub fn timer(&mut self) -> &mut Timer {
        &mut self.timer
    }

    /// Determine if a serial interrupt needs to be triggered.
    pub fn serial_interrupt(&mut self) -> bool {
        let sc = self.serial[1];

        if sc & Self::SC_REQUEST_MASK != 0 {
            self.serial[1] = sc & !Io::SC_REQUEST_MASK;
            true
        } else {
            false
        }
    }

    /// Write to the HDMA start register without triggering HDMA start.
    #[inline]
    pub fn hdma_reg_write(&mut self, value: u8) {
        self.hdma[4] = value;
    }

    /// Returns a handle to the serial buffer
    ///
    /// This buffer contains every character logged to the serial port.
    /// Mainly used in tests.
    pub fn serial_buffer(&self) -> &[char] {
        &self.serial_buffer
    }
}

impl MemoryRead<u16, u8> for Io {
    #[inline]
    fn read(&self, addr: u16) -> u8 {
        match addr {
            0xFF00 => self.joypad.read(),
            0xFF01 => {
                // Serial read
                self.serial[0]
            }
            0xFF02 => {
                // Serial control
                self.serial[1]
            }
            0xFF04..=0xFF07 => {
                self.timer.read(addr)
            }
            0xFF0F => self.int_flags,
            0xFF10..=0xFF26 => {
                let idx = (addr - 0xFF10) as usize;
                self.sound[idx]
            }
            0xFF30..=0xFF3F => {
                let idx = (addr - 0xFF30) as usize;
                self.waveform_ram[idx]
            }
            0xFF4D => self.prep_speed_switch,
            0xFF50 => self.disable_boot_rom,
            0xFF51..=0xFF55 => {
                // HDMA registers
                let idx = (addr - 0xFF51) as usize;
                self.hdma[idx]
            }
            0xFF56 => self.rp,
            0xFF03 | 0xFF08..=0xFF0E | 0xFF27..=0xFF2F | 0xFF4C..=0xFF4E | 0xFF57..=0xFF67 | 0xFF6D..=0xFF6F | 0xFF71..=0xFF7F => {
                // Invalid registers -- ignore reads from these
                log::warn!("Invalid read from 0x{:X}", addr);
                0xFF
            }
            _ => unreachable!(),
        }
    }
}

impl MemoryWrite<u16, u8> for Io {
    #[inline]
    fn write(&mut self, addr: u16, value: u8) {
        match addr {
            0xFF00 => self.joypad.write(value),
            0xFF01 => {
                // Serial write
                self.serial[0] = value;
            }
            0xFF02 => {
                // Serial control
                self.serial[1] = value;
                if value == 0x81 {
                    let c = self.serial[0] as char;
                    self.serial_buffer.push(c);
                }
            }
            0xFF04..=0xFF07 => {
                self.timer.write(addr, value);
            }
            0xFF0F => {
                self.int_flags = value;
            }
            0xFF10..=0xFF26 => {
                let idx = (addr - 0xFF10) as usize;
                self.sound[idx] = value;
            }
            0xFF30..=0xFF3F => {
                let idx = (addr - 0xFF30) as usize;
                self.waveform_ram[idx] = value;
            }
            0xFF4D => {
                self.prep_speed_switch = value;
            }
            0xFF50 => {
                self.disable_boot_rom = value;
            }
            0xFF51..=0xFF55 => {
                // HDMA registers
                if addr == 0xFF55 {
                    if !self.hdma_active {
                        self.hdma_active = true;
                    } else if value & 1 << 7 == 0 {
                        self.hdma_stopped = true;
                    }
                }

                let idx = (addr - 0xFF51) as usize;
                self.hdma[idx] = value;
            }
            0xFF56 => {
                self.rp = value;
            }
            0xFF03 | 0xFF08..=0xFF0E | 0xFF27..=0xFF2F | 0xFF4C..=0xFF4E | 0xFF57..=0xFF67 | 0xFF6C..=0xFF6F | 0xFF71..=0xFF7F => {
                // Invalid registers -- ignore writes to these
                log::warn!("Invalid write to 0x{:X}: {}", addr, value)
            }
            _ => unreachable!(),
        }
    }
}

pub enum MemoryType {
    Rom,
    Ram,
    CartridgeRam,
    Vram,
    Hram,
    Other,
}

impl std::fmt::Display for MemoryType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MemoryType::Rom => write!(f, "ROM"),
            MemoryType::Ram => write!(f, "RAM"),
            MemoryType::CartridgeRam => write!(f, "CRAM"),
            MemoryType::Vram => write!(f, "VRAM"),
            MemoryType::Hram => write!(f, "HRAM"),
            MemoryType::Other => write!(f, "OTHER"),
        }
    }
}

#[cfg_attr(feature = "save", derive(serde::Serialize), derive(serde::Deserialize))]
/// 64K memory map for the GBC
pub struct MemoryBus {
    /// ROM:       0x0000 - 0x7FFF
    /// Cart RAM:  0xA000 - 0xBFFF
    controller: Controller,

    /// PPU
    ///
    /// Video RAM: 0x8000 - 0x9FFF
    ppu: Ppu,

    /// Work RAM:  0xC000 - 0xDFFF
    ram: Ram,

    // ..ignored

    // I/O:        0xFF00 - 0xFF7F
    io: Io,

    /// High RAM:  0xFF80 - 0xFFFE (120 bytes)
    high_ram: Box<[u8]>,

    /// Interrupt enable  - 0xFFFF
    pub int_enable: u8,

    cgb: bool,

    boot_rom: bool,
}

impl MemoryBus {
    pub const HRAM_BASE_ADDR: u16 = 0xFF80;
    pub const HRAM_LAST_ADDR: u16 = 0xFFFE;

    pub fn new(cgb: bool) -> Self {
        Self {
            controller: Controller::new(),
            ppu: Ppu::new(cgb, false),
            ram: Ram::new(cgb),
            io: Io::new(),
            high_ram: Box::new([0u8; 0x80]),
            int_enable: 0,
            cgb: true,
            boot_rom: false,
        }
    }

    pub fn from_cartridge(cartridge: Cartridge) -> Result<Self> {
        let cgb = cartridge.cgb();
        let boot_rom = cartridge.boot_rom;
        let controller = Controller::from_cartridge(cartridge)?;

        Ok(Self {
            controller,
            ppu: Ppu::new(cgb, boot_rom),
            ram: Ram::new(cgb),
            io: Io::new(),
            high_ram: Box::new([0u8; 0x80]),
            int_enable: 0,
            cgb,
            boot_rom,
        })
    }

    pub fn step(&mut self, cycles: u16, speed: bool, interrupts: &mut Vec<Interrupt>) {
        // Execute a step of the PPU.
        //
        // The PPU will "catch up" based on what happened in the CPU.
        self.ppu.step(cycles, speed, interrupts);

        // Update the internal timer and trigger an interrupt, if needed
        // Note that the timer may tick multiple times for a single instruction
        if self.timer().step(cycles, speed) {
            interrupts.push(Interrupt::Timer);
        }

        // Check if a serial interrupt needs to be triggered
        //
        // TODO: This does not happen every cycle, right?
        if self.io.serial_interrupt() {
            // TODO: Implement correct timing for serial interrupts
            //interrupts.push(Interrupt::Serial);
        }

        // Update the RTC, if present
        if let Some(rtc) = self.controller.rtc.as_mut() {
            rtc.step(cycles, speed);
        }
    }

    /// Reset the memory bus
    pub fn reset(&mut self) {
        let cgb = self.cgb;
        let boot_rom = self.boot_rom;

        self.controller.reset();

        self.ppu = Ppu::new(cgb, boot_rom);
        self.ram = Ram::new(cgb);
        self.io = Io::new();
        self.high_ram = Box::new([0u8; 0x80]);
        self.int_enable = 0;
    }

    /// Given an address in memory, returns the type of memory and bank
    /// number that the address is located in.
    pub fn memory_info(&self, addr: u16) -> (MemoryType, u16) {
        if addr >= Rom::BASE_ADDR && addr < Rom::BASE_ADDR + Rom::BANK_SIZE as u16 {
            // ROM first bank
            (MemoryType::Rom, self.controller.rom.active_bank_0)
        } else if addr >= Rom::BASE_ADDR + Rom::BANK_SIZE as u16 && addr <= Rom::LAST_ADDR {
            // ROM second bank
            (MemoryType::Rom, self.controller.rom.active_bank_1)
        } else if addr >= CartridgeRam::BASE_ADDR && addr <= CartridgeRam::LAST_ADDR {
            // Cartridge RAM
            (MemoryType::CartridgeRam, match &self.controller.ram {
                None => 0,
                Some(ram) => ram.active_bank as u16,
            })
        } else if addr >= Ram::BASE_ADDR && addr < Ram::BASE_ADDR + Ram::BANK_SIZE as u16 {
            // Work RAM first bank
            (MemoryType::Ram, 0)
        } else if addr >= Ram::BASE_ADDR + Ram::BANK_SIZE as u16 && addr <= Ram::LAST_ADDR {
            // Work RAM second bank
            (MemoryType::Ram, self.ram.active_bank as u16)
        } else if addr >= Vram::BASE_ADDR && addr <= Vram::LAST_ADDR {
            // VRAM
            (MemoryType::Vram, self.ppu.vram.active_bank as u16)
        } else if addr >= Self::HRAM_BASE_ADDR && addr <= Self::HRAM_LAST_ADDR {
            // HRAM
            (MemoryType::Hram, 0)
        } else {
            (MemoryType::Other, 0)
        }
    }

    pub fn controller(&self) -> &Controller {
        &self.controller
    }

    pub fn controller_mut(&mut self) -> &mut Controller {
        &mut self.controller
    }

    /// Return a reference to the joypad
    pub fn joypad(&mut self) -> &mut Joypad {
        self.io.joypad()
    }

    pub fn timer(&mut self) -> &mut Timer {
        self.io.timer()
    }

    /// Return a reference to the I/O memory space
    pub fn io(&self) -> &Io {
        &self.io
    }

    /// Return a mutable reference to the I/O memory space
    pub fn io_mut(&mut self) -> &mut Io {
        &mut self.io
    }

    /// Return a reference to the PPU
    pub fn ppu(&self) -> &Ppu {
        &self.ppu
    }

    /// Return a mutable reference to the PPU
    pub fn ppu_mut(&mut self) -> &mut Ppu {
        &mut self.ppu
    }
}

impl MemoryRead<u16, u8> for MemoryBus {
    /// Read a single byte from an arbitrary memory address.
    ///
    /// This will be converted into a read from the relevant memory section.
    fn read(&self, addr: u16) -> u8 {
        match addr {
            BootRom::BASE_ADDR..=BootRom::LAST_ADDR if self.controller.boot_rom.is_some() => {
                // If the boot ROM is active, read from it instead of cartridge ROM
                self.controller.boot_rom.as_ref().unwrap().read(addr)
            }
            Rom::BASE_ADDR..=Rom::LAST_ADDR | CartridgeRam::BASE_ADDR..=CartridgeRam::LAST_ADDR => {
                self.controller.read(addr)
            }
            Vram::BASE_ADDR..=Vram::LAST_ADDR | 0xFE00..=0xFE9F | 0xFF40..=0xFF4B | 0xFF68..=0xFF6B | Vram::BANK_SELECT_ADDR => {
                self.ppu.read(addr)
            }
            Ram::BASE_ADDR..=Ram::LAST_ADDR => self.ram.read(addr),
            0xE000..=0xFDFF => {
                // Echo RAM
                self.read(addr - 0x2000)
            }
            0xFEA0..=0xFEFF => {
                // Prohibited memory area
                // If OAM is locked, returns 0xFF
                // Otherwise, behavior depends on DMG vs. CGB
                if self.ppu().oam_locked() {
                    0xFF
                } else {
                    if !self.cgb {
                        0
                    } else {
                        // From Pan Docs:
                        //   "Returns the high nibble of the lower address byte twice,
                        //    e.g. FFAx returns AA, FFBx returns BB, and so forth."
                        let upper_nibble = ((addr & 0xF0) >> 4) as u8;
                        upper_nibble << 4 | upper_nibble
                    }
                }
            }
            Ram::BANK_SELECT_ADDR => self.ram.active_bank,
            0xFF00..=0xFF7F => {
                self.io.read(addr)
            }
            0xFF80..=0xFFFE => {
                let addr = addr as usize - 0xFF80;
                self.high_ram[addr]
            }
            0xFFFF => self.int_enable,
        }
    }
}

impl MemoryWrite<u16, u8> for MemoryBus {
    fn write(&mut self, addr: u16, value: u8) {
        match addr {
            Rom::BASE_ADDR..=Rom::LAST_ADDR | CartridgeRam::BASE_ADDR..=CartridgeRam::LAST_ADDR => {
                self.controller.write(addr, value);
            }
            Ram::BASE_ADDR..=Ram::LAST_ADDR => self.ram.write(addr, value),
            0xE000..=0xFDFF => {
                // Echo RAM
            }
            Vram::BASE_ADDR..=Vram::LAST_ADDR | 0xFE00..=0xFE9F | 0xFF40..=0xFF4B | 0xFF68..=0xFF6C | Vram::BANK_SELECT_ADDR => {
                self.ppu.write(addr, value);
            }
            0xFF80..=0xFFFE => {
                let addr = addr as usize - 0xFF80;
                self.high_ram[addr] = value;
            }
            0xFEA0..=0xFEFF => {
                // Prohibited memory area -- no effect on writes
            }
            0xFF50 => {
                // Disable boot ROM
                if self.io.disable_boot_rom == 0 && value & 0x1 != 0 {
                    self.controller.boot_rom.take();
                    self.io.disable_boot_rom = 1;
                }
            }
            Ram::BANK_SELECT_ADDR => self.ram.update_bank(value),
            0xFF00..=0xFF7F => {
                self.io.write(addr, value)
            }
            0xFFFF => {
                self.int_enable = value;
            }
        }
    }
}

/// Write a 16-bit word to memory. This maps into 2 8-bit writes
/// to the relevant memory region.
impl MemoryWrite<u16, u16> for MemoryBus {
    #[inline]
    fn write(&mut self, addr: u16, value: u16) {
        let value = value.to_le_bytes();
        self.write(addr, value[0]);
        self.write(addr+1, value[1]);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn ram_operations() {
        let mut ram = Ram::new(true);

        ram.write(Ram::BASE_ADDR, 0x66u8);
        let value: u8 = ram.read(Ram::BASE_ADDR);
        assert_eq!(value, 0x66);

        ram.write(Ram::BASE_ADDR + 0x1234u16, 0x66u8);
        let value: u8 = ram.read(Ram::BASE_ADDR + 0x1234u16);
        assert_eq!(value, 0x66);
    }
}
