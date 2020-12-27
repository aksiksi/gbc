use crate::cartridge::{Cartridge, Ram as CartridgeRam, Rom};
use crate::error::Result;
use crate::joypad::Joypad;

/// Generic traits that provide access to some memory.
///
/// `A` is the address size, and `V` is the value size.
pub trait MemoryRead<A, V> {
    /// Read a single value from an address.
    fn read(&self, addr: A) -> V;
}

pub trait MemoryRange<A, V> {
    /// Returns a read-only slice of an address range.
    ///
    /// Primarily used for instruction decoding.
    fn range(&self, range: std::ops::RangeFrom<A>) -> &[V];
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
pub enum Ram {
    Unbanked {
        /// Two static banks, 4K each
        /// Non-CGB mode
        data: [u8; Self::BANK_SIZE * 2],
    },
    Banked {
        /// Eight static banks, 4K each
        /// CGB mode
        data: [u8; Self::BANK_SIZE * 8],
        active_bank: u8,
    },
}

impl Ram {
    const BANK_SIZE: usize = 4 * 1024; // 4K
    pub const BASE_ADDR: u16 = 0xC000;
    pub const LAST_ADDR: u16 = 0xDFFF;

    pub fn new(cgb: bool) -> Self {
        if cgb {
            Self::Banked {
                data: [0u8; Self::BANK_SIZE * 8],
                active_bank: 0,
            }
        } else {
            Self::Unbanked {
                data: [0u8; Self::BANK_SIZE * 2],
            }
        }
    }

    /// Update the active RAM bank
    pub fn update_bank(&mut self, bank: u8) {
        match self {
            Self::Banked {
                data: _,
                active_bank,
            } => {
                *active_bank = bank;
            }
            _ => panic!("Received RAM bank change request on unbanked RAM"),
        }
    }
}

impl MemoryRead<u16, u8> for Ram {
    #[inline]
    fn read(&self, addr: u16) -> u8 {
        let addr = (addr - Self::BASE_ADDR) as usize;

        match self {
            Self::Unbanked { data } => data[addr],
            Self::Banked { data, active_bank } => {
                match addr {
                    0x0000..=0x0FFF => {
                        // Read from the first bank
                        data[addr]
                    }
                    _ => {
                        // Read from the switchable bank
                        let bank_offset = *active_bank as usize * Self::BANK_SIZE;
                        data[bank_offset + addr]
                    }
                }
            }
        }
    }
}

impl MemoryWrite<u16, u8> for Ram {
    #[inline]
    fn write(&mut self, addr: u16, value: u8) {
        let addr = (addr - Self::BASE_ADDR) as usize;

        match self {
            Self::Unbanked { data } => {
                data[addr] = value;
            }
            Self::Banked { data, active_bank } => {
                match addr {
                    0x0000..=0x0FFF => {
                        // Write to the first bank
                        data[addr] = value;
                    }
                    _ => {
                        // Write to the switchable bank
                        let bank_offset = *active_bank as usize * Self::BANK_SIZE;
                        data[bank_offset + addr] = value;
                    }
                }
            }
        }
    }
}

impl MemoryWrite<u16, u16> for Ram {
    #[inline]
    fn write(&mut self, addr: u16, value: u16) {
        let addr = (addr - Self::BASE_ADDR) as usize;
        let value = value.to_le_bytes();

        match self {
            Self::Unbanked { data } => {
                data[addr] = value[0];
                data[addr + 1] = value[1];
            }
            Self::Banked { data, active_bank } => {
                match addr {
                    0x0000..=0x0FFF => {
                        // Write to the first bank
                        data[addr] = value[0];
                        data[addr + 1] = value[1];
                    }
                    _ => {
                        // Write to the switchable bank
                        let bank_offset = *active_bank as usize * Self::BANK_SIZE;
                        data[bank_offset + addr] = value[0];
                        data[bank_offset + addr + 1] = value[1];
                    }
                }
            }
        }
    }
}

impl std::fmt::Debug for Ram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unbanked { data } => f
                .debug_struct("Ram::Unbanked")
                .field("size", &data.len())
                .finish(),
            Self::Banked { data, active_bank } => f
                .debug_struct("CartridgeRam::Banked")
                .field("size", &data.len())
                .field("active_bank", &active_bank)
                .finish(),
        }
    }
}

pub enum Vram {
    Unbanked {
        /// Static bank, 8K
        ///
        /// Non-CGB mode
        data: [u8; Self::BANK_SIZE],
    },
    Banked {
        /// Two static banks, 8K each
        ///
        /// CGB mode
        data: [u8; Self::BANK_SIZE * 2],
        active_bank: u8,
    },
}

impl Vram {
    const BANK_SIZE: usize = 8 * 1024;
    pub const BASE_ADDR: u16 = 0x8000;
    pub const LAST_ADDR: u16 = 0x9FFF;

    pub fn new(cgb: bool) -> Self {
        if cgb {
            Self::Banked {
                data: [0u8; Self::BANK_SIZE * 2],
                active_bank: 0,
            }
        } else {
            Self::Unbanked {
                data: [0u8; Self::BANK_SIZE],
            }
        }
    }

    /// Update the active VRAM bank
    pub fn update_bank(&mut self, bank: u8) {
        match self {
            Self::Banked {
                data: _,
                active_bank,
            } => {
                *active_bank = bank;
            }
            _ => panic!("Received VRAM bank change request on unbanked VRAM"),
        }
    }
}

impl MemoryRead<u16, u8> for Vram {
    #[inline]
    fn read(&self, addr: u16) -> u8 {
        let addr = (addr - Self::BASE_ADDR) as usize;
        match self {
            Self::Unbanked { data } => {
                // Bank 0 (static)
                data[addr]
            }
            Self::Banked { data, active_bank } => {
                let bank_offset = *active_bank as usize * Self::BANK_SIZE;
                data[bank_offset + addr]
            }
        }
    }
}

impl MemoryWrite<u16, u8> for Vram {
    #[inline]
    fn write(&mut self, addr: u16, value: u8) {
        let addr = (addr - Self::BASE_ADDR) as usize;
        match self {
            Self::Unbanked { data } => {
                // Bank 0 (static)
                data[addr] = value;
            }
            Self::Banked { data, active_bank } => {
                let bank_offset = *active_bank as usize * Self::BANK_SIZE;
                data[bank_offset + addr] = value;
            }
        }
    }
}

impl std::fmt::Debug for Vram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unbanked { data } => f
                .debug_struct("Vram::Unbanked")
                .field("vram_size", &data.len())
                .finish(),
            Self::Banked { data, active_bank } => f
                .debug_struct("Vram::Banked")
                .field("vram_size", &data.len())
                .field("active_bank", &active_bank)
                .finish(),
        }
    }
}

/// Memory-mapped I/O registers and buffers
#[derive(Debug)]
pub struct Io {
    /// Joypad register: 0xFF00
    joypad: Joypad,

    /// Range: 0xFF04 - 0xFF07
    port1: [u8; 3],

    /// Range: 0xFF10 - 0xFF26
    sound: [u8; 23],

    /// Range: 0xFF30 - 0xFF3F
    waveform_ram: [u8; 16],

    /// Range: 0xFF40 - 0xFF4B
    lcd: [u8; 12],

    // KEY1: 0xFF4D
    prep_speed_switch: u8,

    /// Range: 0xFF4F
    vram_bank: u8,

    /// Range: 0xFF50
    disable_boot_rom: u8,

    /// Range: 0xFF51 - 0xFF55
    hdma: [u8; 4],

    /// Range: 0xFF68 - 0xFF69
    bcp: [u8; 2],

    /// Range: 0xFF70
    wram_bank: u8,
}

impl Io {
    pub const BASE_ADDR: u16 = 0xFF00;
    pub const LAST_ADDR: u16 = 0xFF7F;
    pub const VRAM_BANK_SELECT_ADDR: u16 = 0xFF4F;
    pub const WRAM_BANK_SELECT_ADDR: u16 = 0xFF70;

    pub fn new() -> Self {
        Self {
            joypad: Joypad::new(),
            port1: [0; 3],
            sound: [0; 23],
            waveform_ram: [0; 16],
            lcd: [0; 12],
            prep_speed_switch: 0,
            vram_bank: 0,
            disable_boot_rom: 0,
            hdma: [0; 4],
            bcp: [0; 2],
            wram_bank: 0,
        }
    }

    /// Returns currently selected VRAM bank
    pub fn vram_bank(&self) -> u8 {
        self.vram_bank
    }

    /// Returns currently selected WRAM bank
    pub fn wram_bank(&self) -> u8 {
        self.wram_bank
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
}

impl MemoryRead<u16, u8> for Io {
    #[inline]
    fn read(&self, addr: u16) -> u8 {
        let idx = (addr - Self::BASE_ADDR) as usize;

        match addr {
            0xFF00 => self.joypad.read(),
            0xFF04..=0xFF07 => self.port1[idx],
            0xFF10..=0xFF26 => self.sound[idx],
            0xFF30..=0xFF3F => self.waveform_ram[idx],
            0xFF40..=0xFF4B => self.lcd[idx],
            0xFF4D => self.prep_speed_switch,
            Self::VRAM_BANK_SELECT_ADDR => self.vram_bank,
            0xFF50 => self.disable_boot_rom,
            0xFF51..=0xFF55 => self.hdma[idx],
            0xFF68..=0xFF69 => self.bcp[idx],
            Self::WRAM_BANK_SELECT_ADDR => self.wram_bank,
            _ => panic!("Invalid write to address {}", addr),
        }
    }
}

impl MemoryWrite<u16, u8> for Io {
    #[inline]
    fn write(&mut self, addr: u16, value: u8) {
        let idx = (addr - Self::BASE_ADDR) as usize;

        match addr {
            0xFF00 => self.joypad.write(value),
            0xFF04..=0xFF07 => {
                self.port1[idx] = value;
            }
            0xFF10..=0xFF26 => {
                self.sound[idx] = value;
            }
            0xFF30..=0xFF3F => {
                self.waveform_ram[idx] = value;
            }
            0xFF40..=0xFF4B => {
                self.lcd[idx] = value;
            }
            0xFF4D => {
                // Switch speed immediately
                // TODO: See if this needs to happen after a STOP?
                if value & 0x1 != 0 {
                    self.prep_speed_switch = 1 << 7;
                } else {
                    self.prep_speed_switch = 0;
                }
            }
            Self::VRAM_BANK_SELECT_ADDR => {
                self.vram_bank = value;
            }
            0xFF50 => {
                self.disable_boot_rom = value;
            }
            0xFF51..=0xFF55 => {
                self.hdma[idx] = value;
            }
            0xFF68..=0xFF69 => {
                self.bcp[idx] = value;
            }
            Self::WRAM_BANK_SELECT_ADDR => {
                self.wram_bank = value;
            }
            _ => panic!("Invalid write to address {}: value {}", addr, value),
        }
    }
}

/// 64K memory map for the GBC
#[derive(Debug)]
pub struct MemoryBus {
    /// ROM:       0x0000 - 0x7FFF
    /// Cart RAM:  0xA000 - 0xBFFF
    cartridge: Cartridge,

    /// Video RAM: 0x8000 - 0x9FFF
    vram: Vram,

    /// Work RAM:  0xC000 - 0xDFFF
    ram: Ram,

    // ..ignored

    // I/O:        0xFF00 - 0xFF7F
    io: Io,

    /// High RAM:  0xFF80 - 0xFFFE
    high_ram: [u8; 0x80],

    /// 0xFFFF
    int_enable_reg: u8,
}

impl MemoryBus {
    pub fn new() -> Self {
        Self {
            cartridge: Cartridge::new(),
            vram: Vram::new(true),
            ram: Ram::new(true),
            io: Io::new(),
            high_ram: [0u8; 0x80],
            int_enable_reg: 0,
        }
    }

    pub fn from_cartridge(cartridge: Cartridge) -> Result<Self> {
        Ok(Self {
            cartridge,
            vram: Vram::new(true),
            ram: Ram::new(true),
            io: Io::new(),
            high_ram: [0u8; 0x80],
            int_enable_reg: 0,
        })
    }

    pub fn cartridge(&mut self) -> &mut Cartridge {
        &mut self.cartridge
    }

    /// Return a reference to the joypad
    pub fn joypad(&mut self) -> &mut Joypad {
        self.io.joypad()
    }

    /// Return a reference to the I/O memory space
    pub fn io(&self) -> &Io {
        &self.io
    }
}

impl MemoryRead<u16, u8> for MemoryBus {
    /// Read a single byte from an arbitrary memory address.
    ///
    /// This will be converted into a read from the relevant memory section.
    fn read(&self, addr: u16) -> u8 {
        match addr {
            Rom::BASE_ADDR..=Rom::LAST_ADDR => self.cartridge.rom.read(addr),
            Vram::BASE_ADDR..=Vram::LAST_ADDR => self.vram.read(addr),
            CartridgeRam::BASE_ADDR..=CartridgeRam::LAST_ADDR => {
                self.cartridge.ram.as_ref().unwrap().read(addr)
            }
            Ram::BASE_ADDR..=Ram::LAST_ADDR => self.ram.read(addr),
            Io::BASE_ADDR..=Io::LAST_ADDR => self.io.read(addr),
            0xFF80..=0xFFFE => {
                let addr = addr as usize - 0xFF80;
                self.high_ram[addr]
            }
            _ => panic!("Unable to read from address: {:?}", addr),
        }
    }
}

impl MemoryRange<u16, u8> for MemoryBus {
    /// Return a range of bytes from the relevant memory.
    fn range(&self, range: std::ops::RangeFrom<u16>) -> &[u8] {
        match range.start {
            Rom::BASE_ADDR..=Rom::LAST_ADDR => self.cartridge.rom.range(range),
            // 0x8000..=0x9FFF => self.vram.read(addr),
            CartridgeRam::BASE_ADDR..=CartridgeRam::LAST_ADDR => {
                self.cartridge.ram.as_ref().unwrap().range(range)
            }
            // 0xC000..=0xDFFF => self.ram.read(addr),
            0xFF80..=0xFFFE => {
                let start = range.start as usize - 0xFF80;
                &self.high_ram[start..]
            }
            _ => panic!("Unable to handle range: {:?}", range),
        }
    }
}

impl MemoryWrite<u16, u8> for MemoryBus {
    fn write(&mut self, addr: u16, value: u8) {
        match addr {
            Rom::BASE_ADDR..=Rom::LAST_ADDR => self.cartridge.write(addr, value),
            Vram::BASE_ADDR..=Vram::LAST_ADDR => self.vram.write(addr, value),
            CartridgeRam::BASE_ADDR..=CartridgeRam::LAST_ADDR => {
                self.cartridge.write(addr, value)
            }
            Ram::BASE_ADDR..=Ram::LAST_ADDR => self.ram.write(addr, value),
            Io::BASE_ADDR..=Io::LAST_ADDR => {
                self.io.write(addr, value);

                // Snoop for bank changes
                if addr == Io::VRAM_BANK_SELECT_ADDR {
                    let bank = self.io.vram_bank();
                    self.vram.update_bank(bank);
                } else if addr == Io::WRAM_BANK_SELECT_ADDR {
                    let bank = self.io.wram_bank();
                    self.ram.update_bank(bank);
                }
            }
            0xFF80..=0xFFFE => {
                let addr = addr as usize - 0xFF80;
                self.high_ram[addr] = value;
            }
            _ => panic!("Unable to write to address: {:?}", addr),
        }
    }
}

/// Write a 16-bit word to memory.
impl MemoryWrite<u16, u16> for MemoryBus {
    fn write(&mut self, addr: u16, value: u16) {
        match addr {
            CartridgeRam::BASE_ADDR..=CartridgeRam::LAST_ADDR => {
                self.cartridge.ram.as_mut().unwrap().write(addr, value)
            }
            Ram::BASE_ADDR..=Ram::LAST_ADDR => self.ram.write(addr, value),
            0xFF80..=0xFFFE => {
                let addr = addr as usize - 0xFF80;
                let value = value.to_le_bytes();
                self.high_ram[addr] = value[0];
                self.high_ram[addr + 1] = value[1];
            }
            _ => panic!("Unable to write to address: {:?}", addr),
        }
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
