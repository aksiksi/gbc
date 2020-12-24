use crate::cartridge::{Cartridge, Ram as CartridgeRam, Rom};
use crate::error::Result;

/// Generic traits that provide access to some memory.
/// `A` is the address size, and `V` is the value size.
pub trait MemoryRead<A, V> {
    /// Read a single value from an address.
    fn read(&self, addr: A) -> V;
}

pub trait MemoryRange<A, V> {
    /// Returns a read-only slice of an address range.
    /// Primarily used for instruction decoding.
    fn range(&self, range: std::ops::Range<A>) -> &[V];
}

pub trait MemoryWrite<A, V> {
    /// Write a single value to an address.
    fn write(&mut self, addr: A, value: V);
}

/// Internal console work RAM
///
/// 0xC000 - 0xCFFF: Bank 0,   4K, static
/// 0xD000 - 0xDFFF: Bank 1,   4K  (non-CGB mode)
/// 0xD000 - 0xDFFF: Bank 1-7, 4K, switchable (CGB mode)
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
    }
}

impl Ram {
    const BANK_SIZE: usize = 4 * 1024; // 4K
    pub const BASE_ADDR: u16 = 0xC000;

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
}

impl MemoryRead<u16, u8> for Ram {
    #[inline]
    fn read(&self, addr: u16) -> u8 {
        let addr = (addr - Self::BASE_ADDR) as usize;

        match self {
            Self::Unbanked { data } => {
               data[addr]
            },
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
            },
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

impl std::fmt::Debug for Ram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unbanked { data } => f
                .debug_struct("Ram::Unbanked")
                .field("size", &data.len())
                .finish(),
            Self::Banked {
                data,
                active_bank,
            } => f
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
        /// Non-CGB mode
        data: [u8; Self::BANK_SIZE],
    },
    Banked {
        /// Two static banks, 8K each
        /// CGB mode
        data: [u8; Self::BANK_SIZE * 2],
        active_bank: u8,
    }
}

impl Vram {
    const BANK_SIZE: usize = 8 * 1024;
    pub const BASE_ADDR: u16 = 0x8000;

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
            Self::Banked {
                data,
                active_bank,
            } => f
                .debug_struct("Vram::Banked")
                .field("vram_size", &data.len())
                .field("active_bank", &active_bank)
                .finish(),
        }
    }
}

/// 64K memory map for the GBC
#[derive(Debug)]
pub struct MemoryBus {
    /// 0x0000 - 0x7FFF
    rom: Rom,

    /// 0x8000 - 0x9FFF
    vram: Vram,

    /// 0xA000 - 0xBFFF
    cartridge_ram: Option<CartridgeRam>,

    /// 0xC000 - 0xDFFF
    ram: Ram,

    // ..ignored
    /// 0xFF80 - 0xFFFE
    high_ram: [u8; 0x80],

    /// 0xFFFF
    int_enable_reg: u8,
}

impl MemoryBus {
    pub fn from_cartridge(cartridge: &mut Cartridge) -> Result<Self> {
        Ok(Self {
            rom: cartridge.get_rom()?,
            vram: Vram::new(true),
            cartridge_ram: cartridge.get_ram()?,
            ram: Ram::new(true),
            high_ram: [0u8; 0x80],
            int_enable_reg: 0,
        })
    }

    pub fn rom(&self) -> &Rom {
        &self.rom
    }
}

impl MemoryRead<u16, u8> for MemoryBus {
    /// Read a single byte from an arbitrary memory address.
    /// This will be converted into a read from the relevant memory section.
    fn read(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x7FFF => self.rom.read(addr),
            0x8000..=0x9FFF => self.vram.read(addr),
            0xA000..=0xBFFF => self.cartridge_ram.as_ref().unwrap().read(addr),
            0xC000..=0xDFFF => self.ram.read(addr),
            0xFF80..=0xFFFE => {
                let addr = addr as usize - 0xFF80;
                self.high_ram[addr]
            }
            _ => panic!("Unable to read from address: {:?}", addr),
        }
    }
}

impl MemoryWrite<u16, u8> for MemoryBus {
    fn write(&mut self, addr: u16, value: u8) {
        match addr {
            0x8000..=0x9FFF => self.vram.write(addr, value),
            0xA000..=0xBFFF => self.cartridge_ram.as_mut().unwrap().write(addr, value),
            0xC000..=0xDFFF => self.ram.write(addr, value),
            0xFF80..=0xFFFE => {
                let addr = addr as usize - 0xFF80;
                self.high_ram[addr] = value;
            }
            _ => panic!("Unable to write to address: {:?}", addr),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::cartridge::RomSize;

    #[test]
    fn test_rom_operations() {
        let mut rom = Rom::new(RomSize::_2M);

        rom.write(0u16, 0x66u8);
        let value: u8 = rom.read(0u16);
        assert_eq!(value, 0x66);

        rom.write(0x1234u16, 0x66u8);
        let value: u8 = rom.read(0x1234u16);
        assert_eq!(value, 0x66);
    }

    #[test]
    fn test_ram_operations() {
        let mut ram = Ram::new(true);

        ram.write(Ram::BASE_ADDR, 0x66u8);
        let value: u8 = ram.read(Ram::BASE_ADDR);
        assert_eq!(value, 0x66);

        ram.write(Ram::BASE_ADDR + 0x1234u16, 0x66u8);
        let value: u8 = ram.read(Ram::BASE_ADDR + 0x1234u16);
        assert_eq!(value, 0x66);
    }
}
