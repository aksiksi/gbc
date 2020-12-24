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
/// 0xD000 - 0xDFFF: Bank 1, 4K (non-CGB mode)
/// 0xD000 - 0xDFFF: Bank 1-7, 4K, switchable (CGB mode)
pub struct Ram {
    /// Static bank, 4K
    bank0: [u8; Self::BANK_SIZE],

    /// Dynamic bank, depends on active bank
    ///
    /// Non-CGB mode: 1 x 4K
    /// CGB mode: 8 x 4K
    bank1: Vec<u8>,

    /// Currently active bank
    ///
    /// Note: This is ignored in non-CGB mode
    active_bank: u8,
}

impl Ram {
    const BANK_SIZE: usize = 4 * 1024; // 4K
    const NUM_BANKS: usize = 8;

    pub fn new(cgb: bool) -> Self {
        let bank0 = [0u8; Self::BANK_SIZE];

        let bank1 = if cgb {
            vec![0u8; Self::BANK_SIZE * Self::NUM_BANKS]
        } else {
            vec![0u8; Self::BANK_SIZE]
        };

        Self {
            bank0,
            bank1,
            active_bank: 0,
        }
    }
}

impl MemoryRead<u16, u8> for Ram {
    #[inline]
    fn read(&self, addr: u16) -> u8 {
        let addr = addr as usize;

        match addr {
            0xC000..=0xCFFF => {
                // Bank 0 (static)
                self.bank0[addr]
            }
            0xD000..=0xDFFF => {
                // Bank 1 (dynamic)
                let bank_offset = self.active_bank as usize * Self::BANK_SIZE;
                self.bank1[bank_offset + addr]
            }
            _ => panic!("Unexpected read from: {}", addr),
        }
    }
}

impl MemoryWrite<u16, u8> for Ram {
    #[inline]
    fn write(&mut self, addr: u16, value: u8) {
        let addr = addr as usize;

        match addr {
            0xC000..=0xCFFF => {
                // Bank 0 (static)
                self.bank0[addr] = value;
            }
            0xD000..=0xDFFF => {
                // Bank 1 (dynamic)
                let bank_offset = self.active_bank as usize * Self::BANK_SIZE;
                self.bank1[bank_offset + addr] = value;
            }
            _ => panic!("Unexpected write from: {}", addr),
        }
    }
}

impl std::fmt::Debug for Ram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Memory")
            .field("bank0", &self.bank0[0])
            .field("bank1", &self.bank1[0])
            .field("active_bank", &self.active_bank)
            .finish()
    }
}

pub enum Vram {
    Unbanked {
        /// Static bank, 8K
        data: [u8; Self::BANK_SIZE],
    },
    Banked {
        /// Two static banks, 8K each
        /// This mode is only present in CGB mode
        data: [u8; Self::BANK_SIZE * 2],
        active_bank: u8,
    }
}

impl Vram {
    const BANK_SIZE: usize = 8 * 1024;

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
        let addr = addr as usize;

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
        let addr = addr as usize;

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
