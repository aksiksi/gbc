use crate::cartridge::{Cartridge, Ram as CartridgeRam, RamSize, Rom, RomSize};
use crate::error::Result;

/// Internal console work RAM
///
/// 0xC000 - 0xCFFF: Bank 0,   4K, static
/// 0xD000 - 0xDFFF: Bank 1-7, 4K, switchable
pub struct Ram {
    /// Static bank, 4K
    bank0: [u8; Self::BANK_SIZE],

    /// Dynamic bank, depends on active bank
    bank1: Vec<u8>,

    /// Currently active bank -- ignored in non-CGB mode
    active_bank: u16,
}

impl Ram {
    const BANK_SIZE: usize = 4 * 1024; // 4K
    const NUM_BANKS: usize = 8;

    pub fn new() -> Self {
        let bank0 = [0u8; Self::BANK_SIZE];
        let bank1 = vec![0u8; Self::BANK_SIZE * Self::NUM_BANKS];

        Self {
            bank0,
            bank1,
            active_bank: 0,
        }
    }

    #[inline]
    pub fn read(&self, addr: u16) -> u8 {
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

    #[inline]
    pub fn write(&mut self, addr: u16, value: u8) {
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

/// 64K memory map for the GBC
#[derive(Debug)]
pub struct MemoryBus {
    /// 0x0000 - 0x7FFF
    rom: Rom,

    /// 0x8000 - 0x9FFF
    vram: Ram,

    /// 0xA000 - 0xBFFF
    ram_switchable: Option<CartridgeRam>,

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
            vram: Ram::new(), // TODO(aksiksi): Define new type for VRAM?
            ram_switchable: cartridge.get_ram()?,
            ram: Ram::new(),
            high_ram: [0u8; 0x80],
            int_enable_reg: 0,
        })
    }

    /// Read  a single from an arbitrary memory address.
    /// This will be converted into a read from the relevant memory section.
    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x7FFF => self.rom.read(addr),
            0x8000..=0x9FFF => self.vram.read(addr),
            0xA000..=0xBFFF => self.ram_switchable.as_ref().unwrap().read(addr),
            0xC000..=0xDFFF => self.ram.read(addr),
            0xFF80..=0xFFFE => {
                let addr = addr as usize - 0xFF80;
                self.high_ram[addr]
            }
            _ => panic!("Unable to read from address: {:?}", addr),
        }
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        match addr {
            0x8000..=0x9FFF => self.vram.write(addr, value),
            0xA000..=0xBFFF => self.ram_switchable.as_mut().unwrap().write(addr, value),
            0xC000..=0xDFFF => self.ram.write(addr, value),
            0xFF80..=0xFFFE => {
                let addr = addr as usize - 0xFF80;
                self.high_ram[addr] = value;
            }
            _ => panic!("Unable to write to address: {:?}", addr),
        }
    }

    pub fn rom(&self) -> &Rom {
        &self.rom
    }
}
