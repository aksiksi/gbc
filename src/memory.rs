use crate::cartridge::{Cartridge, Ram as CartridgeRam, Rom, RomSize, RamSize};
use crate::error::Result;

#[derive(Clone, Copy, Debug)]
pub struct Addr(pub u16);

impl PartialOrd for Addr {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl PartialEq for Addr {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl From<Addr> for usize {
    fn from(a: Addr) -> usize {
        a.0 as usize
    }
}

impl From<Addr> for u16 {
    fn from(a: Addr) -> u16 {
        a.0
    }
}

impl std::ops::Sub for Addr {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        Self(self.0 - other.0)
    }
}

impl std::ops::Add for Addr {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        Self(self.0 + other.0)
    }
}

/// Internal console RAM (8K)
pub struct Ram([u8; Self::RAM_SIZE]);

impl Ram {
    const RAM_SIZE: usize = 8 * 1024;

    pub fn new() -> Self {
        Self([0u8; Self::RAM_SIZE])
    }

    #[inline]
    pub fn read(&self, addr: Addr) -> u8 {
        self.0[addr.0 as usize]
    }

    #[inline]
    pub fn write(&mut self, addr: Addr, value: u8) {
        self.0[addr.0 as usize] = value
    }
}

impl std::fmt::Debug for Ram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Ram")
         .field("size", &self.0.len())
         .finish()
    }
}


/// 64K memory map for the GBC
#[derive(Debug)]
pub struct Memory {
    /// 0x0000 - 0x7FFF
    rom: Rom,

    /// 0x8000 - 0x9FFF
    vram: Ram,

    /// 0xA000 - 0xBFFF
    ram_switchable: CartridgeRam,

    /// 0xC000 - 0xDFFF
    ram: Ram,

    // ..ignored

    /// 0xFF80 - 0xFFFE
    high_ram: [u8; 0x80],

    /// 0xFFFF
    int_enable_reg: u8,
}

impl Memory {
    pub fn from_cartridge(cartridge: &mut Cartridge) -> Result<Self> {
        Ok(Self {
            rom: cartridge.get_rom()?,
            vram: Ram::new(),
            ram_switchable: CartridgeRam::new(RamSize::_2K),
            ram: Ram::new(),
            high_ram: [0u8; 0x80],
            int_enable_reg: 0,
        })
    }

    /// Read  a single from an arbitrary memory address.
    /// This will be converted into a read from the relevant memory section.
    pub fn read(&self, addr: Addr) -> u8 {
        match addr.0 {
            0x0000..=0x7FFF => {
                self.rom.read(addr)
            }
            0x8000..=0x9FFF => {
                let base = Addr(0x8000);
                self.vram.read(addr - base)
            }
            0xA000..=0xBFFF => {
                let base = Addr(0xA000);
                self.ram_switchable.read(addr - base)
            }
            0xC000..=0xDFFF => {
                let base = Addr(0xC000);
                self.ram.read(addr - base)
            }
            0xFF80..=0xFFFE => {
                let addr = usize::from(addr - Addr(0xFF80));
                self.high_ram[addr]
            }
            _ => panic!("abc")
        }
    }

    // pub fn rom(&self) -> &[u8] {
    //     &self.rom.0[..]
    // }

    // pub fn rom_mut(&mut self) -> &mut [u8] {
    //     &mut self.rom.0[..]
    // }

    pub fn ram(&self) -> &[u8] {
        &self.ram.0[..]
    }

    pub fn ram_mut(&mut self) -> &mut [u8] {
        &mut self.ram.0[..]
    }

    pub fn vram(&self) -> &[u8] {
        &self.vram.0[..]
    }

    pub fn vram_mut(&mut self) -> &mut [u8] {
        &mut self.vram.0[..]
    }
}
