use std::convert::TryFrom;

use crate::error::{CartridgeError, Error, Result};
use crate::memory::{MemoryRead, MemoryWrite};
use crate::rtc::Rtc;

// Cartridge RAM size
#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "save", derive(serde::Serialize), derive(serde::Deserialize))]
#[repr(u8)]
pub enum RamSize {
    NotPresent,
    _2K,
    _8K,
    _32K,  // 4 banks, 8K each
    _128K, // 16 banks, 8K each
    _64K,  // 8 banks, 8K each
}

/// Convert from RAM size variant to raw RAM size, in bytes
impl From<RamSize> for usize {
    fn from(s: RamSize) -> usize {
        match s {
            RamSize::_2K => 2 * 1024,
            RamSize::_8K => 8 * 1024,
            RamSize::_32K => 32 * 1024,
            RamSize::_64K => 64 * 1024,
            RamSize::_128K => 128 * 1024,
            RamSize::NotPresent => 0,
        }
    }
}

/// Convert from raw RAM size in bytes to a RAM size variant
impl TryFrom<usize> for RamSize {
    type Error = Error;

    fn try_from(size: usize) -> std::result::Result<Self, Self::Error> {
        match size {
            x if x == 2 * 1024 => Ok(RamSize::_2K),
            x if x == 8 * 1024 => Ok(RamSize::_8K),
            x if x == 32 * 1024 => Ok(RamSize::_32K),
            x if x == 64 * 1024 => Ok(RamSize::_64K),
            x if x == 128 * 1024 => Ok(RamSize::_128K),
            _ => Err(Error::InvalidValue(format!("Invalid RamSize: {}", size))),
        }
    }
}

impl TryFrom<u8> for RamSize {
    type Error = Error;

    fn try_from(val: u8) -> std::result::Result<Self, Self::Error> {
        match val {
            x if x == RamSize::_2K as u8 => Ok(RamSize::_2K),
            x if x == RamSize::_8K as u8 => Ok(RamSize::_8K),
            x if x == RamSize::_32K as u8 => Ok(RamSize::_32K),
            x if x == RamSize::_64K as u8 => Ok(RamSize::_64K),
            x if x == RamSize::_128K as u8 => Ok(RamSize::_128K),
            x if x == RamSize::NotPresent as u8 => Ok(RamSize::NotPresent),
            _ => Err(Error::InvalidValue(format!("Invalid RamSize: {}", val))),
        }
    }
}

/// Cartridge RAM
#[cfg_attr(feature = "save", derive(serde::Serialize), derive(serde::Deserialize))]
pub struct Ram {
    data: Vec<u8>,
    pub(crate) active_bank: u8,
    num_banks: u8,
    ram_size: RamSize,

    /// Dirty flag that is set on write to cartridge RAM
    pub(crate) is_dirty: bool,
}

/// 8 KB switchable/banked external RAM
impl Ram {
    const BANK_SIZE: usize = 8 * 1024; // 8K
    pub const BASE_ADDR: u16 = 0xA000;
    pub const LAST_ADDR: u16 = 0xBFFF;

    /// Create empty RAM
    fn new(ram_size: RamSize) -> Option<Self> {
        match ram_size {
            RamSize::NotPresent => {
                None
            }
            // Otherwise, we have banked RAM
            _ => {
                // Get raw RAM size in bytes
                let size = usize::from(ram_size);
                let data = vec![0u8; size];
                let num_banks = if ram_size == RamSize::_2K {
                    1
                } else {
                    (size / Self::BANK_SIZE) as u8
                };

                Some(Self {
                    data,
                    active_bank: 0,
                    num_banks,
                    ram_size,
                    is_dirty: false,
                })
            }
        }
    }

    /// Create `Ram` from raw bytes
    ///
    /// This function will return an error if the data does not have
    /// a valid RAM size.
    pub fn from_bytes(data: &[u8]) -> Result<Self> {
        let size = data.len();
        let ram_size = RamSize::try_from(size)?;

        let data = data.to_owned();
        let num_banks = if ram_size == RamSize::_2K {
            1
        } else {
            (size / Self::BANK_SIZE) as u8
        };

        Ok(Self {
            data,
            active_bank: 0,
            num_banks,
            ram_size,
            is_dirty: false,
        })
    }

    pub(crate) fn data(&self) -> &[u8] {
        &self.data
    }

    /// Handle a bank change request
    pub(crate) fn set_bank(&mut self, bank: u8) {
        if self.num_banks == 1 {
            log::warn!("Switching bank on unbanked RAM!");
        }

        self.active_bank = bank & (self.num_banks - 1);
    }
}

impl MemoryRead<u16, u8> for Ram {
    /// Read a byte of data from the current active bank
    #[inline]
    fn read(&self, addr: u16) -> u8 {
        let addr = (addr - Self::BASE_ADDR) as usize;
        let bank_offset = self.active_bank as usize * Self::BANK_SIZE;
        self.data[bank_offset + addr]
    }
}

impl MemoryWrite<u16, u8> for Ram {
    /// Write a byte of data to the current active bank
    #[inline]
    fn write(&mut self, addr: u16, value: u8) {
        let addr = (addr - Self::BASE_ADDR) as usize;
        let bank_offset = self.active_bank as usize * Self::BANK_SIZE;
        let index = bank_offset + addr;
        self.data[index] = value;
        self.is_dirty = true;
    }
}

/// ROM size
#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "save", derive(serde::Serialize), derive(serde::Deserialize))]
#[repr(u8)]
pub enum RomSize {
    _32K,
    _64K,
    _128K,
    _256K,
    _512K,
    _1M,
    _2M,
    _4M,
    _8M,
    _1_1M = 0x52, // 1.1 M
    _1_2M,
    _1_5M,
}

/// Convert from ROM size variant to raw size in bytes
impl From<RomSize> for usize {
    fn from(s: RomSize) -> usize {
        match s {
            RomSize::_32K => 2 * Rom::BANK_SIZE,   // 2 x 16K banks
            RomSize::_64K => 4 * Rom::BANK_SIZE,   // 4 x 16K banks
            RomSize::_128K => 8 * Rom::BANK_SIZE,  // 8 x 16K banks
            RomSize::_256K => 16 * Rom::BANK_SIZE, // 8 x 16K banks
            RomSize::_512K => 32 * Rom::BANK_SIZE, // 32 x 16K banks
            RomSize::_1M => 64 * Rom::BANK_SIZE,   // 64 x 16K banks
            RomSize::_1_1M => 72 * Rom::BANK_SIZE, // 72 x 16K banks
            RomSize::_1_2M => 80 * Rom::BANK_SIZE, // 80 x 16K banks
            RomSize::_1_5M => 96 * Rom::BANK_SIZE, // 96 x 16K banks
            RomSize::_2M => 128 * Rom::BANK_SIZE,  // 128 x 16K banks
            RomSize::_4M => 256 * Rom::BANK_SIZE,  // 256 x 16K banks
            RomSize::_8M => 512 * Rom::BANK_SIZE,  // 512 x 16K banks
        }
    }
}

impl TryFrom<u8> for RomSize {
    type Error = Error;

    fn try_from(val: u8) -> std::result::Result<Self, Self::Error> {
        match val {
            x if x == RomSize::_32K as u8 => Ok(RomSize::_32K),
            x if x == RomSize::_64K as u8 => Ok(RomSize::_64K),
            x if x == RomSize::_128K as u8 => Ok(RomSize::_128K),
            x if x == RomSize::_256K as u8 => Ok(RomSize::_256K),
            x if x == RomSize::_512K as u8 => Ok(RomSize::_512K),
            x if x == RomSize::_1M as u8 => Ok(RomSize::_1M),
            x if x == RomSize::_1_1M as u8 => Ok(RomSize::_1_1M),
            x if x == RomSize::_1_2M as u8 => Ok(RomSize::_1_2M),
            x if x == RomSize::_1_5M as u8 => Ok(RomSize::_1_5M),
            x if x == RomSize::_2M as u8 => Ok(RomSize::_2M),
            x if x == RomSize::_4M as u8 => Ok(RomSize::_4M),
            x if x == RomSize::_8M as u8 => Ok(RomSize::_8M),
            _ => Err(Error::InvalidValue(format!("Invalid RomSize: {}", val))),
        }
    }
}

#[cfg_attr(feature = "save", derive(serde::Serialize), derive(serde::Deserialize))]
/// ROM
pub struct Rom {
    /// ROM data for all banks
    ///
    /// Bank 0: static, 16K
    /// Bank 1-7: dynamic
    #[cfg_attr(feature = "save", serde(skip))]
    data: Vec<u8>,

    /// Active bank 0 -- used in large MBC1 carts, otherwise always 0
    pub(crate) active_bank_0: u16,

    /// Currently active bank 1 -- ignored for `None` ROMs
    pub(crate) active_bank_1: u16,

    /// Total number of banks
    num_banks: u16,

    /// Size of ROM
    rom_size: RomSize,
}

impl Rom {
    pub const BANK_SIZE: usize = 16 * 1024; // 16K
    pub const BASE_ADDR: u16 = 0x0000;
    pub const LAST_ADDR: u16 = 0x7FFF;

    pub fn new(rom_size: RomSize) -> Self {
        let size = usize::from(rom_size);
        let num_banks = size / Self::BANK_SIZE;

        Self {
            data: vec![0; size],
            active_bank_0: 0,
            active_bank_1: 1,
            num_banks: num_banks as u16,
            rom_size,
        }
    }

    /// Construct a `Rom` from raw bytes
    pub fn from_bytes(data: &[u8], rom_size: RomSize) -> Self {
        let size = usize::from(rom_size);
        let num_banks = size / Self::BANK_SIZE;
        let data = data.to_owned();

        assert!(size == data.len());

        Self {
            data,
            active_bank_0: 0,
            active_bank_1: 1,
            num_banks: num_banks as u16,
            rom_size,
        }
    }

    /// Load data into this ROM
    pub fn load(&mut self, data: Vec<u8>) {
        let size = usize::from(self.rom_size);
        assert!(size == data.len());
        self.data = data;
    }

    pub fn update_bank_0(&mut self, bank: u16) {
        assert!(bank < self.num_banks);
        self.active_bank_0 = bank;
    }

    pub fn update_bank(&mut self, bank: u16) {
        assert!(bank < self.num_banks);
        self.active_bank_1 = bank;
    }
}

impl MemoryRead<u16, u8> for Rom {
    #[inline]
    fn read(&self, addr: u16) -> u8 {
        let addr = addr as usize;

        match addr {
            0x0000..=0x3FFF => {
                // Bank 0
                let bank_offset = self.active_bank_0 as usize * Self::BANK_SIZE;
                self.data[bank_offset + addr]
            }
            0x4000..=0x7FFF => {
                // Bank 1 (dynamic)
                let addr = addr - 0x4000;
                let bank_offset = self.active_bank_1 as usize * Self::BANK_SIZE;
                self.data[bank_offset + addr]
            }
            _ => unreachable!("Unexpected read from: {}", addr),
        }
    }
}

// This is only used by tests
impl MemoryWrite<u16, u8> for Rom {
    #[inline]
    fn write(&mut self, addr: u16, value: u8) {
        let addr = addr as usize;

        match addr {
            0x0000..=0x3FFF => {
                // Bank 0
                let bank_offset = self.active_bank_0 as usize * Self::BANK_SIZE;
                self.data[bank_offset + addr] = value;
            }
            0x4000..=0x7FFF => {
                // Bank 1 (dynamic)
                let addr = addr - 0x4000;
                let bank_offset = self.active_bank_1 as usize * Self::BANK_SIZE;
                self.data[bank_offset + addr] = value;
            }
            _ => unreachable!("Unexpected read from: {}", addr),
        }
    }
}

pub struct BootRom {
    data: &'static [u8; 256],
}

impl BootRom {
    pub const BASE_ADDR: u16 = 0x0000;
    pub const LAST_ADDR: u16 = 0x00FF;

    pub fn new() -> Self {
        Self {
            data: include_bytes!("dmg_boot.bin"),
        }
    }
}

impl MemoryRead<u16, u8> for BootRom {
    #[inline]
    fn read(&self, addr: u16) -> u8 {
        let addr = addr as usize;
        self.data[addr]
    }
}

#[cfg_attr(feature = "save", derive(serde::Serialize), derive(serde::Deserialize))]
/// Cartridge ROM + RAM controller.
pub struct Controller {
    /// Boot ROM
    #[cfg_attr(feature = "save", serde(skip))]
    pub(crate) boot_rom: Option<BootRom>,

    /// Cartridge ROM
    pub(crate) rom: Rom,

    /// Cartridge RAM
    pub(crate) ram: Option<Ram>,

    /// ROM size
    rom_size: RomSize,

    /// RAM size
    ram_size: RamSize,

    /// Cartridge type
    cartridge_type: CartridgeType,

    /// RTC
    pub(crate) rtc: Option<Rtc>,

    /// If `true`, RTC will be mapped in to cartridge RAM address range
    rtc_active: bool,

    /// Bank mode (simple: false, advanced: true)
    banking_mode: bool,

    /// RAM/RTC enable flag
    ///
    /// If `false`, writes are ignored
    ram_enable: bool,

    /// RAM/ROM bank select register
    ram_rom_bank: u8,
}

impl Controller {
    /// Create a default controller
    pub fn new() -> Self {
        let rom_size = RomSize::_32K;
        let ram_size = RamSize::_8K;

        Self {
            boot_rom: None,
            rom: Rom::new(rom_size),
            ram: Ram::new(ram_size),
            rom_size,
            ram_size,
            cartridge_type: CartridgeType::Mbc1,
            rtc: None,
            rtc_active: false,
            banking_mode: false,
            ram_enable: false,
            ram_rom_bank: 0,
        }
    }

    /// Create a controller from a `Cartridge`
    pub fn from_cartridge(cartridge: Cartridge) -> Result<Self> {
        // Extract ROM and RAM info from cartridge header
        let cartridge_type = cartridge.cartridge_type()?;
        let rom_size = cartridge.rom_size()?;
        let ram_size = cartridge.ram_size()?;
        let rom = Rom::from_bytes(&cartridge.data, rom_size);
        let boot_rom = if cartridge.boot_rom {
            BootRom::new().into()
        } else {
            None
        };

        let ram = Ram::new(ram_size);

        let rtc = if cartridge_type.is_rtc() {
            Rtc::new().into()
        } else {
            None
        };

        Ok(Self {
            boot_rom,
            rom,
            ram,
            rom_size,
            ram_size,
            cartridge_type,
            rtc,
            rtc_active: false,
            banking_mode: false,
            ram_enable: false,
            ram_rom_bank: 0,
        })
    }

    /// Load raw RAM data into this controller
    pub fn load_ram(&mut self, data: &[u8]) -> Result<()> {
        let ram = Ram::from_bytes(data)?;
        self.ram.replace(ram);
        Ok(())
    }

    /// Load raw RTC data into this controller
    pub fn load_rtc(&mut self, data: &[u8]) -> Result<()> {
        let mut rtc = Rtc::from_bytes(data)?;
        rtc.advance();
        self.rtc.replace(rtc);
        Ok(())
    }

    /// Load data into this controller from raw ROM data
    ///
    /// This is used by the save state feature.
    #[cfg(feature = "save")]
    pub(crate) fn load_rom(&mut self, data: Vec<u8>) {
        self.rom.load(data);
    }

    /// Reset this controller
    ///
    /// ROM remains unchanged, while the RAM is reset
    pub(crate) fn reset(&mut self) {
        self.ram = Ram::new(self.ram_size);
    }
}

impl MemoryRead<u16, u8> for Controller {
    #[inline]
    fn read(&self, addr: u16) -> u8 {
        match addr {
            Rom::BASE_ADDR..=Rom::LAST_ADDR => self.rom.read(addr),
            Ram::BASE_ADDR..=Ram::LAST_ADDR => {
                if !self.rtc_active {
                    self.ram.as_ref().unwrap().read(addr)
                } else {
                    self.rtc.as_ref().unwrap().read()
                }
            }
            _ => unreachable!("Invalid read from 0x{:X}", addr),
        }
    }
}

// TODO: Clean this up perhaps?
// RTC functions are missing, and it does not handle ROM bank_0 switching
impl MemoryWrite<u16, u8> for Controller {
    /// Handle ROM and RAM bank changes as well as regular writes to cartridge RAM
    #[inline]
    fn write(&mut self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x1FFF if self.cartridge_type.is_mbc1() => {
                // Cartridge RAM enable/disable
                if value & 0xF == 0xA {
                    self.ram_enable = true;
                } else {
                    self.ram_enable = false;
                }
            }
            0x2000..=0x3FFF if self.cartridge_type.is_mbc1() => {
                // MBC1 ROM bank select (5 bit register)
                let value = value & 0x1F;
                let value = if value == 0 { 1 } else { value };
                self.rom.update_bank(value as u16);
            }
            0x4000..=0x5FFF if self.cartridge_type.is_mbc1() => {
                // MBC1 RAM bank select OR upper 2 bits of ROM bank (2 bit register)
                let value = value & 0x03;

                if usize::from(self.ram_size) == RamSize::_32K.into() {
                    // Switch RAM bank, but only in advanced banking mode
                    self.ram.as_mut().unwrap().set_bank(value);
                } else if usize::from(self.rom_size) >= RomSize::_1M.into() {
                    // For large ROM carts, there are two options:
                    if !self.banking_mode {
                        // 1. Simple banking mode: upper two bits of bank 1
                        let value = self.rom.active_bank_1 | (value as u16) << 5;
                        self.rom.update_bank(value);
                    } else {
                        // 2. Advanced banking mode: select bank 0
                        let bank0 = match value {
                            0 => 0,
                            1 => 0x20,
                            2 => 0x40,
                            3 => 0x60,
                            _ => unreachable!(),
                        };

                        self.rom.update_bank_0(bank0);
                    }
                }

                self.ram_rom_bank = value;
            }
            0x6000..=0x7FFF if self.cartridge_type.is_mbc1() => {
                // MBC1 banking mode select (1 bit)
                let large_ram = usize::from(self.ram_size) >= RamSize::_32K.into();
                let large_rom = usize::from(self.rom_size) >= RomSize::_1M.into();
                if !large_ram && !large_rom {
                    // No effect on small carts
                    return;
                }

                let banking_mode = value & 0x01 == 1;

                if self.ram_enable && large_ram && banking_mode {
                    // Large RAM, switch to previously selected bank immediately
                    self.ram.as_mut().unwrap().set_bank(self.ram_rom_bank);
                }

                self.banking_mode = banking_mode;
            }
            0x0000..=0x3FFF if self.cartridge_type.is_mbc2() => {
                // MBC2 ROM bank select

                // Ignore RAM enable requests
                if value & (1 << 7) == 0 {
                    return;
                }

                let addr_upper = (addr >> 8) as u8;

                if addr_upper & 1 != 0 {
                    // If the lower bit of the upper byte of the address is 1,
                    // we have a valid ROM bank select request
                    let value = value & 0xF;
                    let value = if value == 0 { 1 } else { value };
                    self.rom.update_bank(value as u16);
                }
            }
            0x0000..=0x1FFF if self.cartridge_type.is_mbc3() => {
                // Cartridge RAM and RTC enable/disable
                self.ram_enable = value == 0xA;
            }
            0x2000..=0x3FFF if self.cartridge_type.is_mbc3() => {
                // MBC3 ROM bank select (7 bit register)
                let value = value & 0b01111111;
                let value = if value == 0 { 1 } else { value };
                self.rom.update_bank(value as u16);
            }
            0x4000..=0x5FFF if self.cartridge_type.is_mbc3() => {
                // MBC3 RAM bank select OR RTC register select
                let value = value & 0x0F;

                match value {
                    0x0..=0x3 => {
                        self.ram.as_mut().unwrap().set_bank(value);
                        self.rtc_active = false;
                    }
                    0x8..=0xC => {
                        self.rtc.as_mut().unwrap().select(value);
                        self.rtc_active = true;
                    }
                    _ => unreachable!(),
                }
            }
            0x6000..=0x7FFF if self.cartridge_type.is_mbc3() => {
                if let Some(rtc) = &mut self.rtc {
                    rtc.latch(value);
                }
            }
            0x0000..=0x1FFF if self.cartridge_type.is_mbc5() => {
                // Cartridge RAM enable/disable
                self.ram_enable = value == 0b1010;
            }
            0x2000..=0x2FFF if self.cartridge_type.is_mbc5() => {
                // MBC5 ROM bank select (lower 8 bits)
                let value = (self.rom.active_bank_1 & !0xFF) | value as u16;
                self.rom.update_bank(value);
            }
            0x3000..=0x3FFF if self.cartridge_type.is_mbc5() => {
                // MBC5 ROM bank select (9th bit)
                let value = self.rom.active_bank_1 | (value as u16 & 0x1) << 8;
                self.rom.update_bank(value);
            }
            0x4000..=0x5FFF if self.cartridge_type.is_mbc5() => {
                // MBC5 RAM bank select (4 bits)
                self.ram.as_mut().unwrap().set_bank(value & 0xF);
            }

            Ram::BASE_ADDR..=Ram::LAST_ADDR if !self.rtc_active => {
                // Forward RAM writes as-is
                if self.ram_enable {
                    self.ram.as_mut().unwrap().write(addr, value)
                }
            }
            Ram::BASE_ADDR..=Ram::LAST_ADDR if self.rtc_active => {
                // If RTC is active, writes go to the RTC registers
                if self.ram_enable {
                    self.rtc.as_mut().unwrap().write(value);
                }
            }

            // All other writes are ignored
            _ => (),
        }
    }
}

/// GB/GBC cartridge types
#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "save", derive(serde::Serialize), derive(serde::Deserialize))]
#[repr(u8)]
pub enum CartridgeType {
    Rom,
    Mbc1,
    Mbc1Ram,
    Mbc1RamBattery,
    Mbc2 = 0x5,
    Mbc2Battery,
    RomRam = 0x8,
    RomRamBattery,
    Mmm01 = 0xB,
    Mmm01Ram,
    Mmm01RamBattery,
    Mbc3TimerBattery = 0xF,
    Mbc3TimerRamBattery,
    Mbc3,
    Mbc3Ram,
    Mbc3RamBattery,
    Mbc4 = 0x15,
    Mbc4Ram,
    Mbc4RamBattery,
    Mbc5 = 0x19,
    Mbc5Ram,
    Mbc5RamBattery,
    Mbc5Rumble,
    Mbc5RumbleRam,
    Mbc5RumbleRamBattery,
    PocketCamera = 0xFC,
    BandaiTama5,
    HuC3,
    HuC1RamBattery,
}

impl CartridgeType {
    pub fn is_none(&self) -> bool {
        use CartridgeType::*;
        match self {
            Rom | RomRam | RomRamBattery => true,
            _ => false,
        }
    }

    pub fn is_mbc1(&self) -> bool {
        use CartridgeType::*;
        match self {
            Mbc1 | Mbc1Ram | Mbc1RamBattery => true,
            _ => false,
        }
    }

    pub fn is_mbc2(&self) -> bool {
        use CartridgeType::*;
        match self {
            Mbc2 | Mbc2Battery => true,
            _ => false,
        }
    }

    pub fn is_mbc3(&self) -> bool {
        use CartridgeType::*;
        match self {
            Mbc3 | Mbc3Ram | Mbc3RamBattery | Mbc3TimerBattery | Mbc3TimerRamBattery => true,
            _ => false,
        }
    }

    pub fn is_mbc4(&self) -> bool {
        use CartridgeType::*;
        match self {
            Mbc4 | Mbc4Ram | Mbc4RamBattery => true,
            _ => false,
        }
    }

    pub fn is_mbc5(&self) -> bool {
        use CartridgeType::*;
        match self {
            Mbc5 | Mbc5Ram | Mbc5RamBattery | Mbc5Rumble | Mbc5RumbleRam | Mbc5RumbleRamBattery => {
                true
            }
            _ => false,
        }
    }

    pub fn is_battery_backed(&self) -> bool {
        use CartridgeType::*;
        match self {
            RomRamBattery | Mbc1RamBattery | Mbc3RamBattery | Mbc3TimerRamBattery | Mbc4RamBattery | Mbc5RamBattery | Mbc5RumbleRamBattery => true,
            _ => false,
        }
    }

    pub fn is_rtc(&self) -> bool {
        use CartridgeType::*;
        match self {
            Mbc3TimerBattery | Mbc3TimerRamBattery => true,
            _ => false,
        }
    }
}

impl TryFrom<u8> for CartridgeType {
    type Error = Error;

    fn try_from(val: u8) -> std::result::Result<Self, Self::Error> {
        match val {
            x if x == CartridgeType::Rom as u8 => Ok(CartridgeType::Rom),
            x if x == CartridgeType::Mbc1 as u8 => Ok(CartridgeType::Mbc1),
            x if x == CartridgeType::Mbc1Ram as u8 => Ok(CartridgeType::Mbc1Ram),
            x if x == CartridgeType::Mbc1RamBattery as u8 => Ok(CartridgeType::Mbc1RamBattery),
            x if x == CartridgeType::Mbc2 as u8 => Ok(CartridgeType::Mbc2),
            x if x == CartridgeType::Mbc2Battery as u8 => Ok(CartridgeType::Mbc2Battery),
            x if x == CartridgeType::RomRam as u8 => Ok(CartridgeType::RomRam),
            x if x == CartridgeType::RomRamBattery as u8 => Ok(CartridgeType::RomRamBattery),
            x if x == CartridgeType::Mmm01 as u8 => Ok(CartridgeType::Mmm01),
            x if x == CartridgeType::Mmm01Ram as u8 => Ok(CartridgeType::Mmm01Ram),
            x if x == CartridgeType::Mmm01RamBattery as u8 => Ok(CartridgeType::Mmm01RamBattery),
            x if x == CartridgeType::Mbc3TimerBattery as u8 => Ok(CartridgeType::Mbc3TimerBattery),
            x if x == CartridgeType::Mbc3TimerRamBattery as u8 => {
                Ok(CartridgeType::Mbc3TimerRamBattery)
            }
            x if x == CartridgeType::Mbc3 as u8 => Ok(CartridgeType::Mbc3),
            x if x == CartridgeType::Mbc3Ram as u8 => Ok(CartridgeType::Mbc3Ram),
            x if x == CartridgeType::Mbc3RamBattery as u8 => Ok(CartridgeType::Mbc3RamBattery),
            x if x == CartridgeType::Mbc4 as u8 => Ok(CartridgeType::Mbc4),
            x if x == CartridgeType::Mbc4Ram as u8 => Ok(CartridgeType::Mbc4Ram),
            x if x == CartridgeType::Mbc4RamBattery as u8 => Ok(CartridgeType::Mbc4RamBattery),
            x if x == CartridgeType::Mbc5 as u8 => Ok(CartridgeType::Mbc5),
            x if x == CartridgeType::Mbc5Ram as u8 => Ok(CartridgeType::Mbc5Ram),
            x if x == CartridgeType::Mbc5RamBattery as u8 => Ok(CartridgeType::Mbc5RamBattery),
            x if x == CartridgeType::Mbc5Rumble as u8 => Ok(CartridgeType::Mbc5Rumble),
            x if x == CartridgeType::Mbc5RumbleRam as u8 => Ok(CartridgeType::Mbc5RumbleRam),
            x if x == CartridgeType::Mbc5RumbleRamBattery as u8 => {
                Ok(CartridgeType::Mbc5RumbleRamBattery)
            }
            x if x == CartridgeType::PocketCamera as u8 => Ok(CartridgeType::PocketCamera),
            x if x == CartridgeType::BandaiTama5 as u8 => Ok(CartridgeType::BandaiTama5),
            x if x == CartridgeType::HuC3 as u8 => Ok(CartridgeType::HuC3),
            x if x == CartridgeType::HuC1RamBattery as u8 => Ok(CartridgeType::HuC1RamBattery),
            _ => Err(Error::InvalidValue(format!(
                "Invalid CartridgeType: {}",
                val
            ))),
        }
    }
}

pub struct Cartridge {
    /// Raw cartridge data
    pub(crate) data: Vec<u8>,

    /// If `true`, boot ROM is executed on boot/reset,
    /// prior to loading the game
    pub(crate) boot_rom: bool,
}

impl Cartridge {
    pub fn from_bytes(data: Vec<u8>, boot_rom: bool) -> Self {
        Self {
            data,
            boot_rom,
        }
    }

    /// Tries to figure out if this is a valid cartridge.
    pub fn validate(&self) -> Result<()> {
        if self.title().is_err() {
            return Err(CartridgeError::Title.into());
        }

        if self.manufacturer_code().is_err() {
            return Err(CartridgeError::ManufacturerCode.into());
        }

        if self.licensee_code().is_err() {
            return Err(CartridgeError::LicenseeCode.into());
        }

        if self.cartridge_type().is_err() {
            return Err(CartridgeError::Type.into());
        }

        if self.rom_size().is_err() {
            return Err(CartridgeError::RomSize.into());
        }

        if self.ram_size().is_err() {
            return Err(CartridgeError::RamSize.into());
        }

        Ok(())
    }

    /// Nintendo logo
    pub fn logo(&self) -> &[u8] {
        &self.data[0x104..=0x133]
    }

    /// Game title (uppercase ASCII)
    pub fn title(&self) -> Result<&str> {
        let raw = &self.data[0x134..0x143];
        Ok(std::str::from_utf8(raw)?)
    }

    pub fn manufacturer_code(&self) -> Result<&str> {
        let raw = &self.data[0x13F..=0x142];
        Ok(std::str::from_utf8(raw)?)
    }

    /// CGB flag
    /// `false`: supports old functions
    /// `true`: CGB only
    pub fn cgb(&self) -> bool {
        let cgb = self.data[0x143];
        match cgb {
            0x80 | 0xC0 => true,
            _ => false,
        }
    }

    pub fn licensee_code(&self) -> Result<&str> {
        let raw = &self.data[0x144..=0x145];
        let code: &str = std::str::from_utf8(raw)?;

        Ok(match code {
            "00" => "none",
            "01" => "Nintendo R&D 1",
            "13" => "Electronic Arts",
            "31" => "Nintendo",
            _ => "Other",
        })
    }

    /// SGB flag
    pub fn sgb(&self) -> bool {
        let sgb = self.data[0x146];
        match sgb {
            0x0 => false,
            0x3 => true,
            _ => panic!("Unknown SGB value: {}", sgb),
        }
    }

    /// Cartridge type
    pub fn cartridge_type(&self) -> Result<CartridgeType> {
        CartridgeType::try_from(self.data[0x147])
    }

    /// ROM size
    pub fn rom_size(&self) -> Result<RomSize> {
        RomSize::try_from(self.data[0x148])
    }

    /// RAM size
    pub fn ram_size(&self) -> Result<RamSize> {
        RamSize::try_from(self.data[0x149])
    }

    /// Destination code
    ///
    /// `true` if Japanese, `false` otherwise
    pub fn destination_code(&self) -> bool {
        let code = self.data[0x14A];
        match code {
            0x0 => true,
            0x1 => false,
            _ => panic!("Unknown destination code: {}", code),
        }
    }

    pub fn header_checksum(&self) -> u8 {
        self.data[0x14D]
    }

    /// Returns `true` if computed checksum matches the header checksum
    pub fn verify_header_checksum(&self) -> bool {
        let mut checksum: u8 = 0;
        for b in &self.data[0x134..=0x14C] {
            checksum = checksum.wrapping_sub(*b).wrapping_sub(1);
        }

        checksum == self.header_checksum()
    }

    pub fn global_checksum(&self) -> u16 {
        let upper = self.data[0x14E] as u16;
        let lower = self.data[0x14F] as u16;
        upper << 8 | lower
    }
}

#[cfg(test)]
mod test {
    use std::fs::File;
    use std::io::Read;
    use std::path::Path;

    use super::*;

    #[test]
    fn parse_cartridge_header() {
        let sample_rom_path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("..")
            .join("samples")
            .join("pokemon_gold.gbc");

        let mut file = File::open(sample_rom_path).unwrap();
        let mut data = Vec::new();
        file.read_to_end(&mut data).unwrap();

        let cartridge = Cartridge::from_bytes(data, false);

        // Info: https://datacrystal.romhacking.net/wiki/Pok%C3%A9mon_Gold_and_Silver
        assert_eq!(cartridge.title().unwrap(), "POKEMON_GLDAAUE");
        assert_eq!(
            cartridge.cartridge_type().unwrap(),
            CartridgeType::Mbc3TimerRamBattery
        );
        assert_eq!(cartridge.ram_size().unwrap(), RamSize::_32K);
        assert_eq!(cartridge.rom_size().unwrap(), RomSize::_2M);
        assert_eq!(cartridge.sgb(), true);
        assert_eq!(cartridge.cgb(), true);
        assert_eq!(cartridge.licensee_code().unwrap(), "Nintendo R&D 1");
        assert!(cartridge.verify_header_checksum());
    }
}
