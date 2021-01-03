use std::convert::{TryFrom, TryInto};
use std::fs::File;
use std::io::{Read, Seek, SeekFrom};
use std::path::Path;

use crate::error::{Error, Result};
use crate::memory::{MemoryRead, MemoryWrite};

// Cartridge RAM size
#[derive(Copy, Clone, Debug, PartialEq)]
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
pub enum Ram {
    Unbanked {
        data: Box<[u8; Self::BANK_SIZE]>,
        ram_size: RamSize,
    },
    Banked {
        data: Vec<u8>,
        active_bank: u8,
        num_banks: u16,
        ram_size: RamSize,
    },
}

/// 8 KB switchable/banked external RAM
impl Ram {
    const BANK_SIZE: usize = 8 * 1024; // 8K
    pub const BASE_ADDR: u16 = 0xA000;
    pub const LAST_ADDR: u16 = 0xBFFF;

    pub fn new(ram_size: RamSize) -> Option<Self> {
        // TODO: Handle MBC2 internal RAM (512 bytes)

        match ram_size {
            // For 2K and 8K RAM sizes, the RAM is unbanked
            RamSize::_2K | RamSize::_8K => {
                let data = Box::new([0u8; Self::BANK_SIZE]);
                Some(Self::Unbanked { data, ram_size })
            }
            RamSize::NotPresent => {
                // TODO: logging
                None
            }
            // Otherwise, we have banked RAM
            _ => {
                // Get raw RAM size in bytes
                let size = usize::from(ram_size);
                let data = vec![0u8; size];
                let num_banks = (size / Self::BANK_SIZE) as u16;

                Some(Self::Banked {
                    data,
                    active_bank: 0,
                    num_banks,
                    ram_size,
                })
            }
        }
    }

    /// Handle a bank change request
    pub fn set_bank(&mut self, bank: u8) {
        match self {
            Self::Banked {
                data: _, active_bank, ..
            } => {
                *active_bank = bank;
            }
            _ => (),
        }
    }
}

impl MemoryRead<u16, u8> for Ram {
    /// Read a byte of data from the current active bank
    #[inline]
    fn read(&self, addr: u16) -> u8 {
        let addr = (addr - Self::BASE_ADDR) as usize;
        match &self {
            Self::Unbanked { data, .. } => data[addr],
            Self::Banked {
                data, active_bank, ..
            } => {
                let bank_offset = *active_bank as usize * Self::BANK_SIZE;
                data[bank_offset + addr]
            }
        }
    }
}

impl MemoryWrite<u16, u8> for Ram {
    /// Write a byte of data to the current active bank
    #[inline]
    fn write(&mut self, addr: u16, value: u8) {
        let addr = (addr - Self::BASE_ADDR) as usize;
        match self {
            Self::Unbanked { data, .. } => {
                data[addr] = value;
            }
            Self::Banked {
                data, active_bank, ..
            } => {
                let bank_offset = *active_bank as usize * Self::BANK_SIZE;
                data[bank_offset + addr] = value;
            }
        }
    }
}

/// ROM size
#[derive(Copy, Clone, Debug, PartialEq)]
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

/// ROM
pub struct Rom {
    /// Static bank, 16K
    bank0: Box<[u8; Self::BANK_SIZE]>,

    /// Dynamic bank, depends on active bank
    bank1: Vec<u8>,

    /// Currently active bank -- ignored for `None` ROMs
    pub(crate) active_bank: u16,

    /// Total number of banks
    num_banks: u16,
}

impl Rom {
    pub const BANK_SIZE: usize = 16 * 1024; // 16K
    pub const BASE_ADDR: u16 = 0x0000;
    pub const LAST_ADDR: u16 = 0x7FFF;

    // TODO: Define method(s) for interrupts and jump vectors
    // Jump Vectors in first ROM bank

    pub fn new(rom_size: RomSize) -> Self {
        let bank0 = Box::new([0u8; Self::BANK_SIZE]);

        let size = usize::from(rom_size);
        let num_banks = size / Self::BANK_SIZE;

        // Construct a `Vec` for bank 1 based on total ROM size (minus bank 0 size)
        let bank1 = vec![0u8; size - Self::BANK_SIZE];

        Self {
            bank0,
            bank1,
            active_bank: 0,
            num_banks: num_banks as u16,
        }
    }

    pub fn from_file(rom_size: RomSize, rom_file: &mut File) -> Result<Self> {
        // Create a ROM with the required size
        let mut rom = Self::new(rom_size);

        // Seek to beginning of the ROM file
        rom_file.seek(SeekFrom::Start(0))?;

        // Read bank 0 from the ROM file
        rom_file.read_exact(&mut *rom.bank0)?;

        // Read bank 1 from the ROM file
        rom_file.read_exact(&mut rom.bank1)?;

        Ok(rom)
    }
}

impl MemoryRead<u16, u8> for Rom {
    #[inline]
    fn read(&self, addr: u16) -> u8 {
        let addr = addr as usize;

        match addr {
            0x0000..=0x3FFF => {
                // Bank 0 (static)
                self.bank0[addr]
            }
            0x4000..=0x7FFF => {
                // Bank 1 (dynamic)
                assert!(self.active_bank < self.num_banks);
                let addr = addr - 0x4000;
                let bank_offset = self.active_bank as usize * Self::BANK_SIZE;
                self.bank1[bank_offset + addr]
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
                // Bank 0 (static)
                self.bank0[addr] = value;
            }
            0x4000..=0x7FFF => {
                // Bank 1 (dynamic)
                assert!(self.active_bank < self.num_banks);
                let addr = addr - 0x4000;
                let bank_offset = self.active_bank as usize * Self::BANK_SIZE;
                self.bank1[bank_offset + addr] = value;
            }
            _ => unreachable!("Unexpected read from: {}", addr),
        }
    }
}

/// Cartridge ROM + RAM controller.
pub struct Controller {
    /// Cartridge ROM
    pub rom: Rom,

    /// Cartridge RAM
    pub ram: Option<Ram>,

    /// ROM size
    rom_size: RomSize,

    /// RAM size
    ram_size: RamSize,

    /// Cartridge type
    cartridge_type: CartridgeType,

    /// Bank mode (simple: false, advanced: true)
    banking_mode: bool,
}

impl Controller {
    /// Create a default controller
    pub fn new() -> Self {
        let rom_size = RomSize::_32K;
        let ram_size = RamSize::_8K;

        Self {
            rom: Rom::new(rom_size),
            ram: Ram::new(ram_size),
            rom_size,
            ram_size,
            cartridge_type: CartridgeType::Mbc1,
            banking_mode: false,
        }
    }

    /// Create a controller from a `Cartridge`
    pub fn from_cartridge(mut cartridge: Cartridge) -> Result<Self> {
        // Extract ROM and RAM info from cartridge header
        let cartridge_type = cartridge.cartridge_type()?;
        let rom_size = cartridge.rom_size()?;
        let ram_size = cartridge.ram_size()?;
        let rom = Rom::from_file(rom_size, &mut cartridge.rom_file)?;
        let ram = Ram::new(ram_size);

        Ok(Self {
            rom,
            ram,
            rom_size,
            ram_size,
            cartridge_type,
            banking_mode: false,
        })
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
                println!("RAM enable/disable");
            }
            0x2000..=0x3FFF if self.cartridge_type.is_mbc1() => {
                // MBC1 ROM bank select (5 bit register)
                let value = value & 0b00011111;
                let value = if value == 0 { 1 } else { value };
                self.rom.active_bank = value as u16;
            }
            0x4000..=0x5FFF if self.cartridge_type.is_mbc1() => {
                // MBC1 RAM bank select OR upper 2 bits of ROM bank (2 bit register)
                let value = value & 0x03;

                if usize::from(self.ram_size) >= RamSize::_32K.into() && self.banking_mode {
                    // Switch RAM bank, but only in advanced banking mode
                    self.ram.as_mut().unwrap().set_bank(value);
                } else if usize::from(self.rom_size) >= RomSize::_1M.into() {
                    // Set as upper two bits of ROM bank
                    self.rom.active_bank |= (value as u16) << 5;
                }
            }
            0x6000..=0x7FFF if self.cartridge_type.is_mbc1() => {
                // Banking mode select (1 bit)
                self.banking_mode = value & 0x01 == 1;
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
                    self.rom.active_bank = value as u16;
                }
            }
            0x0000..=0x1FFF if self.cartridge_type.is_mbc3() => {
                // Cartridge RAM and RTC enable/disable
                println!("RAM/RTC enable/disable");
            }
            0x2000..=0x3FFF if self.cartridge_type.is_mbc3() => {
                // MBC3 ROM bank select (7 bit register)
                let value = value & 0b01111111;
                let value = if value == 0 { 1 } else { value };
                self.rom.active_bank = value as u16;
            }
            0x4000..=0x5FFF if self.cartridge_type.is_mbc3() => {
                // MBC3 RAM bank select OR RTC register select (2 bits)
                let value = value & 0x03;

                match value {
                    0x0..=0x3 => self.ram.as_mut().unwrap().set_bank(value),
                    0x8..=0xC => todo!("Enable reading/writing to RTC register"),
                    _ => unreachable!(),
                }
            }
            0x6000..=0x7FFF if self.cartridge_type.is_mbc3() => {
                todo!("Latch clock data, write only")
            }
            0x0000..=0x1FFF if self.cartridge_type.is_mbc5() => {
                // Cartridge RAM enable/disable
                println!("RAM enable/disable");
            }
            0x2000..=0x2FFF if self.cartridge_type.is_mbc5() => {
                // MBC5 ROM bank select (lower 8 bits)
                self.rom.active_bank |= value as u16;
            }
            0x3000..=0x3FFF if self.cartridge_type.is_mbc5() => {
                // MBC5 ROM bank select (9th bit)
                let value = (value & 0x1) as u16;
                self.rom.active_bank |= value << 8;
            }
            0x4000..=0x5FFF if self.cartridge_type.is_mbc5() => {
                // MBC5 RAM bank select (4 bits)
                self.ram.as_mut().unwrap().set_bank(value & 0xF);
            }

            // Forward RAM writes as-is
            Ram::BASE_ADDR..=Ram::LAST_ADDR => self.ram.as_mut().unwrap().write(addr, value),

            _ => unreachable!("Unexpected write to: {}", addr),
        }
    }
}

/// GB/GBC cartridge types
#[derive(Copy, Clone, Debug, PartialEq)]
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
    /// ROM file
    pub rom_file: File,

    /// Cartridge header
    ///
    /// See: https://gbdev.gg8.se/wiki/articles/The_Cartridge_Header
    pub header: [u8; Self::HEADER_SIZE],
}

impl Cartridge {
    const HEADER_SIZE: usize = 0x50; // bytes
    const HEADER_OFFSET: u64 = 0x100;

    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self> {
        let mut rom_file = File::open(&path)?;
        let mut header = [0u8; Self::HEADER_SIZE];

        // Read the header in
        rom_file.seek(SeekFrom::Start(Self::HEADER_OFFSET))?;
        rom_file.read(&mut header)?;

        let cartridge = Self {
            rom_file,
            header,
        };

        Ok(cartridge)
    }

    /// Entry point
    pub fn entry_point(&self) -> [u8; 4] {
        let raw = &self.header[0..=3];
        raw.try_into().unwrap()
    }

    /// Nintendo logo
    pub fn logo(&self) -> &[u8] {
        &self.header[4..=0x33]
    }

    /// Game title (uppercase ASCII)
    pub fn title(&self) -> Result<&str> {
        let raw = &self.header[0x34..0x43];
        Ok(std::str::from_utf8(raw)?)
    }

    pub fn manufacturer_code(&self) -> Result<&str> {
        let raw = &self.header[0x3F..=0x42];
        Ok(std::str::from_utf8(raw)?)
    }

    /// CGB flag
    /// `false`: supports old functions
    /// `true`: CGB only
    pub fn cgb(&self) -> bool {
        let cgb = self.header[0x43];
        match cgb {
            0x80 | 0xC0 => true,
            _ => false,
        }
    }

    pub fn licensee_code(&self) -> Result<&str> {
        let raw = &self.header[0x44..=0x45];
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
        let sgb = self.header[0x46];
        match sgb {
            0x0 => false,
            0x3 => true,
            _ => panic!("Unknown SGB value: {}", sgb),
        }
    }

    /// Cartridge type
    pub fn cartridge_type(&self) -> Result<CartridgeType> {
        CartridgeType::try_from(self.header[0x47])
    }

    /// ROM size
    pub fn rom_size(&self) -> Result<RomSize> {
        RomSize::try_from(self.header[0x48])
    }

    /// RAM size
    pub fn ram_size(&self) -> Result<RamSize> {
        RamSize::try_from(self.header[0x49])
    }

    /// Destination code
    ///
    /// `true` if Japanese, `false` otherwise
    pub fn destination_code(&self) -> bool {
        let code = self.header[0x4A];
        match code {
            0x0 => true,
            0x1 => false,
            _ => panic!("Unknown destination code: {}", code),
        }
    }

    pub fn header_checksum(&self) -> u8 {
        self.header[0x4D]
    }

    /// Returns `true` if computed checksum matches the header checksum
    pub fn verify_header_checksum(&self) -> bool {
        let mut checksum: u8 = 0;
        for b in &self.header[0x34..=0x4C] {
            checksum = checksum.wrapping_sub(*b).wrapping_sub(1);
        }

        checksum == self.header_checksum()
    }

    pub fn global_checksum(&self) -> u16 {
        let upper = self.header[0x4E] as u16;
        let lower = self.header[0x4F] as u16;
        upper << 8 | lower
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_cartridge_header() {
        let sample_rom_path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("..")
            .join("samples")
            .join("pokemon_gold.gbc");

        let cartridge = Cartridge::from_file(&sample_rom_path).unwrap();

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
