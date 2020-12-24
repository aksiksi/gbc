use std::convert::{TryFrom, TryInto};
use std::fs::File;
use std::io::{Read, Seek, SeekFrom};
use std::path::Path;

use crate::error::{Error, Result};
use crate::memory::{MemoryRange, MemoryRead, MemoryWrite};

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
        data: [u8; Self::BANK_SIZE],
        ram_size: RamSize,
    },
    Banked {
        data: Vec<u8>,
        active_bank: u16,
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
        match ram_size {
            // For 2K and 8K RAM sizes, the RAM is unbanked
            RamSize::_2K | RamSize::_8K => {
                let data = [0u8; Self::BANK_SIZE];
                Some(Self::Unbanked { data, ram_size })
            }
            RamSize::NotPresent => {
                // TODO: logging
                eprintln!("No cartridge RAM should be created");
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
    fn update_bank(&mut self, _addr: u16, _value: u8) {
        unimplemented!()
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

impl MemoryRange<u16, u8> for Ram {
    /// Return a slice of memory corresponding to the given range.
    ///
    /// Note: This internally handles both banked and unbanked cartridge RAM.
    #[inline]
    fn range(&self, range: std::ops::RangeFrom<u16>) -> &[u8] {
        let start = (range.start - Self::BASE_ADDR) as usize;

        match self {
            Self::Unbanked { data, .. } => &data[start..],
            Self::Banked {
                data, active_bank, ..
            } => {
                let bank_offset = *active_bank as usize * Self::BANK_SIZE;
                &data[bank_offset + start..]
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

impl std::fmt::Debug for Ram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unbanked { data: _, ram_size } => f
                .debug_struct("CartridgeRam::Unbanked")
                .field("ram_size", &ram_size)
                .finish(),
            Self::Banked {
                data: _,
                active_bank,
                num_banks,
                ram_size,
            } => f
                .debug_struct("CartridgeRam::Banked")
                .field("active_bank", &active_bank)
                .field("num_banks", &num_banks)
                .field("ram_size", &ram_size)
                .finish(),
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
    bank0: [u8; Self::BANK_SIZE],

    /// Dynamic bank, depends on active bank
    bank1: Vec<u8>,

    /// Currently active bank -- ignored for `None` ROMs
    active_bank: u16,

    /// Total number of banks
    num_banks: u16,

    /// ROM size
    rom_size: RomSize,
}

impl Rom {
    pub const BANK_SIZE: usize = 16 * 1024; // 16K
    pub const BASE_ADDR: u16 = 0x0000;
    pub const LAST_ADDR: u16 = 0x7FFF;

    // TODO: Define method(s) for interrupts and jump vectors
    // Jump Vectors in first ROM bank

    pub fn new(rom_size: RomSize) -> Self {
        let bank0 = [0u8; Self::BANK_SIZE];

        let size = usize::from(rom_size);
        let num_banks = size / Self::BANK_SIZE;

        // Construct a `Vec` for bank 1 based on total ROM size (minus bank 0 size)
        let bank1 = vec![0u8; size - Self::BANK_SIZE];

        Self {
            bank0,
            bank1,
            active_bank: 0,
            num_banks: num_banks as u16,
            rom_size,
        }
    }

    pub fn from_file(rom_size: RomSize, rom_file: &mut File) -> Result<Self> {
        // Create a ROM with the required size
        let mut rom = Self::new(rom_size);

        // Seek to beginning of the ROM file
        rom_file.seek(SeekFrom::Start(0))?;

        // Read bank 0 from the ROM file
        rom_file.read_exact(&mut rom.bank0)?;

        // Read bank 1 from the ROM file
        rom_file.read_exact(&mut rom.bank1)?;

        Ok(rom)
    }

    /// Handle a bank change request
    fn update_bank(&mut self, _addr: u16, _value: u8) {
        unimplemented!()
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
                let bank_offset = self.active_bank as usize * Self::BANK_SIZE;
                self.bank1[bank_offset + addr]
            }
            _ => panic!("Unexpected read from: {}", addr),
        }
    }
}

impl MemoryRead<u16, u16> for Rom {
    /// Read the next 2 bytes in ROM as a single u16 (LS byte first)
    #[inline]
    fn read(&self, addr: u16) -> u16 {
        let addr = addr as usize;

        match addr {
            0x0000..=0x3FFF => {
                // Bank 0 (static)
                let lower = self.bank0[addr] as u16;
                let upper = self.bank0[addr + 1] as u16;
                (upper << 8) | lower
            }
            0x4000..=0x7FFF => {
                // Bank 1 (dynamic)
                let bank_offset = self.active_bank as usize * Self::BANK_SIZE;
                let lower = self.bank1[bank_offset + addr] as u16;
                let upper = self.bank1[bank_offset + addr + 1] as u16;
                (upper << 8) | lower
            }
            _ => panic!("Unexpected read from: {}", addr),
        }
    }
}

impl MemoryRange<u16, u8> for Rom {
    /// Returns a slice of bytes from the ROM bank corresponding to
    /// the given `start` address.
    ///
    /// Note that this does not handle cross-bank slices.
    #[inline]
    fn range(&self, range: std::ops::RangeFrom<u16>) -> &[u8] {
        let start = range.start;

        match start {
            0x0000..=0x3FFF => {
                // Bank 0 (static)
                &self.bank0[start as usize..]
            }
            0x4000..=0x7FFF => {
                // Bank 1 (dynamic)
                let bank_offset = self.active_bank as usize * Self::BANK_SIZE;
                &self.bank1[bank_offset + start as usize..]
            }
            _ => panic!("Range start is larger than allowed: {}", start),
        }
    }
}

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
                let bank_offset = self.active_bank as usize * Self::BANK_SIZE;
                self.bank1[bank_offset + addr] = value;
            }
            _ => panic!("Unexpected write to: {}", addr),
        }
    }
}

impl std::fmt::Debug for Rom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Rom")
            .field("bank0", &self.bank0[0])
            .field("bank1", &self.bank1[0])
            .field("active_bank", &self.active_bank)
            .field("num_banks", &self.num_banks)
            .field("rom_size", &self.rom_size)
            .finish()
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
    fn is_none(&self) -> bool {
        use CartridgeType::*;
        match self {
            Rom | RomRam | RomRamBattery => true,
            _ => false,
        }
    }

    fn is_mbc1(&self) -> bool {
        use CartridgeType::*;
        match self {
            Mbc1 | Mbc1Ram | Mbc1RamBattery => true,
            _ => false,
        }
    }

    fn is_mbc2(&self) -> bool {
        use CartridgeType::*;
        match self {
            Mbc2 | Mbc2Battery => true,
            _ => false,
        }
    }

    fn is_mbc3(&self) -> bool {
        use CartridgeType::*;
        match self {
            Mbc3 | Mbc3Ram | Mbc3RamBattery | Mbc3TimerBattery | Mbc3TimerRamBattery => true,
            _ => false,
        }
    }

    fn is_mbc4(&self) -> bool {
        use CartridgeType::*;
        match self {
            Mbc4 | Mbc4Ram | Mbc4RamBattery => true,
            _ => false,
        }
    }

    fn is_mbc5(&self) -> bool {
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

#[derive(Debug)]
pub struct Cartridge {
    /// ROM file
    rom_file: File,

    /// Cartridge header
    /// See: https://gbdev.gg8.se/wiki/articles/The_Cartridge_Header
    header: [u8; Self::HEADER_SIZE],
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

        Ok(Self { rom_file, header })
    }

    /// Raw header data
    pub fn header(&self) -> &[u8] {
        &self.header
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

    /// Builds ROM based on cartridge options and returns it.
    pub fn get_rom(&mut self) -> Result<Rom> {
        let rom_size = self.rom_size()?;
        Rom::from_file(rom_size, &mut self.rom_file)
    }

    /// Builds cartridge RAM based on cartridge options and returns it, if available.
    pub fn get_ram(&self) -> Result<Option<Ram>> {
        let ram_size = self.ram_size()?;
        Ok(Ram::new(ram_size))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_valid_cartridge_header() {
        let sample_rom_path = Path::new(env!("CARGO_MANIFEST_DIR"))
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
