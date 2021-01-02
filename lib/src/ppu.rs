//! Gameboy PPU and LCD handling
//!
//! # Overview
//!
//! ## Background
//!
//! The Gameboy screen buffer holds 256x256 pixels, or 32x32 *tiles*.
//! However, the LCD only displays 160x144 pixels at a time.
//!
//! `SCROLLX` and `SCROLLY` registers contain the location of the background at the
//! upper left of the screen (i.e., it is scrollable). Note that the background
//! wraps around the screen edges.
//!
//! The *Background Tile Map* contains 32 rows of 32 bytes each. Each byte
//! maps to a tile number. Tiles are stored in the *Tile Data Table* in VRAM
//! in one of these two regions:
//!
//! * 0x8000-0x8FFF (tile number = 0 to 255)
//! * 0x8800-0x97FF (tile number = -128 to 127, 0th tile at 0x9000)
//!
//! The region is set/modified using the LCDC register.
//!
//! *BG Display Data*, or the actual content of the background (256 x 256 pixels),
//! is stored at either:
//!
//! * 0x9800-0x9BFF
//! * 0x9C00-0x9FFF
//!
//! The region is set using bit 3 of the LCDC register.
//!
//! The aforementioned scroll registers determine which area of the BG is displayed
//! on the 160x144 LCD.
//!
//! ## Window
//!
//! WX and WY control where the window is displayed on the LCD. Note that the window
//! does not wrap and is not scrollable.
//!
//! ## LCD
//!
//! Each row of 160 pixels takes 108.7 us to display. If you multiply that by 144 rows,
//! the total display time is ~15.66 ms.
//!
//! Once the frame is displayed, the VBLANK period lasts 10 lines, which maps to ~1.09 ms.
//! This is when VRAM data can be accessed.
//!
//! The combination of these two periods nets us ~60 fps.
use crate::memory::{MemoryRead, MemoryWrite};

#[derive(Clone, Copy)]
pub struct Rgba {
    pub red: u8,
    pub green: u8,
    pub blue: u8,
    pub alpha: u8,
}

impl Rgba {
    pub fn white() -> Self {
        Self {
            red: 0xFF,
            green: 0xFF,
            blue: 0xFF,
            alpha: 0xFF,
        }
    }
}

/// Buffer that holds pixel data for a single frame.
///
/// Each pixel is encoded in RGBA8888 format, starting from lowest address:
///
/// * Red (1 byte)
/// * Green (1 byte)
/// * Blue (1 byte)
/// * Alpha (1 byte)
pub struct FrameBuffer {
    pub data: Box<[[Rgba; Self::WIDTH]; Self::HEIGHT]>,
    pub ready: bool,
}

impl FrameBuffer {
    /// 160x144 pixels in a frame
    const WIDTH: usize = 160;
    const HEIGHT: usize = 144;

    pub fn new() -> Self {
        Self {
            data: Box::new([[Rgba::white(); Self::WIDTH]; Self::HEIGHT]),
            ready: false,
        }
    }

    /// Write a single pixel to the buffer.
    ///
    /// `x` is the "column", `y` is the "row".
    pub fn write(&mut self, x: usize, y: usize, pixel: Rgba) {
        self.data[y][x] = pixel;
        println!("Wrote pixel to frame buffer");
    }

    /// Reset this frame buffer
    pub fn reset(&mut self) {
        for col in self.data.iter_mut() {
            for pixel in col.iter_mut() {
                *pixel = Rgba::white();
            }
        }
    }
}

pub struct Vram {
    /// Two static banks, 8K each
    ///
    /// CGB mode
    data: Vec<u8>,
    pub active_bank: u8,
    cgb: bool,
}

impl Vram {
    const BANK_SIZE: usize = 8 * 1024;
    pub const BASE_ADDR: u16 = 0x8000;
    pub const LAST_ADDR: u16 = 0x9FFF;
    pub const BANK_SELECT_ADDR: u16 = 0xFF4F;

    pub fn new(cgb: bool) -> Self {
        let data;

        if cgb {
            data = vec![0u8; Self::BANK_SIZE * 2];
        } else {
            data = vec![0u8; Self::BANK_SIZE];
        }

        Self {
            data,
            active_bank: 0,
            cgb,
        }
    }

    /// Update the active VRAM bank
    pub fn update_bank(&mut self, bank: u8) {
        assert!(self.cgb && bank < 2);
        self.active_bank = bank;
    }

    /// Read a byte from a specific bank
    pub fn read_bank(&self, bank: u8, addr: u16) -> u8 {
        let addr = (addr - Self::BASE_ADDR) as usize;
        let bank_offset = bank as usize * Self::BANK_SIZE;
        self.data[bank_offset + addr]
    }
}

impl MemoryRead<u16, u8> for Vram {
    #[inline]
    fn read(&self, addr: u16) -> u8 {
        let addr = (addr - Self::BASE_ADDR) as usize;
        let bank_offset = self.active_bank as usize * Self::BANK_SIZE;
        self.data[bank_offset + addr]
    }
}

impl MemoryWrite<u16, u8> for Vram {
    #[inline]
    fn write(&mut self, addr: u16, value: u8) {
        let addr = (addr - Self::BASE_ADDR) as usize;
        let bank_offset = self.active_bank as usize * Self::BANK_SIZE;
        self.data[bank_offset + addr] = value;
    }
}

#[derive(Clone, Copy, Default)]
struct LcdControl {
    /// Raw register value
    pub raw: u8,

    pub lcd_display_enable: bool, // bit 7
    pub window_tile_map_select: bool, // bit 6
    pub window_display_enable: bool, // bit 5
    pub bg_tile_data_select: bool, // bit 4
    pub bg_tile_map_select: bool, // bit 3
    pub sprite_size: bool, // bit 2
    pub sprite_enable: bool, // bit 1
    pub bg_priority: bool, // bit 0
}

impl LcdControl {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn set(&mut self, raw: u8) {
        self.raw = raw;

        self.lcd_display_enable = raw & (1 << 7) != 0;
        self.window_tile_map_select = raw & (1 << 6) != 0;
        self.window_display_enable = raw & (1 << 5) != 0;
        self.bg_tile_data_select = raw & (1 << 4) != 0;
        self.bg_tile_map_select = raw & (1 << 3) != 0;
        self.sprite_size = raw & (1 << 2) != 0;
        self.sprite_enable = raw & (1 << 1) != 0;
        self.bg_priority = raw & (1 << 0) != 0;
    }
}

#[derive(Clone, Copy, PartialEq)]
#[repr(u8)]
enum StatMode {
    Hblank = 0,
    Vblank,
    OamScan,
    OamRead,
}

/// LCD STAT register
#[derive(Clone, Copy, PartialEq)]
struct LcdStat {
    /// Raw register value
    pub raw: u8,

    pub ly_enabled: bool, // bit 6
    pub oam_enabled: bool, // bit 5
    pub vblank_enabled: bool, // bit 4
    pub hblank_enabled: bool, // bit 3
    pub coincidence: bool, // bit 2
    pub mode: StatMode, // bits 0-1
}

impl LcdStat {
    pub fn new() -> Self {
        Self {
            raw: 0,
            mode: StatMode::OamScan,
            coincidence: false,
            hblank_enabled: false,
            vblank_enabled: false,
            oam_enabled: false,
            ly_enabled: false,
        }
    }

    pub fn set(&mut self, raw: u8) {
        self.raw = raw;
        self.mode = match raw & 0x3 {
            0 => StatMode::Hblank,
            1 => StatMode::Vblank,
            2 => StatMode::OamScan,
            3 => StatMode::OamRead,
            _ => unreachable!(),
        };

        self.coincidence = self.raw & (1 << 2) != 0;
        self.hblank_enabled = self.raw & (1 << 3) != 0;
        self.vblank_enabled = self.raw & (1 << 4) != 0;
        self.oam_enabled = self.raw & (1 << 5) != 0;
        self.ly_enabled = self.raw & (1 << 6) != 0;
    }
}

pub struct Ppu {
    /// Video RAM (0x8000 - 0x9FFF)
    vram: Vram,

    /// OAM (0xFE00-0xFE9F)
    ///
    /// 40 sprites/objects can be loaded into this RAM. Each 4-byte object
    /// consists of:
    ///
    /// 1. y-coordinate (8 bits)
    /// 2. x-coordinate (8 bits)
    /// 3. CHR code (8 bits)
    /// 4. BG and OBJ display priority (1 bit)
    /// 5. Vertical flip (1 bit)
    /// 6. Horizontal flip (1 bit)
    /// 6. DMG mode palette (1 bit)
    /// 7. Character blank (1 bit)
    /// 8. Color palette (3 bits)
    oam: [u8; 160],

    /// LCD control register (0xFF40)
    lcdc: LcdControl,

    /// LCD status register (0xFF41)
    stat: LcdStat,

    /// Background position registers (0xFF42, 0xFF43)
    ///
    /// Range: 0x00-0xFF (256 x 256 pixels)
    scy: u8,
    scx: u8,

    /// LCD line registers (0xFF44, 0xFF45)
    ly: u8,
    lyc: u8,

    /// Monochrome palette registers (0xFF47-0xFF49)
    bgp: u8,
    obp0: u8,
    obp1: u8,

    /// Window position registers (0xFF4A, 0xFF4B)
    ///
    /// Ranges: 0 <= WY <= 143 and 7 <= WX <= 166
    wy: u8,
    wx: u8,

    /// Color palette index registers (0xFF68-0xFF6B)
    bcps: u8,
    ocps: u8,

    /// Color palette RAM:
    ///
    /// * 8 BG palettes     x 4 bytes per palette = 32 bytes
    /// * 8 sprite palettes x 4 bytes per palette = 32 bytes
    ///
    /// Writes and reads to/from BCPD/OCPD go directly to this RAM area,
    /// based on the current index in BCPS/OCPS.
    palette_ram: [u8; 64],

    /// Buffer for the current frame
    frame_buffer: FrameBuffer,

    /// Interrupt enable flags
    oam_enabled: bool,
    vblank_enabled: bool,
    hblank_enabled: bool,
    ly_enabled: bool,

    /// Register write stack
    ///
    /// Some of the registers, e.g., SCY and SCX, can be written to mid-scanline,
    /// but the write does not go into affect until the end of the scanline. This
    /// stack keeps track of these writes and flushes them on a scanline change.
    write_stack: Vec<(u16, u8)>,

    /// Last cycle processed
    cycle: u32,
}

impl Ppu {
    // Register addresses
    const LCDC_ADDR: u16 = 0xFF40;
    const STAT_ADDR: u16 = 0xFF41;
    const SCY_ADDR: u16 = 0xFF42;
    const SCX_ADDR: u16 = 0xFF43;
    const LY_ADDR: u16 = 0xFF44;
    const LYC_ADDR: u16 = 0xFF45;
    const WY_ADDR: u16 = 0xFF4A;
    const WX_ADDR: u16 = 0xFF4B;

    const DOTS_PER_LINE: u32 = 456;
    const VBLANK_START_LINE: u8 = 144;

    pub fn new(cgb: bool) -> Self {
        Self {
            vram: Vram::new(cgb),
            oam: [0u8; 160],
            lcdc: LcdControl::new(),
            stat: LcdStat::new(),
            scy: 0,
            scx: 0,
            ly: 0,
            lyc: 0,
            bgp: 0,
            obp0: 0,
            obp1: 0,
            wy: 0,
            wx: 0,
            bcps: 0,
            ocps: 0,
            palette_ram: [0u8; 64],
            frame_buffer: FrameBuffer::new(),
            oam_enabled: false,
            vblank_enabled: false,
            hblank_enabled: false,
            ly_enabled: false,
            write_stack: Vec::with_capacity(5), // 5 registers use this
            cycle: 0,
        }
    }

    /// Update the PPU status registers based on current cycle and CPU speed.
    ///
    /// This function is called once per CPU step from the main frame loop. The
    /// function is called *after* the CPU step completes. The value passed in
    /// for `cycle` includes the time spent by the CPU.
    ///
    /// The returned tuple contains two interrupt flags: (Vblank, LcdStat)
    pub fn step(&mut self, cycle: u32, speed: bool) -> (bool, bool) {
        self.cycle = cycle;

        // Figure out the current dot and scan line
        let dot = if speed {
            // If we are in double-speed mode, we get a dot every 2 cycles
            cycle / 2
        } else {
            cycle
        };

        let line = (dot / Self::DOTS_PER_LINE) as u8;

        // Update the internal PPU status
        // The function returns which interrupts need to be triggered
        let interrupts = self.update_status(dot, line);

        // Render data to the frame, if applicable
        self.render();

        interrupts
    }

    /// Render pixel data to the internal frame buffer
    fn render(&mut self) {
        // If the display is currently disabled, no rendering needs to be done
        if !self.lcdc.lcd_display_enable {
            return;
        }

        match self.stat.mode {
            StatMode::Hblank => {
                // If we are in a HBLANK, do one of the following:
                if self.ly <= 143 {
                    // 1. Render a scanline worth of BG and sprites to the frame buffer
                    self.render_scanline();
                } else {
                    // 2. Indicate that the current frame is ready
                    self.frame_buffer.ready = true;
                }
            }
            _ => ()
        }
    }

    /// Render a single scanline worth of pixel data to the frame buffer
    ///
    /// This is split into rendering BG/window tiles and rendering sprites. Note
    /// that sprites are more often layered on top of the BG.
    fn render_scanline(&mut self) {
        if self.lcdc.bg_priority {
            self.render_tiles();
        }

        if self.lcdc.sprite_enable {
            self.render_sprites();
        }
    }

    /// Render a scanline worth of BG/window tiles.
    fn render_tiles(&mut self) {
        let scanline = self.ly;

        // Select base address for BG tile map based on LCDC register
        let tile_map_base: u16 = if !self.lcdc.bg_tile_map_select {
            0x9800
        } else {
            0x9C00
        };

        // Select base address for BG tile data based on LCDC register
        let tile_data_base: u16 = if !self.lcdc.bg_tile_data_select {
            0x8000
        } else {
            0x8800
        };

        // For each pixel in the current scanline, we need to do the following:
        //
        // 1. Figure out the x and y pixel positions in the BG (256x256 pixels, 32x32 tiles).
        // 2. Using the BG pixel positions, determine the BG tile map x and y.
        // 3. Using the tile coordinates, compute an index into the BG tile map.
        // 4. Get the tile data index (bank 0) and tile attributes (bank 1) from VRAM.
        // 5. Extract tile attributes.
        // 6. Read the actual tile data (16 bytes) from the relevant VRAM bank.
        // 7. Compute the pixel's index within the 8x8 pixel tile, and adjust if flips are
        //    present in this tile.
        // 8. Compute the pixel's color palette index (2 bits).
        // 9. Finally, using the tile's palette number and the index from (8),
        //    compute the RGB value for the pixel.
        for pixel in 0u8..160 {
            // (1)
            let bg_pixel_x = pixel.wrapping_add(self.scx);
            let bg_pixel_y = scanline.wrapping_add(self.scy);

            // (2) and (3)
            let bg_tile_x = bg_pixel_x % 32;
            let bg_tile_y = bg_pixel_y % 32;
            let tile_map_index = (bg_tile_y * 32 + bg_tile_x) as u16;

            // (4)
            let tile_data_index = self.vram.read_bank(0, tile_map_base + tile_map_index) as u16;
            let tile_data_attr = self.vram.read_bank(1, tile_map_base + tile_map_index);

            // (5)
            let tile_palette_num = tile_data_attr & 0x07; // bits 0-2
            let tile_data_bank = (tile_data_attr & (1 << 3)) >> 3; // bit 3
            let horizontal_flip = (tile_data_attr & (1 << 5)) != 0; // bit 5
            let vertical_flip = (tile_data_attr & (1 << 6)) != 0; // bit 6
            let _bg_priority = (tile_data_attr & (1 << 7)) != 0; // bit 7

            // (6)
            let mut tile_data = [0u8; 16];
            for i in 0..tile_data.len() as u16 {
                tile_data[i as usize] = self.vram.read_bank(tile_data_bank,
                                                            tile_data_base + tile_data_index + i);
            }

            // (7)
            let mut tile_pixel_x = bg_pixel_x - bg_tile_x * 8;
            let mut tile_pixel_y = bg_pixel_y - bg_tile_y * 8;

            // Handle flipped pixels
            if horizontal_flip {
                tile_pixel_x = 7 - tile_pixel_x;
            }

            if vertical_flip {
                tile_pixel_y = 7 - tile_pixel_y;
            }

            // (8)
            //
            // The y position of the pixel maps to the "line" (2 bytes) in the tile data
            // The x position maps to the bit we need to check in the upper and lower nibbles of the line
            // Note that the x position is *inverted*: e.g., the leftmost pixel is tracked in bit 7 of each nibble
            let line_idx = tile_pixel_y as usize * 2;
            let lower = tile_data[line_idx];
            let upper = tile_data[line_idx + 1];
            let pixel_pos = 7 - tile_pixel_x; // This corrects the inversion noted above

            let lower_bit = (lower & 1 << pixel_pos) >> pixel_pos;
            let upper_bit = (upper & 1 << pixel_pos) >> pixel_pos;
            let color_index = upper_bit << 1 | lower_bit;

            // (9)
            let palette_index = (tile_palette_num * 4 + color_index) as usize;
            let pixel_color = (self.palette_ram[palette_index + 1] as u16) << 8 |
                              self.palette_ram[palette_index] as u16;

            let red = (pixel_color & 0x001F) as u8;
            let green = ((pixel_color & 0x03E0) >> 5) as u8;
            let blue = ((pixel_color & 0x7F00) >> 10) as u8;
            let alpha = 0xFF; // BG is always opaque

            // Finally, push the pixel to the frame buffer
            let pixel_data = Rgba {
                red,
                blue,
                green,
                alpha,
            };

            self.frame_buffer.write(pixel as usize, scanline as usize, pixel_data);
        }
    }

    fn render_sprites(&mut self) {
        todo!()
    }

    fn update_status(&mut self, dot: u32, line: u8) -> (bool, bool) {
        // If we have a scanline change, flush the write stack to affected registers
        if line != self.ly {
            while let Some((addr, value)) = self.write_stack.pop() {
                self.write(addr, value);
            }
        }

        // Set LY to current scan line
        self.ly = line;

        // Compute LY conincidence
        let prev_ly_coincidence = self.stat.coincidence;
        let ly_coincidence = self.ly == self.lyc;

        // Figure out which stat mode we are in based on line and dot
        let prev_mode = self.stat.mode;
        let mode = if line < Self::VBLANK_START_LINE {
            let dot = dot % Self::DOTS_PER_LINE;
            match dot {
                0..=79 => StatMode::OamScan,
                80..=329 => StatMode::OamRead,
                _ => StatMode::Hblank,
            }
        } else {
            // VBLANK
            StatMode::Vblank
        };

        let mut stat = mode as u8;
        if ly_coincidence {
            stat |= 1 << 2;
        }

        // Update STAT register
        self.stat.set(stat);

        // VBLANK interrupt is fired if the mode has changed and the current mode is VBLANK.
        //
        // Note: This is fired once per frame.
        let vblank_interrupt = prev_mode != mode && mode == StatMode::Vblank;

        // If any of the STAT interrupt conditions are met, fire an interrupt.
        //
        // 1. LY coincidence interrupt is enabled and changed from false to true
        // 2. STAT mode interrupt is enabled and has changed
        let stat_interrupt = (self.ly_enabled && !prev_ly_coincidence && ly_coincidence) || {
            prev_mode != mode && match mode {
                StatMode::Hblank => self.hblank_enabled,
                StatMode::Vblank => self.vblank_enabled,
                StatMode::OamScan | StatMode::OamRead => self.oam_enabled,
            }
        };

        (vblank_interrupt, stat_interrupt)
    }

    /// Compute the current palette RAM index based on value of BCPS
    #[inline]
    fn palette_index(&self, sprite: bool) -> u8 {
        // Extract palette index information from BCPS
        let byte = self.bcps & 0x1;
        let palette_data_num = (self.bcps & (1 << 2 | 1 << 1)) >> 1;
        let palette_num = (self.bcps & (1 << 5 | 1 << 4 | 1 << 3)) >> 3;

        // Compute the index
        let mut index = palette_num * 4 + palette_data_num + byte;
        if sprite {
            // Offset palette RAM index by 32 bytes in case of a sprite
            index += 32;
        }

        index
    }

    /// Write a single byte of data to palette RAM.
    ///
    /// This handles writes to BCPD (0xFF69).
    fn palette_write(&mut self, value: u8, sprite: bool) {
        let auto_increment = self.bcps & (1 << 7) != 0;
        let index = self.palette_index(sprite);

        // Write the byte to palette RAM
        self.palette_ram[index as usize] = value;

        if auto_increment {
            // Auto-increment BCPS/OCPS on write to BCPD (wrapping at bit 5)
            let reg = if !sprite {
                &mut self.bcps
            } else {
                &mut self.ocps
            };

            let mut index = (*reg & 0x3F) + 1;

            if index > 0x3F {
                index = 0x00;
            }

            // Replace lower 5 bits of BCPS/OCPS with new index
            *reg = (*reg & !0x3F) | index;
        }
    }

    /// Read a single byte from palette RAM.
    fn palette_read(&self, sprite: bool) -> u8 {
        let index = self.palette_index(sprite);
        self.palette_ram[index as usize]
    }

    /// Returns `true` if VRAM is locked to CPU
    fn vram_locked(&self) -> bool {
        match self.stat.mode {
            // Locked during OAM read (mode 3)
            StatMode::OamRead => true,
            _ => false,
        }
    }

    /// Returns `true` if OAM is locked to CPU
    fn oam_locked(&self) -> bool {
        match self.stat.mode {
            // Locked during Scan and Read
            StatMode::OamScan | StatMode::OamRead => true,
            StatMode::Vblank | StatMode::Hblank => false,
        }
    }

    pub fn vram(&self) -> &Vram {
        &self.vram
    }

    pub fn vram_mut(&mut self) -> &mut Vram {
        &mut self.vram
    }

    /// Get a reference to the frame buffer
    pub fn frame_buffer(&self) -> &FrameBuffer {
        &self.frame_buffer
    }
}

impl MemoryRead<u16, u8> for Ppu {
    #[inline]
    fn read(&self, addr: u16) -> u8 {
        match addr {
            Vram::BASE_ADDR..=Vram::LAST_ADDR => {
                if !self.vram_locked() {
                    self.vram.read(addr)
                } else {
                    // PPU returns 0xFF if VRAM is locked
                    0xFF
                }
            }
            0xFE00..=0xFE9F => {
                let idx = (addr as usize) - 0xFE00;
                self.oam[idx]
            }
            Self::LCDC_ADDR => self.lcdc.raw,
            Self::STAT_ADDR => self.stat.raw,
            Self::SCY_ADDR => self.scy,
            Self::SCX_ADDR => self.scx,
            Self::LY_ADDR => self.ly,
            Self::LYC_ADDR => self.lyc,
            0xFF47 => self.bgp,
            0xFF48 => self.obp0,
            0xFF49 => self.obp1,
            Self::WY_ADDR => self.wy,
            Self::WX_ADDR => self.wx,
            0xFF68 => self.bcps,
            0xFF69 => self.palette_read(false),
            0xFF6A => self.ocps,
            0xFF6B => self.palette_read(true),
            _ => panic!("Unexpected read from addr {}", addr),
        }
    }
}

impl MemoryWrite<u16, u8> for Ppu {
    #[inline]
    fn write(&mut self, addr: u16, value: u8) {
        match addr {
            Vram::BASE_ADDR..=Vram::LAST_ADDR => {
                if !self.vram_locked() {
                    self.vram.write(addr, value);
                }
            }
            0xFE00..=0xFE9F => {
                if !self.oam_locked() {
                    let idx = (addr as usize) - 0xFE00;
                    self.oam[idx] = value;
                }
            }
            Self::LCDC_ADDR => self.lcdc.set(value),
            Self::STAT_ADDR => {
                self.stat.set(value);
            }
            Self::SCY_ADDR | Self::SCX_ADDR | Self::LYC_ADDR | Self::WY_ADDR | Self::WX_ADDR => {
                // Latch these writes until the next scanline change
                self.write_stack.push((addr, value));
            }
            Self::LY_ADDR => {
                if self.ly & (1 << 7) != 0 {
                    // If bit 7 == 1 and it is getting reset, clear out LY entirely
                    if value & (1 << 7) == 0 {
                        self.ly = 0;
                    }
                } else {
                    // If bit 7 == 0, accept all writes
                    self.ly = value;
                }
            }
            0xFF47 => self.bgp = value,
            0xFF48 => self.obp0 = value,
            0xFF49 => self.obp1 = value,
            0xFF68 => self.bcps = value,
            0xFF69 => self.palette_write(value, false),
            0xFF6A => self.ocps = value,
            0xFF6B => self.palette_write(value, true),
            _ => panic!("Unexpected write to addr {} value {}", addr, value),
        }
    }
}
