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
use std::collections::VecDeque;

use crate::cpu::Interrupt;
use crate::memory::{MemoryRead, MemoryWrite};

pub const LCD_WIDTH: usize = 160;
pub const LCD_HEIGHT: usize = 144;

#[derive(Clone, Copy, Debug)]
pub struct GameboyRgba {
    pub red: u8,
    pub green: u8,
    pub blue: u8,
    pub alpha: u8,
}

impl GameboyRgba {
    pub fn white() -> Self {
        Self {
            red: 0xFF,
            green: 0xFF,
            blue: 0xFF,
            alpha: 0xFF,
        }
    }

    /// Scale this color to regular RGB (0-255).
    ///
    /// Note that Gameboy colors have a range of 0-31 (5 bits).
    pub fn scale_to_rgb(&mut self) {
        self.red = ((self.red as u32 * 255) / 31) as u8;
        self.blue = ((self.blue as u32 * 255) / 31) as u8;
        self.green = ((self.green as u32 * 255) / 31) as u8;
        self.alpha = ((self.alpha as u32 * 255) / 31) as u8;
    }
}

// Basic DMG/monochrome color palette
static DMG_PALETTE: [GameboyRgba; 4] = [
    GameboyRgba {
        red: 175, green: 203, blue: 70, alpha: 255
    },
    GameboyRgba {
        red: 121, green: 170, blue: 109, alpha: 255
    },
    GameboyRgba {
        red: 34, green: 111, blue: 95, alpha: 255
    },
    GameboyRgba {
        red: 8, green: 41, blue: 85, alpha: 255
    },
];

/// Buffer that holds pixel data for a single frame.
pub struct FrameBuffer {
    pub data: Box<[GameboyRgba; LCD_WIDTH * LCD_HEIGHT]>,
    pub ready: bool,
}

impl FrameBuffer {
    pub fn new() -> Self {
        Self {
            data: Box::new([GameboyRgba::white(); LCD_WIDTH * LCD_HEIGHT]),
            ready: false,
        }
    }

    /// Write a single pixel to the buffer.
    ///
    /// `x` is the "column", `y` is the "row".
    pub fn write(&mut self, x: usize, y: usize, pixel: GameboyRgba) {
        self.data[y * LCD_WIDTH + x] = pixel;
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

#[derive(Clone, Copy)]
struct LcdControl {
    /// Raw register value
    pub raw: u8,
}

impl LcdControl {
    pub fn new() -> Self {
        Self {
            raw: 0x91,
        }
    }

    pub fn set(&mut self, raw: u8) {
        self.raw = raw;
    }

    pub fn lcd_display_enable(&self) -> bool {
        self.raw & (1 << 7) != 0
    }

    #[allow(unused)]
    pub fn window_tile_map_select(&self) -> bool {
        self.raw & (1 << 6) != 0
    }

    #[allow(unused)]
    pub fn window_display_enable(&self) -> bool {
        self.raw & (1 << 5) != 0
    }

    pub fn bg_tile_data_select(&self) -> bool {
        self.raw & (1 << 4) == 0
    }

    pub fn bg_tile_map_select(&self) -> bool {
        self.raw & (1 << 3) != 0
    }

    pub fn sprite_size(&self) -> bool {
        self.raw & (1 << 2) != 0
    }

    pub fn sprite_enable(&self) -> bool {
        self.raw & (1 << 1) != 0
    }

    pub fn bg_priority(&self) -> bool {
        self.raw & (1 << 0) != 0
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

/// Contains raw data for a single sprite in OAM
///
/// Note: y and x coordinates need to be converted
struct Sprite {
    pub y: u8,
    pub x: u8,
    pub tile_number: u8,
    pub attr: u8,
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

    /// OAM DMA (0xFF46)
    oam_dma: u8,
    pub oam_dma_active: bool,

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

    /// BG color palette RAM:
    ///
    /// * 8 BG palettes     x 4 colors x 2 bytes per color = 64 bytes
    ///
    /// Writes and reads to and from BCPD go directly to this RAM area,
    /// based on the current index in BCPS.
    bg_palette_ram: [u8; 64],

    /// Same as above, but for sprites
    sprite_palette_ram: [u8; 64],

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
    write_stack: VecDeque<(u16, u8)>,

    /// Last cycle processed
    cycle: u32,

    /// If `true`, operate in CGB mode
    cgb: bool,
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
            oam_dma: 0,
            oam_dma_active: false,
            bgp: 0xFC,
            obp0: 0xFF,
            obp1: 0xFF,
            wy: 0,
            wx: 0,
            bcps: 0,
            ocps: 0,
            bg_palette_ram: [0xFF; 64],
            sprite_palette_ram: [0xFF; 64],
            frame_buffer: FrameBuffer::new(),
            oam_enabled: false,
            vblank_enabled: false,
            hblank_enabled: false,
            ly_enabled: false,
            write_stack: VecDeque::with_capacity(5), // 5 registers use this
            cycle: 0,
            cgb,
        }
    }

    /// Update the PPU status registers based on current cycle and CPU speed.
    ///
    /// This function is called once per CPU step from the main frame loop. The
    /// function is called *after* the CPU step completes. The value passed in
    /// for `cycle` includes the time spent by the CPU.
    ///
    /// This method returns a `Vec` of interrupts that need to be triggered, if
    /// any.
    pub fn step(&mut self, cycle: u32, speed: bool, interrupts: &mut Vec<Interrupt>) {
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
        //
        // This also returns which interrupts need to be triggered
        let stat_mode_change = self.update_status(dot, line, interrupts);

        // Render data to the frame
        self.render(stat_mode_change);
    }

    /// Returns: (stat_mode_change, vblank_interrupt, stat_interrupt)
    fn update_status(&mut self, dot: u32, line: u8, interrupts: &mut Vec<Interrupt>) -> bool {
        // If we have a scanline change, flush the write stack to affected registers
        if line != self.ly {
            while let Some((addr, value)) = self.write_stack.pop_back() {
                match addr {
                    Self::SCY_ADDR => self.scy = value,
                    Self::SCX_ADDR => self.scx = value,
                    Self::LYC_ADDR => self.lyc = value,
                    Self::WY_ADDR => self.wy = value,
                    Self::WX_ADDR => self.wy = value,
                    _ => unreachable!("Unexpected address on stack: {}", addr),
                }
            }
        }

        // Set LY to current scan line
        self.ly = line;

        // Compute LY conincidence
        let prev_ly_coincidence = self.stat.coincidence;
        let ly_coincidence = self.ly == self.lyc;

        // Figure out which stat mode we are in based on line and dot.
        //
        // Recall that we have 456 dots in a line.
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

        let stat_mode_change = prev_mode != mode;

        // VBLANK interrupt is fired if the mode has changed and the current mode is VBLANK.
        //
        // Note: This is fired once per frame.
        if stat_mode_change && mode == StatMode::Vblank {
            interrupts.push(Interrupt::Vblank);
        }

        // If any of the STAT interrupt conditions are met, fire an interrupt.
        //
        // 1. LY coincidence interrupt is enabled and changed from false to true
        // 2. STAT mode interrupt is enabled and has changed
        let stat_interrupt = (self.ly_enabled && !prev_ly_coincidence && ly_coincidence) || {
            stat_mode_change && match mode {
                StatMode::Hblank => self.hblank_enabled,
                StatMode::Vblank => self.vblank_enabled,
                StatMode::OamScan | StatMode::OamRead => self.oam_enabled,
            }
        };

        if stat_interrupt {
            interrupts.push(Interrupt::LcdStat);
        }

        stat_mode_change
    }

    /// Render pixel data to the internal frame buffer
    fn render(&mut self, stat_mode_change: bool) {
        // If the display is currently disabled, no rendering needs to be done
        if !self.lcdc.lcd_display_enable() {
            return;
        }

        // If we are not at the start of a stat mode, we have nothing to do
        if !stat_mode_change {
            return;
        }

        match self.stat.mode {
            StatMode::Hblank if self.ly <= 143 => {
                // If we are at the start of HBLANK, render a scanline worth
                // of BG and sprites to the frame buffer
                self.render_scanline();
            }
            StatMode::Vblank if self.ly == 144 => {
                // At the start of VBLANK, indicate that the current frame is ready
                self.frame_buffer.ready = true;
            }
            _ => ()
        }
    }

    /// Find all visible sprites on this scanline.
    ///
    /// Sprites will be sorted according to required priority:
    ///
    /// CGB mode: in OAM order
    /// DMG mode: sorted by x-pos
    fn find_visible_sprites(&self, scanline: u8) -> Vec<Sprite> {
        let size = if self.lcdc.sprite_size() {
            16
        } else {
            8
        };

        let mut sprites = Vec::new();

        for chunk in self.oam.chunks_exact(4) {
            let y = chunk[0];
            let x = chunk[1];
            let tile_number = chunk[2];
            let attr = chunk[3];

            let y_pos = y.wrapping_sub(16);
            let visible = scanline >= y_pos && scanline < y_pos.wrapping_add(size);

            // We can only have 10 sprites on a single scanline
            if visible && sprites.len() < 10 {
                sprites.push(Sprite {
                    y,
                    x,
                    tile_number,
                    attr,
                });
            }
        }

        // Sort according to x-pos
        if !self.cgb {
            sprites.sort_by(|a, b| {
                a.x.cmp(&b.x)
            });
        }

        sprites
    }

    /// Render a single scanline worth of pixel data to the frame buffer
    ///
    /// This is split into rendering BG/window tiles and rendering sprites. Note
    /// that sprites are more often layered on top of the BG.
    fn render_scanline(&mut self) {
        let scanline = self.ly;

        // Select base address for BG tile map based on LCDC register
        let bg_tile_map_base: u16 = if !self.lcdc.bg_tile_map_select() {
            0x9800
        } else {
            0x9C00
        };

        // Select base address for window tile map based on LCDC register
        let window_tile_map_base: u16 = if !self.lcdc.window_tile_map_select() {
            0x9800
        } else {
            0x9C00
        };

        let sprites = self.find_visible_sprites(scanline);

        for pixel in 0..LCD_WIDTH as u8 {
            let mut pixel_data = None;
            let bg_priority;
            let bg_color_index;

            // Fetch pixel data for the BG or window, depending on which is currently active
            if self.lcdc.bg_priority() {
                let window_display =
                    self.lcdc.window_display_enable() &&
                    scanline >= self.wy &&
                    pixel >= self.wx.wrapping_sub(7);

                let (data, priority, color_index) = if !window_display {
                    let bg_pixel_x = pixel.wrapping_add(self.scx);
                    let bg_pixel_y = scanline.wrapping_add(self.scy);
                    self.fetch_bg_pixel_data(bg_pixel_x, bg_pixel_y, bg_tile_map_base)
                } else {
                    self.fetch_bg_pixel_data(pixel, scanline, window_tile_map_base)
                };

                pixel_data = Some(data);
                bg_priority = priority;
                bg_color_index = color_index;
            } else {
                if !self.cgb {
                    // On DMG, reset the BG to white
                    pixel_data = Some(DMG_PALETTE[0]);
                }

                bg_priority = false;
                bg_color_index = 0;
            }

            if self.lcdc.sprite_enable() {
                // Fetch sprite pixel data
                let result = self.fetch_sprite_pixel_data(&sprites, pixel, scanline);
                if let Some((data, priority, color_index)) = result {
                    if !bg_priority && color_index != 0 {
                        // If one of the below conditions is met, draw the sprite
                        // on top of the BG pixel:
                        //
                        // 1. BG priority bit is _not_ set
                        // 2. Sprite pixel is not transparent
                        // 3. Transparent BG, always draw sprite on top
                        // 4. Sprite priority for non-transparent BG
                        // 5. No BG was rendered
                        if bg_color_index == 0 || priority || pixel_data.is_none() {
                            pixel_data = Some(data);
                        }
                    }
                }
            }

            if let Some(data) = pixel_data {
                // Push the pixel to the frame buffer
                self.frame_buffer.write(pixel as usize, scanline as usize, data);
            }
        }
    }

    /// Fetches BG data for a single pixel
    ///
    /// Given the x and y position of the pixel in the BG map, we need to do the following:
    ///
    /// 1. Using the BG pixel positions, determine the BG tile map x and y.
    /// 2. Using the tile coordinates, compute an index into the BG tile map.
    /// 3. Get the tile data index (bank 0) and tile attributes (bank 1) from VRAM.
    /// 4. Extract tile attributes.
    /// 5. Read the actual tile data (16 bytes) from the relevant VRAM bank.
    /// 6. Compute the pixel's index within the 8x8 pixel tile, and adjust if flips are
    ///    present in this tile.
    ///
    /// In `fetch_pixel_data`:
    ///
    /// 7. Compute the pixel's color palette index (2 bits).
    /// 8. Finally, using the tile's palette number and the index from (8),
    ///    compute the RGB value for the pixel.
    ///
    /// Returns: (pixel data, BG priority, pixel color index)
    fn fetch_bg_pixel_data(&self, bg_pixel_x: u8, bg_pixel_y: u8, tile_map_base: u16) -> (GameboyRgba, bool, u8) {
        // Select base address for BG tile data based on LCDC register
        let (tile_data_base, tile_data_index_signed) = if !self.lcdc.bg_tile_data_select() {
            (0x8000, false)
        } else {
            (0x9000, true)
        };

        // (2) and (3)
        let bg_tile_x = bg_pixel_x / 8;
        let bg_tile_y = bg_pixel_y / 8;
        let tile_map_index = (bg_tile_y as u16 * 32 + bg_tile_x as u16) as u16;

        // (4)
        let tile_number = self.vram.read_bank(0, tile_map_base + tile_map_index);
        let tile_data_attr = if self.cgb {
            self.vram.read_bank(1, tile_map_base + tile_map_index)
        } else {
            // No 2nd bank for tile attributes in DMG mode
            0
        };

        // (5)
        let tile_palette_num = tile_data_attr & 0x07; // bits 0-2
        let tile_data_bank = (tile_data_attr & (1 << 3)) >> 3; // bit 3
        let horizontal_flip = (tile_data_attr & (1 << 5)) != 0; // bit 5
        let vertical_flip = (tile_data_attr & (1 << 6)) != 0; // bit 6
        let bg_priority = if self.cgb {
            (tile_data_attr & (1 << 7)) == 0 // bit 7
        } else {
            false
        };

        // (6)
        let mut tile_data = [0u8; 16];
        for i in 0..tile_data.len() as u16 {
            let addr;

            if !tile_data_index_signed || tile_number <= 127 {
                // If we are in 8000 mode OR 8800 mode with tile number <= 127,
                // just add the index to the base address as normal.
                let tile_data_index = tile_number as u16 * 16 + i;
                addr = tile_data_base + tile_data_index;
            } else {
                // For "signed" tiles in 8800 mode:
                //
                // * Tile 128 -> 0x8800-0x880F
                // * Tile 255 -> 0x8FF0-0x8FFF
                let tile_data_index = (tile_number as u16 - 128) * 16 + i;
                addr = 0x8800 + tile_data_index;
            }

            tile_data[i as usize] = self.vram.read_bank(tile_data_bank, addr);
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

        let (pixel_data, color_index) =
            self.fetch_pixel_data(tile_data, tile_pixel_x,
                                  tile_pixel_y, tile_palette_num, false);

        (pixel_data, bg_priority, color_index)
    }

    /// We go through a similar sequence as done in the BG method.
    ///
    /// The main difference is that sprite info is stored in OAM, so we need  to
    /// walk through OAM to determine which sprite needs to be rendered at this location.
    fn fetch_sprite_pixel_data(&self, sprites: &[Sprite], pixel: u8, scanline: u8) -> Option<(GameboyRgba, bool, u8)> {
        let tile_data_base = 0x8000;

        let size = if self.lcdc.sprite_size() {
            16
        } else {
            8
        };

        for sprite in sprites {
            let x_pos = sprite.x.wrapping_sub(8);
            let visible = pixel >= x_pos && pixel < x_pos.wrapping_add(8);
            if !visible {
                continue;
            }

            // Upper left corner of the tile
            let tile_y = sprite.y - 16;
            let tile_x = sprite.x - 8;
            let tile_number = sprite.tile_number;
            let attr = sprite.attr;

            let palette_num;
            let vram_bank;
            if self.cgb {
                palette_num = attr & 0x07;
                vram_bank = (attr & 1 << 3) >> 3;
            } else {
                palette_num = (attr & 1 << 4) >> 4;
                vram_bank = 0;
            };

            let horizontal_flip = (attr & 1 << 5) != 0;
            let vertical_flip = (attr & 1 << 6) != 0;
            let priority = (attr & 1 << 7) == 0;

            // If this is true, pixel data is part of the lower tile for this
            // 8x16 sprite
            let lower_tile = scanline >= tile_y + 8;

            // Convert tile number to index in VRAM
            let tile_index = if size == 8 {
                tile_number as u16
            } else if !lower_tile {
                tile_number as u16 & 0xFE
            } else {
                tile_number as u16 | 0x01
            };

            // Fetch tile data for the pixel
            let mut tile_data= [0u8; 16];
            for i in 0..tile_data.len() as u16 {
                let tile_data_index = tile_index * 16 + i;
                let addr = tile_data_base + tile_data_index;
                tile_data[i as usize] = self.vram.read_bank(vram_bank, addr);
            }

            // Find the location of the pixel _within_ the tile data
            let mut tile_pixel_x = pixel - tile_x;
            let mut tile_pixel_y = scanline - tile_y;

            if vertical_flip {
                tile_pixel_y = (size - 1) - tile_pixel_y;
            }

            // Handle flipped pixels
            if horizontal_flip {
                tile_pixel_x = 7 - tile_pixel_x;
            }

            // Correct pixel_y in lower sprite tile
            if lower_tile && tile_pixel_y >= 8 {
                tile_pixel_y -= 8;
            }

            let (pixel_data, color_index) =
                self.fetch_pixel_data(tile_data, tile_pixel_x,
                                      tile_pixel_y, palette_num, true);

            // If the sprite pixel is not transparent, return it
            // Otherwise, check the next sprite in the list (priority ordered)
            if color_index != 0 {
                return Some((pixel_data, priority, color_index));
            }
        }

        None
    }

    /// Returns pixel data for a single BG/window or sprite pixel.
    ///
    /// Returns: (pixel data, color index)
    fn fetch_pixel_data(&self, tile_data: [u8; 16], tile_pixel_x: u8,
                        tile_pixel_y: u8, tile_palette_num: u8, sprite: bool) -> (GameboyRgba, u8) {
        // (7)
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

        // (8)
        let mut pixel_data;

        if self.cgb {
            let palette_index = (tile_palette_num * 4 + color_index) as usize;
            let palette_ram = if sprite {
                &self.sprite_palette_ram
            } else {
                &self.bg_palette_ram
            };

            let pixel_color = (palette_ram[palette_index + 1] as u16) << 8 |
                              palette_ram[palette_index] as u16;

            let red = (pixel_color & 0x001F) as u8;
            let green = ((pixel_color & 0x03E0) >> 5) as u8;
            let blue = ((pixel_color & 0x7F00) >> 10) as u8;
            let alpha = 0xFF; // BG is always opaque

            pixel_data = GameboyRgba {
                red,
                blue,
                green,
                alpha,
            };

            pixel_data.scale_to_rgb();
        } else {
            let palette_reg = if !sprite {
                self.bgp
            } else {
                match tile_palette_num {
                    0 => self.obp0,
                    _ => self.obp1,
                }
            };

            // In DMG mode, extract the color palette index from BGP/OBP
            let palette_index = match color_index {
                0 => palette_reg & 0b00000011,
                1 => (palette_reg & 0b00001100) >> 2,
                2 => (palette_reg & 0b00110000) >> 4,
                3 => (palette_reg & 0b11000000) >> 6,
                _ => unreachable!(),
            };

            pixel_data = DMG_PALETTE[palette_index as usize];
        }

        (pixel_data, color_index)
    }

    /// Write a single byte of data to palette RAM.
    ///
    /// This handles writes to BCPD (0xFF69) and OCPD (0xFF6B).
    fn palette_write(&mut self, value: u8, sprite: bool) {
        let auto_increment;
        let index;
        let palette_ram;

        if !sprite {
            auto_increment = self.bcps & (1 << 7) != 0;
            index = (self.bcps & 0x3F) as usize;
            palette_ram = &mut self.bg_palette_ram;
        } else {
            auto_increment = self.ocps & (1 << 7) != 0;
            index = (self.ocps & 0x3F) as usize;
            palette_ram = &mut self.sprite_palette_ram;
        }

        // Write the byte to relevant palette RAM
        palette_ram[index] = value;

        if auto_increment {
            // Auto-increment BCPS/OCPS on write (wrapping at bit 5)
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
        let index = (self.bcps & 0x3F) as usize;
        if !sprite {
            self.bg_palette_ram[index]
        } else {
            self.sprite_palette_ram[index]
        }
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
    pub fn oam_locked(&self) -> bool {
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
            0xFF46 => self.oam_dma,
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
                let idx = (addr as usize) - 0xFE00;
                self.oam[idx] = value;
            }
            Self::LCDC_ADDR => self.lcdc.set(value),
            Self::STAT_ADDR => {
                self.stat.set(value);
            }
            Self::SCY_ADDR | Self::SCX_ADDR | Self::LYC_ADDR | Self::WY_ADDR | Self::WX_ADDR => {
                // Latch these writes until the next scanline change (FIFO)
                self.write_stack.push_front((addr, value));
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
            0xFF46 => {
                self.oam_dma = value;
                self.oam_dma_active = true;
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
