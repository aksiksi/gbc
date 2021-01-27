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
    }
}

// Basic DMG/monochrome color palette
static DMG_PALETTE: [GameboyRgba; 4] = [
    // White
    GameboyRgba {
        red: 0xE0, green: 0xF8, blue: 0xD0, alpha: 255
    },

    // Light gray
    GameboyRgba {
        red: 0x88, green: 0xC0, blue: 0x70, alpha: 255
    },

    // Dark gray
    GameboyRgba {
        red: 0x34, green: 0x68, blue: 0x56, alpha: 255
    },

    // Black
    GameboyRgba {
        red: 0x08, green: 0x18, blue: 0x20, alpha: 255
    },
];

/// Buffer that holds pixel data for a single frame.
pub struct FrameBuffer {
    data: Box<[GameboyRgba; LCD_WIDTH * LCD_HEIGHT]>,
    pub(crate) ready: bool,
}

impl FrameBuffer {
    pub fn new() -> Self {
        Self {
            data: Box::new([GameboyRgba::white(); LCD_WIDTH * LCD_HEIGHT]),
            ready: false,
        }
    }

    /// Read a single pixel from the buffer.
    ///
    /// `x` is the "column", `y` is the "row".
    #[inline]
    pub fn read(&self, x: usize, y: usize) -> GameboyRgba {
        self.data[y * LCD_WIDTH + x]
    }

    /// Write a single pixel to the buffer.
    ///
    /// `x` is the "column", `y` is the "row".
    #[inline]
    pub fn write(&mut self, x: usize, y: usize, pixel: GameboyRgba) {
        self.data[y * LCD_WIDTH + x] = pixel;
    }
}

pub struct Vram {
    /// DMG: One static bank, 8K
    /// CGB: Two static banks, 8K each
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
        let bank = bank & 0x1;

        if !self.cgb {
            // No VRAM bank switching on DMG
            assert!(bank == 0);
        }

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

    pub fn lcd_display_enable(&self) -> bool {
        self.raw & (1 << 7) != 0
    }

    pub fn window_tile_map(&self) -> u16 {
        if self.raw & (1 << 6) == 0 {
            0x9800
        } else {
            0x9C00
        }
    }

    pub fn window_display_enable(&self) -> bool {
        self.raw & (1 << 5) != 0
    }

    pub fn bg_tile_data_select(&self) -> bool {
        self.raw & (1 << 4) == 0
    }

    pub fn bg_tile_map(&self) -> u16 {
        if self.raw & (1 << 3) == 0 {
            0x9800
        } else {
            0x9C00
        }
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
pub enum StatMode {
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
}

impl LcdStat {
    pub fn new() -> Self {
        Self {
            raw: 0,
        }
    }

    pub fn ly_enabled(&self) -> bool {
        self.raw & (1 << 6) != 0
    }

    pub fn oam_enabled(&self) -> bool {
        self.raw & (1 << 5) != 0
    }

    pub fn vblank_enabled(&self) -> bool {
        self.raw & (1 << 4) != 0
    }

    pub fn hblank_enabled(&self) -> bool {
        self.raw & (1 << 3) != 0
    }

    #[allow(dead_code)]
    pub fn coincidence(&self) -> bool {
        self.raw & (1 << 2) != 0
    }

    pub fn mode(&self) -> StatMode {
        match self.raw & 0x3 {
            0 => StatMode::Hblank,
            1 => StatMode::Vblank,
            2 => StatMode::OamScan,
            3 => StatMode::OamRead,
            _ => unreachable!(),
        }
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
    pub vram: Vram,

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
    pub oam: [u8; 160],

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

    /// Sprites that are visible on this scanline
    sprites: Vec<Sprite>,

    /// Current dot being rendered in this scanline
    dot: u16,

    /// Previous STAT interrupt state
    prev_stat_interrupt: bool,

    /// If `true`, operate in CGB mode
    cgb: bool,
}

impl Ppu {
    pub const OAM_START_ADDR: u16 = 0xFE00;
    pub const OAM_LAST_ADDR: u16 = 0xFE9F;

    // Register addresses
    const LCDC_ADDR: u16 = 0xFF40;
    const STAT_ADDR: u16 = 0xFF41;
    const SCY_ADDR: u16 = 0xFF42;
    const SCX_ADDR: u16 = 0xFF43;
    const LY_ADDR: u16 = 0xFF44;
    const LYC_ADDR: u16 = 0xFF45;
    const WY_ADDR: u16 = 0xFF4A;
    const WX_ADDR: u16 = 0xFF4B;

    const DOTS_PER_LINE: u16 = 456;
    const VBLANK_START_LINE: u8 = 144;
    const TOTAL_LINES: u8 = 154;
    const OAM_SCAN_DOTS: u16 = 80;
    const OAM_READ_DOTS: u16 = 172;
    const HBLANK_DOTS: u16 = 204;
    const VBLANK_DOTS: u16 = Self::DOTS_PER_LINE * (Self::TOTAL_LINES - Self::VBLANK_START_LINE) as u16;

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
            sprites: Vec::with_capacity(10),
            dot: 0,
            prev_stat_interrupt: false,
            cgb,
        }
    }

    /// Returns the next: (dot, scanline, STAT mode)
    fn get_next_dot(&self, cycles: u16, speed: bool) -> (u16, u8, StatMode) {
        let mut line = self.ly;
        let mut dot = self.dot;

        // Figure out the number of pixels to render in this step
        let dots = if speed {
            // If we are in double-speed mode, we render a pixel every 2 cycles
            cycles / 2
        } else {
            cycles
        };

        dot += dots;

        if dot >= Self::DOTS_PER_LINE {
            // Move to the next scanline
            line += 1;
            dot -= Self::DOTS_PER_LINE;
        }

        if line == Self::TOTAL_LINES {
            // Start of new frame
            line = 0;
        }

        // Figure out which stat mode we are in based on line and dot.
        //
        // Recall that we have 456 dots in a line.
        let mode = if line < Self::VBLANK_START_LINE {
            if dot < Self::OAM_SCAN_DOTS {
                StatMode::OamScan
            } else if dot >= Self::OAM_SCAN_DOTS && dot < Self::OAM_SCAN_DOTS + Self::OAM_READ_DOTS {
                StatMode::OamRead
            } else {
                StatMode::Hblank
            }
        } else {
            // VBLANK
            StatMode::Vblank
        };

        (dot, line, mode)
    }

    /// Update the PPU status registers based on current cycle and CPU speed.
    ///
    /// This function is called once per CPU step from the main frame loop. The
    /// function is called *after* the CPU step completes. The value passed in
    /// for `cycles` is the number of cycles spent in the CPU.
    ///
    /// If any interrupts need to be triggered, they are pushed to the input `interrupts`
    /// vector.
    pub fn step(&mut self, cycles: u16, speed: bool, interrupts: &mut Vec<Interrupt>) {
        let (dot, line, mode) = self.get_next_dot(cycles, speed);

        self.dot = dot;
        self.ly = line;

        if self.lcdc.lcd_display_enable() {
            // Update the internal PPU status
            //
            // This also returns which interrupts need to be triggered
            let stat_mode_change = self.update_status(mode, interrupts);

            if stat_mode_change {
                // Render data to the frame
                self.render();
            }
        }
    }

    /// Returns: (stat_mode_change, vblank_interrupt, stat_interrupt)
    fn update_status(&mut self, mode: StatMode, interrupts: &mut Vec<Interrupt>) -> bool {
        let ly_coincidence = self.ly == self.lyc;

        let mut stat = mode as u8;
        if ly_coincidence {
            // Set the LY conincidence bit
            stat |= 1 << 2;
        }

        let prev_mode = self.stat.mode();
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
        let stat_interrupt = (ly_coincidence && self.stat.ly_enabled()) || {
            match mode {
                StatMode::Hblank => self.stat.hblank_enabled(),
                StatMode::Vblank => self.stat.vblank_enabled(),
                StatMode::OamScan | StatMode::OamRead => self.stat.oam_enabled(),
            }
        };

        if !self.prev_stat_interrupt && stat_interrupt {
            interrupts.push(Interrupt::LcdStat);
        }

        // Update STAT register (upper 5 bits are user-controlled)
        self.stat.raw = self.stat.raw & 0xF8 | stat;

        self.prev_stat_interrupt = stat_interrupt;

        stat_mode_change
    }

    /// Given a number of cycles, returns the _next_ mode and number of cycles
    /// the PPU would remain in that mode.
    pub fn next_mode(&self, cycles: u16, speed: bool) -> (StatMode, u16) {
        let (.., mode) = self.get_next_dot(cycles, speed);

        // Determine the number of cycles the PPU would spend in the next mode
        let dots = match mode {
            StatMode::OamScan => Self::OAM_SCAN_DOTS,
            StatMode::OamRead => Self::OAM_READ_DOTS,
            StatMode::Hblank => Self::HBLANK_DOTS,
            StatMode::Vblank => Self::VBLANK_DOTS,
        };

        let cycles_in_mode = if speed {
            dots * 2
        } else {
            dots
        };

        (mode, cycles_in_mode as u16)
    }

    /// Render pixel data to the internal frame buffer
    ///
    /// Once we get to VBLANK, we mark the frame buffer as ready for
    /// rendering.
    fn render(&mut self) {
        match self.stat.mode() {
            StatMode::OamRead => {
                // At the end of OAM scan/start of OAM read, build a list of
                // visible sprites on this scanline. OAM is locked in this mode.
                self.sprites.clear();
                self.find_visible_sprites();
            }
            StatMode::Hblank => {
                // If we are at the end of OAM read/start of HBLANK, render a
                // scanline worth of BG and sprites to the frame buffer.
                //
                // Both VRAM and OAM are locked in this mode.
                self.render_scanline();
            }
            StatMode::Vblank => {
                // Mark the frame as ready for rendering
                self.frame_buffer.ready = true;
            }
            _ => (),
        }
    }

    /// Find all visible sprites on this scanline.
    ///
    /// Sprites will be sorted according to required priority:
    ///
    /// CGB mode: in OAM order
    /// DMG mode: sorted by x-pos
    fn find_visible_sprites(&mut self) {
        let scanline = self.ly;

        let size = if self.lcdc.sprite_size() {
            16
        } else {
            8
        };

        for chunk in self.oam.chunks_exact(4) {
            let y = chunk[0];
            let x = chunk[1];
            let tile_number = chunk[2];
            let attr = chunk[3];

            let sprite_start = y.wrapping_sub(16);
            let sprite_end = sprite_start.wrapping_add(size);

            // There are two cases of vertical position of a sprite:
            //
            // 1. Sprite is _partially_ visible at top of screen. This means that the
            //    upper edge of the sprite will wrap around 0.
            // 2. Sprite is within the screen OR _partially_ visible at the bottom. If
            //    the sprite is at the bottom, no wrap around will occur.
            let visible = if sprite_start < sprite_end {
                // Case (2)
                sprite_start <= scanline && scanline < sprite_end
            } else {
                // Case (1)
                scanline < sprite_end
            };

            // We can only have 10 sprites on a single scanline
            if visible && self.sprites.len() < 10 {
                self.sprites.push(Sprite {
                    y,
                    x,
                    tile_number,
                    attr,
                });
            }
        }

        // Sort according to x-pos
        if !self.cgb {
            self.sprites.sort_by(|a, b| {
                a.x.cmp(&b.x)
            });
        }
    }

    /// Render a single pixel to the frame buffer (screen).
    ///
    /// This is split into rendering the BG/window pixel and rendering the sprite
    /// pixel. Note that sprites are more often layered on top of the BG, depending
    /// on color and priority.
    fn render_pixel(&mut self, pixel: u8) {
        let scanline = self.ly;

        // Select base address for BG and window tile maps based on LCDC register
        let bg_tile_map_base: u16 = self.lcdc.bg_tile_map();
        let window_tile_map_base: u16 = self.lcdc.window_tile_map();

        let mut pixel_data;
        let bg_priority;
        let bg_color_index;

        // Fetch pixel data for the BG or window, depending on which is currently active.
        //
        // Conditions:
        //
        // 1. If the priority bit is set, the BG will _always_ have priority over sprites
        // 2. If priority bit is reset (CGB): BG and window are still rendered, but sprites get priority
        // 3. If priority bit is reset (DMG): BG and window turn white and sprites get priority
        if self.lcdc.bg_priority() || self.cgb {
            // Check if this pixel is inside the window area
            let in_window =
                self.lcdc.window_display_enable() &&
                scanline >= self.wy &&
                pixel >= self.wx.wrapping_sub(7);

            let (data, priority, color_index) = if !in_window {
                // Normal BG pixel
                let bg_pixel_x = pixel.wrapping_add(self.scx);
                let bg_pixel_y = scanline.wrapping_add(self.scy);
                self.fetch_bg_pixel_data(bg_pixel_x, bg_pixel_y, bg_tile_map_base)
            } else {
                // Window pixel
                let pixel_x = pixel - self.wx.wrapping_sub(7);
                let pixel_y = scanline - self.wy;
                self.fetch_bg_pixel_data(pixel_x, pixel_y, window_tile_map_base)
            };

            pixel_data = Some(data);

            // On CGB, if LCDC priority is reset, BG & window lose priority
            bg_priority = self.lcdc.bg_priority() && priority;
            bg_color_index = color_index;
        } else {
            // On DMG, reset the BG to white in non-priority mode
            pixel_data = Some(DMG_PALETTE[0]);
            bg_priority = false;
            bg_color_index = 0;
        }

        // Render sprites
        if self.lcdc.sprite_enable() {
            // Fetch sprite pixel data
            //
            // The method returns `None` if the sprite pixel is transparent.
            if let Some((data, priority)) = self.fetch_sprite_pixel_data(pixel) {
                if bg_color_index == 0 || (!bg_priority && priority) || pixel_data.is_none() {
                    pixel_data = Some(data);
                }
            }
        }

        // Push the pixel to the frame buffer
        if let Some(data) = pixel_data {
            self.frame_buffer.write(pixel as usize, scanline as usize, data);
        }
    }

    /// Render a single scanline worth of pixel data to the frame buffer
    fn render_scanline(&mut self) {
        for pixel in 0..LCD_WIDTH as u8 {
            self.render_pixel(pixel);
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
            (tile_data_attr & (1 << 7)) == 1 // bit 7
        } else {
            // On DMG, BG priority is based on the color index
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

    /// We go through a similar sequence as in the BG method.
    ///
    /// The first difference is that sprite info is stored in OAM, so we need to
    /// walk through OAM to determine which sprite needs to be rendered at this location.
    ///
    /// The second difference is that sprites can be either a single tile (8x8) or two
    /// vertically stacked tiles (8x16). In case of the latter, we need to adjust our logic
    /// based on which tile the current pixel lies in (upper vs. lower).
    fn fetch_sprite_pixel_data(&self, pixel: u8) -> Option<(GameboyRgba, bool)> {
        let tile_data_base = 0x8000;

        let size = if self.lcdc.sprite_size() {
            16
        } else {
            8
        };

        let scanline = self.ly;

        for sprite in &self.sprites {
            // Same logic as vertical position check in `find_visible_sprites()`.
            let sprite_start = sprite.x.wrapping_sub(8);
            let sprite_end = sprite.x;
            let visible = if sprite_start < sprite_end {
                sprite_start <= pixel && pixel < sprite_end
            } else {
                pixel < sprite_end
            };

            if !visible {
                // If the sprite is not visible at this pixel, skip it
                continue;
            }

            // Find the position of upper left corner of the tile on the screen. We
            // need to use wrapping adds here to address the boundary conditions.
            //
            // For example, suppose we have a 8x16 sprite that is halfway above the screen -
            // i.e., y = 8 -> y - 16 = -8. The upper corner of the tile is indeed at y = -8,
            // but we still need to display pixels that are within the _lower_ half of the tile.
            //
            // This is why wrapping operations are used throughout this function. Once we have
            // the pixel positions within the tile (e.g. `tile_pixel_x`), we are done.
            let tile_y = sprite.y.wrapping_sub(16);
            let tile_x = sprite.x.wrapping_sub(8);

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

            // Find the location of the pixel _within_ the tile data
            let mut tile_pixel_x = pixel.wrapping_sub(tile_x);
            let mut tile_pixel_y = scanline.wrapping_sub(tile_y);

            // Handle flipped pixels
            if vertical_flip {
                tile_pixel_y = (size - 1) - tile_pixel_y;
            }

            if horizontal_flip {
                tile_pixel_x = 7 - tile_pixel_x;
            }

            // If this is true, pixel data is part of the lower tile for this
            // 8x16 sprite
            let lower_tile = tile_pixel_y >= 8;
            if lower_tile {
                // Correct pixel_y in lower sprite tile
                tile_pixel_y -= 8;
            }

            // Convert tile number to index in VRAM
            let tile_index = if size == 8 {
                tile_number as u16
            } else if !lower_tile {
                tile_number as u16 & 0xFE
            } else {
                tile_number as u16 | 0x01
            };

            // Fetch tile data for the pixel
            let mut tile_data = [0u8; 16];
            for i in 0..tile_data.len() as u16 {
                let tile_data_index = tile_index * 16 + i;
                let addr = tile_data_base + tile_data_index;
                tile_data[i as usize] = self.vram.read_bank(vram_bank, addr);
            }

            let (pixel_data, color_index) =
                self.fetch_pixel_data(tile_data, tile_pixel_x,
                                      tile_pixel_y, palette_num, true);

            // If the sprite pixel is not transparent, return it
            // Otherwise, check the next sprite in the list (priority ordered)
            if color_index != 0 {
                return Some((pixel_data, priority));
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
            let palette_index = (tile_palette_num * 8 + color_index * 2) as usize;
            let palette_ram = if sprite {
                &self.sprite_palette_ram
            } else {
                &self.bg_palette_ram
            };

            let pixel_color = (palette_ram[palette_index + 1] as u16) << 8 |
                              palette_ram[palette_index] as u16;

            let red = (pixel_color & 0x001F) as u8;
            let green = ((pixel_color & 0x03E0) >> 5) as u8;
            let blue = ((pixel_color & 0x7C00) >> 10) as u8;
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
        self.lcdc.lcd_display_enable() && match self.stat.mode() {
            // Locked during OAM read (mode 3)
            StatMode::OamRead => true,
            _ => false,
        }
    }

    /// Returns `true` if OAM is locked to CPU
    pub fn oam_locked(&self) -> bool {
        self.lcdc.lcd_display_enable() && match self.stat.mode() {
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

    /// Get a reference to the frame buffer, if it's ready.
    ///
    /// The frame will be ready only during VBLANK.
    pub fn frame_buffer(&mut self) -> Option<&FrameBuffer> {
        if self.frame_buffer.ready {
            self.frame_buffer.ready = false;
            Some(&self.frame_buffer)
        } else {
            None
        }
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
            Vram::BANK_SELECT_ADDR => {
                // Reading the bank select register returns the active bank in bit 0,
                // with all other bits set to 1
                let bank = self.vram.active_bank;
                bank | 0xFE
            }
            Self::OAM_START_ADDR..=Self::OAM_LAST_ADDR => {
                let idx = (addr - Self::OAM_START_ADDR) as usize;
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
            Vram::BANK_SELECT_ADDR => self.vram.update_bank(value),
            Self::OAM_START_ADDR..=Self::OAM_LAST_ADDR => {
                if !self.oam_locked() {
                    let idx = (addr - Self::OAM_START_ADDR) as usize;
                    self.oam[idx] = value;
                }
            }
            Self::LCDC_ADDR => {
                self.lcdc.raw = value;
                if value & 1 << 7 == 0 {
                    // Disabling the LCD resets LY
                    self.ly = 0;
                }
            }
            Self::STAT_ADDR => {
                // Lower 3 bits are read-only
                let value = value & 0xF8;
                self.stat.raw = value | self.stat.raw & 0x07;
            }
            Self::SCY_ADDR => self.scy = value,
            Self::SCX_ADDR => self.scx = value,
            Self::LYC_ADDR => self.lyc = value,
            Self::WY_ADDR => self.wy = value,
            Self::WX_ADDR => self.wx = value,
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
            0xFF69 => {
                if !self.vram_locked() {
                    self.palette_write(value, false);
                }
            }
            0xFF6A => self.ocps = value,
            0xFF6B => {
                if !self.vram_locked() {
                    self.palette_write(value, true);
                }
            }
            _ => panic!("Unexpected write to addr {} value {}", addr, value),
        }
    }
}
