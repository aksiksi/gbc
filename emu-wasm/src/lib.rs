mod utils;

use wasm_bindgen::prelude::*;

use gbc::Gameboy as Gameboy_;
use gbc::cartridge::Cartridge as Cartridge_;
use gbc::ppu::{GameboyRgb, LCD_WIDTH, LCD_HEIGHT};

#[wasm_bindgen]
pub struct Cartridge(Cartridge_);

#[wasm_bindgen]
impl Cartridge {
    #[wasm_bindgen(constructor)]
    pub fn new(data: Box<[u8]>) -> Result<Cartridge, JsValue> {
        let cartridge = Cartridge_::from_bytes(data.to_vec(), false);
        cartridge.validate()
                 .map(|_| Self(cartridge))
                 .map_err(|e| JsValue::from_str(&e.to_string()))
    }
}

#[wasm_bindgen]
pub struct Gameboy(Gameboy_);

#[wasm_bindgen]
impl Gameboy {
    /// Create a new `Gameboy` from a valid cartridge
    #[wasm_bindgen(constructor)]
    pub fn new(cartridge: Cartridge) -> Result<Gameboy, JsValue> {
        Gameboy_::init(cartridge.0, false)
                 .map(|gameboy| Self(gameboy))
                 .map_err(|e| JsValue::from_str(&e.to_string()))
    }

    /// Run the Gameboy for a single frame.
    ///
    /// Returns a pointer to the first pixel in the frame buffer. This
    /// allows JS to overlay this memory on a Uint8Array to avoid copying the
    /// entire frame.
    pub fn frame(&mut self) -> *const GameboyRgb {
        // TODO: Take in joypad events
        let frame_buffer = self.0.frame(None);
        frame_buffer.data.as_ptr()
    }

    pub fn lcd_width() -> usize {
        LCD_WIDTH
    }

    pub fn lcd_height() -> usize {
        LCD_HEIGHT
    }
}

#[wasm_bindgen]
pub fn init() {
    utils::set_panic_hook();
    wasm_logger::init(wasm_logger::Config::default());
}
