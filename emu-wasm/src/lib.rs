use wasm_bindgen::prelude::*;

use gbc::cartridge::Cartridge as Cartridge_;
use gbc::joypad::{JoypadEvent, JoypadInput as JoypadInput_};
use gbc::ppu::{GameboyRgb, LCD_HEIGHT, LCD_WIDTH};
use gbc::Gameboy as Gameboy_;

// Re-exported JopypadInput enum
#[wasm_bindgen]
#[derive(Clone, Copy)]
pub enum JoypadInput {
    Up,
    Down,
    Left,
    Right,
    A,
    B,
    Start,
    Select,
}

impl From<JoypadInput> for JoypadInput_ {
    fn from(input: JoypadInput) -> Self {
        match input {
            JoypadInput::Up => JoypadInput_::Up,
            JoypadInput::Down => JoypadInput_::Down,
            JoypadInput::Left => JoypadInput_::Left,
            JoypadInput::Right => JoypadInput_::Right,
            JoypadInput::A => JoypadInput_::A,
            JoypadInput::B => JoypadInput_::B,
            JoypadInput::Start => JoypadInput_::Start,
            JoypadInput::Select => JoypadInput_::Select,
        }
    }
}

#[wasm_bindgen]
pub struct Cartridge(Cartridge_);

#[wasm_bindgen]
impl Cartridge {
    #[wasm_bindgen(constructor)]
    pub fn new(data: Box<[u8]>) -> Result<Cartridge, JsValue> {
        let cartridge = Cartridge_::from_bytes(Vec::from(data), false);
        cartridge
            .validate()
            .map(|_| Self(cartridge))
            .map_err(|e| JsValue::from_str(&e.to_string()))
    }
}

#[wasm_bindgen]
pub struct Gameboy {
    inner: Gameboy_,
    inputs: Vec<JoypadEvent>,
}

#[wasm_bindgen]
impl Gameboy {
    /// Create a new `Gameboy` from a valid cartridge
    #[wasm_bindgen(constructor)]
    pub fn new(cartridge: Cartridge) -> Result<Gameboy, JsValue> {
        let inner =
            Gameboy_::init(cartridge.0, false).map_err(|e| JsValue::from_str(&e.to_string()))?;
        let inputs = Vec::new();

        Ok(Self { inner, inputs })
    }

    /// Run the Gameboy for a single frame.
    ///
    /// Returns a pointer to the first pixel in the frame buffer. This
    /// allows JS to overlay this memory on a Uint8Array to avoid copying the
    /// entire frame.
    pub fn frame(&mut self) -> *const GameboyRgb {
        let frame_buffer = self.inner.frame(Some(&self.inputs));
        self.inputs.clear();
        frame_buffer.data.as_ptr()
    }

    pub fn reset(&mut self) {
        self.inner.reset();
        self.inputs.clear();
    }

    /// Record a joypad input for the next frame
    pub fn joypad_input(&mut self, input: JoypadInput, down: JsValue) -> Result<(), JsValue> {
        let input = JoypadInput_::from(input);

        let event = if down.is_truthy() {
            JoypadEvent::Down(input)
        } else {
            JoypadEvent::Up(input)
        };

        self.inputs.push(event);

        Ok(())
    }

    /// Save the state of the Gameboy
    pub fn save(&self) -> Result<Box<[u8]>, JsValue> {
        self.inner
            .save()
            .map(|v| v.into_boxed_slice())
            .map_err(|e| JsValue::from_str(&e.to_string()))
    }

    /// Create a Gameboy from an existing save state
    pub fn load(data: &[u8], cartridge: Cartridge) -> Result<Gameboy, JsValue> {
        Gameboy_::load(data, cartridge.0)
            .map(|gameboy| Self {
                inner: gameboy,
                inputs: Vec::new(),
            })
            .map_err(|e| JsValue::from_str(&e.to_string()))
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
    wasm_logger::init(wasm_logger::Config::default());
}
