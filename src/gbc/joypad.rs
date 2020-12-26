#[derive(Clone, Copy, Debug)]
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

#[derive(Clone, Copy, Debug)]
pub enum JoypadEvent {
    Up(JoypadInput),
    Down(JoypadInput),
}

#[derive(Clone, Copy, Debug)]
enum JoypadSelection {
    None,
    Buttons,
    Directions,
}

#[derive(Debug)]
pub struct Joypad {
    /// Raw register state
    raw: u8,

    /// Current joypad selection for correct event handling
    selection: JoypadSelection,
}

impl Joypad {
    pub fn new() -> Self {
        Self {
            // All buttons are unpressed by default
            raw: 0xFF,
            selection: JoypadSelection::None,
        }
    }

    /// Update register based on incoming joypad event
    ///
    /// This sets the right bits in the `raw` state based on
    /// the event. If the event does not apply to current selection,
    /// it is dropped.
    pub fn handle_event(&mut self, _event: JoypadEvent) {
        todo!()
    }

    pub fn read(&self) -> u8 {
        self.raw
    }

    pub fn write(&mut self, data: u8) {
        self.raw = data;

        // Update the selection keys based on value written
        // This information is used during event handling
        self.selection = if self.raw & (1 << 5) == 0 {
            JoypadSelection::Buttons
        } else if self.raw & (1 << 4) == 0 {
            JoypadSelection::Directions
        } else {
            JoypadSelection::None
        };
    }
}

