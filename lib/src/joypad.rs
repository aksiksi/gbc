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

impl JoypadInput {
    fn to_bit(&self) -> u8 {
        match self {
            Self::A | Self::Right => 0,
            Self::B | Self::Left => 1,
            Self::Select | Self::Up => 2,
            Self::Start | Self::Down => 3,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum JoypadEvent {
    Up(JoypadInput),
    Down(JoypadInput),
}

impl JoypadEvent {
    fn input(&self) -> JoypadInput {
        match self {
            Self::Up(input) | Self::Down(input) => input.clone(),
        }
    }

    fn selection(&self) -> JoypadSelection {
        match self.input() {
            JoypadInput::A | JoypadInput::B | JoypadInput::Select | JoypadInput::Start => JoypadSelection::Buttons,
            _ => JoypadSelection::Directions,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
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

    /// Update raw joypad register based on incoming joypad event.
    ///
    /// This sets the right bits in the `raw` state based on
    /// the event. If the event does not apply to current selection,
    /// it is simply ignored.
    pub fn handle_event(&mut self, event: JoypadEvent) {
        // Does the button match the selection
        if event.selection() != self.selection {
            return;
        }

        let is_down = if let JoypadEvent::Down(_) = event {
            true
        } else {
            false
        };

        // Convert the input to its relevant bit position
        let input = event.input();
        let bit = input.to_bit();

        if is_down {
            self.raw |= 1 << bit;
        } else {
            self.raw &= !(1 << bit);
        }
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
