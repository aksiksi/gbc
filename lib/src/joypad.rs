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
    buttons: u8,
    directions: u8,
    selection: JoypadSelection,
}

impl Joypad {
    pub fn new() -> Self {
        Self {
            // All buttons are unpressed by default
            buttons: 0xFF,
            directions: 0xFF,
            selection: JoypadSelection::None,
        }
    }

    /// Update raw joypad register based on incoming joypad event.
    pub fn handle_event(&mut self, event: JoypadEvent) -> bool {
        let reg = match event.selection() {
            JoypadSelection::Buttons => &mut self.buttons,
            _ => &mut self.directions,
        };

        let is_down = if let JoypadEvent::Down(_) = event {
            true
        } else {
            false
        };

        // Convert the input to its relevant bit position
        let bit = event.input().to_bit();

        if is_down {
            *reg &= !(1 << bit);
            true
        } else {
            *reg |= 1 << bit;
            false
        }
    }

    pub fn read(&self) -> u8 {
        match self.selection {
            JoypadSelection::Buttons => self.buttons & 0x0F,
            JoypadSelection::Directions => self.directions & 0x0F,
            JoypadSelection::None => 0x0F,
        }
    }

    pub fn write(&mut self, data: u8) {
        // Update the selection keys based on value written
        // This information is used during event handling
        self.selection = if data & (1 << 5) == 0 {
            JoypadSelection::Buttons
        } else if data & (1 << 4) == 0 {
            JoypadSelection::Directions
        } else {
            JoypadSelection::None
        };
    }
}
