/// 8-bit register names
#[derive(Debug)]
#[allow(non_snake_case)]
pub enum Reg8 {
    A,
    F,
    B,
    C,
    D,
    E,
    H,
    L,
}

/// 16-bit register names
/// This includes the "combo" registers
#[derive(Debug)]
#[allow(non_snake_case)]
pub enum Reg16 {
    AF,
    BC,
    DE,
    HL,
    PC,
    SP,
}

#[derive(Debug)]
pub enum Flag {
    Zero,
    Subtract,
    HalfCarry,
    Carry,
}

#[derive(Debug)]
pub struct Flags {
    raw: u8,
    zero: bool,
    subtract: bool,
    half_carry: bool,
    carry: bool,
}

impl Flags {
    const ZERO_MASK: u8 = (1 << 7);
    const SUBTRACT_MASK: u8 = (1 << 6);
    const HALF_CARRY_MASK: u8 = (1 << 5);
    const CARRY_MASK: u8 = (1 << 4);

    pub fn new() -> Self {
        Self {
            raw: 0,
            zero: false,
            subtract: false,
            half_carry: false,
            carry: false,
        }
    }

    pub fn set_all(&mut self) {
        self.raw = 0xFF;
        self.zero = true;
        self.subtract = true;
        self.half_carry = true;
        self.carry = true;
    }

    pub fn set(&mut self, flag: Flag) {
        match flag {
            Flag::Zero => {
                self.zero = true;
                self.raw |= Self::ZERO_MASK;
            }
            Flag::Subtract => {
                self.subtract = true;
                self.raw |= Self::SUBTRACT_MASK;
            }
            Flag::HalfCarry => {
                self.half_carry = true;
                self.raw |= Self::HALF_CARRY_MASK;
            }
            Flag::Carry => {
                self.carry = true;
                self.raw |= Self::CARRY_MASK;
            }
        }
    }

    pub fn clear(&mut self, flag: Flag) {
        match flag {
            Flag::Zero => {
                self.zero = false;
                self.raw &= !Self::ZERO_MASK;
            }
            Flag::Subtract => {
                self.subtract = true;
                self.raw &= !Self::SUBTRACT_MASK;
            }
            Flag::HalfCarry => {
                self.half_carry = true;
                self.raw &= !Self::HALF_CARRY_MASK;
            }
            Flag::Carry => {
                self.carry = true;
                self.raw &= !Self::CARRY_MASK;
            }
        }
    }

    pub fn clear_all(&mut self) {
        self.raw = 0;
        self.zero = false;
        self.subtract = false;
        self.half_carry = false;
        self.carry = false;
    }

    pub fn raw(&self) -> u8 {
        self.raw
    }

    pub fn is_zero(&self) -> bool {
        self.zero
    }

    pub fn is_subtract(&self) -> bool {
        self.subtract
    }

    pub fn is_half_carry(&self) -> bool {
        self.half_carry
    }

    pub fn is_carry(&self) -> bool {
        self.carry
    }
}

#[derive(Debug)]
#[allow(non_snake_case)]
pub struct RegisterFile {
    A: u8,
    F: u8,
    B: u8,
    C: u8,
    D: u8,
    E: u8,
    H: u8,
    L: u8,
    pub PC: u16,
    pub SP: u16,
    flags: Flags,
}

impl RegisterFile {
    /// Returns a new register file
    ///
    /// PC is initialized to 0x100 on boot
    /// SP is initialized to 0xFFFE on boot
    pub fn new() -> Self {
        Self {
            A: 0,
            F: 0,
            B: 0,
            C: 0,
            D: 0,
            E: 0,
            H: 0,
            L: 0,
            PC: 0x0100,
            SP: 0xFFFE,
            flags: Flags::new(),
        }
    }

    pub fn write_u16(&mut self, reg: Reg16, value: u16) {
        match reg {
            Reg16::AF => {
                self.A = (value & 0xFF) as u8;
                self.F = (value >> 8) as u8;
            }
            Reg16::BC => {
                self.B = (value & 0xFF) as u8;
                self.C = (value >> 8) as u8;
            }
            Reg16::DE => {
                self.D = (value & 0xFF) as u8;
                self.E = (value >> 8) as u8;
            }
            Reg16::HL => {
                self.H = (value & 0xFF) as u8;
                self.L = (value >> 8) as u8;
            }
            Reg16::PC => self.PC = value,
            Reg16::SP => self.SP = value,
        }
    }

    pub fn read_u16(&self, reg: Reg16) -> u16 {
        match reg {
            Reg16::AF => (self.F as u16) << 8 | self.A as u16,
            Reg16::BC => (self.C as u16) << 8 | self.B as u16,
            Reg16::DE => (self.E as u16) << 8 | self.D as u16,
            Reg16::HL => (self.L as u16) << 8 | self.H as u16,
            Reg16::SP => self.SP,
            Reg16::PC => self.PC,
        }
    }

    pub fn write_u8(&mut self, reg: Reg8, value: u8) {
        match reg {
            Reg8::A => self.A = value as u8,
            Reg8::F => self.F = value as u8,
            Reg8::B => self.B = value as u8,
            Reg8::C => self.C = value as u8,
            Reg8::D => self.D = value as u8,
            Reg8::E => self.E = value as u8,
            Reg8::H => self.H = value as u8,
            Reg8::L => self.L = value as u8,
        }
    }

    pub fn read_u8(&self, reg: Reg8) -> u8 {
        match reg {
            Reg8::A => self.A,
            Reg8::F => self.F,
            Reg8::B => self.B,
            Reg8::C => self.C,
            Reg8::D => self.D,
            Reg8::E => self.E,
            Reg8::H => self.H,
            Reg8::L => self.L,
        }
    }

    pub fn flags(&self) -> &Flags {
        &self.flags
    }

    pub fn flags_mut(&mut self) -> &mut Flags {
        &mut self.flags
    }

    pub fn pc(&self) -> u16 {
        self.PC
    }

    pub fn sp(&self) -> u16 {
        self.SP
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_combined_regs() {
        let mut regs = RegisterFile::new();

        regs.write_u8(Reg8::A, 0x10);
        regs.write_u8(Reg8::F, 0xFF);
        assert_eq!(regs.read_u16(Reg16::AF), 0xFF10);

        regs.write_u16(Reg16::BC, 0xBEEF);
        assert_eq!(regs.read_u8(Reg8::B), 0xEF);
        assert_eq!(regs.read_u8(Reg8::C), 0xBE);
    }

    #[test]
    fn test_flags() {
        let mut regs = RegisterFile::new();
        let flags = regs.flags_mut();

        flags.set(Flag::Zero);
        assert!(flags.is_zero());

        flags.clear(Flag::Zero);
        assert!(!flags.is_zero());

        flags.set_all();
        assert!(flags.is_carry());

        flags.clear_all();
        assert!(!flags.is_carry());
    }
}
