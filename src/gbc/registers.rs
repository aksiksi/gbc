/// 8-bit register names
#[derive(Clone, Copy, Debug, PartialEq)]
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
#[derive(Clone, Copy, Debug, PartialEq)]
#[allow(non_snake_case)]
pub enum Reg16 {
    AF,
    BC,
    DE,
    HL,
    PC,
    SP,
}

/// A trait that defines basic register operations.
pub trait RegisterOps<R, V> {
    fn read(&self, reg: R) -> V;
    fn write(&mut self, reg: R, value: V);
}

#[derive(Debug, PartialEq)]
pub enum Flag {
    Zero,
    Subtract,
    HalfCarry,
    Carry,
}

#[derive(Debug)]
#[allow(non_snake_case)]
pub struct RegisterFile {
    // Registers
    A: u8,
    F: u8, // Flags register
    B: u8,
    C: u8,
    D: u8,
    E: u8,
    H: u8,
    L: u8,
    pub PC: u16,
    pub SP: u16,

    // Individual flag bits
    zero: bool,
    subtract: bool,
    half_carry: bool,
    carry: bool,
}

impl RegisterFile {
    /// Returns a new register file
    ///
    /// * Registers are initialized to some values on boot
    /// * SP is initialized to 0xFFFE on boot
    /// * PC is initialized to 0x100 on boot (once internal ROM completes)
    ///
    /// TODO: do we want to mimic the internal ROM?
    pub fn new() -> Self {
        Self {
            A: 0xB0,
            F: 0x01,
            B: 0x13,
            C: 0x00,
            D: 0xD8,
            E: 0x00,
            H: 0x4D,
            L: 0x01,
            PC: 0x0100,
            SP: 0xFFFE,
            zero: false,
            subtract: false,
            half_carry: false,
            carry: false,
        }
    }

    /// Set a flag
    pub fn set(&mut self, flag: Flag, value: bool) {
        match flag {
            Flag::Zero => {
                self.zero = value;
            }
            Flag::Subtract => {
                self.subtract = value;
            }
            Flag::HalfCarry => {
                self.half_carry = value;
            }
            Flag::Carry => {
                self.carry = value;
            }
        }
    }

    /// Clear a flag
    pub fn clear(&mut self, flag: Flag) {
        match flag {
            Flag::Zero => {
                self.zero = false;
            }
            Flag::Subtract => {
                self.subtract = false;
            }
            Flag::HalfCarry => {
                self.half_carry = false;
            }
            Flag::Carry => {
                self.carry = false;
            }
        }
    }

    pub fn flags(&self) -> u8 {
        self.F
    }

    pub fn zero(&self) -> bool {
        self.zero
    }

    pub fn subtract(&self) -> bool {
        self.subtract
    }

    pub fn half_carry(&self) -> bool {
        self.half_carry
    }

    pub fn carry(&self) -> bool {
        self.carry
    }
}

impl RegisterOps<Reg8, u8> for RegisterFile {
    fn read(&self, reg: Reg8) -> u8 {
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

    fn write(&mut self, reg: Reg8, value: u8) {
        match reg {
            Reg8::A => self.A = value,
            Reg8::F => self.F = value,
            Reg8::B => self.B = value,
            Reg8::C => self.C = value,
            Reg8::D => self.D = value,
            Reg8::E => self.E = value,
            Reg8::H => self.H = value,
            Reg8::L => self.L = value,
        }
    }
}

impl RegisterOps<Reg16, u16> for RegisterFile {
    fn read(&self, reg: Reg16) -> u16 {
        match reg {
            Reg16::AF => (self.F as u16) << 8 | self.A as u16,
            Reg16::BC => (self.C as u16) << 8 | self.B as u16,
            Reg16::DE => (self.E as u16) << 8 | self.D as u16,
            Reg16::HL => (self.L as u16) << 8 | self.H as u16,
            Reg16::SP => self.SP,
            Reg16::PC => self.PC,
        }
    }

    fn write(&mut self, reg: Reg16, value: u16) {
        match reg {
            Reg16::AF => {
                self.A = value as u8;
                self.F = (value >> 8) as u8;
            }
            Reg16::BC => {
                self.B = value as u8;
                self.C = (value >> 8) as u8;
            }
            Reg16::DE => {
                self.D = value as u8;
                self.E = (value >> 8) as u8;
            }
            Reg16::HL => {
                self.H = value as u8;
                self.L = (value >> 8) as u8;
            }
            Reg16::PC => self.PC = value,
            Reg16::SP => self.SP = value,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn combined_regs() {
        let mut regs = RegisterFile::new();

        regs.write(Reg8::A, 0x10);
        regs.write(Reg8::F, 0xFF);
        assert_eq!(regs.read(Reg16::AF), 0xFF10);

        regs.write(Reg16::BC, 0xBEEF);
        assert_eq!(regs.read(Reg8::B), 0xEF);
        assert_eq!(regs.read(Reg8::C), 0xBE);
    }

    #[test]
    fn flags() {
        let mut registers = RegisterFile::new();

        registers.set(Flag::Zero, true);
        assert!(registers.zero());

        registers.clear(Flag::Zero);
        assert!(!registers.zero());
    }
}
