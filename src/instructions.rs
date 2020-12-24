use std::convert::TryInto;

use crate::registers::{Reg16, Reg8};

/// A single argument to an instruction.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Arg {
    /// 8-bit register
    Reg8(Reg8),

    /// 16-bit register
    Reg16(Reg16),

    /// 8-bit immediate
    Imm8(u8),

    /// 8-bit signed immediate
    Imm8i(i8),

    /// 16-bit immediate
    Imm16(u16),

    /// Memory address (register)
    Mem(Reg16),

    /// Memory address (immediate)
    MemImm(u16),

    /// Memory address in HL
    MemHl,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Cond {
    None,
    NotZero,
    Zero,
    NotCarry,
    Carry,
}

/// Represents a single CPU instruction.
///
/// Tuple contains either: (source) or (dest) or (dest, source)
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Instruction {
    /// Load an 8-bit or 16-bit value from src into dest
    /// src: Imm8 or Reg8 or Imm16 or Mem
    /// dst: Reg8 or Reg16 or Mem
    Ld(Arg, Arg),

    /// Load an 8-bit value into register A
    /// src: Imm8 or Mem or MemImm
    LdA(Arg),

    /// Load A into Arg
    /// dst: Reg8 or Mem or MemImm
    LdArgA(Arg),

    /// Load value at address (0xFF00 + C) into A
    /// Same as: LD A, ($FF00 + C)
    LdAMemC,

    /// Load A into 0xFF00 + C
    LdMemCA,

    /// Load value at address (HL) into A, then decrement HL
    LddAMemHl,

    /// Load A into address (HL), then decrement HL
    LddMemHlA,

    /// Load value at address (HL) into A, then increment HL
    LdiAMemHl,

    /// Load A into address (HL), then decrement HL
    LdiMemHlA,

    /// Load value at address (0xFF00 + Imm8) into A
    LdhA(u8),

    /// Load A into address (0xFF00 + Imm8)
    LdhMemImmA(u8),

    /// Load HL into SP
    LdSpHl,

    /// Load SP + Imm8i into HL
    ///
    /// Flags:
    ///     * Zero: reset
    ///     * Subtract: reset
    ///     * HalfCarry: set or reset
    ///     * Carry: set or reset
    LdHlSpImm8i(i8),

    /// Load SP into address (MemImm)
    LdMemImmSp(u16),

    /// Push Reg16 (register pair) onto stack
    PushReg16(Reg16),

    /// Pop 2 bytes off the stack into Reg16
    PopReg16(Reg16),

    /// Add Reg8 or Imm8 or value at address (Reg16) to A
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: set if carry from bit 3
    ///     * Carry: set if carry from bit 7
    Add(Arg),

    /// Add carry flag **and** Reg8 or Imm8 or value at address (Reg16) to A
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: set if carry from bit 3
    ///     * Carry: set if carry from bit 7
    Adc(Arg),

    /// Subtract Reg8 or Imm8 or value at address (Reg16) from A
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: set
    ///     * HalfCarry: set if no borrow from bit 4
    ///     * Carry: set if no borrow
    Sub(Arg),

    /// Subtract carry flag **and** Reg8 or Imm8 or value at address (Reg16) from A
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: set
    ///     * HalfCarry: set if no borrow from bit 4
    ///     * Carry: set if no borrow
    Sbc(Arg),

    /// AND Reg8 or Imm8 or value at address (Reg16) with A.
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: set
    ///     * Carry: reset
    And(Arg),

    /// OR Reg8 or Imm8 or value at address (Reg16) with A.
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: reset
    Or(Arg),

    /// XOR Reg8 or Imm8 or value at address (HL) with A.
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: reset
    Xor(Arg),

    /// Compare A with Reg8 or Imm8 or value at address (HL)
    ///
    /// Note: This is equivalent to `SUB A, n`, but with results
    /// thrown away.
    ///
    /// Flags:
    ///     * Zero: set if result 0 (i.e., A == n)
    ///     * Subtract: set
    ///     * HalfCarry: set if no borrow from bit 4
    ///     * Carry: set for no borrow (i.e., A < n)
    Cp(Arg),

    /// Increment Reg8 or Reg16 or value at address (HL)
    ///
    /// **Note:** Reg16 variant does not affect flags
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: set if carry from bit 3
    ///     * Carry: not affected
    Inc(Arg),

    /// Decrement Reg8 or Reg16 or value at address (HL)
    ///
    /// **Note:** Reg16 variant does not affect flags
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: set
    ///     * HalfCarry: set if no borrow from bit 4
    ///     * Carry: not affected
    Dec(Arg),

    /// Add Reg16 to HL.
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: set if carry from bit 11
    ///     * Carry: set if carry from bit 15
    AddHlReg16(Reg16),

    /// Add Imm8i to SP.
    ///
    /// Flags:
    ///     * Zero: reset
    ///     * Subtract: reset
    ///     * HalfCarry: set if carry from bit 11
    ///     * Carry: set if carry from bit 15
    AddSpImm8i(i8),

    /// Swap upper & lower nibbles of Reg8 or value at memory address (Reg16)
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: reset
    Swap(Arg),

    /// Adjusts register A to correct BCD representation.
    ///
    /// Flags:
    ///     * Zero: set if register A is 0
    ///     * Subtract: not affected
    ///     * HalfCarry: reset
    ///     * Carry: set or reset according to operation
    Daa,

    /// Complement the carry flag
    ///
    /// Flags:
    ///     * Zero: not affected
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: complemented
    Ccf,

    /// Set the carry flag
    ///
    /// Flags:
    ///     * Zero: not affected
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: set
    Scf,

    /// NOP
    Nop,

    /// Power down the CPU until an interrupt occurs
    Halt,

    /// Halt CPU & LCD until an interrupt occurs
    Stop,

    /// Disables interrupts **after** this instruction completes
    Di,

    /// Enables interrupts **after** this instruction completes
    Ei,

    /// Restart
    ///
    /// Push current address to stack, then jump to address 0x0000 + n
    ///
    /// n must be one of: [0x00, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38]
    Rst(u8),

    /// Rotate A left. Place old bit 7 in carry flag.
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: contains old bit 7
    Rlca,

    /// Rotate A left through carry flag.
    ///
    /// e.g., new bit 0 of A = carry flag
    ///       new carry flag = bit 7 of A
    ///       new bit 7 of A = bit 6 of A
    ///       ..etc
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: contains old bit 7
    Rla,

    /// Rotate A right. Place old bit 0 in carry flag.
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: contains old bit 0
    Rrca,

    /// Rotate A right through carry flag.
    ///
    /// e.g., new bit 7 of A = carry flag
    ///       new carry flag = bit 0 of A
    ///       new bit 6 of A = bit 7 of A
    ///       ..etc
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: contains old bit 0
    Rra,

    /// Rotate Reg8 or (HL) left. Place old bit 7 in carry flag.
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: contains old bit 7
    Rlc(Arg),

    /// Rotate Reg8 or (HL) left through carry flag.
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: contains old bit 7
    Rl(Arg),

    /// Rotate Reg8 or (HL) right. Place old bit 0 in carry flag.
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: contains old bit 0
    Rrc(Arg),

    /// Rotate Reg8 or (HL) right through carry flag.
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: contains old bit 0
    Rr(Arg),

    /// Shift Reg8 or (HL) left into carry.
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: contains old bit 7
    Sla(Arg),

    /// Shift Reg8 or (HL) right into carry.
    /// Note: MSB does not change.
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: contains old bit 0
    Sra(Arg),

    /// Shift Reg8 or (HL) right into carry.
    /// Note: MSB is set to 0.
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: contains old bit 0
    Srl(Arg),

    /// Test bit `b` in Reg8 or (HL).
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: set
    ///     * Carry: not affected
    Bit(Arg, u8),

    /// Set bit `b` in Reg8 or (HL).
    ///
    /// Flags: None
    Set(Arg, u8),

    /// Reset bit `b` in Reg8 or (HL).
    ///
    /// Flags: None
    Res(Arg, u8),

    /// Jump to address `Addr`
    /// If `Cond` != `Cond::None`, jump has a condition.
    Jp(u16, Cond),

    /// Jump to address (HL)
    /// If `Cond` != `Cond::None`, jump has a condition.
    JpHl,

    /// Add `n` to current address and jump to it
    /// If `Cond` != `Cond::None`, jump has a condition.
    Jr(i8, Cond),

    /// Push next instruction address to stack and jump to address.
    /// If `Cond` != `Cond::None`, jump has a condition.
    Call(u16, Cond),

    /// Pop two bytes from stack & jump to the address.
    ///
    /// NotZero: Pop two bytes from stack & jump to the address if Z flag is reset.
    /// Zero: Pop two bytes from stack & jump to the address if Z flag is set.
    /// NotCarry: Pop two bytes from stack & jump to the address if C flag is reset.
    /// Carry: Pop two bytes from stack & jump to the address if C flag is set.
    Ret(Cond),

    /// Pop two bytes from stack & jump to the address, **then** enable interrupts.
    RetI,
}

/// Number of cycles required
/// If this is a conditional, second arg is if the path is not taken (faster)
#[derive(Debug, PartialEq)]
pub struct Cycles(pub u8, pub u8);

impl Cycles {
    pub fn taken(&self) -> u8 {
        self.0
    }

    pub fn not_taken(&self) -> u8 {
        self.1
    }
}

impl From<u8> for Cycles {
    fn from(count: u8) -> Self {
        Self(count, count)
    }
}

impl Instruction {
    /// Decode a single instruction from a 3-byte slice.
    /// Returns: instruction, instruction size, cycle count
    pub fn decode(data: &[u8]) -> (Self, u8, Cycles) {
        use Instruction::*;

        // Safely extract the next arg as 8-bit and 16-bit immediates
        // If we are at the end of the memory range, we will return 0.
        let arg8 = if data.len() >= 2 { data[1] } else { 0 };
        let arg16 = if data.len() >= 3 {
            u16::from_le_bytes(data[1..3].try_into().unwrap())
        } else {
            0
        };

        let (inst, size, cycles) = match data[0] {
            0x00 => (Nop, 1, 4.into()),

            // Load
            0x01 => (Ld(Arg::Reg16(Reg16::BC), Arg::Imm16(arg16)), 3, 12.into()),
            0x11 => (Ld(Arg::Reg16(Reg16::DE), Arg::Imm16(arg16)), 3, 12.into()),
            0x21 => (Ld(Arg::Reg16(Reg16::HL), Arg::Imm16(arg16)), 3, 12.into()),
            0x31 => (Ld(Arg::Reg16(Reg16::SP), Arg::Imm16(arg16)), 3, 12.into()),
            0x06 => (Ld(Arg::Reg8(Reg8::B), Arg::Imm8(arg8)), 2, 8.into()),
            0x16 => (Ld(Arg::Reg8(Reg8::D), Arg::Imm8(arg8)), 2, 8.into()),
            0x26 => (Ld(Arg::Reg8(Reg8::H), Arg::Imm8(arg8)), 2, 8.into()),
            0x36 => (Ld(Arg::Mem(Reg16::HL), Arg::Imm8(arg8)), 2, 12.into()),
            0x0A => (Ld(Arg::Reg8(Reg8::A), Arg::Mem(Reg16::BC)), 1, 8.into()),
            0x1A => (Ld(Arg::Reg8(Reg8::A), Arg::Mem(Reg16::DE)), 1, 8.into()),
            0x2A => (LdiAMemHl, 1, 8.into()),
            0x3A => (LddAMemHl, 1, 8.into()),
            0x0E => (Ld(Arg::Reg8(Reg8::C), Arg::Imm8(arg8)), 2, 8.into()),
            0x1E => (Ld(Arg::Reg8(Reg8::E), Arg::Imm8(arg8)), 2, 8.into()),
            0x2E => (Ld(Arg::Reg8(Reg8::L), Arg::Imm8(arg8)), 2, 8.into()),
            0x3E => (Ld(Arg::Reg8(Reg8::A), Arg::Imm8(arg8)), 2, 8.into()),
            0xE0 => (LdhMemImmA(arg8), 2, 12.into()),
            0xF0 => (LdhA(arg8), 2, 12.into()),

            // Xor
            0xA8 => (Xor(Arg::Reg8(Reg8::B)), 1, 4.into()),
            0xA9 => (Xor(Arg::Reg8(Reg8::C)), 1, 4.into()),
            0xAA => (Xor(Arg::Reg8(Reg8::D)), 1, 4.into()),
            0xAB => (Xor(Arg::Reg8(Reg8::E)), 1, 4.into()),
            0xAC => (Xor(Arg::Reg8(Reg8::H)), 1, 4.into()),
            0xAD => (Xor(Arg::Reg8(Reg8::L)), 1, 4.into()),
            0xAE => (Xor(Arg::MemHl), 1, 8.into()),
            0xAF => (Xor(Arg::Reg8(Reg8::A)), 1, 4.into()),
            0xEE => (Xor(Arg::Imm8(arg8)), 2, 8.into()),

            // Inc
            0x03 => (Inc(Arg::Reg16(Reg16::BC)), 1, 8.into()),
            0x13 => (Inc(Arg::Reg16(Reg16::DE)), 1, 8.into()),
            0x23 => (Inc(Arg::Reg16(Reg16::HL)), 1, 8.into()),
            0x33 => (Inc(Arg::Reg16(Reg16::SP)), 1, 8.into()),
            0x04 => (Inc(Arg::Reg8(Reg8::B)), 1, 4.into()),
            0x14 => (Inc(Arg::Reg8(Reg8::D)), 1, 4.into()),
            0x24 => (Inc(Arg::Reg8(Reg8::H)), 1, 4.into()),
            0x34 => (Inc(Arg::MemHl), 1, 12.into()),
            0x0C => (Inc(Arg::Reg8(Reg8::C)), 1, 4.into()),
            0x1C => (Inc(Arg::Reg8(Reg8::E)), 1, 4.into()),
            0x2C => (Inc(Arg::Reg8(Reg8::L)), 1, 4.into()),
            0x3C => (Inc(Arg::Reg8(Reg8::A)), 1, 4.into()),

            // Dec
            0x05 => (Dec(Arg::Reg8(Reg8::B)), 1, 4.into()),
            0x15 => (Dec(Arg::Reg8(Reg8::D)), 1, 4.into()),
            0x25 => (Dec(Arg::Reg8(Reg8::H)), 1, 4.into()),
            0x35 => (Dec(Arg::MemHl), 1, 12.into()),
            0x0B => (Dec(Arg::Reg16(Reg16::BC)), 1, 8.into()),
            0x1B => (Dec(Arg::Reg16(Reg16::DE)), 1, 8.into()),
            0x2B => (Dec(Arg::Reg16(Reg16::HL)), 1, 8.into()),
            0x3B => (Dec(Arg::Reg16(Reg16::SP)), 1, 8.into()),
            0x0D => (Dec(Arg::Reg8(Reg8::C)), 1, 4.into()),
            0x1D => (Dec(Arg::Reg8(Reg8::E)), 1, 4.into()),
            0x2D => (Dec(Arg::Reg8(Reg8::L)), 1, 4.into()),
            0x3D => (Dec(Arg::Reg8(Reg8::A)), 1, 4.into()),

            // Cp
            0xBF => (Cp(Arg::Reg8(Reg8::A)), 1, 4.into()),
            0xB8 => (Cp(Arg::Reg8(Reg8::B)), 1, 4.into()),
            0xB9 => (Cp(Arg::Reg8(Reg8::C)), 1, 4.into()),
            0xBA => (Cp(Arg::Reg8(Reg8::D)), 1, 4.into()),
            0xBB => (Cp(Arg::Reg8(Reg8::E)), 1, 4.into()),
            0xBC => (Cp(Arg::Reg8(Reg8::H)), 1, 4.into()),
            0xBD => (Cp(Arg::Reg8(Reg8::L)), 1, 4.into()),
            0xBE => (Cp(Arg::MemHl), 1, 8.into()),
            0xFE => (Cp(Arg::Imm8(arg8)), 2, 8.into()),

            // Jump
            0x18 => {
                let offset = arg8 as i8;
                (Jr(offset, Cond::None), 2, Cycles(12, 8))
            }
            0x28 => {
                let offset = arg8 as i8;
                (Jr(offset, Cond::Zero), 2, Cycles(12, 8))
            }
            0xC3 => {
                let addr = arg16;
                (Jp(addr, Cond::None), 3, 16.into())
            }

            other => panic!("Unknown instruction: {}", other),
        };

        (inst, size, cycles)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use Instruction::*;

    #[test]
    fn test_decode_ld() {
        // Vector of (input instruction, expected decoded, size, cycle count)
        #[rustfmt::skip]
        let test_vectors: &[([u8; 3], Instruction, u8, Cycles)] = &[
            ([0x01, 0x34, 0x12], Ld(Arg::Reg16(Reg16::BC), Arg::Imm16(0x1234)), 3, 12.into()),
            ([0x11, 0x34, 0x12], Ld(Arg::Reg16(Reg16::DE), Arg::Imm16(0x1234)), 3, 12.into()),
            ([0x21, 0x34, 0x12], Ld(Arg::Reg16(Reg16::HL), Arg::Imm16(0x1234)), 3, 12.into()),
            ([0x31, 0x34, 0x12], Ld(Arg::Reg16(Reg16::SP), Arg::Imm16(0x1234)), 3, 12.into()),

            ([0x06, 0x34, 0x00], Ld(Arg::Reg8(Reg8::B),  Arg::Imm8(0x34)), 2, 8.into()),
            ([0x16, 0x34, 0x00], Ld(Arg::Reg8(Reg8::D),  Arg::Imm8(0x34)), 2, 8.into()),
            ([0x26, 0x34, 0x00], Ld(Arg::Reg8(Reg8::H),  Arg::Imm8(0x34)), 2, 8.into()),
            ([0x36, 0x34, 0x00], Ld(Arg::Mem(Reg16::HL), Arg::Imm8(0x34)), 2, 12.into()),

            ([0x0A, 0x34, 0x00], Ld(Arg::Reg8(Reg8::A),  Arg::Mem(Reg16::BC)), 1, 8.into()),
            ([0x1A, 0x34, 0x00], Ld(Arg::Reg8(Reg8::A),  Arg::Mem(Reg16::DE)), 1, 8.into()),
            ([0x2A, 0x34, 0x00], LdiAMemHl, 1, 8.into()),
            ([0x3A, 0x34, 0x00], LddAMemHl, 1, 8.into()),

            ([0x0E, 0x34, 0x00], Ld(Arg::Reg8(Reg8::C),  Arg::Imm8(0x34)), 2, 8.into()),
            ([0x1E, 0x34, 0x00], Ld(Arg::Reg8(Reg8::E),  Arg::Imm8(0x34)), 2, 8.into()),
            ([0x2E, 0x34, 0x00], Ld(Arg::Reg8(Reg8::L),  Arg::Imm8(0x34)), 2, 8.into()),
            ([0x3E, 0x34, 0x00], Ld(Arg::Reg8(Reg8::A),  Arg::Imm8(0x34)), 2, 8.into()),

            ([0xE0, 0x34, 0x00], LdhMemImmA(0x34), 2, 12.into()),
            ([0xF0, 0x34, 0x00], LdhA(0x34), 2, 12.into()),
        ];

        for (input, expected, expected_size, expected_cycles) in test_vectors {
            let (inst, size, cycles) = Instruction::decode(&input[..]);
            assert_eq!(expected, &inst);
            assert_eq!(expected_size, &size);
            assert_eq!(expected_cycles, &cycles);
        }
    }
}
