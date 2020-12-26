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

    /// Memory address in [HL](Reg16::HL)
    MemHl,
}

impl From<u8> for Arg {
    fn from(n: u8) -> Self {
        Self::Imm8(n)
    }
}

impl From<i8> for Arg {
    fn from(n: i8) -> Self {
        Self::Imm8i(n)
    }
}

impl From<Reg8> for Arg {
    fn from(r: Reg8) -> Self {
        Self::Reg8(r)
    }
}

impl From<Reg16> for Arg {
    fn from(r: Reg16) -> Self {
        Self::Reg16(r)
    }
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
    /// Load an 8-bit or 16-bit value from `src` into `dst`
    ///
    /// * src: [Imm8](Arg::Imm8) or [Reg8](Arg::Reg8) or [Imm16](Arg::Imm16) or [Mem](Arg::Mem)
    /// * dst: [Reg8](Arg::Reg8) or [Reg16](Arg::Reg16) or [Mem](Arg::Mem)
    Ld { dst: Arg, src: Arg },

    /// Load value at address (0xFF00 + C) into [A](Reg8::A)
    ///
    /// Same as: LD A, ($FF00 + C)
    LdAMemC,

    /// Load [A](Reg8::A) into 0xFF00 + C
    LdMemCA,

    /// Load value at address ([HL](Reg16::HL)) into [A](Reg8::A), then decrement [HL](Reg16::HL)
    LddAMemHl,

    /// Load [A](Reg8::A) into address ([HL](Reg16::HL)), then decrement [HL](Reg16::HL)
    LddMemHlA,

    /// Load value at address ([HL](Reg16::HL)) into [A](Reg8::A), then increment [HL](Reg16::HL)
    LdiAMemHl,

    /// Load [A](Reg8::A) into address ([HL](Reg16::HL)), then increment [HL](Reg16::HL)
    LdiMemHlA,

    /// Load value at address (0xFF00 + [Imm8](Arg::Imm8)) into [A](Reg8::A)
    LdhA { offset: u8 },

    /// Load [A](Reg8::A) into address (0xFF00 + [Imm8](Arg::Imm8))
    Ldh { offset: u8 },

    /// Load SP + Imm8i into [HL](Reg16::HL)
    ///
    /// ### Flags
    ///
    /// * Zero: reset
    /// * Subtract: reset
    /// * HalfCarry: set or reset
    /// * Carry: set or reset
    LdHlSpImm8i { offset: i8 },

    /// Load SP into address `addr`
    LdMemImmSp { addr: u16 },

    /// Push [Reg16](Arg::Reg16) (register pair) onto stack
    Push { src: Reg16 },

    /// Pop 2 bytes off the stack into [Reg16](Arg::Reg16)
    Pop { dst: Reg16 },

    /// Add [Reg8](Arg::Reg8) or [Imm8](Arg::Imm8) or value at address ([Reg16](Arg::Reg16)) to [A](Reg8::A)
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0
    /// * Subtract: reset
    /// * HalfCarry: set if carry from bit 3
    /// * Carry: set if carry from bit 7
    Add { src: Arg },

    /// Add carry flag **and** [Reg8](Arg::Reg8) or [Imm8](Arg::Imm8) or value at address ([Reg16](Arg::Reg16)) to [A](Reg8::A)
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0
    /// * Subtract: reset
    /// * HalfCarry: set if carry from bit 3
    /// * Carry: set if carry from bit 7
    Adc { src: Arg },

    /// Subtract [Reg8](Arg::Reg8) or [Imm8](Arg::Imm8) or value at address ([Reg16](Arg::Reg16)) from [A](Reg8::A)
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0
    /// * Subtract: set
    /// * HalfCarry: set if no borrow from bit 4
    /// * Carry: set if no borrow
    Sub { src: Arg },

    /// Subtract carry flag **and** [Reg8](Arg::Reg8) or [Imm8](Arg::Imm8) or value at address ([Reg16](Arg::Reg16)) from [A](Reg8::A)
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0
    /// * Subtract: set
    /// * HalfCarry: set if no borrow from bit 4
    /// * Carry: set if no borrow
    Sbc { src: Arg },

    /// AND [Reg8](Arg::Reg8) or [Imm8](Arg::Imm8) or value at address ([Reg16](Arg::Reg16)) with [A](Reg8::A).
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0
    /// * Subtract: reset
    /// * HalfCarry: set
    /// * Carry: reset
    And { src: Arg },

    /// OR [Reg8](Arg::Reg8) or [Imm8](Arg::Imm8) or value at address ([Reg16](Arg::Reg16)) with [A](Reg8::A).
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0
    /// * Subtract: reset
    /// * HalfCarry: reset
    /// * Carry: reset
    Or { src: Arg },

    /// XOR [Reg8](Arg::Reg8) or [Imm8](Arg::Imm8) or value at address ([HL](Reg16::HL)) with [A](Reg8::A).
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0
    /// * Subtract: reset
    /// * HalfCarry: reset
    /// * Carry: reset
    Xor { src: Arg },

    /// Compare [A](Reg8::A) with [Reg8](Arg::Reg8) or [Imm8](Arg::Imm8) or value at address ([HL](Reg16::HL))
    ///
    /// Note: This is equivalent to `SUB A, n`, but with results
    /// thrown away.
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0 (i.e., A == n)
    /// * Subtract: set
    /// * HalfCarry: set if no borrow from bit 4
    /// * Carry: set for no borrow (i.e., A < n)
    Cp { src: Arg },

    /// Increment [Reg8](Arg::Reg8) or [Reg16](Arg::Reg16) or value at address ([HL](Reg16::HL))
    ///
    /// **Note:** Reg16 variant does not affect flags
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0
    /// * Subtract: reset
    /// * HalfCarry: set if carry from bit 3
    /// * Carry: not affected
    Inc { dst: Arg },

    /// Decrement [Reg8](Arg::Reg8) or [Reg16](Arg::Reg16) or value at address ([HL](Reg16::HL))
    ///
    /// **Note:** Reg16 variant does not affect flags
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0
    /// * Subtract: set
    /// * HalfCarry: set if no borrow from bit 4
    /// * Carry: set if borrow, otherwise reset
    Dec { dst: Arg },

    /// Add [Reg16](Arg::Reg16) to [HL](Reg16::HL).
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0
    /// * Subtract: reset
    /// * HalfCarry: set if carry from bit 11
    /// * Carry: set if carry from bit 15
    AddHlReg16 { src: Reg16 },

    /// Add Imm8i to SP.
    ///
    /// ### Flags
    ///
    /// * Zero: reset
    /// * Subtract: reset
    /// * HalfCarry: set if carry from bit 11
    /// * Carry: set if carry from bit 15
    AddSpImm8i { offset: i8 },

    /// Swap upper & lower nibbles of [Reg8](Arg::Reg8) or value at memory address ([Reg16](Arg::Reg16))
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0
    /// * Subtract: reset
    /// * HalfCarry: reset
    /// * Carry: reset
    Swap { dst: Arg },

    /// Adjusts register [A](Reg8::A) to correct BCD representation.
    ///
    /// ### Flags
    ///
    /// * Zero: set if register A is 0
    /// * Subtract: not affected
    /// * HalfCarry: reset
    /// * Carry: set or reset according to operation
    Daa,

    /// Complements register [A](Reg8::A).
    ///
    /// ### Flags
    ///
    /// * Zero: not affected
    /// * Subtract: set
    /// * HalfCarry: set
    /// * Carry: not affected
    Cpl,

    /// Complement the carry flag
    ///
    /// ### Flags
    ///
    /// * Zero: not affected
    /// * Subtract: reset
    /// * HalfCarry: reset
    /// * Carry: complemented
    Ccf,

    /// Set the carry flag
    ///
    /// ### Flags
    ///
    /// * Zero: not affected
    /// * Subtract: reset
    /// * HalfCarry: reset
    /// * Carry: set
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
    Rst { offset: u8 },

    /// Rotate [A](Reg8::A) left. Place old bit 7 in carry flag.
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0
    /// * Subtract: reset
    /// * HalfCarry: reset
    /// * Carry: contains old bit 7
    Rlca,

    /// Rotate [A](Reg8::A) left through carry flag.
    ///
    /// e.g., new bit 0 of A = carry flag
    ///       new carry flag = bit 7 of A
    ///       new bit 7 of A = bit 6 of A
    ///       ..etc
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0
    /// * Subtract: reset
    /// * HalfCarry: reset
    /// * Carry: contains old bit 7
    Rla,

    /// Rotate [A](Reg8::A) right. Place old bit 0 in carry flag.
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0
    /// * Subtract: reset
    /// * HalfCarry: reset
    /// * Carry: contains old bit 0
    Rrca,

    /// Rotate [A](Reg8::A) right through carry flag.
    ///
    /// e.g., new bit 7 of A = carry flag
    ///       new carry flag = bit 0 of A
    ///       new bit 6 of A = bit 7 of A
    ///       ..etc
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0
    /// * Subtract: reset
    /// * HalfCarry: reset
    /// * Carry: contains old bit 0
    Rra,

    /// Rotate [Reg8](Arg::Reg8) or ([HL](Reg16::HL)) left. Place old bit 7 in carry flag.
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0
    /// * Subtract: reset
    /// * HalfCarry: reset
    /// * Carry: contains old bit 7
    Rlc { dst: Arg },

    /// Rotate [Reg8](Arg::Reg8) or ([HL](Reg16::HL)) left through carry flag.
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0
    /// * Subtract: reset
    /// * HalfCarry: reset
    /// * Carry: contains old bit 7
    Rl { dst: Arg },

    /// Rotate [Reg8](Arg::Reg8) or ([HL](Reg16::HL)) right. Place old bit 0 in carry flag.
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0
    /// * Subtract: reset
    /// * HalfCarry: reset
    /// * Carry: contains old bit 0
    Rrc { dst: Arg },

    /// Rotate [Reg8](Arg::Reg8) or ([HL](Reg16::HL)) right through carry flag.
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0
    /// * Subtract: reset
    /// * HalfCarry: reset
    /// * Carry: contains old bit 0
    Rr { dst: Arg },

    /// Shift [Reg8](Arg::Reg8) or ([HL](Reg16::HL)) left into carry.
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0
    /// * Subtract: reset
    /// * HalfCarry: reset
    /// * Carry: contains old bit 7
    Sla { dst: Arg },

    /// Shift [Reg8](Arg::Reg8) or ([HL](Reg16::HL)) right into carry.
    /// Note: MSB does not change.
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0
    /// * Subtract: reset
    /// * HalfCarry: reset
    /// * Carry: contains old bit 0
    Sra { dst: Arg },

    /// Shift [Reg8](Arg::Reg8) or ([HL](Reg16::HL)) right into carry.
    /// Note: MSB is set to 0.
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0
    /// * Subtract: reset
    /// * HalfCarry: reset
    /// * Carry: contains old bit 0
    Srl { dst: Arg },

    /// Test bit `b` in [Reg8](Arg::Reg8) or ([HL](Reg16::HL)).
    ///
    /// ### Flags
    ///
    /// * Zero: set if result 0
    /// * Subtract: reset
    /// * HalfCarry: set
    /// * Carry: not affected
    Bit { dst: Arg, bit: u8},

    /// Set bit `b` in [Reg8](Arg::Reg8) or ([HL](Reg16::HL)).
    ///
    /// Flags: None
    Set { dst: Arg, bit: u8},

    /// Reset bit `b` in [Reg8](Arg::Reg8) or ([HL](Reg16::HL)).
    ///
    /// Flags: None
    Res { dst: Arg, bit: u8},

    /// Jump to address `Addr`
    ///
    /// If `Cond` != `Cond::None`, jump has a condition.
    Jp { addr: u16, cond: Cond },

    /// Jump to address ([HL](Reg16::HL))
    JpHl,

    /// Add `n` to current address and jump to it
    ///
    /// If `Cond` != `Cond::None`, jump has a condition.
    Jr { offset: i8, cond: Cond },

    /// Push next instruction address to stack and jump to address.
    ///
    /// If `Cond` != `Cond::None`, jump has a condition.
    Call { addr: u16, cond: Cond },

    /// Pop two bytes from stack & jump to the address.
    ///
    /// ### Flags
    ///
    /// * NotZero: Pop two bytes from stack & jump to the address if Z flag is reset.
    /// * Zero: Pop two bytes from stack & jump to the address if Z flag is set.
    /// * NotCarry: Pop two bytes from stack & jump to the address if C flag is reset.
    /// * Carry: Pop two bytes from stack & jump to the address if C flag is set.
    Ret { cond: Cond },

    /// Pop two bytes from stack & jump to the address, **then** enable interrupts.
    RetI,
}

/// Number of cycles required to execute to an instruction.
///
/// If this is a conditional instruction, the second arg represents the number of
/// cycles consumed if the path is not taken (faster).
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
    /// Decode a single instruction from a slice. The slice must contain
    /// at least a single byte (opcode).
    ///
    /// In all cases, we will attempt to extract an argument from the following
    /// 2 bytes. If we are at the end of a memory region, we will return `None` for the
    /// args.
    ///
    /// Returns: instruction, instruction size, cycle count
    pub fn decode(data: &[u8]) -> (Self, u8, Cycles) {
        use Instruction::*;

        // Safely attempt to extract the next arg as 8-bit and 16-bit immediates.
        // If we are at the end of the memory range, we will return 0.
        let arg8 = data.get(1).map(|x| *x);
        let arg16 = if data.len() >= 3 {
            Some(u16::from_le_bytes(data[1..3].try_into().unwrap()))
        } else {
            None
        };

        let (inst, size, cycles) = match data[0] {
            0x00 => (Nop, 1, 4.into()),
            0x10 => (Stop, 2, 4.into()),

            // Load
            0x08 => (Ld { dst: Arg::MemImm(arg16.unwrap()), src: Reg16::SP.into() }, 3, 20.into()),
            0x02 => (Ld { dst: Arg::Mem(Reg16::BC), src: Reg8::A.into() }, 1, 8.into()),
            0x12 => (Ld { dst: Arg::Mem(Reg16::DE), src: Reg8::A.into() }, 1, 8.into()),
            0x0A => (Ld { dst: Arg::Reg8(Reg8::A), src: Arg::Mem(Reg16::BC)}, 1, 8.into()),
            0x1A => (Ld { dst: Arg::Reg8(Reg8::A), src: Arg::Mem(Reg16::DE)}, 1, 8.into()),
            0x01 => (Ld { dst: Arg::Reg16(Reg16::BC), src: Arg::Imm16(arg16.unwrap()) }, 3, 12.into()),
            0x11 => (Ld { dst: Arg::Reg16(Reg16::DE), src: Arg::Imm16(arg16.unwrap()) }, 3, 12.into()),
            0x21 => (Ld { dst: Arg::Reg16(Reg16::HL), src: Arg::Imm16(arg16.unwrap()) }, 3, 12.into()),
            0x31 => (Ld { dst: Arg::Reg16(Reg16::SP), src: Arg::Imm16(arg16.unwrap()) }, 3, 12.into()),
            0x06 => (Ld { dst: Arg::Reg8(Reg8::B), src: Arg::Imm8(arg8.unwrap()) }, 2, 8.into()),
            0x16 => (Ld { dst: Arg::Reg8(Reg8::D), src: Arg::Imm8(arg8.unwrap()) }, 2, 8.into()),
            0x26 => (Ld { dst: Arg::Reg8(Reg8::H), src: Arg::Imm8(arg8.unwrap()) }, 2, 8.into()),
            0x36 => (Ld { dst: Arg::Mem(Reg16::HL), src: Arg::Imm8(arg8.unwrap()) }, 2, 12.into()),
            0x22 => (LdiMemHlA, 1, 8.into()),
            0x32 => (LddMemHlA, 1, 8.into()),
            0x2A => (LdiAMemHl, 1, 8.into()),
            0x3A => (LddAMemHl, 1, 8.into()),
            0x0E => (Ld { dst: Arg::Reg8(Reg8::C), src: Arg::Imm8(arg8.unwrap()) }, 2, 8.into()),
            0x1E => (Ld { dst: Arg::Reg8(Reg8::E), src: Arg::Imm8(arg8.unwrap()) }, 2, 8.into()),
            0x2E => (Ld { dst: Arg::Reg8(Reg8::L), src: Arg::Imm8(arg8.unwrap()) }, 2, 8.into()),
            0x3E => (Ld { dst: Arg::Reg8(Reg8::A), src: Arg::Imm8(arg8.unwrap()) }, 2, 8.into()),
            0x40 => (Ld { dst: Arg::Reg8(Reg8::B), src: Arg::Reg8(Reg8::B) }, 1, 4.into()),
            0x50 => (Ld { dst: Arg::Reg8(Reg8::D), src: Arg::Reg8(Reg8::B) }, 1, 4.into()),
            0x60 => (Ld { dst: Arg::Reg8(Reg8::H), src: Arg::Reg8(Reg8::B) }, 1, 4.into()),
            0x70 => (Ld { dst: Arg::Mem(Reg16::HL), src: Arg::Reg8(Reg8::B) }, 1, 8.into()),
            0x41 => (Ld { dst: Arg::Reg8(Reg8::B), src: Arg::Reg8(Reg8::C) }, 1, 4.into()),
            0x51 => (Ld { dst: Arg::Reg8(Reg8::D), src: Arg::Reg8(Reg8::C) }, 1, 4.into()),
            0x61 => (Ld { dst: Arg::Reg8(Reg8::H), src: Arg::Reg8(Reg8::C) }, 1, 4.into()),
            0x71 => (Ld { dst: Arg::Mem(Reg16::HL), src: Arg::Reg8(Reg8::C) }, 1, 8.into()),
            0x42 => (Ld { dst: Arg::Reg8(Reg8::B), src: Arg::Reg8(Reg8::D) }, 1, 4.into()),
            0x52 => (Ld { dst: Arg::Reg8(Reg8::D), src: Arg::Reg8(Reg8::D) }, 1, 4.into()),
            0x62 => (Ld { dst: Arg::Reg8(Reg8::H), src: Arg::Reg8(Reg8::D) }, 1, 4.into()),
            0x72 => (Ld { dst: Arg::Mem(Reg16::HL), src: Arg::Reg8(Reg8::D) }, 1, 8.into()),
            0x43 => (Ld { dst: Arg::Reg8(Reg8::B), src: Arg::Reg8(Reg8::E) }, 1, 4.into()),
            0x53 => (Ld { dst: Arg::Reg8(Reg8::D), src: Arg::Reg8(Reg8::E) }, 1, 4.into()),
            0x63 => (Ld { dst: Arg::Reg8(Reg8::H), src: Arg::Reg8(Reg8::E) }, 1, 4.into()),
            0x73 => (Ld { dst: Arg::Mem(Reg16::HL), src: Arg::Reg8(Reg8::E) }, 1, 8.into()),
            0x44 => (Ld { dst: Arg::Reg8(Reg8::B), src: Arg::Reg8(Reg8::H) }, 1, 4.into()),
            0x54 => (Ld { dst: Arg::Reg8(Reg8::D), src: Arg::Reg8(Reg8::H) }, 1, 4.into()),
            0x64 => (Ld { dst: Arg::Reg8(Reg8::H), src: Arg::Reg8(Reg8::H) }, 1, 4.into()),
            0x74 => (Ld { dst: Arg::Mem(Reg16::HL), src: Arg::Reg8(Reg8::H) }, 1, 8.into()),
            0x45 => (Ld { dst: Arg::Reg8(Reg8::B), src: Arg::Reg8(Reg8::L) }, 1, 4.into()),
            0x55 => (Ld { dst: Arg::Reg8(Reg8::D), src: Arg::Reg8(Reg8::L) }, 1, 4.into()),
            0x65 => (Ld { dst: Arg::Reg8(Reg8::H), src: Arg::Reg8(Reg8::L) }, 1, 4.into()),
            0x75 => (Ld { dst: Arg::Mem(Reg16::HL), src: Arg::Reg8(Reg8::L) }, 1, 8.into()),
            0x46 => (Ld { dst: Arg::Reg8(Reg8::B), src: Arg::Mem(Reg16::HL) }, 1, 8.into()),
            0x56 => (Ld { dst: Arg::Reg8(Reg8::D), src: Arg::Mem(Reg16::HL) }, 1, 8.into()),
            0x66 => (Ld { dst: Arg::Reg8(Reg8::H), src: Arg::Mem(Reg16::HL) }, 1, 8.into()),
            0x76 => (Halt, 1, 4.into()),
            0x47 => (Ld { dst: Arg::Reg8(Reg8::B), src: Arg::Reg8(Reg8::A) }, 1, 4.into()),
            0x57 => (Ld { dst: Arg::Reg8(Reg8::D), src: Arg::Reg8(Reg8::A) }, 1, 4.into()),
            0x67 => (Ld { dst: Arg::Reg8(Reg8::H), src: Arg::Reg8(Reg8::A) }, 1, 4.into()),
            0x77 => (Ld { dst: Arg::Mem(Reg16::HL), src: Arg::Reg8(Reg8::A) }, 1, 8.into()),
            0x48 => (Ld { dst: Arg::Reg8(Reg8::C), src: Arg::Reg8(Reg8::B) }, 1, 4.into()),
            0x58 => (Ld { dst: Arg::Reg8(Reg8::E), src: Arg::Reg8(Reg8::B) }, 1, 4.into()),
            0x68 => (Ld { dst: Arg::Reg8(Reg8::L), src: Arg::Reg8(Reg8::B) }, 1, 4.into()),
            0x78 => (Ld { dst: Arg::Reg8(Reg8::A), src: Arg::Reg8(Reg8::B) }, 1, 4.into()),
            0x49 => (Ld { dst: Arg::Reg8(Reg8::C), src: Arg::Reg8(Reg8::C) }, 1, 4.into()),
            0x59 => (Ld { dst: Arg::Reg8(Reg8::E), src: Arg::Reg8(Reg8::C) }, 1, 4.into()),
            0x69 => (Ld { dst: Arg::Reg8(Reg8::L), src: Arg::Reg8(Reg8::C) }, 1, 4.into()),
            0x79 => (Ld { dst: Arg::Reg8(Reg8::A), src: Arg::Reg8(Reg8::C) }, 1, 4.into()),
            0x4A => (Ld { dst: Arg::Reg8(Reg8::C), src: Arg::Reg8(Reg8::D) }, 1, 4.into()),
            0x5A => (Ld { dst: Arg::Reg8(Reg8::E), src: Arg::Reg8(Reg8::D) }, 1, 4.into()),
            0x6A => (Ld { dst: Arg::Reg8(Reg8::L), src: Arg::Reg8(Reg8::D) }, 1, 4.into()),
            0x7A => (Ld { dst: Arg::Reg8(Reg8::A), src: Arg::Reg8(Reg8::D) }, 1, 4.into()),
            0x4B => (Ld { dst: Arg::Reg8(Reg8::C), src: Arg::Reg8(Reg8::E) }, 1, 4.into()),
            0x5B => (Ld { dst: Arg::Reg8(Reg8::E), src: Arg::Reg8(Reg8::E) }, 1, 4.into()),
            0x6B => (Ld { dst: Arg::Reg8(Reg8::L), src: Arg::Reg8(Reg8::E) }, 1, 4.into()),
            0x7B => (Ld { dst: Arg::Reg8(Reg8::A), src: Arg::Reg8(Reg8::E) }, 1, 4.into()),
            0x4C => (Ld { dst: Arg::Reg8(Reg8::C), src: Arg::Reg8(Reg8::H) }, 1, 4.into()),
            0x5C => (Ld { dst: Arg::Reg8(Reg8::E), src: Arg::Reg8(Reg8::H) }, 1, 4.into()),
            0x6C => (Ld { dst: Arg::Reg8(Reg8::L), src: Arg::Reg8(Reg8::H) }, 1, 4.into()),
            0x7C => (Ld { dst: Arg::Reg8(Reg8::A), src: Arg::Reg8(Reg8::H) }, 1, 4.into()),
            0x4D => (Ld { dst: Arg::Reg8(Reg8::C), src: Arg::Reg8(Reg8::L) }, 1, 4.into()),
            0x5D => (Ld { dst: Arg::Reg8(Reg8::E), src: Arg::Reg8(Reg8::L) }, 1, 4.into()),
            0x6D => (Ld { dst: Arg::Reg8(Reg8::L), src: Arg::Reg8(Reg8::L) }, 1, 4.into()),
            0x7D => (Ld { dst: Arg::Reg8(Reg8::A), src: Arg::Reg8(Reg8::L) }, 1, 4.into()),
            0x4E => (Ld { dst: Arg::Reg8(Reg8::C), src: Arg::Mem(Reg16::HL) }, 1, 8.into()),
            0x5E => (Ld { dst: Arg::Reg8(Reg8::E), src: Arg::Mem(Reg16::HL) }, 1, 8.into()),
            0x6E => (Ld { dst: Arg::Reg8(Reg8::L), src: Arg::Mem(Reg16::HL) }, 1, 8.into()),
            0x7E => (Ld { dst: Arg::Reg8(Reg8::A), src: Arg::Mem(Reg16::HL) }, 1, 8.into()),
            0x4F => (Ld { dst: Arg::Reg8(Reg8::C), src: Arg::Reg8(Reg8::A) }, 1, 4.into()),
            0x5F => (Ld { dst: Arg::Reg8(Reg8::E), src: Arg::Reg8(Reg8::A) }, 1, 4.into()),
            0x6F => (Ld { dst: Arg::Reg8(Reg8::L), src: Arg::Reg8(Reg8::A) }, 1, 4.into()),
            0x7F => (Ld { dst: Arg::Reg8(Reg8::A), src: Arg::Reg8(Reg8::A) }, 1, 4.into()),
            0xE0 => (Ldh { offset: arg8.unwrap() }, 2, 12.into()),
            0xF0 => (LdhA { offset: arg8.unwrap() }, 2, 12.into()),
            0xE2 => (LdMemCA, 2, 8.into()),
            0xF2 => (LdAMemC, 2, 8.into()),
            0xEA => (Ld { dst: Arg::MemImm(arg16.unwrap()), src: Arg::Reg8(Reg8::A) }, 3, 16.into()),
            0xFA => (Ld { dst: Arg::Reg8(Reg8::A), src: Arg::MemImm(arg16.unwrap()) }, 3, 16.into()),
            0xF8 => (LdHlSpImm8i { offset: arg8.unwrap() as i8 }, 2, 12.into()),
            0xF9 => (Ld { dst: Reg16::SP.into(), src: Reg16::SP.into() }, 1, 8.into()),

            // Misc
            0x27 => (Daa, 1, 4.into()),
            0x37 => (Scf, 1, 4.into()),
            0x2F => (Cpl, 1, 4.into()),
            0x3F => (Ccf, 1, 4.into()),

            // Rotate
            0x07 => (Rlca, 1, 4.into()),
            0x17 => (Rla, 1, 4.into()),
            0x0F => (Rrca, 1, 4.into()),
            0x1F => (Rra, 1, 4.into()),

            // Inc
            0x03 => (Inc { dst: Arg::Reg16(Reg16::BC) }, 1, 8.into()),
            0x13 => (Inc { dst: Arg::Reg16(Reg16::DE) }, 1, 8.into()),
            0x23 => (Inc { dst: Arg::Reg16(Reg16::HL) }, 1, 8.into()),
            0x33 => (Inc { dst: Arg::Reg16(Reg16::SP) }, 1, 8.into()),
            0x04 => (Inc { dst: Arg::Reg8(Reg8::B) }, 1, 4.into()),
            0x14 => (Inc { dst: Arg::Reg8(Reg8::D) }, 1, 4.into()),
            0x24 => (Inc { dst: Arg::Reg8(Reg8::H) }, 1, 4.into()),
            0x34 => (Inc { dst: Arg::MemHl }, 1, 12.into()),
            0x0C => (Inc { dst: Arg::Reg8(Reg8::C) }, 1, 4.into()),
            0x1C => (Inc { dst: Arg::Reg8(Reg8::E) }, 1, 4.into()),
            0x2C => (Inc { dst: Arg::Reg8(Reg8::L) }, 1, 4.into()),
            0x3C => (Inc { dst: Arg::Reg8(Reg8::A) }, 1, 4.into()),

            // Dec
            0x05 => (Dec { dst: Arg::Reg8(Reg8::B) }, 1, 4.into()),
            0x15 => (Dec { dst: Arg::Reg8(Reg8::D) }, 1, 4.into()),
            0x25 => (Dec { dst: Arg::Reg8(Reg8::H) }, 1, 4.into()),
            0x35 => (Dec { dst: Arg::MemHl }, 1, 12.into()),
            0x0B => (Dec { dst: Arg::Reg16(Reg16::BC) }, 1, 8.into()),
            0x1B => (Dec { dst: Arg::Reg16(Reg16::DE) }, 1, 8.into()),
            0x2B => (Dec { dst: Arg::Reg16(Reg16::HL) }, 1, 8.into()),
            0x3B => (Dec { dst: Arg::Reg16(Reg16::SP) }, 1, 8.into()),
            0x0D => (Dec { dst: Arg::Reg8(Reg8::C) }, 1, 4.into()),
            0x1D => (Dec { dst: Arg::Reg8(Reg8::E) }, 1, 4.into()),
            0x2D => (Dec { dst: Arg::Reg8(Reg8::L) }, 1, 4.into()),
            0x3D => (Dec { dst: Arg::Reg8(Reg8::A) }, 1, 4.into()),

            // Add
            0x09 => (AddHlReg16 { src: Reg16::BC }, 1, 8.into()),
            0x19 => (AddHlReg16 { src: Reg16::DE }, 1, 8.into()),
            0x29 => (AddHlReg16 { src: Reg16::HL }, 1, 8.into()),
            0x39 => (AddHlReg16 { src: Reg16::SP }, 1, 8.into()),
            0x80 => (Add { src: Arg::Reg8(Reg8::B) }, 1, 4.into()),
            0x81 => (Add { src: Arg::Reg8(Reg8::C) }, 1, 4.into()),
            0x82 => (Add { src: Arg::Reg8(Reg8::D) }, 1, 4.into()),
            0x83 => (Add { src: Arg::Reg8(Reg8::E) }, 1, 4.into()),
            0x84 => (Add { src: Arg::Reg8(Reg8::H) }, 1, 4.into()),
            0x85 => (Add { src: Arg::Reg8(Reg8::L) }, 1, 4.into()),
            0x86 => (Add { src: Arg::MemHl }, 1, 8.into()),
            0x87 => (Add { src: Arg::Reg8(Reg8::A) }, 1, 4.into()),
            0xC6 => (Add { src: arg8.unwrap().into() }, 2, 8.into()),
            0x88 => (Adc { src: Arg::Reg8(Reg8::B) }, 1, 4.into()),
            0x89 => (Adc { src: Arg::Reg8(Reg8::C) }, 1, 4.into()),
            0x8A => (Adc { src: Arg::Reg8(Reg8::D) }, 1, 4.into()),
            0x8B => (Adc { src: Arg::Reg8(Reg8::E) }, 1, 4.into()),
            0x8C => (Adc { src: Arg::Reg8(Reg8::H) }, 1, 4.into()),
            0x8D => (Adc { src: Arg::Reg8(Reg8::L) }, 1, 4.into()),
            0x8E => (Adc { src: Arg::MemHl }, 1, 8.into()),
            0x8F => (Adc { src: Arg::Reg8(Reg8::A) }, 1, 4.into()),
            0xCE => (Adc { src: arg8.unwrap().into() }, 2, 8.into()),

            // Sub
            0x90 => (Sub { src: Arg::Reg8(Reg8::B) }, 1, 4.into()),
            0x91 => (Sub { src: Arg::Reg8(Reg8::C) }, 1, 4.into()),
            0x92 => (Sub { src: Arg::Reg8(Reg8::D) }, 1, 4.into()),
            0x93 => (Sub { src: Arg::Reg8(Reg8::E) }, 1, 4.into()),
            0x94 => (Sub { src: Arg::Reg8(Reg8::H) }, 1, 4.into()),
            0x95 => (Sub { src: Arg::Reg8(Reg8::L) }, 1, 4.into()),
            0x96 => (Sub { src: Arg::MemHl }, 1, 8.into()),
            0x97 => (Sub { src: Arg::Reg8(Reg8::A) }, 1, 4.into()),
            0x98 => (Sbc { src: Arg::Reg8(Reg8::B) }, 1, 4.into()),
            0x99 => (Sbc { src: Arg::Reg8(Reg8::C) }, 1, 4.into()),
            0x9A => (Sbc { src: Arg::Reg8(Reg8::D) }, 1, 4.into()),
            0x9B => (Sbc { src: Arg::Reg8(Reg8::E) }, 1, 4.into()),
            0x9C => (Sbc { src: Arg::Reg8(Reg8::H) }, 1, 4.into()),
            0x9D => (Sbc { src: Arg::Reg8(Reg8::L) }, 1, 4.into()),
            0x9E => (Sbc { src: Arg::MemHl }, 1, 8.into()),
            0x9F => (Sbc { src: Arg::Reg8(Reg8::A) }, 1, 4.into()),

            // And
            0xA0 => (And { src: Arg::Reg8(Reg8::B) }, 1, 4.into()),
            0xA1 => (And { src: Arg::Reg8(Reg8::C) }, 1, 4.into()),
            0xA2 => (And { src: Arg::Reg8(Reg8::D) }, 1, 4.into()),
            0xA3 => (And { src: Arg::Reg8(Reg8::E) }, 1, 4.into()),
            0xA4 => (And { src: Arg::Reg8(Reg8::H) }, 1, 4.into()),
            0xA5 => (And { src: Arg::Reg8(Reg8::L) }, 1, 4.into()),
            0xA6 => (And { src: Arg::MemHl }, 1, 8.into()),
            0xA7 => (And { src: Arg::Reg8(Reg8::A) }, 1, 4.into()),

            // Xor
            0xA8 => (Xor { src: Arg::Reg8(Reg8::B) }, 1, 4.into()),
            0xA9 => (Xor { src: Arg::Reg8(Reg8::C) }, 1, 4.into()),
            0xAA => (Xor { src: Arg::Reg8(Reg8::D) }, 1, 4.into()),
            0xAB => (Xor { src: Arg::Reg8(Reg8::E) }, 1, 4.into()),
            0xAC => (Xor { src: Arg::Reg8(Reg8::H) }, 1, 4.into()),
            0xAD => (Xor { src: Arg::Reg8(Reg8::L) }, 1, 4.into()),
            0xAE => (Xor { src: Arg::MemHl }, 1, 8.into()),
            0xAF => (Xor { src: Arg::Reg8(Reg8::A) }, 1, 4.into()),
            0xEE => (Xor { src: Arg::Imm8(arg8.unwrap()) }, 2, 8.into()),

            // Or
            0xB0 => (Or { src: Arg::Reg8(Reg8::B) }, 1, 4.into()),
            0xB1 => (Or { src: Arg::Reg8(Reg8::C) }, 1, 4.into()),
            0xB2 => (Or { src: Arg::Reg8(Reg8::D) }, 1, 4.into()),
            0xB3 => (Or { src: Arg::Reg8(Reg8::E) }, 1, 4.into()),
            0xB4 => (Or { src: Arg::Reg8(Reg8::H) }, 1, 4.into()),
            0xB5 => (Or { src: Arg::Reg8(Reg8::L) }, 1, 4.into()),
            0xB6 => (Or { src: Arg::MemHl }, 1, 8.into()),
            0xB7 => (Or { src: Arg::Reg8(Reg8::A) }, 1, 4.into()),

            // Cp
            0xBF => (Cp { src: Arg::Reg8(Reg8::A) }, 1, 4.into()),
            0xB8 => (Cp { src: Arg::Reg8(Reg8::B) }, 1, 4.into()),
            0xB9 => (Cp { src: Arg::Reg8(Reg8::C) }, 1, 4.into()),
            0xBA => (Cp { src: Arg::Reg8(Reg8::D) }, 1, 4.into()),
            0xBB => (Cp { src: Arg::Reg8(Reg8::E) }, 1, 4.into()),
            0xBC => (Cp { src: Arg::Reg8(Reg8::H) }, 1, 4.into()),
            0xBD => (Cp { src: Arg::Reg8(Reg8::L) }, 1, 4.into()),
            0xBE => (Cp { src: Arg::MemHl }, 1, 8.into()),
            0xFE => (Cp { src: Arg::Imm8(arg8.unwrap()) }, 2, 8.into()),

            // Push/pop
            0xC1 => (Pop  { dst: Reg16::BC.into() }, 1, 12.into()),
            0xD1 => (Pop  { dst: Reg16::DE.into() }, 1, 12.into()),
            0xE1 => (Pop  { dst: Reg16::HL.into() }, 1, 12.into()),
            0xF1 => (Pop  { dst: Reg16::AF.into() }, 1, 12.into()),
            0xC5 => (Push { src: Reg16::BC.into() }, 1, 16.into()),
            0xD5 => (Push { src: Reg16::DE.into() }, 1, 16.into()),
            0xE5 => (Push { src: Reg16::HL.into() }, 1, 16.into()),
            0xF5 => (Push { src: Reg16::AF.into() }, 1, 16.into()),

            // Jump
            0x18 => (Jr { offset: arg8.unwrap() as i8, cond: Cond::None }, 2, Cycles(12, 8)),
            0x20 => (Jr { offset: arg8.unwrap() as i8, cond: Cond::NotZero }, 2, Cycles(12, 8)),
            0x28 => (Jr { offset: arg8.unwrap() as i8, cond: Cond::Zero }, 2, Cycles(12, 8)),
            0x30 => (Jr { offset: arg8.unwrap() as i8, cond: Cond::NotCarry }, 2, Cycles(12, 8)),
            0x38 => (Jr { offset: arg8.unwrap() as i8, cond: Cond::Carry }, 2, Cycles(12, 8)),
            0xC2 => (Jp { addr: arg16.unwrap(), cond: Cond::NotZero }, 3, Cycles(16, 12)),
            0xCA => (Jp { addr: arg16.unwrap(), cond: Cond::Zero }, 3, Cycles(16, 12)),
            0xD2 => (Jp { addr: arg16.unwrap(), cond: Cond::NotCarry }, 3, Cycles(16, 12)),
            0xDA => (Jp { addr: arg16.unwrap(), cond: Cond::Carry }, 3, Cycles(16, 12)),
            0xC3 => (Jp { addr: arg16.unwrap(), cond: Cond::None }, 3, 16.into()),
            0xE9 => (JpHl, 1, 4.into()),

            // Call
            0xC4 => (Call { addr: arg16.unwrap(), cond: Cond::NotZero }, 3, Cycles(24, 12)),
            0xD4 => (Call { addr: arg16.unwrap(), cond: Cond::NotCarry }, 3, Cycles(24, 12)),
            0xCC => (Call { addr: arg16.unwrap(), cond: Cond::Zero }, 3, Cycles(24, 12)),
            0xCD => (Call { addr: arg16.unwrap(), cond: Cond::None }, 3, 24.into()),
            0xDC => (Call { addr: arg16.unwrap(), cond: Cond::Carry }, 3, Cycles(24, 12)),

            // Ret
            0xC0 => (Ret { cond: Cond::NotZero }, 1, Cycles(20, 8)),
            0xC8 => (Ret { cond: Cond::Zero }, 1, Cycles(20, 8)),
            0xC9 => (Ret { cond: Cond::None }, 1, 16.into()),
            0xD0 => (Ret { cond: Cond::NotCarry }, 1, Cycles(20, 8)),
            0xD8 => (Ret { cond: Cond::Carry }, 1, Cycles(20, 8)),
            0xD9 => (RetI, 1, 16.into()),

            // Rst
            0xC7 => (Rst { offset: 0x00 }, 1, 16.into()),
            0xD7 => (Rst { offset: 0x10 }, 1, 16.into()),
            0xE7 => (Rst { offset: 0x20 }, 1, 16.into()),
            0xF7 => (Rst { offset: 0x30 }, 1, 16.into()),
            0xCF => (Rst { offset: 0x08 }, 1, 16.into()),
            0xDF => (Rst { offset: 0x18 }, 1, 16.into()),
            0xEF => (Rst { offset: 0x28 }, 1, 16.into()),
            0xFF => (Rst { offset: 0x38 }, 1, 16.into()),

            // Misc
            0xF3 => (Di, 1, 4.into()),
            0xFB => (Ei, 1, 4.into()),

            // CB-prefixed instructions
            0xCB => {
                match data[1] {
                    // Rotate
                    0x00 => (Rlc { dst: Reg8::B.into() }, 2, 8.into()),
                    0x01 => (Rlc { dst: Reg8::C.into() }, 2, 8.into()),
                    0x02 => (Rlc { dst: Reg8::D.into() }, 2, 8.into()),
                    0x03 => (Rlc { dst: Reg8::E.into() }, 2, 8.into()),
                    0x04 => (Rlc { dst: Reg8::H.into() }, 2, 8.into()),
                    0x05 => (Rlc { dst: Reg8::L.into() }, 2, 8.into()),
                    0x06 => (Rlc { dst: Arg::MemHl }, 2, 16.into()),
                    0x07 => (Rlc { dst: Reg8::A.into() }, 2, 8.into()),
                    0x08 => (Rrc { dst: Reg8::B.into() }, 2, 8.into()),
                    0x09 => (Rrc { dst: Reg8::C.into() }, 2, 8.into()),
                    0x0A => (Rrc { dst: Reg8::D.into() }, 2, 8.into()),
                    0x0B => (Rrc { dst: Reg8::E.into() }, 2, 8.into()),
                    0x0C => (Rrc { dst: Reg8::H.into() }, 2, 8.into()),
                    0x0D => (Rrc { dst: Reg8::L.into() }, 2, 8.into()),
                    0x0E => (Rrc { dst: Arg::MemHl }, 2, 16.into()),
                    0x0F => (Rrc { dst: Reg8::A.into() }, 2, 8.into()),
                    0x10 => (Rl { dst: Reg8::B.into() }, 2, 8.into()),
                    0x11 => (Rl { dst: Reg8::C.into() }, 2, 8.into()),
                    0x12 => (Rl { dst: Reg8::D.into() }, 2, 8.into()),
                    0x13 => (Rl { dst: Reg8::E.into() }, 2, 8.into()),
                    0x14 => (Rl { dst: Reg8::H.into() }, 2, 8.into()),
                    0x15 => (Rl { dst: Reg8::L.into() }, 2, 8.into()),
                    0x16 => (Rl { dst: Arg::MemHl }, 2, 16.into()),
                    0x17 => (Rl { dst: Reg8::A.into() }, 2, 8.into()),
                    0x18 => (Rr { dst: Reg8::B.into() }, 2, 8.into()),
                    0x19 => (Rr { dst: Reg8::C.into() }, 2, 8.into()),
                    0x1A => (Rr { dst: Reg8::D.into() }, 2, 8.into()),
                    0x1B => (Rr { dst: Reg8::E.into() }, 2, 8.into()),
                    0x1C => (Rr { dst: Reg8::H.into() }, 2, 8.into()),
                    0x1D => (Rr { dst: Reg8::L.into() }, 2, 8.into()),
                    0x1E => (Rr { dst: Arg::MemHl }, 2, 16.into()),
                    0x1F => (Rr { dst: Reg8::A.into() }, 2, 8.into()),

                    // Shift
                    0x20 => (Sla { dst: Reg8::B.into() }, 2, 8.into()),
                    0x21 => (Sla { dst: Reg8::C.into() }, 2, 8.into()),
                    0x22 => (Sla { dst: Reg8::D.into() }, 2, 8.into()),
                    0x23 => (Sla { dst: Reg8::E.into() }, 2, 8.into()),
                    0x24 => (Sla { dst: Reg8::H.into() }, 2, 8.into()),
                    0x25 => (Sla { dst: Reg8::L.into() }, 2, 8.into()),
                    0x26 => (Sla { dst: Arg::MemHl }, 2, 16.into()),
                    0x27 => (Sla { dst: Reg8::A.into() }, 2, 8.into()),
                    0x28 => (Sra { dst: Reg8::B.into() }, 2, 8.into()),
                    0x29 => (Sra { dst: Reg8::C.into() }, 2, 8.into()),
                    0x2A => (Sra { dst: Reg8::D.into() }, 2, 8.into()),
                    0x2B => (Sra { dst: Reg8::E.into() }, 2, 8.into()),
                    0x2C => (Sra { dst: Reg8::H.into() }, 2, 8.into()),
                    0x2D => (Sra { dst: Reg8::L.into() }, 2, 8.into()),
                    0x2E => (Sra { dst: Arg::MemHl }, 2, 16.into()),
                    0x2F => (Sra { dst: Reg8::A.into() }, 2, 8.into()),
                    0x38 => (Srl { dst: Reg8::B.into() }, 2, 8.into()),
                    0x39 => (Srl { dst: Reg8::C.into() }, 2, 8.into()),
                    0x3A => (Srl { dst: Reg8::D.into() }, 2, 8.into()),
                    0x3B => (Srl { dst: Reg8::E.into() }, 2, 8.into()),
                    0x3C => (Srl { dst: Reg8::H.into() }, 2, 8.into()),
                    0x3D => (Srl { dst: Reg8::L.into() }, 2, 8.into()),
                    0x3E => (Srl { dst: Arg::MemHl }, 2, 16.into()),
                    0x3F => (Srl { dst: Reg8::A.into() }, 2, 8.into()),

                    // Swap
                    0x30 => (Swap { dst: Reg8::B.into() }, 2, 8.into()),
                    0x31 => (Swap { dst: Reg8::C.into() }, 2, 8.into()),
                    0x32 => (Swap { dst: Reg8::D.into() }, 2, 8.into()),
                    0x33 => (Swap { dst: Reg8::E.into() }, 2, 8.into()),
                    0x34 => (Swap { dst: Reg8::H.into() }, 2, 8.into()),
                    0x35 => (Swap { dst: Reg8::L.into() }, 2, 8.into()),
                    0x36 => (Swap { dst: Arg::MemHl }, 2, 16.into()),
                    0x37 => (Swap { dst: Reg8::A.into() }, 2, 8.into()),

                    // Bit
                    0x40..=0x7F => {
                        // TODO: handle them
                        unimplemented!()
                    }
                    other => panic!("Unknown CB instruction: {}", other),
                }
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
    fn decode_ld() {
        // Vector of (input instruction, expected decoded, size, cycle count)
        #[rustfmt::skip]
        let test_vectors: &[([u8; 3], Instruction, u8, Cycles)] = &[
            ([0x01, 0x34, 0x12], Ld { dst: Arg::Reg16(Reg16::BC), src: Arg::Imm16(0x1234) }, 3, 12.into()),
            ([0x11, 0x34, 0x12], Ld { dst: Arg::Reg16(Reg16::DE), src: Arg::Imm16(0x1234) }, 3, 12.into()),
            ([0x21, 0x34, 0x12], Ld { dst: Arg::Reg16(Reg16::HL), src: Arg::Imm16(0x1234) }, 3, 12.into()),
            ([0x31, 0x34, 0x12], Ld { dst: Arg::Reg16(Reg16::SP), src: Arg::Imm16(0x1234) }, 3, 12.into()),

            ([0x06, 0x34, 0x00], Ld { dst: Arg::Reg8(Reg8::B),  src: Arg::Imm8(0x34) }, 2, 8.into()),
            ([0x16, 0x34, 0x00], Ld { dst: Arg::Reg8(Reg8::D),  src: Arg::Imm8(0x34) }, 2, 8.into()),
            ([0x26, 0x34, 0x00], Ld { dst: Arg::Reg8(Reg8::H),  src: Arg::Imm8(0x34) }, 2, 8.into()),
            ([0x36, 0x34, 0x00], Ld { dst: Arg::Mem(Reg16::HL), src: Arg::Imm8(0x34) }, 2, 12.into()),

            ([0x0A, 0x34, 0x00], Ld { dst: Arg::Reg8(Reg8::A),  src: Arg::Mem(Reg16::BC) }, 1, 8.into()),
            ([0x1A, 0x34, 0x00], Ld { dst: Arg::Reg8(Reg8::A),  src: Arg::Mem(Reg16::DE) }, 1, 8.into()),
            ([0x2A, 0x34, 0x00], LdiAMemHl, 1, 8.into()),
            ([0x3A, 0x34, 0x00], LddAMemHl, 1, 8.into()),

            ([0x0E, 0x34, 0x00], Ld { dst: Arg::Reg8(Reg8::C),  src: Arg::Imm8(0x34) }, 2, 8.into()),
            ([0x1E, 0x34, 0x00], Ld { dst: Arg::Reg8(Reg8::E),  src: Arg::Imm8(0x34) }, 2, 8.into()),
            ([0x2E, 0x34, 0x00], Ld { dst: Arg::Reg8(Reg8::L),  src: Arg::Imm8(0x34) }, 2, 8.into()),
            ([0x3E, 0x34, 0x00], Ld { dst: Arg::Reg8(Reg8::A),  src: Arg::Imm8(0x34) }, 2, 8.into()),

            ([0xE0, 0x34, 0x00], Ldh { offset: 0x34 }, 2, 12.into()),
            ([0xF0, 0x34, 0x00], LdhA { offset: 0x34 }, 2, 12.into()),
        ];

        for (input, expected, expected_size, expected_cycles) in test_vectors {
            let (inst, size, cycles) = Instruction::decode(&input[..]);
            assert_eq!(expected, &inst);
            assert_eq!(expected_size, &size);
            assert_eq!(expected_cycles, &cycles);
        }
    }
}
