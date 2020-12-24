use std::convert::TryInto;

use crate::cartridge::Rom;
use crate::cpu::Cpu;
use crate::error::{Error, Result};
use crate::memory::MemoryBus;
use crate::registers::{Flag, Reg8, Reg16, RegisterFile};

#[derive(Clone, Copy, Debug)]
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
#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    /// Load 8-bit immediate value into reg
    LdReg8Imm8(Reg8, u8),

    /// Load second reg into first
    LdReg8Reg8(Reg8, Reg8),

    /// Load value memory at address (HL) into Reg8
    LdReg8Mem(Reg8),

    /// Load Reg8 into memory at address (HL)
    LdMemReg8(Reg8),

    /// Load Imm8 into memory at address (HL)
    LdMemImm8(u8),

    /// Load Reg8 into A (use LdReg8Reg8)

    /// Load memory at address (Reg16) into A
    LdAMem(Reg16), // TODO

    /// Load memory at address (Imm16) into A
    /// Note: LS byte first
    LdAMemImm16(u16),

    /// Load Imm8 into A
    LdAImm8(u8),

    /// Load A into reg
    LdReg8A(Reg8),

    /// Load A to address (Reg16)
    LdMemA(Reg16), // TODO

    /// Load A to address (Imm16)
    /// Note: LS byte first
    LdMemImm16A(u16),

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

    /// Load A into address (0xFF00 + Imm8)
    LdhMemImm8A(u8),

    /// Load value at address (0xFF00 + Imm8) into A
    LdhAMemImm8(u8),

    /// Load Imm16 into Reg16
    LdReg16Imm16(Reg16, u16),

    /// Load HL into SP
    LdSpHl,

    /// Load SP + Imm8 into HL
    ///
    /// Flags:
    ///     * Zero: reset
    ///     * Subtract: reset
    ///     * HalfCarry: set or reset
    ///     * Carry: set or reset
    LdHlSpImm8(i8),

    /// Load SP into address (Imm16)
    LdMemImm16Sp(u16),

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
    AddAReg8(Reg8),
    AddAImm8(u8), // TODO: signed?
    AddAMem(Reg16),

    /// Add carry flag **and** Reg8 or Imm8 or value at address (Reg16) to A
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: set if carry from bit 3
    ///     * Carry: set if carry from bit 7
    AdcAReg8(Reg8),
    AdcAImm8(u8), // TODO: signed?
    AdcAMem(Reg16),

    /// Subtract Reg8 or Imm8 or value at address (Reg16) from A
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: set
    ///     * HalfCarry: set if no borrow from bit 4
    ///     * Carry: set if no borrow
    SubReg8(Reg8),
    SubImm8(u8),
    SubMem(Reg16),

    /// Subtract carry flag **and** Reg8 or Imm8 or value at address (Reg16) from A
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: set
    ///     * HalfCarry: set if no borrow from bit 4
    ///     * Carry: set if no borrow
    SbcAReg8(Reg8),
    SbcAImm8(u8), // TODO: signed?
    SbcAMem(Reg16),

    /// AND Reg8 or Imm8 or value at address (Reg16) with A.
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: set
    ///     * Carry: reset
    AndReg8(Reg8),
    AndImm8(u8),
    AndMem(Reg16),

    /// OR Reg8 or Imm8 or value at address (Reg16) with A.
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: reset
    OrReg8(Reg8),
    OrImm8(u8),
    OrMem(Reg16),

    /// XOR Reg8 or Imm8 or value at address (Reg16) with A.
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: reset
    XorReg8(Reg8),
    XorImm8(u8),
    XorMem(Reg16),

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
    CpReg8(Reg8),
    CpImm8(u8),
    CpMem,

    /// Increment Reg8 or value at address (HL)
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: set if carry from bit 3
    ///     * Carry: not affected
    IncReg8(Reg8),
    IncMem,

    /// Decrement Reg8 or value at address (HL)
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: set
    ///     * HalfCarry: set if no borrow from bit 4
    ///     * Carry: not affected
    DecReg8(Reg8),
    DecMem,

    /// Add Reg16 to HL.
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: set if carry from bit 11
    ///     * Carry: set if carry from bit 15
    AddHlReg16(Reg16),

    /// Add Imm8 to SP.
    ///
    /// Flags:
    ///     * Zero: reset
    ///     * Subtract: reset
    ///     * HalfCarry: set if carry from bit 11
    ///     * Carry: set if carry from bit 15
    AddSpImm8(u8),

    /// Increment Reg16
    ///
    /// Flags: Not affected
    IncReg16(Reg16),

    /// Decrement Reg16
    ///
    /// Flags: Not affected
    DecReg16(Reg16),

    /// Swap upper & lower nibbles of Reg8 or value at memory address (Reg16)
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: reset
    SwapReg8,
    SwapMem(Reg16),

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
    Rlc(Reg8),
    RlcMem,

    /// Rotate Reg8 or (HL) left through carry flag.
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: contains old bit 7
    Rl(Reg8),
    RlMem,

    /// Rotate Reg8 or (HL) right. Place old bit 0 in carry flag.
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: contains old bit 0
    Rrc(Reg8),
    RrcMem,

    /// Rotate Reg8 or (HL) right through carry flag.
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: contains old bit 0
    Rr(Reg8),
    RrMem,

    /// Shift Reg8 or (HL) left into carry.
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: contains old bit 7
    Sla(Reg8),
    SlaMem,

    /// Shift Reg8 or (HL) right into carry.
    /// Note: MSB does not change.
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: contains old bit 0
    Sra(Reg8),
    SraMem,

    /// Shift Reg8 or (HL) right into carry.
    /// Note: MSB is set to 0.
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: reset
    ///     * Carry: contains old bit 0
    Srl(Reg8),
    SrlMem,

    /// Test bit `b` in Reg8 or (HL).
    ///
    /// Flags:
    ///     * Zero: set if result 0
    ///     * Subtract: reset
    ///     * HalfCarry: set
    ///     * Carry: not affected
    Bit(Reg8, u8),
    BitMem(u8),

    /// Set bit `b` in Reg8 or (HL).
    ///
    /// Flags: None
    Set(Reg8, u8),
    SetMem(u8),

    /// Reset bit `b` in Reg8 or (HL).
    ///
    /// Flags: None
    Res(Reg8, u8),
    ResMem(u8),

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
    pub fn decode(data: [u8; 3]) -> (Self, u16, Cycles) {
        use Instruction::*;

        // Extract the next arg as a 16-bit number
        let arg16 = u16::from_le_bytes(data[1..3].try_into().unwrap());

        let (inst, size, cycles) = match data[0] {
            0x00 => (Nop, 1, 4.into()),

            // LdReg16Imm16
            0x01 => (LdReg16Imm16(Reg16::BC, arg16), 3, 12.into()),
            0x11 => (LdReg16Imm16(Reg16::DE, arg16), 3, 12.into()),
            0x21 => (LdReg16Imm16(Reg16::HL, arg16), 3, 12.into()),
            0x31 => (LdReg16Imm16(Reg16::SP, arg16), 3, 12.into()),

            // Inc
            0x03 => (IncReg16(Reg16::BC), 1, 8.into()),
            0x13 => (IncReg16(Reg16::DE), 1, 8.into()),
            0x23 => (IncReg16(Reg16::HL), 1, 8.into()),
            0x33 => (IncReg16(Reg16::SP), 1, 8.into()),
            0x04 => (IncReg8(Reg8::B), 1, 4.into()),
            0x14 => (IncReg8(Reg8::D), 1, 4.into()),
            0x24 => (IncReg8(Reg8::H), 1, 4.into()),
            0x34 => (IncMem, 1, 12.into()),
            0x0C => (IncReg8(Reg8::C), 1, 4.into()),
            0x1C => (IncReg8(Reg8::E), 1, 4.into()),
            0x2C => (IncReg8(Reg8::L), 1, 4.into()),
            0x3C => (IncReg8(Reg8::A), 1, 4.into()),

            // Dec
            0x04 => (DecReg8(Reg8::B), 1, 4.into()),
            0x14 => (DecReg8(Reg8::D), 1, 4.into()),
            0x24 => (DecReg8(Reg8::H), 1, 4.into()),
            0x34 => (DecMem, 1, 12.into()),
            0x0B => (DecReg16(Reg16::BC), 1, 8.into()),
            0x1B => (DecReg16(Reg16::DE), 1, 8.into()),
            0x2B => (DecReg16(Reg16::HL), 1, 8.into()),
            0x3B => (DecReg16(Reg16::SP), 1, 8.into()),
            0x0D => (DecReg8(Reg8::C), 1, 4.into()),
            0x1D => (DecReg8(Reg8::E), 1, 4.into()),
            0x2D => (DecReg8(Reg8::L), 1, 4.into()),
            0x3D => (DecReg8(Reg8::A), 1, 4.into()),

            // Cp
            0xBF => (CpReg8(Reg8::A), 1, 4.into()),
            0xB8 => (CpReg8(Reg8::B), 1, 4.into()),
            0xB9 => (CpReg8(Reg8::C), 1, 4.into()),
            0xBA => (CpReg8(Reg8::D), 1, 4.into()),
            0xBB => (CpReg8(Reg8::E), 1, 4.into()),
            0xBC => (CpReg8(Reg8::H), 1, 4.into()),
            0xBD => (CpReg8(Reg8::L), 1, 4.into()),
            0xBE => (CpMem, 1, 8.into()),
            0xFE => (CpImm8(data[1]), 2, 8.into()),

            // Jump
            0x28 => {
                let offset = data[1] as i8;
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

    /// Executes this instruction on the CPU
    pub fn execute(&self, cpu: &mut Cpu) {
        use Instruction::*;

        let flags = &mut cpu.flags;
        let memory = &mut cpu.memory;
        let registers = &mut cpu.registers;

        match self {
            Nop => (),
            LdReg16Imm16(dst, src) => {
                registers.write_u16(*dst, *src);
            }
            IncReg8(dst) => {
                let curr = registers.read_u8(*dst);
                registers.write_u8(*dst, curr.wrapping_add(1));
            }
            IncReg16(dst) => {
                let curr = registers.read_u16(*dst);
                registers.write_u16(*dst, curr.wrapping_add(1));
            }
            IncMem => {
                let addr = registers.read_u16(Reg16::HL);
                let curr = memory.read(addr);
                memory.write(addr, curr.wrapping_add(1));
            }
            DecReg8(dst) => {
                let curr = registers.read_u8(*dst);
                registers.write_u8(*dst, curr.wrapping_sub(1));
            }
            DecReg16(dst) => {
                let curr = registers.read_u16(*dst);
                registers.write_u16(*dst, curr.wrapping_sub(1));
            }
            DecMem => {
                let addr = registers.read_u16(Reg16::HL);
                let curr = memory.read(addr);
                memory.write(addr, curr.wrapping_sub(1));
            } 
            CpImm8(src) => {
                let a = registers.read_u8(Reg8::A);

                if *src == a {
                    flags.set(Flag::Zero);
                } else if *src > a {
                    flags.set(Flag::Carry);
                }
            }
            Jp(addr, cond) => {
                let ok = match cond {
                    Cond::None => true,
                    Cond::NotZero if !flags.is_zero() => true,
                    Cond::Zero if flags.is_zero() => true,
                    Cond::NotCarry if !flags.is_carry() => true,
                    Cond::Carry if flags.is_carry() => true,
                    _ => false,
                };

                if ok {
                    registers.PC = *addr;
                }
            }
            Jr(offset, cond) => {
                let ok = match cond {
                    Cond::None => true,
                    Cond::NotZero if !flags.is_zero() => true,
                    Cond::Zero if flags.is_zero() => true,
                    Cond::NotCarry if !flags.is_carry() => true,
                    Cond::Carry if flags.is_carry() => true,
                    _ => false,
                };

                if ok {
                    // Numeric casts from signed to unsigned will sign extend
                    registers.PC = registers.PC.wrapping_add(*offset as u16);
                }
            }
            other => panic!("Unknown")
        }
    }
}
