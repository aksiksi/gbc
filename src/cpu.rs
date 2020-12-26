use crate::instructions::{Arg, Cond, Instruction};
use crate::memory::{MemoryBus, MemoryRange, MemoryRead, MemoryWrite};
use crate::registers::{Flag, Flags, Reg16, Reg8, RegisterFile, RegisterOps};

#[derive(Clone, Copy)]
enum Op {
    And,
    Or,
    Xor,
}

#[derive(Debug)]
pub struct Cpu {
    pub registers: RegisterFile,
    pub flags: Flags,
    pub memory: MemoryBus,
}

impl Cpu {
    pub fn new(memory: MemoryBus) -> Self {
        let registers = RegisterFile::new();
        let flags = Flags::new();
        Self {
            registers,
            flags,
            memory,
        }
    }

    /// Executes the next instruction and returns the number of cycles it
    /// took to complete.
    pub fn step(&mut self) -> u8 {
        let pc = self.registers.PC;

        // This returns a read-only slice of the relevant memory, starting from PC.
        // The slice is used to decode the next instruction.
        //
        // Note that the slice is unbounded: it ends at the end of the relevant memory/bank.
        // For example, if the current PC is in ROM bank 0, `data` will contain a slice
        // from PC to the end of ROM bank 0.
        let data = self.memory.range(pc..);

        // Decode the instruction
        let (inst, size, cycles) = Instruction::decode(data);

        dbg!(inst);

        // Execute the instruction on this CPU
        self.execute(inst);

        if pc == self.registers.PC {
            // If we did not execute a jump, proceed to next instruction as usual
            self.registers.PC = pc + size as u16;
            cycles.not_taken()
        } else {
            // If we did jump, do not update the PC, and return the correct number
            // of cycles.
            cycles.taken()
        }
    }

    /// Execute a single instruction on this CPU
    pub fn execute(&mut self, instruction: Instruction) {
        use Instruction::*;

        let pc = &mut self.registers.PC;

        match instruction {
            Nop => (),
            Halt => (),
            Di => {
                // TODO: Disable interrupts
            }
            Ei => {
                // TODO: Enable interrupts
            }
            Ld { dst, src } => match (dst, src) {
                (Arg::Reg16(dst), Arg::Imm16(src)) => {
                    self.registers.write(dst, src);
                }
                (Arg::Reg8(dst), Arg::Imm8(src)) => {
                    self.registers.write(dst, src);
                }
                (Arg::Reg8(dst), Arg::Reg8(src)) => {
                    self.registers.write(dst, self.registers.read(src));
                }
                (Arg::Reg8(dst), Arg::Mem(src)) => {
                    let addr = self.registers.read(src);
                    let value = self.memory.read(addr);
                    self.registers.write(dst, value);
                }
                (Arg::Mem(dst), Arg::Reg8(src)) => {
                    let addr = self.registers.read(dst);
                    self.memory.write(addr, self.registers.read(src));
                }
                (Arg::MemImm(dst), Arg::Reg8(src)) => {
                    self.memory.write(dst, self.registers.read(src));
                }
                (Arg::MemImm(dst), Arg::Reg16(src)) => {
                    self.memory.write(dst, self.registers.read(src));
                }
                (Arg::Reg8(dst), Arg::MemImm(src)) => {
                    let value = self.memory.read(src);
                    self.registers.write(dst, value);
                }
                _ => panic!("Unexpected dst and src: {:?}, {:?}", dst, src),
            },
            LdMemCA => {
                let addr = 0xFF00 + self.registers.read(Reg8::C) as u16;
                self.memory.write(addr, self.registers.read(Reg8::A));
            }
            LdAMemC => {
                let addr = 0xFF00 + self.registers.read(Reg8::C) as u16;
                self.registers.write(Reg8::A, self.memory.read(addr));
            }
            LdiAMemHl => {
                let addr = self.registers.read(Reg16::HL);
                let value = self.memory.read(addr);
                self.registers.write(Reg8::A, value);
                self.registers.write(Reg16::HL, addr.wrapping_add(1));
            }
            LdiMemHlA => {
                let addr = self.registers.read(Reg16::HL);
                self.memory.write(addr, self.registers.read(Reg8::A));
                self.registers.write(Reg16::HL, addr.wrapping_add(1));
            }
            LddAMemHl => {
                let addr = self.registers.read(Reg16::HL);
                let value = self.memory.read(addr);
                self.registers.write(Reg8::A, value);
                self.registers.write(Reg16::HL, addr.wrapping_sub(1));
            }
            LddMemHlA => {
                let addr = self.registers.read(Reg16::HL);
                self.memory.write(addr, self.registers.read(Reg8::A));
                self.registers.write(Reg16::HL, addr.wrapping_sub(1));
            }
            LdhA { offset } => {
                let value = self.memory.read(0xFF00 + offset as u16);
                self.registers.write(Reg8::A, value);
            }
            Ldh { offset } => {
                let a = self.registers.read(Reg8::A);
                self.memory.write(0xFF00 + offset as u16, a);
            }
            Inc { dst } => self.inc(dst),
            Dec { dst } => self.dec(dst),
            Add { src } => self.add(src),
            Adc { src } => self.adc(src),
            AddHlReg16 { src } => self.add_hl(src),
            Sub { src } => self.sub(src, true),
            Sbc { src } => self.sbc(src),
            And { src } => self.logical(src, Op::And),
            Or { src } => self.logical(src, Op::Or),
            Xor { src } => self.logical(src, Op::Xor),
            Cp { src } => self.sub(src, false),
            Pop { dst } => {
                // Increment SP
                self.registers.SP += 2;

                let lower = self.memory.read(self.registers.SP);
                let upper = self.memory.read(self.registers.SP - 1);
                let value = (upper as u16) << 8 | lower as u16;

                self.registers.write(dst, value);
            }
            Push { src } => {
                let sp = self.registers.SP;
                let value = self.registers.read(src);
                let lower = value as u8;
                let upper = (value >> 8) as u8;

                // Write upper and lower bytes seperately to the stack.
                // We cannot use the `MemoryWrite` trait because it assumes
                // that memory addresses increase instead of decrease.
                self.memory.write(sp, lower);
                self.memory.write(sp-1, upper);

                // Decrement SP
                self.registers.SP -= 2;
            }
            Jp { addr, cond } => {
                let ok = match cond {
                    Cond::None => true,
                    Cond::NotZero if !self.flags.zero() => true,
                    Cond::Zero if self.flags.zero() => true,
                    Cond::NotCarry if !self.flags.carry() => true,
                    Cond::Carry if self.flags.carry() => true,
                    _ => false,
                };

                if ok {
                    *pc = addr;
                }
            }
            Jr { offset, cond } => {
                let ok = match cond {
                    Cond::None => true,
                    Cond::NotZero if !self.flags.zero() => true,
                    Cond::Zero if self.flags.zero() => true,
                    Cond::NotCarry if !self.flags.carry() => true,
                    Cond::Carry if self.flags.carry() => true,
                    _ => false,
                };

                if ok {
                    // Numeric casts from signed to unsigned will sign extend
                    *pc = pc.wrapping_add(offset as u16);
                }
            }
            other => panic!("Cannot execute instruction: {:?}", other),
        }
    }

    fn add(&mut self, src: Arg) {
        let a = self.registers.read(Reg8::A);

        let val = match src {
            Arg::Reg8(src) => self.registers.read(src),
            Arg::Imm8(src) => src,
            Arg::MemHl => {
                let addr = self.registers.read(Reg16::HL);
                let val = self.memory.read(addr);
                val
            }
            _ => panic!("Unexpected src {:?}", src),
        };

        let half_carry = ((val & 0xF) + (a & 0xF)) & 0x10 == 0x10;
        let (result, carry) = a.overflowing_add(val);

        self.registers.write(Reg8::A, result);

        self.flags.set(Flag::Zero, result == 0);
        self.flags.set(Flag::Subtract, false);
        self.flags.set(Flag::HalfCarry, half_carry);
        self.flags.set(Flag::Carry, carry);
    }

    /// ADD with carry
    fn adc(&mut self, src: Arg) {
        let a = self.registers.read(Reg8::A);

        let val = match src {
            Arg::Reg8(src) => self.registers.read(src),
            Arg::Imm8(src) => src,
            Arg::MemHl => {
                let addr = self.registers.read(Reg16::HL);
                let val = self.memory.read(addr);
                val
            }
            _ => panic!("Unexpected src {:?}", src),
        };

        let curr_carry = if self.flags.carry() { 1u8 } else { 0u8 };

        // Compute half-carry based on the current value of the carry flag
        let half_carry = ((val & 0xF) + (a & 0xF) + curr_carry) & 0x10 == 0x10;

        let (result, carry) = match a.overflowing_add(val) {
            // If the first add overflows, propagate the carry flag
            (result, true) => (result.wrapping_add(1), true),

            // If the first add does not overflow, try another overflowing_add
            (result, false) => result.overflowing_add(1),
        };

        self.registers.write(Reg8::A, result);

        self.flags.set(Flag::Zero, result == 0);
        self.flags.set(Flag::Subtract, false);
        self.flags.set(Flag::HalfCarry, half_carry);
        self.flags.set(Flag::Carry, carry);
    }

    /// 16-bit version of ADD for HL
    fn add_hl(&mut self, src: Reg16) {
        let hl = self.registers.read(Reg16::HL);
        let half_carry: bool;

        let (result, carry) = {
            let val = self.registers.read(src);
            half_carry = ((val & 0x00FF) + (hl & 0x00FF)) & 0x0100 == 0x0100;
            hl.overflowing_add(val)
        };

        self.registers.write(Reg16::HL, result);

        self.flags.set(Flag::Zero, result == 0);
        self.flags.set(Flag::Subtract, false);
        self.flags.set(Flag::HalfCarry, half_carry);
        self.flags.set(Flag::Carry, carry);
    }

    fn sub(&mut self, src: Arg, write: bool) {
        let a = self.registers.read(Reg8::A);

        let val = match src {
            Arg::Reg8(src) => self.registers.read(src),
            Arg::Imm8(src) => src,
            Arg::MemHl => {
                let addr = self.registers.read(Reg16::HL);
                let val = self.memory.read(addr);
                val
            }
            _ => panic!("Unexpected src {:?}", src),
        };

        let half_carry = ((val & 0xF) + (a & 0xF)) & 0x10 == 0x10;
        let (result, carry) = a.overflowing_sub(val);

        // Write back the result for non-CP
        if write {
            self.registers.write(Reg8::A, result);
        }

        self.flags.set(Flag::Zero, result == 0);
        self.flags.set(Flag::Subtract, true);
        self.flags.set(Flag::HalfCarry, half_carry);
        self.flags.set(Flag::Carry, carry);
    }

    /// SUB with carry
    fn sbc(&mut self, src: Arg) {
        let a = self.registers.read(Reg8::A);

        let val = match src {
            Arg::Reg8(src) => self.registers.read(src),
            Arg::Imm8(src) => src,
            Arg::MemHl => {
                let addr = self.registers.read(Reg16::HL);
                let val = self.memory.read(addr);
                val
            }
            _ => panic!("Unexpected src {:?}", src),
        };

        let curr_carry = if self.flags.carry() { 1u8 } else { 0u8 };

        // Compute half-carry based on the current value of the carry flag
        let half_carry = ((val & 0xF) + (a & 0xF) + curr_carry) & 0x10 == 0x10;

        let (result, carry) = match a.overflowing_sub(val) {
            // If the first sub overflows, propagate the carry flag
            (result, true) => (result.wrapping_sub(1), true),

            // If the first sub does not overflow, try another overflowing_sub
            (result, false) => result.overflowing_sub(1),
        };

        self.registers.write(Reg8::A, result);

        self.flags.set(Flag::Zero, result == 0);
        self.flags.set(Flag::Subtract, true);
        self.flags.set(Flag::HalfCarry, half_carry);
        self.flags.set(Flag::Carry, carry);
    }

    fn logical(&mut self, src: Arg, op: Op) {
        let a = self.registers.read(Reg8::A);

        let val = match src {
            Arg::Reg8(src) => self.registers.read(src),
            Arg::Imm8(src) => src,
            Arg::MemHl => {
                let addr = self.registers.read(Reg16::HL);
                let val = self.memory.read(addr);
                val
            }
            _ => panic!("Unexpected src {:?}", src),
        };

        let result = match op {
            Op::And => a & val,
            Op::Or => a | val,
            Op::Xor => a ^ val,
        };

        self.registers.write(Reg8::A, result);

        self.flags.set(Flag::Zero, result == 0);
        self.flags.set(Flag::Subtract, false);
        self.flags.set(Flag::Carry, false);

        if let Op::And = op {
            self.flags.set(Flag::HalfCarry, true);
        } else {
            self.flags.set(Flag::HalfCarry, false);
        }
    }

    /// Increment instruction
    fn inc(&mut self, dst: Arg) {
        let half_carry: bool;

        let result = match dst {
            Arg::Reg8(dst) => {
                let curr = self.registers.read(dst);
                let result = curr.wrapping_add(1);
                half_carry = ((curr & 0x0F) + 1) & 0x10 == 0x10;
                self.registers.write(dst, result);
                result as u16
            }
            Arg::Reg16(dst) => {
                let curr = self.registers.read(dst);
                let result = curr.wrapping_add(1);
                half_carry = ((curr & 0x00FF) + 1) & 0x0100 == 0x0100;
                self.registers.write(dst, result);
                result
            }
            Arg::MemHl => {
                let addr = self.registers.read(Reg16::HL);
                let curr = self.memory.read(addr);
                let result = curr.wrapping_add(1);
                half_carry = ((curr & 0x0F) + 1) & 0x10 == 0x10;
                self.memory.write(addr, result);
                result as u16
            }
            _ => panic!("Unexpected dst: {:?}", dst),
        };

        self.flags.set(Flag::Zero, result == 0);
        self.flags.set(Flag::Subtract, false);
        self.flags.set(Flag::HalfCarry, half_carry);
    }

    /// Decrement instruction
    fn dec(&mut self, dst: Arg) {
        let mut update_flags = true;
        let mut half_carry = false;

        let (result, carry) = match dst {
            Arg::Reg8(dst) => {
                let curr = self.registers.read(dst);

                // If lower nibble == 0, set the half-carry bit
                half_carry = curr & 0x0F == 0;

                let (result, borrow) = curr.overflowing_sub(1);
                self.registers.write(dst, result);

                (result as u16, borrow)
            }
            Arg::Reg16(dst) => {
                update_flags = false; // 16-bit variant does not touch flags
                let result = self.registers.read(dst).wrapping_sub(1);
                self.registers.write(dst, result);
                (result, false)
            }
            Arg::MemHl => {
                let addr = self.registers.read(Reg16::HL);
                let curr = self.memory.read(addr);

                // If lower nibble == 0, set the half-carry bit
                half_carry = curr & 0xFF == 0;

                let (result, borrow) = curr.overflowing_sub(1);
                self.memory.write(addr, result);

                (result as u16, borrow)
            }
            _ => panic!("Unexpected dst: {:?}", dst),
        };

        if update_flags {
            self.flags.set(Flag::Zero, result == 0);
            self.flags.set(Flag::Subtract, true);
            self.flags.set(Flag::HalfCarry, half_carry);
            self.flags.set(Flag::Carry, carry);
        }
    }

    pub fn memory(&self) -> &MemoryBus {
        &self.memory
    }

    pub fn reset(&mut self) {
        unimplemented!()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::cartridge::{RamSize, RomSize};

    fn get_cpu() -> Cpu {
        let memory = MemoryBus::new(RomSize::_1M, RamSize::_32K);
        Cpu::new(memory)
    }

    #[test]
    fn add() {
        let mut cpu = get_cpu();

        cpu.registers.write(Reg8::A, 0);

        // Normal add
        let inst = Instruction::Add { src: 0x10u8.into() };
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0x10);

        // Overflow
        cpu.registers.write(Reg8::B, 0xF0);
        let inst = Instruction::Add {
            src: Reg8::B.into(),
        };
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0x00);
        assert!(cpu.flags.zero());
        assert!(cpu.flags.carry());

        // Half overflow
        cpu.registers.write(Reg8::A, 0x3C);
        let inst = Instruction::Add { src: 0xFFu8.into() };
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0x3B);
        assert!(!cpu.flags.zero());
        assert!(!cpu.flags.subtract());
        assert!(cpu.flags.half_carry());
        assert!(cpu.flags.carry());
    }

    #[test]
    fn add_hl() {
        let mut cpu = get_cpu();

        cpu.registers.write(Reg8::A, 0);
        cpu.registers.write(Reg16::HL, 0);
        cpu.registers.write(Reg16::DE, 0x1234);

        let inst = Instruction::AddHlReg16 { src: Reg16::DE };

        // Normal add
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg16::HL), 0x1234);
        assert!(!cpu.flags.zero());
        assert!(!cpu.flags.subtract());
        assert!(!cpu.flags.half_carry());
        assert!(!cpu.flags.carry());

        // Overflow
        cpu.registers.write(Reg16::DE, 0xEDCC);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg16::HL), 0x0);
        assert!(cpu.flags.zero());
        assert!(!cpu.flags.subtract());
        assert!(cpu.flags.half_carry());
        assert!(cpu.flags.carry());
    }

    #[test]
    fn adc() {
        let mut cpu = get_cpu();

        cpu.registers.write(Reg8::A, 0);

        // Normal add
        let inst = Instruction::Adc { src: 0x10u8.into() };
        cpu.flags.set(Flag::Carry, true);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0x11);
        assert!(!cpu.flags.zero());
        assert!(!cpu.flags.carry());

        // Overflow
        let inst = Instruction::Adc { src: Reg8::B.into() };
        cpu.flags.set(Flag::Carry, true);
        cpu.registers.write(Reg8::A, 0xE1);
        cpu.registers.write(Reg8::B, 0x1E);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0x00);
        assert!(cpu.flags.zero());
        assert!(cpu.flags.half_carry());
        assert!(cpu.flags.carry());
    }

    #[test]
    fn sub() {
        let mut cpu = get_cpu();

        cpu.registers.write(Reg8::A, 0x10);

        // Normal sub
        let inst = Instruction::Sub { src: 0x10u8.into() };
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0);
        assert!(cpu.flags.zero());
        assert!(cpu.flags.subtract());

        // Underflow
        let inst = Instruction::Sub { src: 0x10u8.into() };
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0xF0);
        assert!(!cpu.flags.zero());
        assert!(cpu.flags.subtract());
        assert!(cpu.flags.carry());

        // Half underflow
        cpu.registers.write(Reg8::A, 0x3E);
        let inst = Instruction::Sub { src: 0xFu8.into() };
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0x2F);
        assert!(!cpu.flags.zero());
        assert!(cpu.flags.subtract());
    }

    #[test]
    fn sbc() {
        let mut cpu = get_cpu();

        // Normal sub
        let inst = Instruction::Sbc { src: 0x09u8.into() };
        cpu.registers.write(Reg8::A, 0x0A);
        cpu.flags.set(Flag::Carry, true);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0x00);
        assert!(cpu.flags.zero());
        assert!(!cpu.flags.carry());

        // Overflow
        let inst = Instruction::Sbc { src: Reg8::B.into() };
        cpu.flags.set(Flag::Carry, true);
        cpu.registers.write(Reg8::A, 0x3B);
        cpu.registers.write(Reg8::B, 0x4F);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0xEB);
        assert!(!cpu.flags.zero());
        assert!(cpu.flags.subtract());
        assert!(cpu.flags.half_carry());
        assert!(cpu.flags.carry());
    }

    #[test]
    fn logical() {
        let mut cpu = get_cpu();

        // AND
        let inst = Instruction::And { src: 0x02u8.into() };
        cpu.registers.write(Reg8::A, 0x0A);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0x02);
        assert!(!cpu.flags.zero());
        assert!(cpu.flags.half_carry());

        // OR
        let inst = Instruction::Or { src: 0xF0u8.into() };
        cpu.registers.write(Reg8::A, 0x0A);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0xFA);
        assert!(!cpu.flags.zero());
        assert!(!cpu.flags.half_carry());

        // XOR
        let inst = Instruction::Xor { src: 0xFFu8.into() };
        cpu.registers.write(Reg8::A, 0x0F);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0xF0);
        assert!(!cpu.flags.zero());
        assert!(!cpu.flags.half_carry());
    }

    #[test]
    fn inc() {
        let mut cpu = get_cpu();

        cpu.registers.write(Reg8::A, 0xFF);

        // Overflow
        let inst = Instruction::Inc {
            dst: Reg8::A.into(),
        };
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0);
        assert!(cpu.flags.zero());
        assert!(cpu.flags.half_carry());
        assert!(!cpu.flags.subtract());
    }

    #[test]
    fn dec() {
        let mut cpu = get_cpu();

        cpu.registers.write(Reg8::A, 0);

        // Underflow
        let inst = Instruction::Dec {
            dst: Reg8::A.into(),
        };
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0xFF);
        assert!(!cpu.flags.zero());
        assert!(cpu.flags.half_carry());
        assert!(cpu.flags.subtract());
    }

    #[test]
    fn cp() {
        let mut cpu = get_cpu();

        cpu.registers.write(Reg8::A, 0x3C);
        cpu.registers.write(Reg8::B, 0x2F);

        let inst = Instruction::Cp {
            src: Reg8::B.into(),
        };
        cpu.execute(inst);
        assert!(!cpu.flags.zero());
        assert!(cpu.flags.subtract());
        assert!(cpu.flags.half_carry());
        assert!(!cpu.flags.carry());

        let inst = Instruction::Cp { src: 0x3Cu8.into() };
        cpu.execute(inst);
        assert!(cpu.flags.zero());
        assert!(cpu.flags.subtract());

        let (addr, value) = (0xBEEF, 0x40u8);
        cpu.memory.write(addr, value);
        cpu.registers.write(Reg16::HL, addr);
        let inst = Instruction::Cp { src: Arg::MemHl };
        cpu.execute(inst);
        assert!(!cpu.flags.zero());
        assert!(cpu.flags.subtract());
        assert!(!cpu.flags.half_carry());
        assert!(cpu.flags.carry());
    }

    #[test]
    fn push_and_pop() {
        let mut cpu = get_cpu();

        cpu.registers.write(Reg16::SP, 0xFFFE);
        cpu.registers.write(Reg16::HL, 0x1234);

        let inst = Instruction::Push { src: Reg16::HL.into() };
        cpu.execute(inst);
        assert_eq!(cpu.registers.SP, 0xFFFC);

        let inst = Instruction::Pop { dst: Reg16::AF.into() };
        cpu.execute(inst);
        assert_eq!(cpu.registers.SP, 0xFFFE);
        assert_eq!(cpu.registers.read(Reg16::AF), 0x1234);
    }
}
