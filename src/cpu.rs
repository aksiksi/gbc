use crate::instructions::{Arg, Cond, Instruction};
use crate::memory::{MemoryBus, MemoryRange, MemoryRead, MemoryWrite};
use crate::registers::{Flag, Flags, Reg16, Reg8, RegisterFile, RegisterOps};

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
            Xor { src } => self.xor(src),
            Inc { dst } => self.inc(dst),
            Dec { dst } => self.dec(dst),
            Add { src } => self.add(src),
            Cp  { src } => self.cp(src),
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

    fn xor(&mut self, src: Arg) {
        let a = self.registers.read(Reg8::A);

        let result = match src {
            Arg::Reg8(src) => {
                let curr = self.registers.read(src);
                a ^ curr
            }
            Arg::Imm8(src) => a ^ src,
            Arg::MemHl => {
                let addr = self.registers.read(Reg16::HL);
                let curr = self.memory.read(addr);
                a ^ curr
            }
            _ => panic!("Unexpected src: {:?}", src),
        };

        self.registers.write(Reg8::A, result);

        // Flags
        self.flags.set(Flag::Zero, result == 0);
        self.flags.clear(Flag::Subtract);
        self.flags.clear(Flag::Carry);
        self.flags.clear(Flag::HalfCarry);
    }

    fn add(&mut self, src: Arg) {
        let a = self.registers.read(Reg8::A);
        let half_carry: bool;

        let (result, carry) = match src {
            Arg::Reg8(src) => {
                let val = self.registers.read(src);
                half_carry = ((val & 0xF) + (a & 0xF)) & 0x10 == 0x10;
                val.overflowing_add(a)
            }
            Arg::Imm8(src) => {
                half_carry = ((src & 0xF) + (a & 0xF)) & 0x10 == 0x10;
                src.overflowing_add(a)
            }
            Arg::Mem(src) => {
                let addr = self.registers.read(src);
                let val = self.memory.read(addr);
                half_carry = ((val & 0xF) + (a & 0xF)) & 0x10 == 0x10;
                val.overflowing_add(a)
            }
            _ => panic!("Unexpected src {:?}", src),
        };

        self.registers.write(Reg8::A, result);

        self.flags.set(Flag::Zero, result == 0);
        self.flags.set(Flag::Subtract, false);
        self.flags.set(Flag::HalfCarry, half_carry);
        self.flags.set(Flag::Carry, carry);
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

    fn cp(&mut self, src: Arg) { let a = self.registers.read(Reg8::A);

        let other = match src {
            Arg::Reg8(src) => self.registers.read(src),
            Arg::Imm8(src) => src,
            Arg::MemHl => {
                let addr = self.registers.read(Reg16::HL);
                self.memory.read(addr)
            }
            _ => panic!("Unexpected src: {:?}", src),
        };

        // Flags
        if other == a {
            self.flags.set(Flag::Zero, true);
        } else if other > a {
            self.flags.set(Flag::Carry, true);
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
    use crate::cartridge::{RomSize, RamSize};

    #[test]
    fn test_add() {
        let memory = MemoryBus::new(RomSize::_1M, RamSize::_32K);
        let mut cpu = Cpu::new(memory);

        // Normal add
        let inst = Instruction::Add { src: Arg::Imm8(0x10) };
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0x10);

        // Overflow
        cpu.registers.write(Reg8::B, 0xF0);
        let inst = Instruction::Add { src: Reg8::B.into() };
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0x00);
        assert!(cpu.flags.zero());
        assert!(cpu.flags.carry());
    }
}