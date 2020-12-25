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

        let flags = &mut self.flags;
        let memory = &mut self.memory;
        let regs = &mut self.registers;

        let pc = &mut regs.PC;

        match instruction {
            Nop => (),
            Halt => (),
            Ld { dst, src } => match (dst, src) {
                (Arg::Reg16(dst), Arg::Imm16(src)) => {
                    regs.write(dst, src);
                }
                (Arg::Reg8(dst), Arg::Imm8(src)) => {
                    regs.write(dst, src);
                }
                (Arg::Reg8(dst), Arg::Reg8(src)) => {
                    regs.write(dst, regs.read(src));
                }
                (Arg::Reg8(dst), Arg::Mem(src)) => {
                    let addr = regs.read(src);
                    let value = memory.read(addr);
                    regs.write(dst, value);
                }
                (Arg::Mem(dst), Arg::Reg8(src)) => {
                    let addr = regs.read(dst);
                    memory.write(addr, regs.read(src));
                }
                (Arg::MemImm(dst), Arg::Reg8(src)) => {
                    memory.write(dst, regs.read(src));
                }
                (Arg::MemImm(dst), Arg::Reg16(src)) => {
                    memory.write(dst, regs.read(src));
                }
                (Arg::Reg8(dst), Arg::MemImm(src)) => {
                    let value = memory.read(src);
                    regs.write(dst, value);
                }
                _ => panic!("Unexpected dst and src: {:?}, {:?}", dst, src),
            },
            LdMemCA => {
                let addr = 0xFF00 + regs.read(Reg8::C) as u16;
                memory.write(addr, regs.read(Reg8::A));
            }
            LdAMemC => {
                let addr = 0xFF00 + regs.read(Reg8::C) as u16;
                regs.write(Reg8::A, memory.read(addr));
            }
            LdiAMemHl => {
                let addr = regs.read(Reg16::HL);
                let value = memory.read(addr);
                regs.write(Reg8::A, value);
                regs.write(Reg16::HL, addr.wrapping_add(1));
            }
            LdiMemHlA => {
                let addr = regs.read(Reg16::HL);
                memory.write(addr, regs.read(Reg8::A));
                regs.write(Reg16::HL, addr.wrapping_add(1));
            }
            LddAMemHl => {
                let addr = regs.read(Reg16::HL);
                let value = memory.read(addr);
                regs.write(Reg8::A, value);
                regs.write(Reg16::HL, addr.wrapping_sub(1));
            }
            LddMemHlA => {
                let addr = regs.read(Reg16::HL);
                memory.write(addr, regs.read(Reg8::A));
                regs.write(Reg16::HL, addr.wrapping_sub(1));
            }
            LdhA { offset } => {
                let value = memory.read(0xFF00 + offset as u16);
                regs.write(Reg8::A, value);
            }
            Ldh { offset } => {
                let a = regs.read(Reg8::A);
                memory.write(0xFF00 + offset as u16, a);
            }
            Xor { src } => {
                let a = regs.read(Reg8::A);

                let result = match src {
                    Arg::Reg8(src) => {
                        let curr = regs.read(src);
                        a ^ curr
                    }
                    Arg::Imm8(src) => a ^ src,
                    Arg::MemHl => {
                        let addr = regs.read(Reg16::HL);
                        let curr = memory.read(addr);
                        a ^ curr
                    }
                    _ => panic!("Unexpected src: {:?}", src),
                };

                regs.write(Reg8::A, result);

                // Flags
                flags.set(Flag::Zero, result == 0);
                flags.clear(Flag::Subtract);
                flags.clear(Flag::Carry);
                flags.clear(Flag::HalfCarry);
            }
            Inc { dst } => match dst {
                Arg::Reg8(dst) => {
                    let curr = regs.read(dst);
                    regs.write(dst, curr.wrapping_add(1));
                }
                Arg::Reg16(dst) => {
                    let curr = regs.read(dst);
                    regs.write(dst, curr.wrapping_add(1));
                }
                Arg::MemHl => {
                    let addr = regs.read(Reg16::HL);
                    let curr = memory.read(addr);
                    memory.write(addr, curr.wrapping_add(1));
                }
                _ => panic!("Unexpected dst: {:?}", dst),
            },
            Dec { dst } => {
                let mut update_flags = true;
                let mut half_carry = false;
                let mut carry = false;

                let result = match dst {
                    Arg::Reg8(dst) => {
                        // If lower nibble == 0, set the half-carry bit
                        half_carry = regs.read(dst) & 0x0F == 0;

                        // If value is 0, set the carry flag
                        carry = regs.read(dst) == 0;

                        let result = regs.read(dst).wrapping_sub(1);
                        regs.write(dst, result);
                        result as u16
                    }
                    Arg::Reg16(dst) => {
                        update_flags = false; // 16-bit variant does not touch flags
                        let result = regs.read(dst).wrapping_sub(1);
                        regs.write(dst, result);
                        result
                    }
                    Arg::MemHl => {
                        let addr = regs.read(Reg16::HL);
                        let curr = memory.read(addr);

                        // If lower nibble == 0, set the half-carry bit
                        half_carry = curr & 0xFF == 0;
                        carry = curr == 0;

                        let result = curr.wrapping_sub(1);
                        memory.write(addr, result);
                        result as u16
                    }
                    _ => panic!("Unexpected dst: {:?}", dst),
                };

                if update_flags {
                    flags.set(Flag::Zero, result == 0);
                    flags.set(Flag::Subtract, true);
                    flags.set(Flag::HalfCarry, half_carry);
                    flags.set(Flag::Carry, carry);
                }
            }
            Cp { src } => {
                let a = regs.read(Reg8::A);

                let other = match src {
                    Arg::Reg8(src) => regs.read(src),
                    Arg::Imm8(src) => src,
                    Arg::MemHl => {
                        let addr = regs.read(Reg16::HL);
                        memory.read(addr)
                    }
                    _ => panic!("Unexpected src: {:?}", src),
                };

                // Flags
                if other == a {
                    flags.set(Flag::Zero, true);
                } else if other > a {
                    flags.set(Flag::Carry, true);
                }
            }
            Jp { addr, cond } => {
                let ok = match cond {
                    Cond::None => true,
                    Cond::NotZero if !flags.zero() => true,
                    Cond::Zero if flags.zero() => true,
                    Cond::NotCarry if !flags.carry() => true,
                    Cond::Carry if flags.carry() => true,
                    _ => false,
                };

                if ok {
                    *pc = addr;
                }
            }
            Jr { offset, cond } => {
                let ok = match cond {
                    Cond::None => true,
                    Cond::NotZero if !flags.zero() => true,
                    Cond::Zero if flags.zero() => true,
                    Cond::NotCarry if !flags.carry() => true,
                    Cond::Carry if flags.carry() => true,
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

    pub fn memory(&self) -> &MemoryBus {
        &self.memory
    }

    pub fn reset(&mut self) {
        unimplemented!()
    }
}
