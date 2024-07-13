use std::fs::File;
use std::io::{BufWriter, Write};

use crate::cartridge::Cartridge;
use crate::dma::DmaController;
use crate::error::Result;
use crate::instructions::{Arg, Cond, Cycles, Instruction};
use crate::memory::{MemoryBus, MemoryRead, MemoryWrite};
use crate::registers::{Flag, Reg16, Reg8, RegisterFile, RegisterOps};

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum Interrupt {
    Vblank = 0,
    LcdStat,
    Timer,
    Serial,
    Joypad,
}

/// Types of logical operations
///
/// This allows us to use a single method for all supported
/// logical operations.
#[derive(Clone, Copy)]
enum LogicalOp {
    And,
    Or,
    Xor,
}

/// Small trait that abstracts computation of half-carry
/// between two numbers
trait HalfCarry<T> {
    /// Half carry for addition
    fn half_carry(&self, other: T) -> bool;

    /// Half carry for subtraction
    fn half_carry_sub(&self, other: T) -> bool;
}

impl HalfCarry<u8> for u8 {
    fn half_carry(&self, other: u8) -> bool {
        ((self & 0xF) + (other & 0xF)) & 0x10 != 0
    }

    fn half_carry_sub(&self, other: u8) -> bool {
        (self & 0xF) < (other & 0xF)
    }
}

impl HalfCarry<u16> for u16 {
    fn half_carry(&self, other: u16) -> bool {
        ((self & 0x0FFF) + (other & 0x0FFF)) & 0x1000 != 0
    }

    fn half_carry_sub(&self, other: u16) -> bool {
        (self & 0x00FF) < (other & 0x00FF)
    }
}

#[cfg_attr(feature = "save", derive(serde::Serialize), derive(serde::Deserialize))]
pub struct Cpu {
    pub registers: RegisterFile,
    pub memory: MemoryBus,
    dma: DmaController,
    pub cgb: bool,

    pub halted: bool,
    pub stopped: bool,
    pub speed: bool,

    /// Global interrupt enable flag (Interrupt Master Enable)
    ime: bool,

    /// Trace all instructions executed to a file
    #[cfg_attr(feature = "save", serde(skip))]
    trace: Option<BufWriter<File>>,
}

impl Cpu {
    /// Base CPU frequency, in Hz
    pub const BASE_FREQ: u32 = 4_194_304;

    /// CPU cycle time, in ns
    pub const CYCLE_TIME: u32 = ((1.0 / Self::BASE_FREQ as f64) * 1e9) as u32;

    /// Create an empty CPU without a cartridge
    ///
    /// Mainly used for tests
    pub fn new(cgb: bool) -> Self {
        let memory = MemoryBus::new(cgb);
        let registers = RegisterFile::new(cgb);

        Self {
            registers,
            memory,
            dma: DmaController::new(cgb),
            cgb,
            ime: false,
            halted: false,
            stopped: false,
            speed: false,
            trace: None,
        }
    }

    /// Create a CPU from a cartridge
    pub fn from_cartridge(cartridge: Cartridge, trace: bool) -> Result<Self> {
        let cgb = cartridge.cgb();
        let boot_rom = cartridge.boot_rom;
        let memory = MemoryBus::from_cartridge(cartridge)?;

        let registers = if boot_rom {
            // If boot ROM is required, keep registers empty
            RegisterFile::empty()
        } else {
            // Otherwise, init registers based on mode
            RegisterFile::new(cgb)
        };

        let dma = DmaController::new(cgb);

        // If tracing is enabled, create a trace file in the current directory
        let trace = if trace {
            let f = File::create("gbc.trace")?;
            Some(BufWriter::new(f))
        } else {
            None
        };

        Ok(Self {
            registers,
            memory,
            dma,
            cgb,
            ime: false,
            halted: false,
            stopped: false,
            speed: false,
            trace,
        })
    }

    /// Current clock cycle duration, in ns. This value is based
    /// on the current value in the speed I/O register.
    #[inline]
    pub fn cycle_time(speed: bool) -> u32 {
        if speed {
            Self::CYCLE_TIME / 2
        } else {
            Self::CYCLE_TIME
        }
    }

    /// Reset this CPU to initial state
    ///
    /// This involves resetting memory, the ROM controller, and the PPU
    pub fn reset(&mut self) {
        self.registers = RegisterFile::new(self.cgb);
        self.memory.reset();
        self.dma = DmaController::new(self.cgb);
        self.ime = false;
        self.halted = false;
        self.stopped = false;
        self.speed = false;
    }

    /// Executes the next instruction and returns the number of cycles it
    /// took to complete.
    pub fn step(&mut self) -> (u16, Instruction) {
        // Check for pending interrupts before fetching the next instruction.
        // If an interrupt is serviced, PC will jump to the ISR address.
        let int_cycles = self.service_interrupts();

        // If the CPU is halted, bail out
        if self.halted {
            return (4, Instruction::Nop);
        }

        // Fetch and decode the next instruction at PC
        let (inst, size, cycles) = self.fetch(None);

        if let Some(_) = &mut self.trace {
            self.trace(&inst);
        }

        // Execute the instruction on this CPU
        let (jump, taken) = self.execute(inst);
        let mut cycles = if !jump || jump && !taken {
            // For regular instructions and jumps that are *not* taken,
            // update the PC based on the size of this instruction
            self.registers.PC += size as u16;
            cycles.not_taken() as u16
        } else {
            // For jumps that are taken, the PC is updated within `execute()`
            cycles.taken() as u16
        };

        if self.stopped {
            // If we've entered STOP mode for a speed switch, block the CPU for a bit.
            // Also, do not run through DMA.
            cycles += 8200;
        } else {
            cycles += int_cycles as u16;
            cycles += self.dma(cycles);
        }

        if self.speed {
            cycles /= 2;
        }

        (cycles, inst)
    }

    /// Switch CPU speed
    fn speed_switch(&mut self) {
        if !self.speed {
            // Normal to double speed
            self.memory.io_mut().prep_speed_switch = 1 << 7;
            self.speed = true;
        } else {
            // Double to normal speed
            self.memory.io_mut().prep_speed_switch = 0;
            self.speed = false;
        }

        self.stopped = true;
    }

    #[inline]
    fn trace(&mut self, inst: &Instruction) {
        // Figure out the currently active ROM bank based on the memory region of PC
        let pc = self.registers.PC;
        let (memory_type, bank) = self.memory.memory_info(pc);
        let f = &mut self.trace.as_mut().unwrap();

        // First, write the register state prior to executing this instruction
        write!(f, "{}\n\n", self.registers).unwrap();

        // Then write the instruction
        write!(f, "{:03}:{}:{:#06X} - {}\n\n", bank, memory_type, pc, inst).unwrap();

        f.flush().unwrap();
    }

    /// Execute a single step of DMA (if active).
    fn dma(&mut self, cycles: u16) -> u16 {
        let memory = &mut self.memory;
        self.dma.step(cycles, self.speed, memory)
    }

    /// Fetch the next instruction and return it
    pub fn fetch(&self, addr: Option<u16>) -> (Instruction, u8, Cycles) {
        let addr = addr.unwrap_or(self.registers.PC);

        // Read the next 3 bytes from memory, starting from PC.
        // This is what we will use to decode the next instruction.
        //
        // TODO: Evaluate the boundary cases
        let data: [u8; 3] = [
            self.memory.read(addr),
            self.memory.read(addr+1),
            self.memory.read(addr+2),
        ];

        // Decode the instruction
        Instruction::decode(data)
    }

    /// Disassemble the next `count` instructions, starting at the given address.
    pub fn disassemble(&self, count: usize, addr: Option<u16>) -> Vec<(Instruction, u16)> {
        let mut addr = addr.unwrap_or(self.registers.PC);
        let mut result = Vec::new();

        for _ in  0..count {
            let (inst, size, _) = self.fetch(Some(addr));
            result.push((inst, addr));
            addr = addr.wrapping_add(size as u16);
        }

        result
    }

    /// Figure out which interrupts are pending and service the one with the
    /// highest priority.
    ///
    /// Servicing an interrupt takes 20 clock cycles according to Pandocs.
    ///
    /// See: pg. 27 of GB Programming Manual
    fn service_interrupts(&mut self) -> u8 {
        let int_enable = self.memory.read(0xFFFF);
        let int_flags = self.memory.read(0xFF0F);

        // If no interrupts are pending, bail out now
        if int_enable & int_flags == 0 {
            return 0;
        }

        // If the CPU is currently halted and there is a pending interrupt,
        // leave HALT state, *even if IME is disabled*.
        if self.halted {
            self.halted = (int_enable & int_flags) == 0;
        }

        // If the IME is disabled, do not process any interrupts
        if !self.ime {
            return 0;
        }

        // Iterate over each interrupt in priority order and service
        // the first one
        for int in 0..5 {
            let enabled = (int_enable & 1 << int) != 0;
            let pending = (int_flags & 1 << int) != 0;

            if enabled && pending {
                // Disable interrupts
                self.ime = false;

                // Clear the pending flag
                self.memory.write(0xFF0F, int_flags & !(1 << int));

                // Push current PC to the stack
                self.push(self.registers.PC);

                // Compute the ISR address to jump to
                let isr = (int << 3) + 0x40;

                self.registers.PC = isr;

                break;
            }
        }

        20
    }

    /// Trigger a particular interrupt
    #[inline]
    pub fn trigger_interrupt(&mut self, interrupt: Interrupt) {
        let bit = interrupt as u8;
        let int_flags = self.memory.read(0xFF0F);
        self.memory.write(0xFF0F, int_flags | 1 << bit);
    }

    /// Execute a single instruction on this CPU
    ///
    /// This method returns a tuple of: (jump, taken)
    ///
    /// * `jump`: Whether or not this was a jump-type instruction (e.g., jp, ret, call)
    /// * `taken`: Whether or not the jump was taken. If this is not a jump, returns `false`
    pub fn execute(&mut self, instruction: Instruction) -> (bool, bool) {
        use Instruction::*;

        let mut jump = false;
        let mut taken = false;

        match instruction {
            Nop => (),
            Halt => {
                self.halted = true;
            }
            Stop => {
                if self.cgb && self.memory.io().prep_speed_switch & 0x1 != 0 {
                    // Switch speed
                    self.speed_switch();
                }

                // Reset timer DIV register
                self.memory.timer().write(0xFF04, 0);
            }
            Di => {
                self.ime = false;
            }
            Ei => {
                self.ime = true;
            }

            // Load
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
                (Arg::Mem(dst), Arg::Imm8(src)) => {
                    let addr = self.registers.read(dst);
                    self.memory.write(addr, src);
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
                (Arg::Reg16(Reg16::SP), Arg::Reg16(Reg16::HL)) => {
                    let value = self.registers.read(Reg16::HL);
                    self.registers.write(Reg16::SP, value);
                }
                _ => unreachable!("Unexpected dst and src: {}, {}", dst, src),
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
            LdHlSpImm8i { offset } | AddSpImm8i { offset } => {
                let offset = offset as u16;
                let val = self.registers.read(Reg16::SP);

                // These instructions use 8-bit carry and half-carry rules
                let half_carry = ((val & 0xF) + (offset & 0xF)) & 0x10 != 0;
                let carry = ((val & 0xFF) + (offset & 0xFF)) & 0x100 != 0;

                let result = val.wrapping_add(offset);

                // Destination register is the only difference between these
                // two instructions
                if let LdHlSpImm8i { .. } = instruction {
                    self.registers.write(Reg16::HL, result);
                } else {
                    self.registers.write(Reg16::SP, result);
                }

                self.registers.set(Flag::Zero, false);
                self.registers.set(Flag::Subtract, false);
                self.registers.set(Flag::HalfCarry, half_carry);
                self.registers.set(Flag::Carry, carry);
            }
            Inc { dst } => self.inc(dst),
            Dec { dst } => self.dec(dst),
            Add { src } => self.add(src),
            Adc { src } => self.adc(src),
            AddHlReg16 { src } => self.add_hl(src),
            Sub { src } => self.sub(src, true),
            Sbc { src } => self.sbc(src),
            And { src } => self.logical(src, LogicalOp::And),
            Or { src } => self.logical(src, LogicalOp::Or),
            Xor { src } => self.logical(src, LogicalOp::Xor),
            Cp { src } => self.sub(src, false),
            Pop { dst } => {
                let value = self.pop();
                self.registers.write(dst, value);
            }
            Push { src } => {
                let value = self.registers.read(src);
                self.push(value);
            }
            Ret { cond } => {
                let ok = match cond {
                    Cond::None => true,
                    Cond::NotZero => !self.registers.zero(),
                    Cond::Zero => self.registers.zero(),
                    Cond::NotCarry => !self.registers.carry(),
                    Cond::Carry => self.registers.carry(),
                };

                jump = true;

                if ok {
                    // Pop address from the stack iff the condition is met
                    self.registers.PC = self.pop();
                    taken = true;
                }
            }
            RetI => {
                let addr = self.pop();
                self.registers.PC = addr;
                self.ime = true;
                jump = true;
                taken = true;
            }
            Rst { offset } => {
                // Push next PC onto stack, then jump to offset
                self.push(self.registers.PC + 1);
                self.registers.PC = 0x0000 + offset as u16;
                jump = true;
                taken = true;
            }
            Jp { addr, cond } | Call { addr, cond } => {
                let ok = match cond {
                    Cond::None => true,
                    Cond::NotZero => !self.registers.zero(),
                    Cond::Zero => self.registers.zero(),
                    Cond::NotCarry => !self.registers.carry(),
                    Cond::Carry => self.registers.carry(),
                };

                jump = true;

                if ok {
                    // If this is a CALL, push the *next* PC to the stack
                    if let Call { .. } = instruction {
                        // CALL is always 3 bytes long
                        let next = self.registers.PC + 3;
                        self.push(next);
                    }

                    self.registers.PC = addr;
                    taken = true;
                }
            }
            JpHl => {
                let addr = self.registers.read(Reg16::HL);
                self.registers.PC = addr;
                jump = true;
                taken = true;
            }
            Jr { offset, cond } => {
                let ok = match cond {
                    Cond::None => true,
                    Cond::NotZero => !self.registers.zero(),
                    Cond::Zero => self.registers.zero(),
                    Cond::NotCarry => !self.registers.carry(),
                    Cond::Carry => self.registers.carry(),
                };

                jump = true;

                if ok {
                    // Numeric casts from signed to unsigned will sign extend
                    let offset = offset as u16;
                    self.registers.PC = self.registers.PC.wrapping_add(2).wrapping_add(offset);
                    taken = true;
                }
            }

            // Rotate variants
            Rlc { dst } => self.rotate(dst, true, false, false),
            Rl { dst } => self.rotate(dst, true, true, false),
            Rrc { dst } => self.rotate(dst, false, false, false),
            Rr { dst } => self.rotate(dst, false, true, false),
            Rlca => self.rotate(Arg::Reg8(Reg8::A), true, false, true),
            Rla => self.rotate(Arg::Reg8(Reg8::A), true, true, true),
            Rrca => self.rotate(Arg::Reg8(Reg8::A), false, false, true),
            Rra => self.rotate(Arg::Reg8(Reg8::A), false, true, true),

            // Shift variants
            Sla { dst } => self.shift(dst, true, false),
            Sra { dst } => self.shift(dst, false, true),
            Srl { dst } => self.shift(dst, false, false),

            // Swap upper & lower nibbles
            Swap { dst } => {
                let result = match dst {
                    Arg::Reg8(dst) => {
                        let value = self.registers.read(dst);
                        let value = (value << 4) | (value >> 4);
                        self.registers.write(dst, value);
                        value
                    }
                    Arg::MemHl => {
                        let addr = self.registers.read(Reg16::HL);
                        let value = self.memory.read(addr);
                        let value = (value << 4) | (value >> 4);
                        self.memory.write(addr, value);
                        value
                    }
                    _ => unreachable!("Unexpected dst: {}", dst),
                };

                self.registers.set(Flag::Zero, result == 0);
                self.registers.set(Flag::Subtract, false);
                self.registers.set(Flag::HalfCarry, false);
                self.registers.set(Flag::Carry, false);
            }

            // Bit manipulation instructions
            Bit { dst, bit } => {
                let value = match dst {
                    Arg::Reg8(dst) => self.registers.read(dst),
                    Arg::MemHl => {
                        let addr = self.registers.read(Reg16::HL);
                        self.memory.read(addr)
                    }
                    _ => unreachable!("Unexpected dst: {}", dst),
                };

                let result = value & (1 << bit);

                self.registers.set(Flag::Zero, result == 0);
                self.registers.set(Flag::Subtract, false);
                self.registers.set(Flag::HalfCarry, true);
            }
            Set { dst, bit } => self.set(dst, bit, false),
            Res { dst, bit } => self.set(dst, bit, true),

            // Misc
            Daa => self.daa(),
            Cpl => {
                let curr = self.registers.read(Reg8::A);
                self.registers.write(Reg8::A, !curr);
                self.registers.set(Flag::Subtract, true);
                self.registers.set(Flag::HalfCarry, true);
            }
            Ccf => {
                let f = self.registers.carry();
                self.registers.set(Flag::Carry, !f);
                self.registers.set(Flag::Subtract, false);
                self.registers.set(Flag::HalfCarry, false);
            }
            Scf => {
                self.registers.set(Flag::Carry, true);
                self.registers.set(Flag::Subtract, false);
                self.registers.set(Flag::HalfCarry, false);
            }
        }

        (jump, taken)
    }

    /// Handle rotate instructions
    ///
    /// # Arguments
    ///
    /// * `left`: If true, value is rotated left
    /// * `through`: If true, value is rotated through the carry bit
    /// * `a`: If true, A variant of the instruction is used
    fn rotate(&mut self, dst: Arg, left: bool, through: bool, a: bool) {
        let curr = match dst {
            Arg::Reg8(dst) => {
                self.registers.read(dst)
            }
            Arg::MemHl => {
                let addr = self.registers.read(Reg16::HL);
                self.memory.read(addr)
            }
            _ => unreachable!("Unexpected dst: {}", dst),
        };

        let prev_carry = self.registers.carry();
        let carry;
        let mut value;

        if left {
            carry = curr & (1 << 7) != 0;
            value = curr.rotate_left(1);

            if through {
                // Set bit 0 to old carry
                if prev_carry {
                    value |= 1 << 0;
                } else {
                    value &= !(1 << 0);
                }
            }
        } else {
            carry = curr & (1 << 0) != 0;
            value = curr.rotate_right(1);

            if through {
                if prev_carry {
                    value |= 1 << 7;
                } else {
                    value &= !(1 << 7);
                }
            }
        }

        // Write back the result
        match dst {
            Arg::Reg8(dst) => {
                self.registers.write(dst, value)
            }
            Arg::MemHl => {
                let addr = self.registers.read(Reg16::HL);
                self.memory.write(addr, value);
            }
            _ => unreachable!("Unexpected dst: {}", dst),
        }

        let zero_flag = if a {
            false
        } else {
            value == 0
        };

        // Flags
        self.registers.set(Flag::Zero, zero_flag);
        self.registers.set(Flag::Subtract, false);
        self.registers.set(Flag::HalfCarry, false);
        self.registers.set(Flag::Carry, carry);
    }

    /// Handle shift instructions
    fn shift(&mut self, dst: Arg, left: bool, arithmetic: bool) {
        let curr = match dst {
            Arg::Reg8(dst) => {
                self.registers.read(dst)
            }
            Arg::MemHl => {
                let addr = self.registers.read(Reg16::HL);
                self.memory.read(addr)
            }
            _ => unreachable!("Unexpected dst: {}", dst),
        };

        let carry;
        let value;

        if left {
            carry = curr & (1 << 7) != 0;
            value = curr.wrapping_shl(1);
        } else {
            carry = curr & (1 << 0) != 0;

            if arithmetic {
                // In case of arithmetic right shift, cast to i8 before shifting,
                // then cast back. This takes care of the MSB.
                value = (curr as i8).wrapping_shr(1) as u8;
            } else {
                value = curr.wrapping_shr(1);
            }
        }

        // Write back the result
        match dst {
            Arg::Reg8(dst) => {
                self.registers.write(dst, value)
            }
            Arg::MemHl => {
                let addr = self.registers.read(Reg16::HL);
                self.memory.write(addr, value);
            }
            _ => unreachable!("Unexpected dst: {}", dst),
        }

        // Flags
        self.registers.set(Flag::Zero, value == 0);
        self.registers.set(Flag::Subtract, false);
        self.registers.set(Flag::HalfCarry, false);
        self.registers.set(Flag::Carry, carry);
    }

    /// Helper that pops 2 bytes off the stack
    fn pop(&mut self) -> u16 {
        // Read upper and lower bytes from stack.
        let lower = self.memory.read(self.registers.SP);
        let upper = self.memory.read(self.registers.SP + 1);
        let value = (upper as u16) << 8 | lower as u16;

        // Increment SP
        self.registers.SP += 2;

        value
    }

    /// Helper that pushes 2 bytes to the stack
    fn push(&mut self, value: u16) {
        let lower = value as u8;
        let upper = (value >> 8) as u8;

        // Write upper and lower bytes seperately to the stack.
        // We cannot use the `MemoryWrite` trait because it assumes
        // that memory addresses increase instead of decrease.
        self.memory.write(self.registers.SP-1, upper);
        self.memory.write(self.registers.SP-2, lower);

        // Decrement SP
        self.registers.SP -= 2;
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
            _ => unreachable!("Unexpected src: {}", src),
        };

        let half_carry = a.half_carry(val);
        let (result, carry) = a.overflowing_add(val);

        self.registers.write(Reg8::A, result);

        self.registers.set(Flag::Zero, result == 0);
        self.registers.set(Flag::Subtract, false);
        self.registers.set(Flag::HalfCarry, half_carry);
        self.registers.set(Flag::Carry, carry);
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
            _ => unreachable!("Unexpected src: {}", src),
        };

        let curr_carry = if self.registers.carry() { 1u8 } else { 0u8 };

        // First, add the current carry to A and compute initial carry flags
        let tmp = a.wrapping_add(curr_carry);
        let mut carry = tmp < a;
        let mut half_carry = a.half_carry(curr_carry);

        // Then, add the actual value to A and update the carry flags (if needed)
        let result = tmp.wrapping_add(val);
        carry = carry || result < tmp;
        half_carry = half_carry || tmp.half_carry(val);

        self.registers.write(Reg8::A, result);

        self.registers.set(Flag::Zero, result == 0);
        self.registers.set(Flag::Subtract, false);
        self.registers.set(Flag::HalfCarry, half_carry);
        self.registers.set(Flag::Carry, carry);
    }

    /// 16-bit version of ADD for HL
    fn add_hl(&mut self, src: Reg16) {
        let hl = self.registers.read(Reg16::HL);
        let half_carry: bool;

        let (result, carry) = {
            let val = self.registers.read(src);
            half_carry = hl.half_carry(val);
            hl.overflowing_add(val)
        };

        self.registers.write(Reg16::HL, result);

        self.registers.set(Flag::Subtract, false);
        self.registers.set(Flag::HalfCarry, half_carry);
        self.registers.set(Flag::Carry, carry);
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
            _ => unreachable!("Unexpected src: {}", src),
        };

        let half_carry = a.half_carry_sub(val);
        let (result, carry) = a.overflowing_sub(val);

        // Write back the result for non-CP
        if write {
            self.registers.write(Reg8::A, result);
        }

        self.registers.set(Flag::Zero, result == 0);
        self.registers.set(Flag::Subtract, true);
        self.registers.set(Flag::HalfCarry, half_carry);
        self.registers.set(Flag::Carry, carry);
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
            _ => unreachable!("Unexpected src: {}", src),
        };

        let curr_carry = if self.registers.carry() { 1u8 } else { 0u8 };

        // First, subtract the current carry from A and compute initial carry flags
        let tmp = a.wrapping_sub(curr_carry);
        let mut carry = tmp > a;
        let mut half_carry = a.half_carry_sub(curr_carry);

        // Then, subtract the actual value from A and update the carry flags (if needed)
        let result = tmp.wrapping_sub(val);
        carry = carry || result > tmp;
        half_carry = half_carry || tmp.half_carry_sub(val);

        self.registers.write(Reg8::A, result);

        self.registers.set(Flag::Zero, result == 0);
        self.registers.set(Flag::Subtract, true);
        self.registers.set(Flag::HalfCarry, half_carry);
        self.registers.set(Flag::Carry, carry);
    }

    fn logical(&mut self, src: Arg, op: LogicalOp) {
        let a = self.registers.read(Reg8::A);

        let val = match src {
            Arg::Reg8(src) => self.registers.read(src),
            Arg::Imm8(src) => src,
            Arg::MemHl => {
                let addr = self.registers.read(Reg16::HL);
                let val = self.memory.read(addr);
                val
            }
            _ => unreachable!("Unexpected src: {}", src),
        };

        let result = match op {
            LogicalOp::And => a & val,
            LogicalOp::Or => a | val,
            LogicalOp::Xor => a ^ val,
        };

        self.registers.write(Reg8::A, result);

        self.registers.set(Flag::Zero, result == 0);
        self.registers.set(Flag::Subtract, false);
        self.registers.set(Flag::Carry, false);

        if let LogicalOp::And = op {
            self.registers.set(Flag::HalfCarry, true);
        } else {
            self.registers.set(Flag::HalfCarry, false);
        }
    }

    /// Increment instruction
    fn inc(&mut self, dst: Arg) {
        let mut update_flags = true;
        let mut half_carry = false;

        let result = match dst {
            Arg::Reg8(dst) => {
                let curr = self.registers.read(dst);
                let result = curr.wrapping_add(1);
                half_carry = curr.half_carry(1);
                self.registers.write(dst, result);
                result as u16
            }
            Arg::Reg16(dst) => {
                let curr = self.registers.read(dst);
                let result = curr.wrapping_add(1);
                update_flags = false;
                self.registers.write(dst, result);
                result
            }
            Arg::MemHl => {
                let addr = self.registers.read(Reg16::HL);
                let curr = self.memory.read(addr);
                let result = curr.wrapping_add(1);
                half_carry = curr.half_carry(1);
                self.memory.write(addr, result);
                result as u16
            }
            _ => unreachable!("Unexpected dst: {}", dst),
        };

        if update_flags {
            self.registers.set(Flag::Zero, result == 0);
            self.registers.set(Flag::Subtract, false);
            self.registers.set(Flag::HalfCarry, half_carry);
        }
    }

    /// Decrement instruction
    fn dec(&mut self, dst: Arg) {
        let mut update_flags = true;
        let mut half_carry = false;

        let result = match dst {
            Arg::Reg8(dst) => {
                let curr = self.registers.read(dst);

                // If lower nibble == 0, set the half-carry bit
                half_carry = curr & 0x0F == 0;

                let result = curr.wrapping_sub(1);
                self.registers.write(dst, result);

                result as u16
            }
            Arg::Reg16(dst) => {
                update_flags = false; // 16-bit variant does not touch flags
                let result = self.registers.read(dst).wrapping_sub(1);
                self.registers.write(dst, result);
                result
            }
            Arg::MemHl => {
                let addr = self.registers.read(Reg16::HL);
                let curr = self.memory.read(addr);

                // If lower nibble == 0, set the half-carry bit
                half_carry = curr & 0x0F == 0;

                let result = curr.wrapping_sub(1);
                self.memory.write(addr, result);

                result as u16
            }
            _ => unreachable!("Unexpected dst: {}", dst),
        };

        if update_flags {
            self.registers.set(Flag::Zero, result == 0);
            self.registers.set(Flag::Subtract, true);
            self.registers.set(Flag::HalfCarry, half_carry);
        }
    }

    /// Set and Res instructions
    fn set(&mut self, dst: Arg, bit: u8, reset: bool) {
        let value = match dst {
            Arg::Reg8(dst) => self.registers.read(dst),
            Arg::MemHl => {
                let addr = self.registers.read(Reg16::HL);
                self.memory.read(addr)
            }
            _ => unreachable!("Unexpected dst: {}", dst),
        };

        let result;

        if !reset {
            result = value | (1 << bit);
        } else {
            result = value & !(1 << bit);
        }

        match dst {
            Arg::Reg8(dst) => self.registers.write(dst, result),
            Arg::MemHl => {
                let addr = self.registers.read(Reg16::HL);
                self.memory.write(addr, result);
            }
            _ => unreachable!("Unexpected dst: {}", dst),
        }
    }

    /// Adjust A to BCD following ADD or SUB
    //
    /// Refer to first table on pg. 110 of GB Programming Manual
    fn daa(&mut self) {
        let subtract = self.registers.subtract();
        let half_carry = self.registers.half_carry();
        let mut carry = self.registers.carry();

        let mut a = self.registers.read(Reg8::A);

        if !subtract {
            if carry || a > 0x99 {
                a = a.wrapping_add(0x60);
                carry = true;
            }
            if half_carry || (a & 0x0F) > 0x9 {
                a = a.wrapping_add(0x06);
            }
        } else {
            if half_carry {
                a = a.wrapping_sub(0x06);
            }
            if carry {
                a = a.wrapping_sub(0x60);
            }
        }

        self.registers.write(Reg8::A, a);

        self.registers.set(Flag::Zero, a == 0);
        self.registers.clear(Flag::HalfCarry);
        self.registers.set(Flag::Carry, carry);
    }

    pub fn memory(&self) -> &MemoryBus {
        &self.memory
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn get_cpu() -> Cpu {
        Cpu::new(true)
    }

    #[test]
    fn interrupts() {
        let mut cpu = get_cpu();

        cpu.registers.PC = 0x1000;
        cpu.registers.write(Reg8::A, 0x01u8);
        cpu.registers.write(Reg8::B, 0x40u8);

        // Enable interrupts
        let inst = Instruction::Ei;
        cpu.execute(inst);

        // Enable VBLANK and LCD STAT
        cpu.memory.write(0xFFFF, 0x03u8);

        // Prepare basic ISRs in ROM:
        //
        // Vblank:
        // 1. ADD A, B
        // 2. RETI
        //
        // LcdStat:
        // 1. NOP
        // 2. RET
        let controller = cpu.memory.controller_mut();
        controller.rom.write(0x40, 0x80u8);
        controller.rom.write(0x41, 0xD9u8);
        controller.rom.write(0x48, 0x00u8);
        controller.rom.write(0x49, 0xC9u8);

        // Trigger VBLANK and LCD interrupts
        cpu.trigger_interrupt(Interrupt::Vblank);
        cpu.trigger_interrupt(Interrupt::LcdStat);

        // Execute a CPU step and verify that the ADD
        // in the VBLANK ISR was executed
        cpu.step();
        assert!(!cpu.ime);
        assert_eq!(cpu.registers.PC, 0x41);
        assert_eq!(cpu.registers.read(Reg8::A), 0x41);

        // Step again
        // RETI should restore original PC and re-enable interrupts
        cpu.step();
        assert!(cpu.ime);
        assert_eq!(cpu.registers.PC, 0x1000);

        // Step again -> this should trigger LcdStat and execute a NOP
        cpu.step();
        assert!(!cpu.ime);
        assert_eq!(cpu.registers.PC, 0x49);

        // Last step -> verify PC is restored
        cpu.step();
        assert!(!cpu.ime);
        assert_eq!(cpu.registers.PC, 0x1000);
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
        assert!(cpu.registers.zero());
        assert!(cpu.registers.carry());

        // Half overflow
        cpu.registers.write(Reg8::A, 0x3C);
        let inst = Instruction::Add { src: 0xFFu8.into() };
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0x3B);
        assert!(!cpu.registers.zero());
        assert!(!cpu.registers.subtract());
        assert!(cpu.registers.half_carry());
        assert!(cpu.registers.carry());
    }

    #[test]
    fn add_hl() {
        let mut cpu = get_cpu();

        let old_zero = cpu.registers.zero();

        cpu.registers.write(Reg8::A, 0);
        cpu.registers.write(Reg16::HL, 0);
        cpu.registers.write(Reg16::DE, 0x1234);

        let inst = Instruction::AddHlReg16 { src: Reg16::DE };

        // Normal add
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg16::HL), 0x1234);
        assert!(cpu.registers.zero() == old_zero);
        assert!(!cpu.registers.subtract());
        assert!(!cpu.registers.half_carry());
        assert!(!cpu.registers.carry());

        // Overflow
        cpu.registers.write(Reg16::DE, 0xEDCC);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg16::HL), 0x0);
        assert!(cpu.registers.zero() == old_zero);
        assert!(!cpu.registers.subtract());
        assert!(cpu.registers.half_carry());
        assert!(cpu.registers.carry());
    }

    #[test]
    fn adc() {
        let mut cpu = get_cpu();

        cpu.registers.write(Reg8::A, 0);

        // Normal add
        let inst = Instruction::Adc { src: 0x10u8.into() };
        cpu.registers.set(Flag::Carry, true);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0x11);
        assert!(!cpu.registers.zero());
        assert!(!cpu.registers.carry());

        // Overflow
        let inst = Instruction::Adc { src: Reg8::B.into() };
        cpu.registers.set(Flag::Carry, true);
        cpu.registers.write(Reg8::A, 0xE1);
        cpu.registers.write(Reg8::B, 0x1E);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0x00);
        assert!(cpu.registers.zero());
        assert!(cpu.registers.half_carry());
        assert!(cpu.registers.carry());
    }

    #[test]
    fn sub() {
        let mut cpu = get_cpu();

        cpu.registers.write(Reg8::A, 0x10);

        // Normal sub
        let inst = Instruction::Sub { src: 0x10u8.into() };
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0);
        assert!(cpu.registers.zero());
        assert!(cpu.registers.subtract());

        // Underflow
        let inst = Instruction::Sub { src: 0x10u8.into() };
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0xF0);
        assert!(!cpu.registers.zero());
        assert!(cpu.registers.subtract());
        assert!(cpu.registers.carry());

        // Half underflow
        cpu.registers.write(Reg8::A, 0x3E);
        let inst = Instruction::Sub { src: 0xFu8.into() };
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0x2F);
        assert!(!cpu.registers.zero());
        assert!(cpu.registers.subtract());

        cpu.registers.write(Reg8::F, 0);
        cpu.registers.write(Reg8::A, 0x83);
        cpu.registers.write(Reg8::B, 0x38);
        let inst = Instruction::Sub { src: Reg8::B.into() };
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0x4B);
        assert!(!cpu.registers.zero());
        assert!(cpu.registers.subtract());
        assert!(cpu.registers.half_carry());
    }

    #[test]
    fn sbc() {
        let mut cpu = get_cpu();

        // Normal sub
        let inst = Instruction::Sbc { src: 0x09u8.into() };
        cpu.registers.write(Reg8::A, 0x0A);
        cpu.registers.set(Flag::Carry, true);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0x00);
        assert!(cpu.registers.zero());
        assert!(!cpu.registers.carry());

        // Overflow
        let inst = Instruction::Sbc { src: Reg8::B.into() };
        cpu.registers.set(Flag::Carry, true);
        cpu.registers.write(Reg8::A, 0x3B);
        cpu.registers.write(Reg8::B, 0x4F);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0xEB);
        assert!(!cpu.registers.zero());
        assert!(cpu.registers.subtract());
        assert!(cpu.registers.half_carry());
        assert!(cpu.registers.carry());
    }

    #[test]
    fn logical() {
        let mut cpu = get_cpu();

        // AND
        let inst = Instruction::And { src: 0x02u8.into() };
        cpu.registers.write(Reg8::A, 0x0A);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0x02);
        assert!(!cpu.registers.zero());
        assert!(cpu.registers.half_carry());

        // OR
        let inst = Instruction::Or { src: 0xF0u8.into() };
        cpu.registers.write(Reg8::A, 0x0A);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0xFA);
        assert!(!cpu.registers.zero());
        assert!(!cpu.registers.half_carry());

        // XOR
        let inst = Instruction::Xor { src: 0xFFu8.into() };
        cpu.registers.write(Reg8::A, 0x0F);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0xF0);
        assert!(!cpu.registers.zero());
        assert!(!cpu.registers.half_carry());
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
        assert!(cpu.registers.zero());
        assert!(cpu.registers.half_carry());
        assert!(!cpu.registers.subtract());
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
        assert!(!cpu.registers.zero());
        assert!(cpu.registers.half_carry());
        assert!(cpu.registers.subtract());
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
        assert!(!cpu.registers.zero());
        assert!(cpu.registers.subtract());
        assert!(cpu.registers.half_carry());
        assert!(!cpu.registers.carry());

        let inst = Instruction::Cp { src: 0x3Cu8.into() };
        cpu.execute(inst);
        assert!(cpu.registers.zero());
        assert!(cpu.registers.subtract());

        let (addr, value) = (0xC000, 0x40u8);
        cpu.memory.write(addr, value);
        cpu.registers.write(Reg16::HL, addr);
        let inst = Instruction::Cp { src: Arg::MemHl };
        cpu.execute(inst);
        assert!(!cpu.registers.zero());
        assert!(cpu.registers.subtract());
        assert!(!cpu.registers.half_carry());
        assert!(cpu.registers.carry());
    }

    #[test]
    fn daa() {
        let mut cpu = get_cpu();

        cpu.registers.write(Reg8::A, 0x45);
        cpu.registers.write(Reg8::B, 0x38);

        // ADD, then DAA
        let inst = Instruction::Add { src: Reg8::B.into() };
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0x7D);

        let inst = Instruction::Daa;
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0x83);
        assert!(!cpu.registers.carry());

        // SUB, then DAA
        let inst = Instruction::Sub { src: Reg8::B.into() };
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0x4B);
        assert!(cpu.registers.subtract());

        let inst = Instruction::Daa;
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0x45);
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
        assert_eq!(cpu.registers.read(Reg16::AF), 0x1230);
    }

    #[test]
    fn jumps() {
        let mut cpu = get_cpu();

        let inst = Instruction::Jp { addr: 0x2345, cond: Cond::None };
        cpu.execute(inst);
        assert_eq!(cpu.registers.PC, 0x2345);

        let inst = Instruction::Jp { addr: 0x1234, cond: Cond::Zero };
        cpu.registers.set(Flag::Zero, true);
        cpu.execute(inst);
        assert_eq!(cpu.registers.PC, 0x1234);

        let inst = Instruction::Jr { offset: -0x36, cond: Cond::None };
        cpu.execute(inst);
        assert_eq!(cpu.registers.PC, 0x1200);

        let inst = Instruction::Jr { offset: 1, cond: Cond::NotCarry };
        cpu.registers.write(Reg16::PC, 0xFFFF);
        cpu.registers.set(Flag::Carry, false);
        cpu.execute(inst);
        assert_eq!(cpu.registers.PC, 0x2);

        let inst = Instruction::JpHl;
        cpu.registers.write(Reg16::HL, 0x1234);
        cpu.execute(inst);
        assert_eq!(cpu.registers.PC, 0x1234);

        // CALL, then RET
        let inst = Instruction::Call { addr: 0x1234, cond: Cond::None };
        cpu.registers.write(Reg16::PC, 0xFF00);
        cpu.registers.write(Reg16::SP, 0xFFFE);
        cpu.execute(inst);
        assert_eq!(cpu.registers.PC, 0x1234);
        assert_eq!(cpu.registers.SP, 0xFFFC);

        let inst = Instruction::Ret { cond: Cond::None };
        cpu.execute(inst);
        assert_eq!(cpu.registers.PC, 0xFF03); // Next PC pushed during CALL
        assert_eq!(cpu.registers.SP, 0xFFFE);

        // RST
        let inst = Instruction::Rst { offset: 0x10 };
        cpu.registers.write(Reg16::PC, 0xFF00);
        cpu.registers.write(Reg16::SP, 0xFFFE);
        cpu.execute(inst);
        assert_eq!(cpu.registers.PC, 0x0010);
        assert_eq!(cpu.registers.SP, 0xFFFC);
        assert_eq!(cpu.pop(), 0xFF01);
    }

    #[test]
    fn ret() {
        let mut cpu = get_cpu();

        let inst = Instruction::Ret { cond: Cond::None };
        cpu.push(0x1234);
        cpu.execute(inst);
        assert_eq!(cpu.registers.PC, 0x1234);

        let inst = Instruction::RetI;
        cpu.push(0x1234);
        cpu.execute(inst);
        assert_eq!(cpu.registers.PC, 0x1234);
    }

    #[test]
    fn rotate_shift_swap() {
        let mut cpu = get_cpu();

        let inst = Instruction::Rlc { dst: Reg8::B.into() };
        cpu.registers.write(Reg8::B, 0x85);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::B), 0x0B);
        assert!(!cpu.registers.zero());
        assert!(!cpu.registers.subtract());
        assert!(!cpu.registers.half_carry());
        assert!(cpu.registers.carry());

        let inst = Instruction::Rl { dst: Reg8::L.into() };
        cpu.registers.clear(Flag::Carry);
        cpu.registers.write(Reg8::L, 0x80);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::L), 0);
        assert!(cpu.registers.zero());
        assert!(!cpu.registers.subtract());
        assert!(!cpu.registers.half_carry());
        assert!(cpu.registers.carry());

        let inst = Instruction::Rrc { dst: Reg8::C.into() };
        cpu.registers.clear(Flag::Carry);
        cpu.registers.write(Reg8::C, 0x1);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::C), 0x80);
        assert!(!cpu.registers.zero());
        assert!(!cpu.registers.subtract());
        assert!(!cpu.registers.half_carry());
        assert!(cpu.registers.carry());

        let inst = Instruction::Rr { dst: Reg8::A.into() };
        cpu.registers.clear(Flag::Carry);
        cpu.registers.write(Reg8::A, 0x1);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0);
        assert!(cpu.registers.zero());
        assert!(!cpu.registers.subtract());
        assert!(!cpu.registers.half_carry());
        assert!(cpu.registers.carry());

        let inst = Instruction::Sla { dst: Reg8::D.into() };
        cpu.registers.write(Reg8::D, 0x80);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::D), 0);
        assert!(cpu.registers.zero());
        assert!(!cpu.registers.subtract());
        assert!(!cpu.registers.half_carry());
        assert!(cpu.registers.carry());

        let inst = Instruction::Sra { dst: Reg8::A.into() };
        cpu.registers.write(Reg8::A, 0x8A);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0xC5);
        assert!(!cpu.registers.zero());
        assert!(!cpu.registers.subtract());
        assert!(!cpu.registers.half_carry());
        assert!(!cpu.registers.carry());

        cpu.registers.write(Reg8::A, 0x1);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0);
        assert!(cpu.registers.zero());
        assert!(!cpu.registers.subtract());
        assert!(!cpu.registers.half_carry());
        assert!(cpu.registers.carry());

        let inst = Instruction::Srl { dst: Reg8::A.into() };
        cpu.registers.clear(Flag::Carry);
        cpu.registers.write(Reg8::A, 0xFF);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0x7F);
        assert!(!cpu.registers.zero());
        assert!(!cpu.registers.subtract());
        assert!(!cpu.registers.half_carry());
        assert!(cpu.registers.carry());

        let inst = Instruction::Swap { dst: Reg8::A.into() };
        cpu.registers.write(Reg8::A, 0xF1);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::A), 0x1F);
        assert!(!cpu.registers.zero());
        assert!(!cpu.registers.subtract());
        assert!(!cpu.registers.half_carry());
        assert!(!cpu.registers.carry());
    }

    #[test]
    fn bit() {
        let mut cpu = get_cpu();

        let inst = Instruction::Bit { dst: Reg8::B.into(), bit: 2 };
        cpu.registers.write(Reg8::B, 0x4);
        cpu.execute(inst);
        assert!(!cpu.registers.zero());
        assert!(!cpu.registers.subtract());
        assert!(cpu.registers.half_carry());

        let inst = Instruction::Set { dst: Reg8::B.into(), bit: 3 };
        cpu.registers.write(Reg8::B, 0x7);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::B), 0xF);

        let inst = Instruction::Res { dst: Reg8::B.into(), bit: 3 };
        cpu.registers.write(Reg8::B, 0xF);
        cpu.execute(inst);
        assert_eq!(cpu.registers.read(Reg8::B), 0x7);
    }
}
