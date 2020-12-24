use std::convert::TryInto;

use crate::instructions::Instruction;
use crate::memory::MemoryBus;
use crate::registers::{Flags, RegisterFile};

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
        Self { registers, flags, memory }
    }

    /// Executes the next instruction and returns the number of cycles it
    /// took to complete.
    pub fn step(&mut self) -> u8 {
        let pc = self.registers.PC;

        // Extract the next 3 bytes from ROM, starting from PC.
        // This is enough information to be able to decode the next
        // instruction.
        let rom = self.memory().rom();
        let data: [u8; 3] = rom.range(pc..pc+3).try_into().unwrap();

        // Decode the instruction
        let (inst, size, cycles) = Instruction::decode(data);

        // Execute the instruction on this CPU
        inst.execute(self);

        if pc == self.registers.PC {
            // If we did not execute a jump, proceed to next instruction as usual
            self.registers.PC = pc + size;
            cycles.not_taken()
        } else {
            cycles.taken()
        }
    }

    pub fn memory(&self) -> &MemoryBus {
        &self.memory
    }

    pub fn reset(&mut self) {
        unimplemented!()
    }
}
