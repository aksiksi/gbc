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

    /// Runs the next instruction and returns the number of cycles it consumed
    pub fn step(&mut self) -> usize {
        let old_pc = self.registers.PC;

        // Decode the instruction at PC
        let (pc, inst, cycles) = Instruction::decode(self.registers.PC, self.memory.rom());

        // Execute the instruction on this CPU
        inst.execute(self);

        if old_pc == self.registers.PC {
            // If we did not execute a jump, proceed to next instruction as usual
            self.registers.PC = pc;
            cycles.not_taken() as usize
        } else {
            cycles.taken() as usize
        }
    }

    pub fn memory(&self) -> &MemoryBus {
        &self.memory
    }

    pub fn reset(&mut self) {
        unimplemented!()
    }
}
