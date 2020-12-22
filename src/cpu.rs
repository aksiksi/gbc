use crate::instructions::Instruction;
use crate::memory::MemoryBus;
use crate::registers::RegisterFile;

#[derive(Debug)]
pub struct Cpu {
    registers: RegisterFile,
    memory: MemoryBus,
}

impl Cpu {
    pub fn new(memory: MemoryBus) -> Self {
        let registers = RegisterFile::new();
        Self { registers, memory }
    }

    /// Runs the next instruction and returns the number of cycles it consumed
    pub fn step(&mut self) -> usize {
        let (pc, inst, cycles) = Instruction::decode(self.registers.PC, self.memory.rom());
        self.registers.PC = pc;
        dbg!(inst);
        cycles
    }

    pub fn memory(&self) -> &MemoryBus {
        &self.memory
    }

    pub fn reset(&mut self) {
        unimplemented!()
    }
}
