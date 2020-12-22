use crate::memory::Memory;
use crate::registers::RegisterFile;

#[derive(Debug)]
pub struct Cpu {
    memory: Memory,
    registers: RegisterFile,
}

impl Cpu {
    pub fn new(memory: Memory) -> Self {
        let registers = RegisterFile::new();

        Self {
            memory,
            registers,
        }
    }

    pub fn memory(&self) -> &Memory {
        &self.memory
    }

    /// Consumes the current CPU and returns a new one
    pub fn reset(&mut self) {
        unimplemented!()
    }
}
