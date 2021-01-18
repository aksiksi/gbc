use crate::memory::{MemoryBus, MemoryRead, MemoryWrite};

pub struct DmaController {
    /// OAM DMA counter
    ///
    /// OAM DMA transfers take 162 cycles in total:
    ///
    /// * 2 delay cycles
    /// * 160 transfer cycles
    ///
    /// Once this counter hits 162, DMA is stopped.
    oam_dma_counter: u8,
}

impl DmaController {
    pub const DMA_ADDR: u16 = 0xFF46;

    pub fn new() -> Self {
        Self {
            oam_dma_counter: 0,
        }
    }

    pub fn step(&mut self, cycles: u8, memory: &mut MemoryBus) {
        // OAM DMA
        if memory.ppu().oam_dma_active {
            self.oam_dma(cycles, memory);
        }
    }

    fn oam_dma(&mut self, mut cycles: u8, memory: &mut MemoryBus) {
        let dma_reg = memory.read(Self::DMA_ADDR);
        let source_start_addr = (dma_reg as u16) << 8;

        // OAM DMA transfers 1 byte every 4 clock cycles
        while cycles >= 4 && self.oam_dma_counter < 162 {
            if self.oam_dma_counter < 2 {
                // Delay cycles
                self.oam_dma_counter += 1;
                cycles -= 4;
                continue;
            }

            let source_addr = source_start_addr + (self.oam_dma_counter - 2) as u16;
            let dest_addr = 0xFE00 + (self.oam_dma_counter - 2) as u16;

            let data = memory.read(source_addr);
            memory.write(dest_addr, data);

            self.oam_dma_counter += 1;
            cycles -= 4;
        }

        if self.oam_dma_counter == 162 {
            // DMA completed
            memory.ppu_mut().oam_dma_active = false;
            self.oam_dma_counter = 0;
        }
    }
}
