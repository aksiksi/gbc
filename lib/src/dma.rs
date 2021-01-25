use crate::memory::{MemoryBus, MemoryRead, MemoryWrite};
use crate::ppu::{Ppu, StatMode};

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

    /// HDMA active flag
    hdma_active: bool,

    /// Length of the HDMA transfer specified by user.
    hdma_length: u8,

    /// HDMA transfer mode
    ///
    /// Two options: general purpose (`false`) or HBLANK (`true`).
    ///
    /// In general purpose mode, the transfer is done in one shot. In
    /// HBLANK mode, the transfer is chunked across HBLANKs.
    hdma_hblank: bool,

    /// Number of 16 byte chunks completed during the current transfer.
    hdma_chunks_completed: u8,

    cgb: bool,
}

impl DmaController {
    pub const DMA_ADDR: u16 = 0xFF46;

    pub fn new(cgb: bool) -> Self {
        Self {
            oam_dma_counter: 0,
            hdma_active: false,
            hdma_length: 0,
            hdma_hblank: false,
            hdma_chunks_completed: 0,
            cgb,
        }
    }

    /// Execute a single step of the DMA controller.
    ///
    /// If HDMA is executed, this returns the number of cycles consumed.
    pub fn step(&mut self, cycles: u8, memory: &mut MemoryBus) -> u16 {
        let mut cycles_taken = 0;

        // OAM DMA
        if memory.ppu().oam_dma_active {
            self.oam_dma(cycles, memory);
        }

        // HDMA
        if self.cgb && memory.io().hdma_active {
            // HDMA is a blocking operation. However, it needs the number of
            // cycles spent in the CPU to be able to figure out the next PPU
            // mode for the HBLANK check.
            cycles_taken = self.hdma(cycles, memory);
        }

        cycles_taken
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

            // Write directly to OAM in case it is locked
            let oam_index = (dest_addr - Ppu::OAM_START_ADDR) as usize;
            memory.ppu_mut().oam[oam_index] = data;

            self.oam_dma_counter += 1;
            cycles -= 4;
        }

        if self.oam_dma_counter == 162 {
            // DMA completed
            memory.ppu_mut().oam_dma_active = false;
            self.oam_dma_counter = 0;
        }
    }

    fn hdma(&mut self, cycles: u8, memory: &mut MemoryBus) -> u16 {
        let speed = memory.io().speed();

        // Determine source and destination addresses
        let source_addr_upper = memory.read(0xFF51) as u16;
        let source_addr_lower = memory.read(0xFF52) as u16;
        let dest_addr_upper = memory.read(0xFF53) as u16;
        let dest_addr_lower = memory.read(0xFF54) as u16;

        let source_addr = source_addr_upper << 8 | source_addr_lower;
        let source_addr = source_addr & 0xFFF0; // lower 4 bits are ignored
        let dest_addr = dest_addr_upper << 8 | dest_addr_lower;
        let dest_addr = 0x8000 + (dest_addr & 0x1FF0); // only bits 12-4 are taken

        let mut start_reg = memory.read(0xFF55);

        if !self.hdma_active {
            // New transfer is being started
            self.hdma_length = (start_reg & 0x7F) + 1;
            self.hdma_hblank = start_reg & (1 << 7) != 0;
            self.hdma_chunks_completed = 0;
            self.hdma_active = true;

            // Clear bit 7 to indicate that HDMA is active
            start_reg &= !1 << 7;
        } else {
            // If we are currently in HBLANK HDMA but see that bit 7 has been reset,
            // we need to terminate the transfer.
            if self.hdma_hblank && memory.io().hdma_stopped {
                self.hdma_active = false;

                memory.io_mut().hdma_active = false;
                memory.io_mut().hdma_stopped = false;
                memory.io_mut().hdma_reg_write(start_reg | 1 << 7);

                return 0;
            }
        }

        let num_chunks = if !self.hdma_hblank {
            // Transfer all data at once, and return the number of cycles consumed
            // to the CPU.
            self.hdma_length
        } else {
            // Ask the PPU what the _next_ mode is and how long the PPU will remain in
            // that mode.
            //
            // This needs to be done because we could be _just before_ HBLANK, but since
            // DMA kicks in before the PPU has a chance to catch up, we need to look ahead
            // based on the number of cycles spent in the CPU.
            let (next_mode, cycles_in_mode) = memory.ppu().next_mode(cycles as u16, speed);

            if next_mode != StatMode::Hblank {
                // If we are not in HBLANK, there is nothing to do
                return 0;
            }

            // Figure out the number of chunks we can transfer during this HBLANK
            let num_chunks = if speed {
                cycles_in_mode / 64
            } else {
                cycles_in_mode / 32
            };

            num_chunks as u8
        };

        // Figure out the start and end chunks for this step
        let mut chunk = self.hdma_chunks_completed;
        let end = self.hdma_chunks_completed + num_chunks;
        let end = if end > self.hdma_length {
            self.hdma_length
        } else {
            end
        };

        // Perform the transfer starting from the last transferred chunk
        loop {
            // Stop once all current chunks have been copied OR overall HDMA
            // transfer is complete
            if chunk == end {
                break;
            }

            // Copy over this chunk
            let chunk_start = chunk as u16 * 16;
            let chunk_end = chunk_start + 16;
            for offset in chunk_start..chunk_end {
                let byte = memory.read(source_addr + offset);
                memory.write(dest_addr + offset, byte);
            }

            chunk += 1;
        }

        // Adjust to the _actual_ number of chunks copied this cycle
        let num_chunks = chunk - self.hdma_chunks_completed;

        self.hdma_chunks_completed = chunk;

        if !self.hdma_hblank || self.hdma_chunks_completed == self.hdma_length {
            self.hdma_active = false;

            // Write 0xFF to the start register to signal completion
            memory.io_mut().hdma_active = false;
            memory.io_mut().hdma_stopped = false;
            memory.io_mut().hdma_reg_write(0xFF);
        } else {
            // If HBLANK HDMA is still pending, write the remaining transfer
            // length (minus 1) to the lower 7 bits of the start register.
            let remaining_length = self.hdma_length - self.hdma_chunks_completed - 1;
            memory.io_mut().hdma_reg_write((start_reg & 1 << 7) | remaining_length);
        }

        if speed {
            // In double-speed mode, HDMA transfers a chunk (16 bytes) every
            // 64 cycles
            num_chunks as u16 * 64
        } else {
            num_chunks as u16 * 32
        }
    }
}
