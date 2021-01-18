/// Internal CGB timer functionality
pub struct Timer {
    /// Divider register (0xFF04)
    ///
    /// Ticks at frequency = Cpu::CYCLE_TIME * 256
    ///
    /// It is affected by CGB double-speed mode.
    div: u8,
    div_counter: u16,

    /// Timer counter (0xFF05)
    ///
    /// Ticks at frequency specified by TAC register.
    tima: u8,
    tima_counter: u16,

    /// Timer modulo (0xFF06)
    ///
    /// When TIMA overflows, this value is written to TIMA.
    tma: u8,

    /// Timer control (0xFF07)
    ///
    /// Bit 2: Timer enable
    /// Bits 0-1: Clock select
    tac: u8,
}

impl Timer {
    /// Timer clock runs at 16.384 kHz.
    ///
    /// As a ratio, this is equal to 256 CPU clock cycles.
    pub const TIMER_RATIO: u16 = 256;

    pub fn new() -> Self {
        Self {
            div: 0,
            div_counter: 0,
            tima: 0,
            tima_counter: 0,
            tma: 0,
            tac: 0,
        }
    }

    #[inline]
    pub fn enabled(&self) -> bool {
        (self.tac & 1 << 2) != 0
    }

    /// This function is called once per CPU step from the main Gameboy loop.
    ///
    /// Returns `true` if an interrupt should be triggered.
    pub fn step(&mut self, cycles: u8) -> bool {
        let cycles = cycles as u16;

        // DIV: Ticks once per 256 clock cycles
        // The TAC enable flag does not affect this
        self.div_counter += cycles;
        if self.div_counter >= 256 {
            self.div = self.div.wrapping_add(1);
            self.div_counter -= 256;
        }

        if !self.enabled() {
            return false;
        }

        let threshold = self.tima_ratio();
        let mut interrupt = false;

        self.tima_counter += cycles;
        while self.tima_counter >= threshold {
            let tima = self.tima.wrapping_add(1);

            self.tima = if tima < self.tima {
                // When TIMA overflows, load TMA into TIMA, and trigger an interrupt
                interrupt = true;
                self.tma
            } else {
                tima
            };

            self.tima_counter -= threshold;
        }

        interrupt
    }

    /// Returns the ratio of CPU clock cycle time to current TIMA clock
    /// mode in TAC
    #[inline]
    fn tima_ratio(&self) -> u16 {
        match self.tac & 0x3 {
            0 => Self::TIMER_RATIO * 4,
            1 => Self::TIMER_RATIO / 16,
            2 => Self::TIMER_RATIO / 4,
            3 => Self::TIMER_RATIO,
            _ => unreachable!(),
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            0xFF04 => self.div,
            0xFF05 => self.tima,
            0xFF06 => self.tma,
            0xFF07 => self.tac,
            _ => unreachable!(),
        }
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        match addr {
            0xFF04 => {
                // Writes to DIV reset the register
                self.div = 0;
                self.div_counter = 0;
                self.tima_counter = 0;
            }
            0xFF05 => self.tima = value,
            0xFF06 => self.tma = value,
            0xFF07 => self.tac = value,
            _ => unreachable!(),
        }
    }
}
