/// Internal CGB timer functionality
pub struct Timer {
    /// Divider register (0xFF04)
    ///
    /// Ticks at frequency = Cpu::CYCLE_TIME * 256
    ///
    /// It is affected by CGB double-speed mode.
    div: u8,

    /// Timer counter (0xFF05)
    ///
    /// Ticks at frequency specified by TAC register.
    tima: u8,

    /// Timer modulo (0xFF06)
    ///
    /// When TIMA overflows, this ticks.
    tma: u8,

    /// Timer control (0xFF07)
    ///
    /// Bit 2: Timer enable
    /// Bits 0-1: Clock select
    tac: u8,

    /// Last cycle TIMA was triggered
    last_tima_cycle: u64,
}

impl Timer {
    /// Timer clock runs at 16.384 kHz.
    ///
    /// As a ratio, this is equal to 256 CPU clock cycles.
    pub const TIMER_RATIO: u64 = 256;

    pub fn new() -> Self {
        Self {
            div: 0,
            tima: 0,
            tma: 0,
            tac: 0,
            last_tima_cycle: 0,
        }
    }

    pub fn enabled(&self) -> bool {
        (self.tac & 1 << 2) != 0
    }

    /// This function is called once per CPU step from the main Gameboy loop.
    ///
    /// Note that `cycle` is the absolute number of CPU cycles, *not* just
    /// for the current frame.
    ///
    /// Returns `true` if an interrupt should be triggered.
    pub fn step(&mut self, cycle: u64) -> bool {
        // DIV: Ticks once per 256 clock cycles
        // The TAC enable flag does not affect this
        self.div = (cycle / Self::TIMER_RATIO) as u8;

        if self.enabled() {
            let ratio = self.tima_ratio() as u64;

            // TIMA: Ticks at ratio defined by TAC
            let tima = if cycle - ratio >= self.last_tima_cycle {
                // Tick time
                self.tima.wrapping_add(1)
            } else {
                self.tima
            };

            if tima < self.tima {
                // When TIMA overflows, load TMA into TIMA, and trigger an interrupt
                self.tima = self.tma;
                return true;
            } else {
                self.tima = tima;
            }
        } else {
            self.last_tima_cycle = cycle;
        }

        false
    }

    /// Returns the ratio of CPU clock cycle time to current TIMA clock
    /// mode in TAC
    #[inline]
    fn tima_ratio(&self) -> u64 {
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
            }
            0xFF05 => self.tima = value,
            0xFF06 => self.tma = value,
            0xFF07 => self.tac = value,
            _ => unreachable!(),
        }
    }
}
