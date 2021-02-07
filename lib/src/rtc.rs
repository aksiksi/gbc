//! Real-time Clock implementation for MBC3.
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use crate::cpu::Cpu;
use crate::error::Result;

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
struct RtcTime {
    seconds: u8,
    minutes: u8,
    hours: u8,
    days: u16,
    halt: bool,
    carry: bool,
}

impl RtcTime {
    fn new() -> Self {
        Self {
            seconds: 0,
            minutes: 0,
            hours: 0,
            days: 0,
            halt: false,
            carry: false,
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
struct RtcState {
    /// Current time
    current: RtcTime,

    /// Latched time
    latched: RtcTime,

    /// If `true`, a latch operation has been started
    latch_started: bool,

    /// Last timestamp
    timestamp: DateTime<Utc>,

    /// Last tick cycle
    tick_cycle: u64,

    /// Last cycle seen
    cycle: u64,

    /// Selected register
    selected: u8,
}

impl RtcState {
    fn new() -> Self {
        Self {
            current: RtcTime::new(),
            latched: RtcTime::new(),
            latch_started: false,
            timestamp: Utc::now(),
            tick_cycle: 0,
            cycle: 0,
            selected: 0,
        }
    }

    fn step(&mut self, cycles: u16, speed: bool) {
        let cycle_time = Cpu::cycle_time(speed) as u64;

        if self.current.halt {
            // Clock is halted
            self.timestamp = Utc::now();
            return;
        }

        self.cycle += cycles as u64;

        if (self.cycle - self.tick_cycle) / cycle_time >= Rtc::TICK_INTERVAL {
            self.tick();
        }
    }

    fn tick(&mut self) {
        // Adjust for skew by getting the actual number of seconds
        // since the last tick
        let now = Utc::now();
        let seconds = (now - self.timestamp).num_seconds();
        self.current.seconds += seconds as u8;

        if self.current.seconds > 0x3B {
            self.current.seconds -= 60;
            self.current.minutes += 1;
        }

        if self.current.minutes > 0x3B {
            self.current.minutes = 0;
            self.current.hours += 1;
        }

        if self.current.hours > 0x17 {
            self.current.hours = 0;
            self.current.days += 1;
        }

        if self.current.days > 0x1FF {
            self.current.days = 0;
            self.current.carry = true;
        }

        self.timestamp = now;
        self.tick_cycle = self.cycle;
    }

    fn select(&mut self, register: u8) {
        self.selected = register;
    }

    fn latch(&mut self, value: u8) {
        if value == 0 {
            self.latch_started = true;
        } else if self.latch_started {
            self.latched = self.current;
            self.latch_started = false;
        }
    }

    fn read(&self) -> u8 {
        match self.selected {
            0x08 => {
                // Seconds
                self.latched.seconds
            }
            0x09 => {
                // Minutes
                self.latched.minutes
            }
            0x0A => {
                // Hours
                self.latched.hours
            }
            0x0B => {
                // Lower 8 bits of day
                (self.latched.days & 0xFF) as u8
            }
            0x0C => {
                // Upper bit of day, carry bit, halt flag
                let mut value = (self.latched.days & 1 << 8 >> 8) as u8;
                let halt_bit = if self.latched.halt {
                    1
                } else {
                    0
                };
                let carry_bit = if self.latched.carry {
                    1
                } else {
                    0
                };

                value |= halt_bit << 6;
                value |= carry_bit << 7;

                value
            }
            _ => unreachable!(),
        }
    }

    fn write(&mut self, value: u8) {
        match self.selected {
            0x08 => {
                // Seconds
                self.current.seconds = value;
            }
            0x09 => {
                // Minutes
                self.current.minutes = value;
            }
            0x0A => {
                // Hours
                self.current.hours = value;
            }
            0x0B => {
                // Lower 8 bits of day
                self.current.days |= value as u16;
            }
            0x0C => {
                // Upper bit of day, carry bit, halt flag
                self.current.days &= !(1 << 8);
                self.current.days |= (value as u16 & 1) << 8;
                self.current.halt = value & 1 << 6 != 0;
                self.current.carry = value & 1 << 7 != 0;
            }
            _ => unreachable!(),
        }
    }

    /// Advance the RTC to the current timestamp.
    ///
    /// This needs to be done right after loading an RTC state from a file.
    fn advance(&mut self) {
        let now = Utc::now();

        if self.current.halt {
            self.timestamp = now;
            return;
        }

        let delta = now - self.timestamp;

        // Figure out the number of seconds, minutes, hours, and days that have
        // occurred since the last RTC timestamp
        let seconds = delta.num_seconds();
        assert!(seconds >= 0, "RTC timestamp is more recent than current UTC time");
        let minutes = seconds / 60;
        let hours = minutes / 60;
        let days = hours / 24;

        self.current.seconds = ((self.current.seconds as i64 + seconds) % 60) as u8;
        self.current.minutes = ((self.current.minutes as i64 + minutes) % 60) as u8;
        self.current.hours = ((self.current.hours as i64 + hours) % 24) as u8;
        self.current.days = ((self.current.days as i64 + days) % 512) as u16;

        self.timestamp = now;
    }
}

/// Real-time Clock implementation for MBC3
///
/// On every tick of the clock, the last UTC timestamp is written
/// to a backing file, if any.
#[cfg_attr(feature = "save", derive(serde::Serialize), derive(serde::Deserialize))]
pub struct Rtc {
    /// RTC state
    state: RtcState,
}

impl Rtc {
    // RTC tick frequency, in Hz
    const FREQUENCY: f32 = 32768.0;

    // Tick interval for the RTC, in ns
    const TICK_INTERVAL: u64 = (1e9 / Self::FREQUENCY) as u64;

    pub fn new() -> Self {
        Self {
            state: RtcState::new(),
        }
    }

    /// Create RTC state from raw bytes
    pub fn from_bytes(data: &[u8]) -> Result<Self> {
        let state = bincode::deserialize_from(data)?;
        Ok(Self {
            state
        })
    }

    /// Dump the state of the RTC
    pub fn dump(&self) -> Vec<u8> {
        let mut data = Vec::new();
        bincode::serialize_into(&mut data, &self).unwrap();
        data
    }

    pub fn step(&mut self, cycles: u16, speed: bool) {
        self.state.step(cycles, speed);
    }

    pub fn select(&mut self, register: u8) {
        self.state.select(register);
    }

    pub fn latch(&mut self, value: u8) {
        self.state.latch(value);
    }

    pub fn read(&self) -> u8 {
        self.state.read()
    }

    pub fn write(&mut self, value: u8) {
        self.state.write(value);
    }

    pub fn advance(&mut self) {
        self.state.advance()
    }
}
