//! Real-time Clock implementation for MBC3.
use std::fs::{File, OpenOptions};
use std::io::{Seek, SeekFrom};
use std::path::Path;

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use crate::cpu::Cpu;
use crate::error::Result;

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub struct RtcTime {
    pub seconds: u8,
    pub minutes: u8,
    pub hours: u8,
    pub days: u16, 
    pub halt: bool,
    pub carry: bool,
}

impl RtcTime {
    pub fn new() -> Self {
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
    pub fn new() -> Self {
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

    pub fn step(&mut self, cycles: u16, speed: bool) -> bool {
        let cycle_time = Cpu::cycle_time(speed) as u64;

        if self.current.halt {
            // Clock is halted
            self.timestamp = Utc::now();
            return false;
        }

        self.cycle += cycles as u64;

        if (self.cycle - self.tick_cycle) / cycle_time >= Rtc::TICK_INTERVAL {
            self.tick();
            true
        } else {
            false
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

    pub fn select(&mut self, register: u8) {
        self.selected = register;
    }

    pub fn latch(&mut self, value: u8) {
        if value == 0 {
            self.latch_started = true;
        } else if self.latch_started {
            self.latched = self.current;
            self.latch_started = false;
        }
    }

    pub fn read(&self) -> u8 {
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

    pub fn write(&mut self, value: u8) {
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
    pub fn advance(&mut self) {
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

    /// RTC state file
    #[cfg_attr(feature = "save", serde(skip))]
    file: Option<File>,
}

impl Rtc {
    // RTC tick frequency, in Hz
    const FREQUENCY: f32 = 32768.0;

    // Tick interval for the RTC, in ns
    const TICK_INTERVAL: u64 = (1e9 / Self::FREQUENCY) as u64;

    pub fn new() -> Self {
        Self {
            state: RtcState::new(),
            file: None,
        }
    }

    pub fn step(&mut self, cycles: u16, speed: bool) {
        let tick = self.state.step(cycles, speed);
        if tick {
            // Serialize RTC state to file after each tick
            self.dump().unwrap();
        }
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

        // Serialize RTC state to file after each write
        self.dump().unwrap();
    }

    /// Dump current RTC state to a file. The file is overwritten
    /// every time.
    fn dump(&mut self) -> Result<()> {
        if let Some(file) = &mut self.file {
            file.seek(SeekFrom::Start(0))?;
            bincode::serialize_into(file, &self.state)?;
        }

        Ok(())
    }

    /// If an RTC file exists, load the state it. Otherwise, create a new state.
    ///
    /// Once the state is loaded, adjust current clock based on difference between it and the
    /// last timestamp.
    ///
    /// If `overwrite` is `true`, overwrite any existing file with the current state. This
    /// is used when loading from a save state.
    pub fn with_file<P: AsRef<Path>>(&mut self, rom_path: P, overwrite: bool) -> Result<()> {
        let rtc_path = rom_path.as_ref().with_extension("rtcs");
        let rtc_file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(rtc_path)?;

        self.file = Some(rtc_file);

        let rtc_file = self.file.as_mut().unwrap();

        if overwrite {
            // Overwrite the contents of the backing file with the current RTC state
            self.dump()?;
        } else if rtc_file.metadata()?.len() > 0 {
            // Load last RTC state from file
            self.state = bincode::deserialize_from(rtc_file)?;

            // If the RTC was not halted, advance the RTC until the current time in UTC
            self.state.advance();

            // Persist the updated state to disk
            self.dump()?;
        }

        Ok(())
    }
}
