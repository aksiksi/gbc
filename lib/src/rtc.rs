//! Real-time Clock implementation for MBC3.
use std::fs::{File, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::Path;

use chrono::{DateTime, Utc};

use crate::cpu::Cpu;
use crate::error::Result;

#[derive(Clone, Copy)]
#[cfg_attr(feature = "save", derive(serde::Serialize), derive(serde::Deserialize))]
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

#[cfg_attr(feature = "save", derive(serde::Serialize), derive(serde::Deserialize))]
/// Real-time Clock implementation for MBC3
///
/// On every tick of the clock, the last UTC timestamp is written
/// to a backing file, if any.
pub struct Rtc {
    /// Current time
    current: RtcTime,

    /// Latched time
    latched: RtcTime,

    latch_started: bool,

    /// Last timestamp
    timestamp: DateTime<Utc>,

    /// Last tick cycle
    tick_cycle: u64,

    /// Last cycle seen
    cycle: u64,

    /// Selected register
    selected: u8,

    /// Backing file
    #[cfg_attr(feature = "save", serde(skip))]
    file: Option<File>,
}

impl Rtc {
    // Tick interval for the RTC, in ns
    const TICK_INTERVAL: u64 = (1e9 / 32768 as f32) as u64;

    pub fn new() -> Self {
        Self {
            current: RtcTime::new(),
            latched: RtcTime::new(),
            latch_started: false,
            timestamp: Utc::now(),
            tick_cycle: 0,
            cycle: 0,
            selected: 0,
            file: None,
        }
    }

    pub fn step(&mut self, cycles: u16, speed: bool) {
        let cycle_time = Cpu::cycle_time(speed) as u64;

        if self.current.halt {
            // Clock is halted
            return;
        }

        self.cycle += cycles as u64;

        if (self.cycle - self.tick_cycle) / cycle_time >= Self::TICK_INTERVAL {
            self.tick();
        }
    }

    fn tick(&mut self) {
        self.current.seconds += 1;

        if self.current.seconds > 0x3B {
            self.current.seconds = 0;
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

        self.timestamp = Utc::now();
        self.tick_cycle = self.cycle;

        if let Some(file) = &mut self.file {
            file.seek(SeekFrom::Start(0)).unwrap();
            file.write(self.timestamp.to_rfc3339().as_bytes()).unwrap();
        }
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

    /// If an RTC file exists, load timestamp from the file. Otherwise, create a new one.
    ///
    /// Once a timestamp is loaded, adjust current clock based on difference between it and the
    /// last timestamp.
    ///
    /// If `overwrite` is `true`, overwrite any existing file with the current state. This
    /// is used when loading from a save state.
    pub fn enable_battery<P: AsRef<Path>>(&mut self, rom_path: P, overwrite: bool) -> Result<()> {
        let rtc_path = rom_path.as_ref().with_extension("rtc");
        let mut rtc_file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(rtc_path)?;

        if overwrite {
            // Overwrite the contents of the backing file
            let s = self.timestamp.to_rfc3339();
            rtc_file.write_all(s.as_bytes())?;
            self.file = Some(rtc_file);
            return Ok(());
        }

        let timestamp = if rtc_file.metadata()?.len() != 0 {
            // Load last timestamp from the file
            let mut s = String::new();
            rtc_file.read_to_string(&mut s)?;
            DateTime::parse_from_rfc3339(&s)
                .expect("Failed to parse time from file")
                .with_timezone(&Utc)
                .into()
        } else {
            None
        };

        if let Some(timestamp) = timestamp {

        }

        // TODO
        Ok(())
    }
}
