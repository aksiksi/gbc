use std::io::Write;

use crate::cpu::Cpu;
use crate::memory::MemoryRead;

pub enum Mode {
    Step,
    StepN(u32),
    Continue,
}

pub struct Debugger {
    mode: Mode,
    checks: u32,
    steps: u32,
    breakpoints: Vec<u16>,
}

impl Debugger {
    pub fn new() -> Self {
        Self {
            mode: Mode::Step,
            steps: 0,
            checks: 0,
            breakpoints: Vec::new(),
        }
    }

    pub fn triggered(&mut self, pc: u16) -> bool {
        self.checks += 1;

        match self.mode {
            Mode::Step => true,
            Mode::StepN(n) => {
                (self.checks - self.steps) >= n
            }
            Mode::Continue => {
                self.breakpoints.contains(&pc)
            }
        }
    }

    pub fn repl(&mut self, cpu: &Cpu) {
        self.steps += 1;

        loop {
            print!("gbcdbg> ");
            std::io::stdout().flush().unwrap();

            let mut line = String::new();
            std::io::stdin().read_line(&mut line).unwrap();

            let line: Vec<&str> = line.trim().split(" ").collect();

            match line[0] {
                "q" => {
                    std::process::exit(0);
                }
                "b" if line.len() == 2 => {
                    let addr: u16 = match line[1].parse() {
                        Err(_) => {
                            eprintln!("Invalid address specified");
                            continue;
                        }
                        Ok(v) => v,
                    };

                    self.breakpoints.push(addr);
                }
                "n" if line.len() == 2 => {
                    let n: u32 = line[1].parse().unwrap();
                    self.mode = Mode::StepN(n);
                    return;
                }
                "n" => {
                    self.mode = Mode::Step;
                    return;
                }
                "r" => {
                    self.mode = Mode::Continue;
                    return;
                }
                "p" if line.len() == 2 => {
                    let addr: u16 = match line[1].parse() {
                        Err(_) => {
                            eprintln!("Invalid address specified");
                            continue;
                        }
                        Ok(v) => v,
                    };

                    let value = cpu.memory.read(addr);

                    println!("0x{:x}", value);
                }
                "info" if line.len() == 2 => {
                    match line[1] {
                        "r" | "reg" | "registers" => {
                            dbg!(&cpu.registers);
                        }
                        "b" | "break" | "breakpoints" => {
                            dbg!(&self.breakpoints);
                        }
                        unknown => eprintln!("Unknown option for 'info': {}", unknown),
                    }
                }
                "info" => eprintln!("'info' requires at least 1 argument"),
                unknown => eprintln!("Unknown command: {}", unknown),
            }
        }

    }
}
