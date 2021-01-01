use std::io::Write;

use crate::cpu::Cpu;
use crate::instructions::Instruction;
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
    breakpoints: Vec<(u16, bool)>,
    instructions: Vec<Instruction>,
}

impl Debugger {
    pub fn new() -> Self {
        Self {
            mode: Mode::Step,
            steps: 0,
            checks: 0,
            breakpoints: Vec::new(),
            instructions: Vec::new(),
        }
    }

    pub fn triggered(&mut self, cpu: &Cpu) -> bool {
        // If the CPU is currently halted, keep waiting
        if cpu.is_halted {
            return false;
        }

        self.checks += 1;

        // Keep track of each instruction the CPU executes
        let (inst, _, _) = cpu.fetch();
        self.instructions.push(inst);

        match self.mode {
            Mode::Step => true,
            Mode::StepN(n) => {
                (self.checks - self.steps) >= n
            }
            Mode::Continue => {
                for (addr, enabled) in &self.breakpoints {
                    if *enabled && cpu.registers.PC == *addr {
                        return true;
                    }
                }

                false
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

                    self.breakpoints.push((addr, true));
                }
                "d" if line.len() == 2 => {
                    // Delete a breakpoint
                    let index: usize = line[1].parse().unwrap();
                    if index >= self.breakpoints.len() {
                        eprintln!("Invalid breakpoint {}", index);
                        continue;
                    }

                    self.breakpoints.remove(index);
                }
                "disable" if line.len() == 2 => {
                    // Disable a breakpoint
                    let index: usize = line[1].parse().unwrap();
                    if index >= self.breakpoints.len() {
                        eprintln!("Invalid breakpoint {}", index);
                        continue;
                    }

                    self.breakpoints[index].1 = false;
                }
                "l" | "list" => {
                    // Print the last 5 instructions
                    let mut i = (self.instructions.len() as i32 - 1) - 5;

                    while i >= 0 {
                        println!("inst {}: {:x?}", i+1, self.instructions[i as usize]);
                        i -= 1;
                    }
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
                    // TODO: Parse address in decimal or hex
                    let addr: u16 = match line[1].parse() {
                        Err(_) => {
                            eprintln!("Invalid address specified");
                            continue;
                        }
                        Ok(v) => v,
                    };

                    let value = cpu.memory.read(addr);

                    println!("{:#X}", value);
                }
                "info" if line.len() == 2 => {
                    match line[1] {
                        "r" | "reg" | "registers" => {
                            dbg!(&cpu.registers);
                        }
                        "b" | "break" | "breakpoints" => {
                            let mut i = 0;
                            for (addr, enabled) in &self.breakpoints {
                                println!("breakpoint {}: addr = {:#X}, enabled = {}", i, addr, enabled);
                                i += 1;
                            }
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
