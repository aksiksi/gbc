use std::fs::File;
use std::io::Write;

use crate::cpu::Cpu;
use crate::instructions::Instruction;
use crate::memory::{MemoryRead, MemoryWrite};

const DEBUG_DUMP_FILE: &str = "dump.txt";

pub enum Mode {
    Continue,
    ContinueN(u32),
    ContinuePc(u16),
    Step,
    StepN(u32),
}

pub struct Debugger {
    mode: Mode,
    breakpoints: Vec<(u16, bool)>,
    instructions: Vec<(Instruction, u16)>,
    instruction_dump: Option<File>,
}

impl Debugger {
    pub fn new() -> Self {
        Self {
            mode: Mode::Step,
            breakpoints: Vec::new(),
            instructions: Vec::new(),
            instruction_dump: None,
        }
    }

    /// Returns `true` if a breakpoint was hit
    fn is_breakpoint_hit(&mut self, pc: u16) -> bool {
        let mut breakpoint_hit = false;

        for (addr, enabled) in &self.breakpoints {
            if *enabled && pc == *addr {
                breakpoint_hit = true;
            }
        }

        breakpoint_hit
    }

    pub fn triggered(&mut self, cpu: &Cpu) -> bool {
        // If the CPU is currently halted, keep waiting
        if cpu.is_halted {
            return false;
        }

        let pc = cpu.registers.PC;

        // Keep track of each instruction the CPU executes
        let (inst, _, _) = cpu.fetch(None);
        self.instructions.push((inst, pc));

        // Dump each instruction to a file
        if let Some(f) = self.instruction_dump.as_mut() {
            write!(f, "{:#06x}: {}\n", pc, inst).unwrap();
        }

        let res = match self.mode {
            Mode::Step => true,
            Mode::StepN(n) => {
                if n > 1 {
                    self.mode = Mode::StepN(n-1);
                    false
                } else {
                    // Revert to `step` mode
                    self.mode = Mode::Step;
                    true
                }
            }
            Mode::Continue => self.is_breakpoint_hit(pc),
            Mode::ContinueN(n) => {
                let mut hit = self.is_breakpoint_hit(pc);

                if hit {
                    if n > 1 {
                        self.mode = Mode::ContinueN(n-1);
                        hit = false;
                    } else {
                        // Revert to `step` mode
                        self.mode = Mode::Step;
                    }
                }

                hit
            }
            Mode::ContinuePc(addr) => pc == addr,
        };

        if res {
            // When a breakpoint is hit, print the next instruction
            println!("{}", inst);
        }

        res
    }

    fn parse_u16(input: &str) -> Option<u16> {
        let addr: Option<u16>;

        if input.contains("0x") | input.contains("0X") {
            addr = u16::from_str_radix(&input[2..], 16).ok();
        } else {
            addr = u16::from_str_radix(input, 10).ok();
        }

        addr
    }

    pub fn repl(&mut self, cpu: &mut Cpu) {
        loop {
            print!("gbcdbg> ");
            std::io::stdout().flush().unwrap();

            let mut line = String::new();
            std::io::stdin().read_line(&mut line).unwrap();

            let line: Vec<&str> = line.trim().split(" ").collect();

            match line[0] {
                "" => (),
                "q" | "quit" => {
                    std::process::exit(0);
                }
                "b" if line.len() == 2 => {
                    let addr = match Self::parse_u16(line[1]) {
                        Some(v) => v,
                        None => {
                            eprintln!("Invalid address specified: {}", line[1]);
                            continue;
                        }
                    };

                    let mut found = false;
                    for (other, exists) in self.breakpoints.iter_mut() {
                        if *other == addr {
                            *exists = true;
                            found = true;
                        }
                    }

                    if !found {
                        self.breakpoints.push((addr, true));
                    }
                }
                "b" => eprintln!("'b' requires at least 1 argument"),
                "d" if line.len() == 2 => {
                    // Delete a breakpoint
                    let index: usize = line[1].parse().unwrap();
                    if index >= self.breakpoints.len() {
                        eprintln!("Invalid breakpoint {}", index);
                        continue;
                    }

                    self.breakpoints.remove(index);
                }
                "d" => eprintln!("'d' requires at least 1 argument"),
                "toggle" if line.len() == 2 => {
                    // Toggle a breakpoint
                    let index: usize = line[1].parse().unwrap();
                    if index >= self.breakpoints.len() {
                        eprintln!("Invalid breakpoint {}", index);
                        continue;
                    }

                    self.breakpoints[index].1 = !self.breakpoints[index].1;
                }
                "toggle" => eprintln!("'toggle' requires at least 1 argument"),
                "dump" if line.len() == 2 => {
                    let flag: u32 = line[1].parse().unwrap();
                    if flag == 0 {
                        let _ = self.instruction_dump.take();
                        println!("Disabled instruction dumping");
                    } else {
                        self.instruction_dump = Some(File::create(DEBUG_DUMP_FILE).unwrap());
                        println!("Dumping instructions to {}", DEBUG_DUMP_FILE);
                    }
                }
                "dump" => eprintln!("'dump' requires at least 1 argument"),
                "h" | "hist" => {
                    let count: usize = if line.len() < 2 {
                        5
                    } else {
                        line[1].parse().unwrap()
                    };

                    let total = self.instructions.len();
                    if total < 2 {
                        continue;
                    }

                    // Print the last 5 instructions we've hit
                    let range = if total < count {
                        0..total-1
                    } else {
                        total-count-1..total-1
                    };

                    for (inst, pc) in self.instructions[range].iter() {
                        println!("{:#06x}: {}", pc, inst);
                    }
                }
                "count" => {
                    println!("{}", self.instructions.len());
                }
                "reset" => {
                    // Reset the CPU
                    cpu.reset().unwrap();
                    self.instructions.clear();
                    println!("CPU reset");
                }
                "l" | "list" => {
                    // Number of instructions to disassemble, startng from address below
                    let count: usize = if line.len() >= 2 {
                        line[1].parse().unwrap()
                    } else {
                        5
                    };

                    // Start address - defaults to PC
                    let addr = if line.len() == 3 {
                        Self::parse_u16(line[2])
                    } else {
                        Some(cpu.registers.PC)
                    };

                    for (inst, addr) in cpu.disassemble(count, addr) {
                        println!("{:#06x}: {}", addr, inst);
                    }
                }
                "s" | "step" if line.len() == 2 => {
                    let n: u32 = line[1].parse().unwrap();
                    self.mode = Mode::StepN(n);
                    return;
                }
                "s" | "step" => {
                    self.mode = Mode::Step;
                    return;
                }
                "n" | "next" => {
                    let pc = cpu.registers.PC;
                    let (inst, size, _) = cpu.fetch(None);

                    // If the next instruction is a CALL, keep executing
                    // until the function returns
                    if let Instruction::Call { .. } = inst {
                        self.mode = Mode::ContinuePc(pc + size as u16);
                    } else {
                        self.mode = Mode::Step;
                    }

                    return;
                }
                "c" | "continue" if line.len() == 2 => {
                    // Continue executing until address is hit (breakpoint shortcut)
                    let addr = match Self::parse_u16(line[1]) {
                        Some(addr) => addr,
                        None => {
                            eprintln!("Invalid address specified");
                            continue;
                        }
                    };

                    self.mode = Mode::ContinuePc(addr);
                    return;
                }
                "c" => eprintln!("'c' requires at least 1 argument"),
                "r" if line.len() == 2 => {
                    let n: u32 = line[1].parse().unwrap();
                    self.mode = Mode::ContinueN(n);
                    return;
                }
                "r" => {
                    self.mode = Mode::Continue;
                    return;
                }
                "p" if line.len() == 2 => {
                    let addr = match Self::parse_u16(line[1]) {
                        Some(v) => v,
                        None => {
                            eprintln!("Invalid address specified: {}", line[1]);
                            continue;
                        }
                    };

                    let value = cpu.memory.read(addr);

                    println!("{:#X}", value);
                }
                "p" => eprintln!("'p' requires at least 1 argument"),
                "w" if line.len() == 3 => {
                    let addr = match Self::parse_u16(line[1]) {
                        Some(v) => v,
                        None => {
                            eprintln!("Invalid address specified: {}", line[1]);
                            continue;
                        }
                    };

                    let value = Self::parse_u16(line[2]).unwrap();

                    cpu.memory.write(addr, value);
                }
                "w" => eprintln!("'w' requires at least 2 arguments"),
                "info" if line.len() == 2 => {
                    match line[1] {
                        "r" | "reg" | "registers" => {
                            println!("{}", cpu.registers);
                        }
                        "b" | "break" | "breakpoints" => {
                            let mut i = 0;
                            for (addr, enabled) in &self.breakpoints {
                                println!("{}: addr = {:#06X}, enabled = {}", i, addr, enabled);
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
