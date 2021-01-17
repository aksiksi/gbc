//! Run through the indivdual and combined Blargg GB tests
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::mpsc;
use std::time::Duration;

/// Run a single ROM and check each line in stdout using the provided `line_check_fn`
///
/// `line_check_fn` should return:
///
/// * `Some(true)` if the test passes
/// * `Some(false)` if the test fails
/// * `None` if the line is irrelevant (skipped)
fn run_single_test_rom(
    rom_path: &PathBuf,
    line_check_fn: impl Fn(String) -> Option<bool> + Send + Sync + 'static,
) -> bool {
    let bin: &str = env!("CARGO_BIN_EXE_gbcemu");
    let args = &["--headless", rom_path.to_str().unwrap()];

    let mut cmd = Command::new(bin)
        .args(args)
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();

    let (tx, rx) = mpsc::channel();
    let stdout = cmd.stdout.take().unwrap();

    // Spawn a background thread that checks the emulator output
    // A channel is used to return the emulator status
    std::thread::spawn(move || {
        let mut stdout_reader = BufReader::new(stdout);
        let mut line = String::new();
        let mut passed = false;

        while let Ok(_) = stdout_reader.read_line(&mut line) {
            println!("{}", line.trim());

            if let Some(p) = line_check_fn(line.clone()) {
                passed = p;
                break;
            }

            line.clear();
        }

        tx.send(passed).unwrap();
    });

    // If we do not receive a response within 30 seconds, assume that the test failed
    let passed = rx.recv_timeout(Duration::from_secs(30)).unwrap_or(false);

    cmd.kill().unwrap();

    passed
}

/// Run through Blargg's individual CPU tests
#[test]
fn test_cpu_instrs() {
    const TEST_ROMS: &[&str] = &[
        "01-special.gb",
        "02-interrupts.gb",
        "03-op sp,hl.gb",
        "04-op r,imm.gb",
        "05-op rp.gb",
        "06-ld r,r.gb",
        "07-jr,jp,call,ret,rst.gb",
        "08-misc instrs.gb",
        "09-op r,r.gb",
        "10-bit ops.gb",
        "11-op a,(hl).gb",
    ];

    let rom_paths: Vec<PathBuf> = TEST_ROMS
        .iter()
        .map(|name| {
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("..")
                .join("samples")
                .join("blargg")
                .join("cpu_instrs")
                .join(name)
        })
        .collect();

    for path in rom_paths {
        let passed = run_single_test_rom(&path, |line| {
            if line.contains("Passed") {
                Some(true)
            } else if line.contains("Failed") {
                Some(false)
            } else {
                None
            }
        });

        assert!(passed, "Test {} failed!", path.to_str().unwrap());
    }
}

/// Run through Blargg's instruction timing test ROM
#[test]
fn test_instr_timing() {
    let rom_path =
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("..")
            .join("samples")
            .join("blargg")
            .join("instr_timing")
            .join("instr_timing.gb");

    let passed = run_single_test_rom(&rom_path, |line| {
        if line.contains("Passed") {
            Some(true)
        } else if line.contains("Failed") {
            Some(false)
        } else {
            None
        }
    });

    assert!(passed, "instr_timing failed!");
}
