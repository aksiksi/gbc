//! Common utilities for running test ROMs
use std::io::{BufRead, BufReader};
use std::path::PathBuf;
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
pub fn run_single_test_rom(
    rom_path: &PathBuf,
    timeout: Option<u64>,
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

    // If we do not receive a response within 60 seconds, assume that the test is blocked
    let timeout = timeout.unwrap_or(60);
    let passed = rx.recv_timeout(Duration::from_secs(timeout)).unwrap_or(false);

    cmd.kill().unwrap();

    passed
}
