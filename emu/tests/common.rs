//! Common utilities for running test ROMs
use std::path::PathBuf;
use std::time::{Duration, Instant};

use gbc::Gameboy;

/// Run a single ROM and check each line in stdout using the provided `line_check_fn`
///
/// `output_check_fn` should return:
///
/// * `Some(true)` if the test passes
/// * `Some(false)` if the test fails
/// * `None` if the output contains nothing conclusive (skipped)
pub fn run_single_test_rom(
    rom_path: &PathBuf,
    timeout: Option<u64>,
    output_check_fn: impl Fn(String) -> Option<bool> + Send + Sync + 'static,
) -> bool {
    let mut gameboy = Gameboy::init(rom_path, false, false).unwrap();

    let start = Instant::now();
    let timeout = Duration::from_secs(timeout.unwrap_or(60)); // Default timeout is 60 seconds

    let mut passed = false;

    loop {
        // If we've hit the timeout, the test has failed
        if start.elapsed() > timeout {
            eprintln!("Test timed out after {:?}", timeout);
            break;
        }

        // Run Gameboy for a single frame
        gameboy.frame(None);

        // Check serial output for pass/fail
        let serial = gameboy.serial_output();

        if let Some(result) = output_check_fn(serial) {
            passed = result;
            break;
        }
    }

    passed
}
