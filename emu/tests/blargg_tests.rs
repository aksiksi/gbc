//! Run through the indivdual and combined Blargg GB tests
use std::path::{Path, PathBuf};

mod common;

/// Run through Blargg's CPU instructions test ROM
#[test]
fn test_cpu_instrs() {
    let rom_path =
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("..")
            .join("samples")
            .join("blargg")
            .join("cpu_instrs")
            .join("cpu_instrs.gb");
    let timeout = 180;

    let passed = common::run_single_test_rom(&rom_path, Some(timeout), |line| {
        if line.contains("Passed") {
            Some(true)
        } else if line.contains("Failed") {
            Some(false)
        } else {
            None
        }
    });

    assert!(passed);
}

/// Run through Blargg's individual CPU tests
#[test]
#[ignore]
fn test_cpu_instrs_individual() {
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
        let passed = common::run_single_test_rom(&path, None, |line| {
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
    let timeout = 120;

    let passed = common::run_single_test_rom(&rom_path, Some(timeout), |line| {
        if line.contains("Passed") {
            Some(true)
        } else if line.contains("Failed") {
            Some(false)
        } else {
            None
        }
    });

    assert!(passed);
}
