# gbc

![Rust](https://github.com/aksiksi/gbc/workflows/Rust/badge.svg)

A Gameboy Color (GBC) emulator written in Rust. This is intended as a learning project. 

![donkey_kong_country](https://user-images.githubusercontent.com/916621/106340437-ca4b0600-6267-11eb-818a-961c4f499110.gif)

## Quick Start

### 1. Download

Visit the [Releases](https://github.com/aksiksi/gbc/releases) page and download the latest version for your OS.

### 2. Run

Run a ROM:

```
gbcemu run [path_to_rom]
```

Run with `-h` to view all flags and options.

### 3. Play

Controls:

* Arrows for direction keys
* `S`: A button
* `A`: B button
* `X`: start button
* `Z`: select button

Emulator GUI keys:

* `K`: save emulator state
* `L`: load emulator state
* `;`: reset the emulator
* `O`: draw basic tile outline (toggle)
* `P`: pause (toggle)
* `]`: Fast-forward (while held)
* `=`: Increase emulation speed by 1x
* `-`: Decrease emulation speed by 1x
* `0`: Reset emulation speed to 1x

Note: these are replaced with buttons in the WASM version.

## Games Tested

- [x] Dr. Mario (DMG, no ROM banking)
- [x] Tetris World (DMG, no ROM banking)
- [x] Kirby's Dream Land (DMG, MBC1)
- [x] Tetris World DX (GBC, MBC1)
- [x] Super Mario Bros. Deluxe (GBC, MBC5)
- [x] Pokemon Yellow (GBC, MBC5)
- [x] Donkey Kong Country (GBC, MBC5)
- [x] Dragon Warrior Monsters (GBC, MBC5)
- [x] The Legend of Zelda: Link's Awakening DX (GBC, MBC5)
- [ ] Pokemon Gold (GBC, MBC3+RTC)
    - Hits glitch after intro (#3)
- [ ] Shantae (GBC, MBC5)
    - Not working, stuck on blue screen

## Details

### Project Structure

The emulator is divided into three crates:

1. `lib`: main library for emulating a Gameboy
2. `emu`: emulator frontend GUI written using SDL2 (runs on macOS, Windows, and Linux)
3. `emu-wasm`: `lib` compiled to WASM and running on a [`Canvas2D`](https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API) in JS

### Tests

There are two types of tests:

1. Functional tests: verify the basic functions of the CPU and peripherals
2. Integration tests: run existing test ROMs and ensure that they pass

These tests run on every commit to the repo.

### Building

Due to the SDL dependency, you have to install some dependencies before you can build the emulator. Note that SDL is automatically built as part of the Rust-SDL2 build script, but the script needs a few tools:

1. CMake
2. C compiler (MSVC, GCC, Clang)

Since we statically link against `libsdl` on all platforms to avoid having to ship the DLL with the emulator, you do not need to install SDL for the build.

#### Windows

1. Install the VSC++ build tools: https://visualstudio.microsoft.com/visual-cpp-build-tools/
2. Install `rustup` (this also installs `rustc` and `cargo`): https://www.rust-lang.org/tools/install
3. Install CMake: https://cmake.org/download/
4. `cargo build`

The full build in release mode (including SDL) on a 10 core Windows VM takes ~1 min.

### Debugger

The emulator comes with a simple GDB-like debugger CLI. Note that the debugger is not included by default.

To build the emulator with debugger support:

```
cargo build --manifest-path emu/Cargo.toml --features debug
```

As soon as you run the emulator, it will jump into the REPL. The following commands are available:

* `n`: Step to the next instruction.
    * `n <num>`: Skip the next `num` instructions.
* `info [r]egs`: Dump all registers.
* `p <addr>`: Print the byte at the specified memory address.
* `b <addr>`: Set a breakpoint on an instruction address. Note that you can have multiple active breakpoints.
* `info [b]reak`: List all breakpoints that have been set.
* `disable <index>`: Disable the breakpoint with the given index.
* `d <index>`: Delete the breakpoint with the given address.
* `r`: Continue running the emulator until the next breakpoint is hit.
* `[l]ist`: Disassemble the next five instructions, starting from the current one.
    * `[l]ist <count>`: Disassemble the next `count` instructions, starting from the current one.
* `[h]ist`: Dump the last five *executed* instructions.
    * `[h]ist <count>`: Dump the last `count` *executed* instructions.
