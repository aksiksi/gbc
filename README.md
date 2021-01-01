# gbc

A Gameboy Color (GBC) emulator written in Rust. This is intended as a learning project. 

## Project Structure

The emulator is divided into two crates:

1. `lib`: the main library for emulating a Gameboy
2. `emu`: the emulator frontend GUI (using SDL)

## Building

Due to the SDL dependency, you have to install some dependencies before you can build the emulator. Note that SDL is automatically built as part of the Rust-SDL2 build script, but the script needs a few tools:

1. CMake
2. C compiler (MSVC, GCC, Clang)

Since we statically link against `libsdl` on all platforms to avoid having to ship the DLL with the emulator, you do not need to install SDL for the build.

### Windows

1. Install the VSC++ build tools: https://visualstudio.microsoft.com/visual-cpp-build-tools/
2. Install `rustup` (this also installs `rustc` and `cargo`): https://www.rust-lang.org/tools/install
3. Install CMake: https://cmake.org/download/
4. `cargo build`

## Debugger

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
* `[l]ist`: Dump the last five instructions, including the current one.
