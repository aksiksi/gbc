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
