# gbc

A Gameboy Color (GBC) emulator written in Rust.

This is intended as a learning project.

## Building

SDL2 is automatically built as part of the Rust-SDL2 build script. In addition, we statically link against `libsdl` on all platforms to avoid having to ship the DLL with the emulator (or asking users to install it).

### Windows

1. Install the VSC++ build tools: https://visualstudio.microsoft.com/visual-cpp-build-tools/
2. Install `rustup` (this also installs `rustc` and `cargo`): https://www.rust-lang.org/tools/install
3. Install CMake: https://cmake.org/download/
4. `cargo build --bin gbc --features gui`
