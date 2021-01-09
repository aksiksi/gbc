# Useful References

* Wiki on how OAM DMA works: https://gbdev.gg8.se/wiki/articles/OAM_DMA_tutorial
* MAME instruction handler: https://github.com/mamedev/mame/blob/master/src/devices/cpu/lr35902/opc_main.hxx
* Sameboy (solid emulator in C): https://github.com/LIJI32/SameBoy
* Mooneye (GB emulator in Rust): https://github.com/Gekkio/mooneye-gb
* Mooneye notes: https://gekkio.fi/files/gb-docs/gbctr.pdf
* Blargg sound tests: https://forums.nesdev.com/viewtopic.php?t=13730

# NOTES

## CPU Architecture

### Registers

* 8-bit each, 16-bit combined
    * A + F
    * B + C
    * D + E
    * H + L
* 16-bit
    * SP
    * PC

### Memory

### Cartridge

### Instruction Handling

* Look into abstracting out all arg types to simplify instruction handling
    * e.g., `Operand` can be `Reg8`, `Reg16`, `Imm8u`, `Imm8i`, `Imm16`, `Mem`, `MemImm16`

## Joypad/Gamepad

Register 0xFF00 contains the joypad data. The 5th and 6th bits are used to select between directions and buttons. Whenever a keyboard event comes in from the window loop, we pass that down to the CPU to update the joypad register. Games typically snoop this register to determine whether or not a button is currently pressed.

For now, we only pass in the last pressed button in each frame -- so, 1 joypad input per frame.

## A 2D GUI in Rust

SDL2 is the best option. You get 2D graphics (SW and HW rendering), keyboard events, and sound -- on all platforms. Also, the emulator can statically link against the SDL library.

However, the texture needs to passed into the main emulator loop. The reason is that the LCD can be updated mid-frame (heck, even mid-scanline!). So, we will define a trait within the `gbc` crate and implement it for the SDL Texture type. Then we can pass in a trait object to the inner GB frame handler.

## PPU (GPU) Architecture

* Basic registers, LY updates, and LCD STAT interrupts are fairly straightforward.
* Some of the registers are latched during a scanline. In other words, writing to these registers has no effect until the next scanline. The registers are: SCY, SCX, LYC, WX, and WY.
* 

## APU Architecture

TBD...
