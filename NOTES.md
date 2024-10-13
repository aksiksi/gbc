# Useful References

* https://gbdev.io/pandocs/
* https://gbdev.io/gb-opcodes/optables/
* Wiki on how OAM DMA works: https://gbdev.gg8.se/wiki/articles/OAM_DMA_tutorial
* MAME instruction handler: https://github.com/mamedev/mame/blob/master/src/devices/cpu/lr35902/opc_main.hxx
* Sameboy (solid emulator in C): https://github.com/LIJI32/SameBoy
* Mooneye (GB emulator in Rust): https://github.com/Gekkio/mooneye-gb
* Mooneye notes: https://gekkio.fi/files/gb-docs/gbctr.pdf
* Blargg sound tests: https://forums.nesdev.com/viewtopic.php?t=13730
* Blargg's mem_timing tests: https://www.reddit.com/r/EmuDev/comments/j4xn0s/gb_how_to_get_correct_memory_timings/
    * Passing these tests require that the timer and all peripherals be updated after every memory operation
* DMA: https://gist.github.com/drhelius/3394856

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

## SDL2

Example of streaming texture:

```rust
// Create texture in streaming mode

// .. in frame loop
canvas.clear();

texture.with_lock(None, |pixels, _| {
    // Draw the rendered frame
    for x in 0..LCD_WIDTH {
        for y in 0..LCD_HEIGHT {
            let color = frame_buffer.data[y * LCD_WIDTH + x];
            let i = y * LCD_WIDTH * 4 + x * 4;

            // LE representation of ARGB pixel
            // Caveat: need to be aware of pixel format and platform endianness (?)
            pixels[i+3] = color.alpha;
            pixels[i+2] = color.red;
            pixels[i+1] = color.green;
            pixels[i] = color.blue;
        }
    }
}).unwrap();

canvas.copy(&texture, None, None).unwrap();
canvas.present();
```

From worst to best:

* HW and SW rendering w/ texture target: ~5 ms per frame
    * With HW, copy time decreases, but render time increases (makes sense)
* SW w/ streaming texture: ~4 ms, render time goes way down (50 us), but copy overhead remains
* HW w/ streaming texture: ~2 ms, both render and copy time decrease

# WASM Support

## Cartridge

* Build a `Cartridge` from raw ROM bytes
    * Remove `rom_path` and `rom_file` fields
    * Figure out how to handle battery-backed RAM and RTC

In JS:

* Use `FileReader` to read the file in JS and pass that in (or underlying buffer) to WASM
* Pass the buffer to the Gameboy to initialize it

## Save States

* Use `Vec<u8>` for save state methods. Allow the upper layer to figure out how to handle the raw data.

## Actions Queue

Idea: enqueue actions to perform in future M-cycles (4 clock cycles).

Core loop:
    * CPU step -> returns cycles
    * Memory step - PPU, timer, serial, RTC
    * Trigger interrupts (write to memory)
    * Speed switch (?)

CPU step:
    * Service interrupts
    * Check CPU halted
    * Fetch instruction
    * Trace (debugger)
    * Execute instruction
    * Check stopped
    * Handle DMA if not stopped

Memory step:
    * PPU step (cycles)
    * Timer step (cycles)
    * Serial interrupt
    * RTC (cycles)

### New Approach

Action queue init: [Fetch]

Core step:
    * CPU step
    * Memory step
    * Trigger interrupts
    * Speed switch (?)

CPU step:

```
for action in queue:
    fetch_cycles = None
    switch action:
        case Cpu::ProcessInterrupts
            # Interrupt triggered?
            # Determine highest prio ready interrupt
            # Disable IME
            # Push PC to stack
            # Compute JMP addr
            # Set new PC
            fetch_cycles = Some(1)
        case Cpu::Fetch
            # Fetch instruction at PC
            # Push Execute(Inst) action to queue in N M-cycles
            # Update PC
        case Cpu::Execute:
            # Execute instruction at PC
            # Trace inst. w/ debugger
            # Check taken/not taken
            # Push Cpu::Delay if taken and fetch next later
            fetch_cycles = Some(n)
            # else:
            fetch_cycles = Some(1)
        case Cpu::Delay
            # TODO
        case Cpu::HandleOamDma(pos)
            # Check if OAM DMA active
            # Process 4 bytes every M-cycle (1 per clock cycle)
            # Copy 4 bytes based on pos
            if active:
                queue.push(HandleOamDma(pos+1), 1)
        case Cpu::HandleHdma:
            # Check if HDMA active (in HBLANK)
            # Process 16 bytes???


# Process interrupts every cycle if enabled
if ime:
    queue.push(ProcessInterrupts, 1)

if not halted:
    if let Some(cycles) = fetch_cycles {
        queue.push(Fetch, cycles)
    }




```

Memory step:

```
for action in queue:
    switch action:
        case HandlePpu:
            # Render 1 dot
        case Cpu::Execute:
            # Execute instruction
            # Check taken/not taken
            # Trace w/ debugger
        case Cpu::Delay
            # TODO
        case Cpu::ProcessInterrupts
            # Interrupt triggered?
            # Push Execute(Inst) in 1 M-cycle
        case Cpu::HandleDma
            # Process 1 M-cycle worth of DMA

queue.push(HandlePpu, 1)
queue.push(
```
