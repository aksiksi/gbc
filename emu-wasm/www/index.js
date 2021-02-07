import * as wasm from "gbcemu";
import { memory } from "gbcemu/gbcemu_bg";

wasm.init()

class Emulator {
    constructor() {
        this.lcd_width = wasm.Gameboy.lcd_width();
        this.lcd_height = wasm.Gameboy.lcd_height();
        this.canvas = document.getElementById("emulator");
        this.ctx = this.canvas.getContext("2d");
        this.frameTimer = null;
        this.gameboy = null;
    }

    init(romBuffer) {
        // Setup a Gameboy from the given ROM file
        const romData = new Uint8Array(romBuffer);
        const cartridge = new wasm.Cartridge(romData);
        this.gameboy = new wasm.Gameboy(cartridge);
        console.log("Gameboy loaded!");
    }

    start() {
        // Start a timer for frame rendering (59.7 FPS)
        this.frameTimer = window.setInterval(() => this.renderFrame(), 16.7504);
    }

    stop() {
        clearInterval(this.frameTimer);
    }

    renderFrame() {
        // Get a pointer to the frame in WASM memory, then overlay a Uint8Array on top
        // to avoid copying the frame data out of WASM into JS on every frame
        const frameBufferPtr = this.gameboy.frame();

        // Each pixel in the frame buffer consists of 3 bytes for RGB values
        const frameBuffer = new Uint8Array(memory.buffer, frameBufferPtr, this.lcd_width * this.lcd_height * 3);

        // Create an `imageData` to write pixels to
        const imageData = this.ctx.createImageData(this.lcd_width, this.lcd_height);
        const data = imageData.data;

        // Iterate over the frame buffer pixels and write it to the imageData
        for (var x = 0; x < this.lcd_width; x += 1) {
            for (var y = 0; y < this.lcd_height; y += 1) {
                const source_idx = y * this.lcd_width * 3 + x * 3;
                const red = frameBuffer[source_idx];
                const green = frameBuffer[source_idx+1];
                const blue = frameBuffer[source_idx+2];

                // Frame buffer pixels are just RGB, so we need to add the alpha
                const dest_idx = y * this.lcd_width * 4 + x * 4;
                data[dest_idx] = red;
                data[dest_idx+1] = green;
                data[dest_idx+2] = blue;
                data[dest_idx+3] = 255; // alpha
            }
        }

        // Render the frame on the canvas at position (0, 0)
        this.ctx.putImageData(imageData, 0, 0);

        console.log("Rendered a frame!");
    }
}

async function handleROM() {
    const romFile = this.files[0];
    const romBuffer = await romFile.arrayBuffer();
    emulator.init(romBuffer);
    emulator.start();
}

// Wait for user to upload a ROM
const romPicker = document.getElementById("rompicker");
romPicker.addEventListener("change", handleROM, false);;

const emulator = new Emulator();
