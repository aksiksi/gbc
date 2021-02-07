import * as wasm from "gbcemu";
import { memory } from "gbcemu/gbcemu_bg";

class Emulator {
    constructor() {
        this.lcd_width = wasm.Gameboy.lcd_width();
        this.lcd_height = wasm.Gameboy.lcd_height();
        this.frameTimer = null;
        this.gameboy = null;

        this.canvas = document.getElementById("emulator");
        this.ctx = this.canvas.getContext("2d");

        this.startButton = document.getElementById("start");
        this.pauseButton = document.getElementById("pause");
        this.resetButton = document.getElementById("reset");
        this.romPicker = document.getElementById("rompicker");

        this.startButton.onclick = () => {
            if (this.romPicker.files.length == 0) {
                alert("Please load a ROM first!");
                return;
            }

            const romFile = this.romPicker.files[0];
            romFile.arrayBuffer().then((buffer) => {
                this.pause();
                this.init(buffer);
                this.start();
            });
        };

        this.pauseButton.onclick = () => {
            if (this.pauseButton.textContent == "Pause") {
                this.pause();
                this.pauseButton.textContent = "Resume";
            } else if (this.pauseButton.textContent == "Resume") {
                this.start();
                this.pauseButton.textContent = "Pause";
            }
        };

        this.resetButton.onclick = () => {
            if (this.gameboy != null) {
                this.gameboy.reset();
            }
        };
    }

    // Initialize the emulator from raw ROM data
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

    pause() {
        if (this.frameTimer != null) {
            clearInterval(this.frameTimer);
            this.frameTimer = null;
        }
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
    }

    // Map a raw keycode to a emulator joypad input
    mapKeyCodeToInput(keycode) {
        let joypad_input = null;

        switch (keycode) {
            case "ArrowUp":
                joypad_input = wasm.JoypadInput.Up;
                break;
            case "ArrowDown":
                joypad_input = wasm.JoypadInput.Down;
                break;
            case "ArrowLeft":
                joypad_input = wasm.JoypadInput.Left;
                break;
            case "ArrowRight":
                joypad_input = wasm.JoypadInput.Right;
                break;
            case "KeyS":
                joypad_input = wasm.JoypadInput.A;
                break;
            case "KeyA":
                joypad_input = wasm.JoypadInput.B;
                break;
            case "Enter":
            case "NumpadEnter":
                joypad_input = wasm.JoypadInput.Start;
                break;
            case "ShiftLeft":
            case "ShiftRight":
                joypad_input = wasm.JoypadInput.Select;
                break;
            default:
                // Ignored
                break;
        }

        return joypad_input;
    }

    handleKey(keyevent, down) {
        const joypad_input = this.mapKeyCodeToInput(keyevent.code);

        if (this.gameboy != null && joypad_input != null) {
            this.gameboy.joypad_input(joypad_input, down);
        }
    }
}

wasm.init()

const emulator = new Emulator();

document.addEventListener("keydown", (event) => {
    emulator.handleKey(event, true);
});

document.addEventListener("keyup", (event) => {
    emulator.handleKey(event, false);
});
