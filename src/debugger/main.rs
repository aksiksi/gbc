use gbc::Gameboy;

fn main() {
    let mut gameboy = Gameboy::init("samples/cpu_instrs.gb").unwrap();

    loop {
        gameboy.frame(None);
    }
}
