mod crab8;

use std::sync::Arc;

use crab8::Crab8;
use include_dir::{Dir, include_dir};

static PROJECT_DIR: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/roms");

fn main() {
    start_crab8();
}

fn start_crab8() {
    let mut c8 = Crab8::new();

    let c8_logo: Arc<[u8]> = PROJECT_DIR
        // NOTE: Almost working... kind of
        //.get_file("Space Invaders [David Winter].ch8")
        // NOTE: Not implemented
        //.get_file("jason.ch8")
        // NOTE: Doesn't work properly idk why
        //.get_file("Chip8 emulator Logo [Garstyciuks].ch8")
        // NOTE: Needs input detection
        .get_file("Random Number Test [Matthew Mikolay, 2010].ch8")
        //.get_file("Chip8 Logo.ch8")
        .unwrap()
        .contents()
        .into();

    c8.load_rom(c8_logo);
    c8.start();
}
