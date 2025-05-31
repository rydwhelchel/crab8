mod crab8;

use std::sync::Arc;

use crab8::Config;
use crab8::Crab8;
use include_dir::{Dir, include_dir};

static PROJECT_DIR: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/roms");

fn main() {
    start_crab8();
}

fn start_crab8() {
    let config = Config {
        old_shift: true,
        old_jump_with_register_offset: true,
        old_memory: true,
    };

    let mut c8 = Crab8::new(config);

    let c8_logo: Arc<[u8]> = PROJECT_DIR
        .get_file("Breakout [Carmelo Cortez, 1979].ch8")
        .unwrap()
        .contents()
        .into();

    c8.load_rom(c8_logo);
    c8.start();
}
