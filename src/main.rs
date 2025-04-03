#![allow(dead_code)]
mod crab8;

use crab8::Crab8;
use std::io::Write;

fn main() {
    start_crab8();
}

fn start_crab8<W: Write>() {
    let mut c8: Crab8<W> = Crab8::new();
    c8.load_rom("Chip8 Logo.ch8");
    c8.start();
}
