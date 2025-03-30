#![allow(dead_code)]
mod crab8;

use std::{
    io::{Write, stdout},
    thread, time,
};

use crab8::Crab8;
use termion::screen::IntoAlternateScreen;

fn main() {
    env_logger::init();
    {
        // NOTE: Alt screen sounds like an excellent choice for this
        let mut screen = stdout().into_alternate_screen().unwrap();
        write!(screen, "Welcome to the alternate screen.\n\nPlease wait patiently until we arrive back at the main screen in a about three seconds.").unwrap();
        screen.flush().unwrap();

        thread::sleep(time::Duration::from_secs(3));
    }

    println!("Phew! We are back.");
}

fn start_crab8() {
    let mut c8 = Crab8::new();
    c8.load_rom("Chip8 Logo.ch8");
    c8.start();
}
