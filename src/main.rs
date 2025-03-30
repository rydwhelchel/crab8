#![allow(dead_code)]
mod crab8;

use std::io::{Write, stdin, stdout};

use crab8::Crab8;
use termion::{
    raw::IntoRawMode,
    screen::{IntoAlternateScreen, ToAlternateScreen, ToMainScreen},
    terminal_size,
};

fn write_alt_screen_msg<W: Write>(screen: &mut W) {
    write!(screen, "{}{}Welcome to the alternate screen.{}Press '1' to switch to the main screen or '2' to switch to the alternate screen.{}Press 'q' to exit (and switch back to the main screen).",
           termion::clear::All,
           termion::cursor::Goto(1, 1),
           termion::cursor::Goto(1, 3),
           termion::cursor::Goto(1, 4)).unwrap();

    write!(screen, "Size is {:?}", terminal_size().unwrap()).unwrap();
}

fn main() {
    //for c in stdin.keys() {
    //    match c.unwrap() {
    //        Key::Char('q') => break,
    //        Key::Char('1') => {
    //            write!(screen, "{}", ToMainScreen).unwrap();
    //        }
    //        Key::Char('2') => {
    //            write!(screen, "{}", ToAlternateScreen).unwrap();
    //            write_alt_screen_msg(&mut screen);
    //        }
    //        Key::Char('3') => {
    //            write!(screen, "{}ASDF", ToAlternateScreen).unwrap();
    //            //write_alt_screen_msg(&mut screen);
    //        }
    //        _ => {}
    //    }
    //    screen.flush().unwrap();
    //}
    //
    start_crab8();
}

fn start_crab8() {
    let stdin = stdin();
    let mut screen = stdout()
        .into_raw_mode()
        .unwrap()
        .into_alternate_screen()
        .unwrap();
    screen.flush().unwrap();
    write!(screen, "{}", termion::cursor::Hide).unwrap();
    let mut c8 = Crab8::new(stdin, &mut screen);
    c8.load_rom("Chip8 Logo.ch8");
    c8.start();
}
