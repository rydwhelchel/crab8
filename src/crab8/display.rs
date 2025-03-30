use std::io::Stdout;

use std::io::Write;
use termion::{raw::RawTerminal, screen::AlternateScreen};

pub struct Display<'a> {
    raw: [[bool; 64]; 32],
    screen: &'a mut AlternateScreen<RawTerminal<Stdout>>,
}

impl Display<'_> {
    const SOLID_BLOCK: char = '█';
    const LOWER_HALF_BLOCK: char = '▄';
    const UPPER_HALF_BLOCK: char = '▀';
    const EMPTY_BLOCK: char = ' ';

    pub fn new(screen: &mut AlternateScreen<RawTerminal<Stdout>>) -> Display {
        return Display {
            raw: [[false; 64]; 32],
            screen,
        };
    }

    pub fn clear_screen(&mut self) {
        self.raw = [[false; 64]; 32];
    }

    // Returns true if a bit was flipped
    pub fn draw(&mut self, coords: (u8, u8), sprite: Vec<u8>) -> bool {
        let x = coords.0 as usize;
        let y = coords.1 as usize;
        let mut flipped = false;
        for line in sprite {
            let rev_line: u8 = line.reverse_bits();
            for i in 0..8 {
                // if we're about to go off the screen, stop drawing row
                if x > self.raw[y].len() {
                    break;
                }
                // if curr bit in the sprite is on
                if (rev_line >> i) & 1 == 1 {
                    // set flag if it turns off a bit
                    if self.raw[y][x + i] {
                        flipped = true;
                    }
                    // flip pixel on/off
                    self.raw[y][x + i] = !self.raw[y][x + i];
                }
            }
        }
        self.render();
        return flipped;
    }

    // TODO: This should take the current raw state and output it to term
    // prob using termion
    //
    // revisit w/ https://github.com/redox-os/games/blob/master/src/minesweeper/main.rs
    fn render(&mut self) {
        let mut i = 0;
        let mut line = String::new();
        // render 2 lines at a time
        while i < self.raw.len() - 1 {
            for j in 0..self.raw[i].len() {
                if self.raw[i][j] && self.raw[i + 1][j] {
                    line.push(Self::SOLID_BLOCK);
                } else if self.raw[i][j] && !self.raw[i + 1][j] {
                    line.push(Self::UPPER_HALF_BLOCK);
                } else if !self.raw[i][j] && self.raw[i + 1][j] {
                    line.push(Self::LOWER_HALF_BLOCK);
                } else if !self.raw[i][j] && !self.raw[i + 1][j] {
                    line.push(Self::EMPTY_BLOCK);
                }
            }
            line.push('\n');
            i += 2;
        }
        // idk what I'm doing :)
        write!(
            self.screen,
            "{}{}{}",
            termion::clear::All,
            termion::cursor::Goto(1, 1),
            line
        )
        .unwrap();
        self.screen.flush().unwrap();
    }
}
