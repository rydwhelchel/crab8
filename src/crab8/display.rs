use std::io;

use crossterm::{cursor, queue, style::Print};

pub struct Display {
    raw: [[bool; 64]; 32],
}

impl Display {
    const SOLID_BLOCK: char = '█';
    const LOWER_HALF_BLOCK: char = '▄';
    const UPPER_HALF_BLOCK: char = '▀';
    const EMPTY_BLOCK: char = ' ';

    pub fn new() -> Display {
        return Display {
            raw: [[false; 64]; 32],
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
        for sprite_y in 0..sprite.len() {
            let rev_line: u8 = sprite[sprite_y].reverse_bits();
            for i in 0..8 {
                // if we're about to go off the screen, stop drawing row
                if x > self.raw[y].len() {
                    break;
                }
                // if curr bit in the sprite is on
                if (rev_line >> i) & 1 == 1 {
                    // set flag if it turns off a bit
                    if self.raw[y + sprite_y][x + i] {
                        flipped = true;
                    }
                    // flip pixel on/off
                    self.raw[y + sprite_y][x + i] = !self.raw[y + sprite_y][x + i];
                }
            }
        }
        self.render();
        flipped
    }

    // TODO: This should take the current raw state and output it to term
    // prob using termion
    // revisit w/ https://github.com/redox-os/games/blob/master/src/minesweeper/main.rs
    //
    //prob gonna try crossterm instead
    fn render(&mut self) {
        let mut i = 0;
        // render 2 lines at a time
        while i < self.raw.len() - 1 {
            let mut line = String::new();
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
            queue!(io::stdout(), cursor::MoveTo(0, (i / 2) as u16), Print(line)).unwrap();
            i += 2;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Display;

    #[test]
    fn test_draw() {
        let mut display: Display = Display::new();
        _ = display.draw((0, 0), vec![0xFF]);

        let mut expected_display: [[bool; 64]; 32] = [[false; 64]; 32];
        for i in 0..8 {
            expected_display[0][i] = true;
        }

        assert_eq!(expected_display, display.raw);

        _ = display.draw((1, 1), vec![0xFF, 0xFF]);
        for i in 1..9 {
            expected_display[1][i] = true;
            expected_display[2][i] = true;
        }

        assert_eq!(expected_display, display.raw);
    }
}
