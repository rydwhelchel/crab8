use std::io::{Write, stdout};

use crossterm::{cursor, queue, style::Print};

pub struct Display {
    raw: [[bool; 64]; 32],
}

impl Display {
    const SOLID_BLOCK: char = '█';
    const LOWER_HALF_BLOCK: char = '▄';
    const UPPER_HALF_BLOCK: char = '▀';
    const EMPTY_BLOCK: char = ' ';
    const HORIZONTAL_LINE: char = '─';
    const VERTICAL_LINE: char = '│';
    const TOP_LEFT_CORNER: char = '┌';
    const TOP_RIGHT_CORNER: char = '┐';
    const BOTTOM_LEFT_CORNER: char = '└';
    const BOTTOM_RIGHT_CORNER: char = '┘';
    // Left border of chip8
    const TOP_BORDER: u16 = 2;
    const BOTTOM_BORDER: u16 = Self::TOP_BORDER + 16;
    const LEFT_BORDER: u16 = 10;
    const RIGHT_BORDER: u16 = Self::LEFT_BORDER + 64;

    pub fn new() -> Display {
        Self::render_border();
        Self::render_help_text();
        return Display {
            raw: [[false; 64]; 32],
        };
    }

    fn render_border() {
        // Draw top row
        let mut line: Vec<char> = vec![Self::TOP_LEFT_CORNER];
        for _ in 0..64 {
            line.push(Self::HORIZONTAL_LINE);
        }
        line.push(Self::TOP_RIGHT_CORNER);
        let line: String = line.into_iter().collect();

        queue!(
            stdout(),
            cursor::MoveTo(Self::LEFT_BORDER - 1, Self::TOP_BORDER - 1),
            Print(line),
        )
        .unwrap();

        // Draw sides
        for i in 0..16 {
            queue!(
                stdout(),
                cursor::MoveTo(Self::LEFT_BORDER - 1, Self::TOP_BORDER + i),
                Print(Self::VERTICAL_LINE),
                cursor::MoveTo(Self::RIGHT_BORDER, Self::TOP_BORDER + i),
                Print(Self::VERTICAL_LINE)
            )
            .unwrap();
        }

        // Draw bottom row
        let mut line: Vec<char> = vec![Self::BOTTOM_LEFT_CORNER];
        for _ in 0..64 {
            line.push(Self::HORIZONTAL_LINE);
        }
        line.push(Self::BOTTOM_RIGHT_CORNER);
        let line: String = line.into_iter().collect();

        queue!(
            stdout(),
            cursor::MoveTo(Self::LEFT_BORDER - 1, Self::BOTTOM_BORDER),
            Print(line),
        )
        .unwrap();
    }

    fn render_help_text() {
        let line = "Press 'Shift+Q' to quit.";

        queue!(
            stdout(),
            cursor::MoveTo(Self::LEFT_BORDER, Self::BOTTOM_BORDER + 1),
            Print(line),
        )
        .unwrap();
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
                if x + i >= self.raw[y].len() {
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

    fn render(&mut self) {
        let mut i = 0;
        // render 2 lines at a time
        while i < self.raw.len() - 1 {
            let mut line = String::new();
            for j in 0..self.raw[i].len() {
                if self.raw[i][j] && self.raw[i + 1][j] {
                    // If both top half and bottom half are filled
                    line.push(Self::SOLID_BLOCK);
                } else if self.raw[i][j] && !self.raw[i + 1][j] {
                    // just top half filled
                    line.push(Self::UPPER_HALF_BLOCK);
                } else if !self.raw[i][j] && self.raw[i + 1][j] {
                    // just bottom half filled
                    line.push(Self::LOWER_HALF_BLOCK);
                } else if !self.raw[i][j] && !self.raw[i + 1][j] {
                    // neither filled
                    line.push(Self::EMPTY_BLOCK);
                }
            }
            queue!(
                stdout(),
                cursor::MoveTo(Self::LEFT_BORDER, Self::TOP_BORDER + (i / 2) as u16),
                Print(line)
            )
            .unwrap();
            i += 2;
        }
        stdout().flush().unwrap();
    }
}

#[cfg(test)]
mod tests {
    use super::Display;

    #[test]
    fn test_draw() {
        let mut display: Display = Display::new();
        // TODO: Fix testing draw so it doesn't render when you run test
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

    #[test]
    fn test_bit_check() {
        assert!((1 >> 1) & 1 != 1);
        assert!((2 >> 1) & 1 == 1);
        assert!((3 >> 1) & 1 == 1);
        assert!((7 >> 1) & 1 == 1);
        assert!((7 >> 2) & 1 == 1);
        assert!((7 >> 3) & 1 != 1);
        assert!((4 >> 2) & 1 == 1);
        assert!((5 >> 2) & 1 == 1);
    }
}
