pub struct Display {
    raw: [[bool; 64]; 32],
}

impl Display {
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
        return flipped;
    }

    // TODO: This should take the current raw state and output it to term
    // prob using termion
    fn render() {}
}
