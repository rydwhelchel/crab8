#![allow(dead_code)]
//! Spec of Crab8 largely written as described here
//! https://tobiasvl.github.io/blog/write-a-chip-8-emulator/
use rand::{Rng, rng};
use std::{cmp::Ordering, fs::File};

struct SubroutineStack {
    /// Most roms do not make use of more than 2 spots in the stack.
    stack: [u16; 16],
    /// Pointer in the stack
    sp: usize,
}

impl SubroutineStack {
    pub fn new() -> SubroutineStack {
        return SubroutineStack {
            stack: [0; 16],
            sp: 0,
        };
    }

    pub fn push(&mut self, val: u16) {
        if self.sp == self.stack.len() {
            // Should theoretically be impossible with valid games
            panic!("subroutine stackoverflow");
        }
        self.stack[self.sp] = val;
        self.sp += 1;
    }

    pub fn pop(&mut self) -> u16 {
        if self.sp == 0 {
            panic!("subroutine stackunderflow");
        }
        self.sp -= 1;
        self.stack[self.sp]
    }
}

pub struct Crab8 {
    /// Read&write RAM. Chip8 games modify themselves in memory frequently
    /// Fonts are stored at index 0
    ram: [u8; 4096],
    /// 16 8-bit (one byte) general-purpose variable registers
    /// numbered 0 through F hexadecimal, ie. 0 through 15 in
    /// decimal, called V0 through VF. VF is usually the flag register
    registers: [u8; 16],
    /// The stack is for 16 bit addresses to get pushed to when working with subroutines.
    stack: SubroutineStack,
    /// 32 rows of 64
    display: [[bool; 64]; 32],

    // Pointers
    // NOTE: Consider setting pointers to usize instead for ease of use.
    // Though pointers can only address 12 bits in chip8
    /// program counter - points to current instruction in memory
    pc: u16,
    /// index - general use pointer
    i: u16,

    // Timers
    /// Ticks down 60 times per second, does nothing otherwise
    delay_timer: u8,
    /// Ticks down 60 times per second, beeps all the while
    sound_timer: u8,

    // Not part of the original crab8, just have it for debug purposes
    cycles: usize,
}

impl Crab8 {
    /// PC starts at 0x200 (512) because chip8 used to store its
    /// own internal workings in the first 512 addresses.
    const OFFSET: u16 = 0x200;

    const FONTS: [u8; 80] = [
        0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
        0x20, 0x60, 0x20, 0x20, 0x70, // 1
        0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
        0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
        0x90, 0x90, 0xF0, 0x10, 0x10, // 4
        0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
        0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
        0xF0, 0x10, 0x20, 0x40, 0x40, // 7
        0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
        0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
        0xF0, 0x90, 0xF0, 0x90, 0x90, // A
        0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
        0xF0, 0x80, 0x80, 0x80, 0xF0, // C
        0xE0, 0x90, 0x90, 0x90, 0xE0, // D
        0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
        0xF0, 0x80, 0xF0, 0x80, 0x80, // F
    ];

    pub fn new() -> Crab8 {
        let mut ram = [0; 4096];
        ram[0..80].copy_from_slice(&Self::FONTS);
        return Crab8 {
            ram,
            registers: [0; 16],
            stack: SubroutineStack::new(),
            display: [[false; 64]; 32],
            pc: Self::OFFSET,
            i: 0,
            delay_timer: 0,
            sound_timer: 0,
            cycles: 0,
        };
    }

    pub fn load_rom(&mut self, name: &str) {
        let rom_contents = match std::fs::read("./roms/".to_string() + name) {
            Ok(contents) => contents,
            Err(error) => panic!("{}", error),
        };

        for i in 0..rom_contents.len() {
            self.ram[Self::OFFSET as usize + i] = rom_contents[i];
        }
    }

    pub fn start(&mut self) {
        // TODO: Should limit loop speed to ~700 cycles per second in order to avoid having too
        // high of an update speed.
        loop {
            self.cycle()
        }
    }

    pub fn display(&self) -> [[bool; 64]; 32] {
        return self.display;
    }

    fn cycle(&mut self) {
        if self.pc as usize >= self.ram.len() {
            panic!("pc reached out of bounds on RAM")
        }
        self.execute_instruction(parse_instruction((
            self.ram[self.pc as usize],
            self.ram[self.pc as usize + 1],
        )));
        self.pc += 2;

        // unneeded
        println!("Cycles: {}", self.cycles);
        self.cycles += 1;
    }

    fn execute_instruction(&mut self, ins: Instruction) {
        match ins {
            Instruction::ExecuteMachineLangaugeRoutine(_value) => {
                todo!("do I want to log something on this branch? do I want to implement?")
            }
            Instruction::ClearScreen => {
                self.display = [[false; 64]; 32];
            }
            Instruction::Jump(loc) => {
                self.pc = loc;
            }
            Instruction::CallSubroutine(loc) => {
                self.stack.push(self.pc);
                self.pc = loc;
            }
            Instruction::ReturnSubroutine => {
                self.pc = self.stack.pop();
            }
            Instruction::ValueEqualitySkip(vx, value) => {
                if self.registers[vx as usize] == value {
                    // Skip NEXT instruction
                    self.pc += 2;
                }
            }
            Instruction::ValueInequalitySkip(vx, value) => {
                if self.registers[vx as usize] != value {
                    // Skip NEXT instruction
                    self.pc += 2;
                }
            }
            Instruction::RegisterEqualitySkip(vx, vy) => {
                if self.registers[vx as usize] == self.registers[vy as usize] {
                    // Skip NEXT instruction
                    self.pc += 2;
                }
            }
            Instruction::RegisterInequalitySkip(vx, vy) => {
                if self.registers[vx as usize] != self.registers[vy as usize] {
                    // Skip NEXT instruction
                    self.pc += 2;
                }
            }
            Instruction::SetRegisterToValue(vx, value) => {
                self.registers[vx as usize] = value;
            }
            Instruction::AddToRegisterNoFlag(_vx, _value) => {
                todo!("figure out how to deal with overflow");
            }
            Instruction::SetRegisterToRegister(vx, vy) => {
                self.registers[vx as usize] = self.registers[vy as usize];
            }
            Instruction::OrRegister(vx, vy) => {
                self.registers[vx as usize] |= self.registers[vy as usize];
            }
            Instruction::AndRegister(vx, vy) => {
                self.registers[vx as usize] &= self.registers[vy as usize];
            }
            Instruction::XorRegister(vx, vy) => {
                self.registers[vx as usize] ^= self.registers[vy as usize];
            }
            Instruction::AddRegisterToRegisterWithFlag(_vx, _vy) => {
                todo!("figure out how to deal with overflow");
            }
            Instruction::SubtractXYRegisterWithFlag(_vx, _vy) => {
                todo!("figure out how to deal with overflow");
            }
            Instruction::SubtractYXRegisterWithFlag(_vx, _vy) => {
                todo!("figure out how to deal with overflow");
            }
            Instruction::ShiftRight(_vx, _vy) => {
                todo!("figure out how to capture bit shifted out, decide how to handle config")
            }
            Instruction::ShiftLeft(_vx, _vy) => {
                todo!("figure out how to capture bit shifted out, decide how to handle config")
            }
            Instruction::SetIndex(value) => {
                self.i = value;
            }
            Instruction::JumpWithRegisterOffset(_vx, _value) => {
                todo!("decide how to handle config")
            }
            Instruction::Random(vx, value) => {
                let ran_value: u8 = rng().random();
                self.registers[vx as usize] = ran_value & value;
            }
            Instruction::Draw(vx, vy, n) => {
                let (x, y) = (
                    // mod 64
                    (self.registers[vx as usize] & 63) as usize,
                    // mod 32
                    (self.registers[vy as usize] & 31) as usize,
                );
                // TODO: Improve (remove vec if possible)
                let mut sprite: Vec<u8> = Vec::new();
                for i in 0..n {
                    // get n bytes of the sprite
                    sprite.push(self.ram[(self.i + i as u16) as usize]);
                }
                for line in sprite {
                    let rev_line: u8 = line.reverse_bits();
                    for i in 0..8 {
                        // if we're about to go off the screen, stop drawing row
                        if x > self.display[y].len() {
                            break;
                        }
                        // if curr bit in the sprite is on
                        if (rev_line >> i) & 1 == 1 {
                            // set flag if it turns off a bit
                            if self.display[y][x + i] {
                                self.registers[0xF] = 1;
                            }
                            // flip pixel on/off
                            self.display[y][x + i] = !self.display[y][x + i];
                        }
                    }
                }
            }
            Instruction::SkipIfPressed(_vx) => {
                todo!("implement key press detection");
            }
            Instruction::SkipIfNotPressed(_vx) => {
                todo!("implement key press detection");
            }
            Instruction::GetDelayTimer(vx) => {
                self.registers[vx as usize] = self.delay_timer;
            }
            Instruction::SetDelayTimer(vx) => {
                self.delay_timer = self.registers[vx as usize];
            }
            Instruction::SetSoundTimer(vx) => {
                self.sound_timer = self.registers[vx as usize];
            }
            Instruction::AddToIndex(_vx) => {
                todo!("Read into flag overflow");
                //self.i += self.registers[vx as usize];
            }
            Instruction::GetKey(_vx) => {
                todo!("implement key press detection");
            }
            _ => todo!(),
        }
    }
}

fn combine_bytes(bytes: (u8, u8)) -> u16 {
    (bytes.0 as u16) << 8 | bytes.1 as u16
}

fn compare_ins_remainder(ins: &str, comp: String) -> bool {
    let length = String::len(&comp);
    // This feels like the wrong way to approach this -- would love a better solution
    if ins[4 - length..4].chars().cmp(comp.chars()) == Ordering::Equal {
        return true;
    }
    return false;
}

fn parse_instruction(bytes: (u8, u8)) -> Instruction {
    let instruction: String = format!("{:04x}", combine_bytes(bytes));

    // Unwrapping directly because above format string should always have 4 characters
    match instruction.chars().nth(0).unwrap() {
        '0' => {
            if compare_ins_remainder(&instruction, String::from("0e0")) {
                Instruction::ClearScreen
            } else if compare_ins_remainder(&instruction, String::from("0ee")) {
                Instruction::ReturnSubroutine
            } else {
                Instruction::NotImplemented
            }
        }
        '1' => {
            // TODO: Handle unwrap
            Instruction::Jump(u16::from_str_radix(&instruction[1..4], 16).unwrap())
        }
        '2' => {
            // TODO: Handle unwrap
            Instruction::CallSubroutine(u16::from_str_radix(&instruction[1..4], 16).unwrap())
        }
        '3' => Instruction::ValueEqualitySkip(
            u8::from_str_radix(&instruction[1..2], 16).unwrap(),
            u8::from_str_radix(&instruction[2..4], 16).unwrap(),
        ),
        '4' => Instruction::ValueInequalitySkip(
            u8::from_str_radix(&instruction[1..2], 16).unwrap(),
            u8::from_str_radix(&instruction[2..4], 16).unwrap(),
        ),
        '5' => {
            if instruction.chars().nth(3) == Some('0') {
                Instruction::RegisterEqualitySkip(
                    u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                    u8::from_str_radix(&instruction[2..3], 16).unwrap(),
                )
            } else {
                Instruction::NotImplemented
            }
        }
        '6' => Instruction::SetRegisterToValue(
            u8::from_str_radix(&instruction[1..2], 16).unwrap(),
            u8::from_str_radix(&instruction[2..4], 16).unwrap(),
        ),
        '7' => Instruction::AddToRegisterNoFlag(
            u8::from_str_radix(&instruction[1..2], 16).unwrap(),
            u8::from_str_radix(&instruction[2..4], 16).unwrap(),
        ),
        '8' => match instruction.chars().nth(3).unwrap() {
            '0' => Instruction::SetRegisterToRegister(
                u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                u8::from_str_radix(&instruction[2..3], 16).unwrap(),
            ),
            '1' => Instruction::OrRegister(
                u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                u8::from_str_radix(&instruction[2..3], 16).unwrap(),
            ),
            '2' => Instruction::AndRegister(
                u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                u8::from_str_radix(&instruction[2..3], 16).unwrap(),
            ),
            '3' => Instruction::XorRegister(
                u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                u8::from_str_radix(&instruction[2..3], 16).unwrap(),
            ),
            '4' => Instruction::AddRegisterToRegisterWithFlag(
                u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                u8::from_str_radix(&instruction[2..3], 16).unwrap(),
            ),
            '5' => Instruction::SubtractXYRegisterWithFlag(
                u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                u8::from_str_radix(&instruction[2..3], 16).unwrap(),
            ),
            '6' => Instruction::ShiftRight(
                u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                u8::from_str_radix(&instruction[2..3], 16).unwrap(),
            ),
            '7' => Instruction::SubtractYXRegisterWithFlag(
                u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                u8::from_str_radix(&instruction[2..3], 16).unwrap(),
            ),
            'e' => Instruction::ShiftLeft(
                u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                u8::from_str_radix(&instruction[2..3], 16).unwrap(),
            ),
            _ => Instruction::NotImplemented,
        },
        '9' => {
            if instruction.chars().nth(3) == Some('0') {
                Instruction::RegisterInequalitySkip(
                    u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                    u8::from_str_radix(&instruction[2..3], 16).unwrap(),
                )
            } else {
                Instruction::NotImplemented
            }
        }
        'a' => Instruction::SetIndex(u16::from_str_radix(&instruction[1..4], 16).unwrap()),
        'b' => Instruction::JumpWithRegisterOffset(
            u8::from_str_radix(&instruction[1..2], 16).unwrap(),
            u8::from_str_radix(&instruction[2..4], 16).unwrap(),
        ),
        'c' => Instruction::Random(
            u8::from_str_radix(&instruction[1..2], 16).unwrap(),
            u8::from_str_radix(&instruction[2..4], 16).unwrap(),
        ),
        'd' => Instruction::Draw(
            u8::from_str_radix(&instruction[1..2], 16).unwrap(),
            u8::from_str_radix(&instruction[2..3], 16).unwrap(),
            u8::from_str_radix(&instruction[3..4], 16).unwrap(),
        ),
        'e' => {
            if compare_ins_remainder(&instruction, String::from("9e")) {
                Instruction::SkipIfPressed(u8::from_str_radix(&instruction[1..2], 16).unwrap())
            } else if compare_ins_remainder(&instruction, String::from("a1")) {
                Instruction::SkipIfNotPressed(u8::from_str_radix(&instruction[1..2], 16).unwrap())
            } else {
                Instruction::NotImplemented
            }
        }
        'f' => {
            // TODO: Convert to inner switch
            if compare_ins_remainder(&instruction, String::from("07")) {
                Instruction::GetDelayTimer(u8::from_str_radix(&instruction[1..2], 16).unwrap())
            } else if compare_ins_remainder(&instruction, String::from("15")) {
                Instruction::SetDelayTimer(u8::from_str_radix(&instruction[1..2], 16).unwrap())
            } else if compare_ins_remainder(&instruction, String::from("18")) {
                Instruction::SetSoundTimer(u8::from_str_radix(&instruction[1..2], 16).unwrap())
            } else if compare_ins_remainder(&instruction, String::from("1E")) {
                Instruction::AddToIndex(u8::from_str_radix(&instruction[1..2], 16).unwrap())
            } else if compare_ins_remainder(&instruction, String::from("0A")) {
                Instruction::GetKey(u8::from_str_radix(&instruction[1..2], 16).unwrap())
            } else if compare_ins_remainder(&instruction, String::from("29")) {
                Instruction::FontCharacter(u8::from_str_radix(&instruction[1..2], 16).unwrap())
            } else if compare_ins_remainder(&instruction, String::from("33")) {
                Instruction::DecimalConversion(u8::from_str_radix(&instruction[1..2], 16).unwrap())
            } else if compare_ins_remainder(&instruction, String::from("55")) {
                Instruction::StoreMemory(u8::from_str_radix(&instruction[1..2], 16).unwrap())
            } else if compare_ins_remainder(&instruction, String::from("65")) {
                Instruction::LoadMemory(u8::from_str_radix(&instruction[1..2], 16).unwrap())
            } else {
                Instruction::NotImplemented
            }
        }

        _ => Instruction::NotImplemented,
    }
}

#[allow(dead_code)]
#[derive(PartialEq, Debug)]
enum Instruction {
    /// (NNN) 0NNN, Don't implement unless COSMAC VIP, ETI-660, DREAM 6800
    ExecuteMachineLangaugeRoutine(u16),

    /// 00E0, clear display
    ClearScreen,

    /// (NNN) 1NNN, GOTO NNN
    Jump(u16),

    /// (NNN) 2NNN, Push PC to stack, GOTO NNN
    CallSubroutine(u16),

    /// 00EE, PC == stack.pop
    ReturnSubroutine,

    /// (X, NN) 3XNN, VX == NN then skip next instruction
    ValueEqualitySkip(u8, u8),

    /// (X, NN) 4XNN, VX != NN then skip next instruction
    ValueInequalitySkip(u8, u8),

    /// (X, Y) 5XY0, VX == VY then skip next instruction
    RegisterEqualitySkip(u8, u8),

    /// (X, Y) 9XY0, VX != VY then skip next instruction
    RegisterInequalitySkip(u8, u8),

    /// (X, NN) 6XNN, VX = NN
    SetRegisterToValue(u8, u8),

    /// (X, NN) 7XNN, VX += NN     and do NOT update carry flag
    AddToRegisterNoFlag(u8, u8),

    /// (X, Y) 8XY0, VX = VY
    SetRegisterToRegister(u8, u8),

    /// (X, Y) 8XY1, VX |= VY
    OrRegister(u8, u8),

    /// (X, Y) 8XY2, VX &= VY
    AndRegister(u8, u8),

    /// (X, Y) 8XY3, VX ^= VY
    XorRegister(u8, u8),

    /// (X, Y) 8XY4, VX += VY     and DO update carry flag
    AddRegisterToRegisterWithFlag(u8, u8),

    /// (X, Y) 8XY5, VX -= VY     and VF = if VX>=VY {1} else {0}
    SubtractXYRegisterWithFlag(u8, u8),

    /// (X, Y) 8XY7, VX = VY - VX and VF = if VY>=VX {1} else {0}
    SubtractYXRegisterWithFlag(u8, u8),

    /// (X, Y) 8XY6, Shifts are ambiguous see https://tobiasvl.github.io/blog/write-a-chip-8-emulator/#8xy6-and-8xye-shift
    ShiftRight(u8, u8),

    /// (X, Y) 8XYE, see shift note above
    ShiftLeft(u8, u8),

    /// (NNN) ANNN, i = NNN
    SetIndex(u16),

    /// (X, NN) BXNN, GOTO NN + VX AND DIFFERENTLY BNNN, GOTO NNN + V0 -- Allow program to configure this
    JumpWithRegisterOffset(u8, u8),

    /// (X, NN) CXNN, VX = randomInt & NN
    Random(u8, u8),

    /// (X, Y, N) DXYN, Draw sprite of height N located at i to coords (VX, VY)
    Draw(u8, u8, u8),

    /// (X) EX8E, Skip if KEY in VX is pressed
    SkipIfPressed(u8),

    /// (X) EXA1, Skip if KEY in VX is NOT pressed
    SkipIfNotPressed(u8),

    /// (X) FX07, VX = delay_timer
    GetDelayTimer(u8),

    /// (X) FX15, delay_timer = VX
    SetDelayTimer(u8),

    /// (X) FX18, sound_timer = VX
    SetSoundTimer(u8),

    /// (X) FX1E, i += VX, read about Flag overflow - https://tobiasvl.github.io/blog/write-a-chip-8-emulator/#fx1e-add-to-index
    AddToIndex(u8),

    /// (X) FX0A, VX <- KeyInput - Waits for key input(blocks by decrementing PC until a key is pressed)
    GetKey(u8),

    /// (X) FX29, i = fontLocation(VX)
    FontCharacter(u8),

    /// (X) FX33, converted = decimalStr(VX); ram[i..i+len(converted)] = converted[...];
    DecimalConversion(u8),

    /// (X) FX55, V0..VX are stored from i..i+X. i isn't touched except for older games
    StoreMemory(u8),

    /// (X) FX65, V0..VX are loaded from i..i+X. i isn't touched except for older games
    LoadMemory(u8),

    /// Instruction type for not implemented
    NotImplemented,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_instruction() {
        struct TestCase {
            name: String,
            bytes: (u8, u8),
            expected: Instruction,
        }

        let test_cases: Vec<TestCase> = vec![
            TestCase {
                name: String::from("00E0 - ClearScreen"),
                bytes: (0x00, 0xE0),
                expected: Instruction::ClearScreen,
            },
            TestCase {
                name: String::from("00EE - ReturnSubroutine"),
                bytes: (0x00, 0xEE),
                expected: Instruction::ReturnSubroutine,
            },
            TestCase {
                name: String::from("1--- - Jump 2 nibbles"),
                bytes: (0x10, 123),
                expected: Instruction::Jump(123),
            },
            TestCase {
                // Jump takes 3 hex values, so the the leftover 1 from first byte adds to value
                name: String::from("1--- - Jump 3 nibbles"),
                bytes: (0x11, 123),
                expected: Instruction::Jump(379),
            },
            TestCase {
                name: String::from("2--- - CallSubroutine 2 nibbles"),
                bytes: (0x20, 123),
                expected: Instruction::CallSubroutine(123),
            },
            TestCase {
                name: String::from("2--- - CallSubroutine 3 nibbles"),
                bytes: (0x21, 123),
                expected: Instruction::CallSubroutine(379),
            },
            TestCase {
                name: String::from("3--- - ValueEqualitySkip - 5"),
                bytes: (0x3F, 0x05),
                expected: Instruction::ValueEqualitySkip(15, 5),
            },
            TestCase {
                name: String::from("3--- - ValueEqualitySkip - 21"),
                bytes: (0x31, 0x15),
                expected: Instruction::ValueEqualitySkip(1, 21),
            },
            TestCase {
                name: String::from("3--- - ValueEqualitySkip - 80"),
                bytes: (0x31, 0x50),
                expected: Instruction::ValueEqualitySkip(1, 80),
            },
            TestCase {
                name: String::from("3--- - ValueEqualitySkip - 255"),
                bytes: (0x31, 0xFF),
                expected: Instruction::ValueEqualitySkip(1, 255),
            },
            TestCase {
                name: String::from("4--- - ValueInequalitySkip"),
                bytes: (0x41, 0xFF),
                expected: Instruction::ValueInequalitySkip(1, 255),
            },
            TestCase {
                name: String::from("5--0 - RegisterEqualitySkip"),
                bytes: (0x51, 0xF0),
                expected: Instruction::RegisterEqualitySkip(1, 15),
            },
            TestCase {
                name: String::from("5--0 - RegisterEqualitySkip"),
                bytes: (0x5A, 0xC0),
                expected: Instruction::RegisterEqualitySkip(10, 12),
            },
            TestCase {
                name: String::from("7--- - AddRegisterNoFlag - 1 123"),
                bytes: (0x71, 123),
                expected: Instruction::AddToRegisterNoFlag(1, 123),
            },
            TestCase {
                name: String::from("7--- - AddRegisterNoFlag - 11 222"),
                bytes: (0x7B, 222),
                expected: Instruction::AddToRegisterNoFlag(11, 222),
            },
            TestCase {
                name: String::from("9--0 - RegisterInequalitySkip"),
                bytes: (0x91, 0xF0),
                expected: Instruction::RegisterInequalitySkip(1, 15),
            },
            TestCase {
                name: String::from("9--0 - RegisterInequalitySkip"),
                bytes: (0x9A, 0xC0),
                expected: Instruction::RegisterInequalitySkip(10, 12),
            },
            TestCase {
                name: String::from("9--0 - RegisterInequalitySkip"),
                bytes: (0x91, 0xF0),
                expected: Instruction::RegisterInequalitySkip(1, 15),
            },
            TestCase {
                name: String::from("9--0 - RegisterInequalitySkip"),
                bytes: (0x9A, 0xC0),
                expected: Instruction::RegisterInequalitySkip(10, 12),
            },
            TestCase {
                name: String::from("A--- - SetIndex 2 nibbles"),
                bytes: (0xA0, 123),
                expected: Instruction::SetIndex(123),
            },
            TestCase {
                name: String::from("A--- - SetIndex 3 nibbles"),
                bytes: (0xA1, 123),
                expected: Instruction::SetIndex(379),
            },
            TestCase {
                name: String::from("B--- - JumpWithRegisterOffset - 1 123"),
                bytes: (0xB1, 123),
                expected: Instruction::JumpWithRegisterOffset(1, 123),
            },
            TestCase {
                name: String::from("C--- - Random - 1 123"),
                bytes: (0xC1, 123),
                expected: Instruction::Random(1, 123),
            },
            TestCase {
                name: String::from("D--- - Draw - 1 12 42"),
                bytes: (0xD1, 0xCF),
                expected: Instruction::Draw(1, 12, 15),
            },
            TestCase {
                name: String::from("NotImplemented"),
                bytes: (0xFF, 0),
                expected: Instruction::NotImplemented,
            },
        ];

        for tc in test_cases {
            assert!(
                tc.expected == parse_instruction(tc.bytes),
                "Testing {} with {:?} -- expected {:?}, got {:?}",
                tc.name,
                tc.bytes,
                tc.expected,
                parse_instruction(tc.bytes)
            );
        }
    }
}
