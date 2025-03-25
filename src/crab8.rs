use std::cmp::Ordering;

/// Spec of Crab8 largely written as described here
/// https://tobiasvl.github.io/blog/write-a-chip-8-emulator/
#[allow(dead_code)]
pub struct Crab8 {
    /// Read&write RAM. Chip8 games modify themselves in memory frequently
    /// Fonts are stored at index 0
    ram: [u8; 4096],
    /// 16 8-bit (one byte) general-purpose variable registers
    /// numbered 0 through F hexadecimal, ie. 0 through 15 in
    /// decimal, called V0 through VF. VF is usually the flag register
    registers: [u8; 16],
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
            display: [[false; 64]; 32],
            pc: Self::OFFSET,
            i: 0,
            delay_timer: 0,
            sound_timer: 0,
        };
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

    println!("{}", instruction);
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
        '7' => Instruction::AddRegisterNoFlag(
            u8::from_str_radix(&instruction[1..2], 16).unwrap(),
            u8::from_str_radix(&instruction[2..4], 16).unwrap(),
        ),
        '8' => Instruction::NotImplemented, // TODO:
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
        'e' => Instruction::NotImplemented,
        'f' => Instruction::NotImplemented,
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

    /// (X, NN) 3XNN, VX == NN
    ValueEqualitySkip(u8, u8),

    /// (X, NN) 4XNN, VX != NN
    ValueInequalitySkip(u8, u8),

    /// (X, Y) 5XY0, VX == VY
    RegisterEqualitySkip(u8, u8),

    /// (X, Y) 9XY0, VX != VY
    RegisterInequalitySkip(u8, u8),

    /// (X, NN) 6XNN, VX = NN
    SetRegisterToValue(u8, u8),

    /// (X, NN) 7XNN, VX += NN     and do NOT update carry flag
    AddRegisterNoFlag(u8, u8),

    /// (X, Y) 8XY0, VX = VY
    SetRegisterToRegister(u8, u8),

    /// (X, Y) 8XY1, VX |= VY
    OrRegister(u8, u8),

    /// (X, Y) 8XY2, VX &= VY
    AndRegister(u8, u8),

    /// (X, Y) 8XY3, VX ^= VY
    XorRegister(u8, u8),

    /// (X, Y) 8XY4, VX += VY     and DO update carry flag
    AddRegisterWithFlag(u8, u8),

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
                expected: Instruction::AddRegisterNoFlag(1, 123),
            },
            TestCase {
                name: String::from("7--- - AddRegisterNoFlag - 11 222"),
                bytes: (0x7B, 222),
                expected: Instruction::AddRegisterNoFlag(11, 222),
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
