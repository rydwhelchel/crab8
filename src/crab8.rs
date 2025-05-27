//! Spec of Crab8 largely written as described here
//! https://tobiasvl.github.io/blog/write-a-chip-8-emulator/
mod display;

use crossterm::{
    cursor::{self, Hide, Show},
    event::{Event, KeyCode, KeyEventKind, poll, read},
    execute,
    style::Print,
    terminal::{
        Clear, ClearType, EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode,
        enable_raw_mode,
    },
};
use display::Display;
use log::debug;
use rand::{Rng, rng};
use std::{
    cmp::Ordering,
    io::{self, stdout},
    sync::Arc,
    time::{Duration, Instant},
};

pub struct Config {
    pub old_shift: bool,
    pub old_jump_with_register_offset: bool,
    pub old_memory: bool,
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
    display: Display,

    // Pointers
    pc: u16,
    /// index - general use pointer
    i: u16,

    // Timers
    /// Ticks down 60 times per second, does nothing otherwise
    delay_timer: u8,
    /// Ticks down 60 times per second, beeps all the while
    sound_timer: u8,
    next_tick: Instant,

    // Not part of the original crab8, just have it for debug purposes
    cycles: usize,

    pressed_key: KeyCode,

    // Contains toggles for the different behaviors which chip-8 can use
    config: Config,
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

    const TIMER_TICK: Duration = Duration::from_secs(1 / 60);

    fn get_font_addr(val: u8) -> u16 {
        let hex_val = val % 16;
        (hex_val * 5).into()
    }

    pub fn new(config: Config) -> Crab8 {
        let mut ram = [0; 4096];
        ram[0..80].copy_from_slice(&Self::FONTS);

        enable_raw_mode().unwrap();
        execute!(
            io::stdout(),
            EnterAlternateScreen,
            Clear(ClearType::All),
            Hide
        )
        .unwrap();

        return Crab8 {
            ram,
            registers: [0; 16],
            stack: SubroutineStack::new(),
            display: Display::new(),
            pc: Self::OFFSET,
            i: 0,
            delay_timer: 0,
            sound_timer: 0,
            next_tick: Instant::now(),
            cycles: 0,
            pressed_key: KeyCode::Null,
            config,
        };
    }

    pub fn load_rom(&mut self, rom: Arc<[u8]>) {
        for i in 0..rom.len() {
            self.ram[Self::OFFSET as usize + i] = rom[i];
        }
    }

    pub fn start(&mut self) {
        let interval = Duration::from_secs(1);
        let mut log_time = Instant::now() + interval;
        let mut clear_key_time: Instant = Instant::now();

        loop {
            // results in around 700 cycles per second
            if poll(Duration::from_micros(1_225)).unwrap() {
                match read().unwrap() {
                    Event::Key(event) => match event.code {
                        KeyCode::Char('Q') => {
                            Self::unwind();
                            return;
                        }
                        _ => {
                            if event.kind == KeyEventKind::Release {
                                self.pressed_key = KeyCode::Null;
                            } else {
                                // Makes it so we don't constantly clear key while holding down key
                                clear_key_time = Instant::now() + Duration::from_millis(75);
                                self.pressed_key = event.code;
                            }
                        }
                    },
                    _ => {}
                }
            } else if Instant::now() > clear_key_time {
                self.pressed_key = KeyCode::Null;
            }
            // Chip8 cycle
            self.cycle();

            if Instant::now() > log_time {
                execute!(
                    stdout(),
                    cursor::MoveTo(10, 20),
                    Print(format!("Cycles: {} per second", self.cycles))
                )
                .unwrap();
                self.cycles = 0;
                log_time = Instant::now() + interval;
            }
            // Would be nice to show a full pad with every key you can press and which
            // is currently pressed
            execute!(
                stdout(),
                cursor::MoveTo(10, 21),
                Clear(ClearType::CurrentLine),
                Print(format!("Key Pressed: {}", self.pressed_key))
            )
            .unwrap();
        }
    }

    fn unwind() {
        execute!(io::stdout(), LeaveAlternateScreen, Show).unwrap();
        disable_raw_mode().unwrap();
    }

    fn cycle(&mut self) {
        if self.pc as usize >= self.ram.len() {
            panic!("pc reached out of bounds on RAM")
        }
        self.execute_instruction(
            parse_instruction((self.ram[self.pc as usize], self.ram[self.pc as usize + 1]))
                .unwrap(),
        );
        self.timer_tick();
        self.pc += 2;

        // Counting cycles for debug purposes
        debug!("Cycles: {}", self.cycles);
        self.cycles += 1;
    }

    fn timer_tick(&mut self) {
        let now = Instant::now();
        if self.delay_timer > 0 && now > self.next_tick {
            self.delay_timer -= 1;
        }
        if self.sound_timer > 0 && now > self.next_tick {
            self.sound_timer -= 1;
        }

        self.next_tick = now + Self::TIMER_TICK;
    }

    // TODO: Add something that counts each instruction that is executed
    // on quit, print that something
    fn execute_instruction(&mut self, ins: Instruction) {
        debug!("Executing ins {:?}", ins);
        match ins {
            Instruction::ExecuteMachineLangaugeRoutine(_value) => {
                panic!("attempted to execute machine language routine, exiting")
            }
            Instruction::ClearScreen => {
                self.display.clear_screen();
            }
            Instruction::Jump(loc) => {
                self.pc = loc;
                // Decrement PC to cancel out later increment
                self.pc -= 2;
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
            Instruction::AddToRegisterNoFlag(vx, value) => {
                self.registers[vx as usize] = self.registers[vx as usize].wrapping_add(value);
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
            Instruction::AddRegisterToRegisterWithFlag(vx, vy) => {
                let (new_val, overflowed) =
                    self.registers[vx as usize].overflowing_add(self.registers[vy as usize]);
                self.registers[vx as usize] = new_val;
                self.registers[0xF] = overflowed.into();
            }
            Instruction::SubtractXYRegisterWithFlag(vx, vy) => {
                let (new_val, overflowed) =
                    self.registers[vx as usize].overflowing_sub(self.registers[vy as usize]);
                self.registers[vx as usize] = new_val;
                // flag is set to 1 if an overflow does NOT occur
                self.registers[0xF] = (!overflowed).into();
            }
            Instruction::SubtractYXRegisterWithFlag(vx, vy) => {
                let (new_val, overflowed) =
                    self.registers[vy as usize].overflowing_sub(self.registers[vx as usize]);
                self.registers[vx as usize] = new_val;
                // flag is set to 1 if an overflow does NOT occur
                self.registers[0xF] = (!overflowed).into();
            }
            Instruction::ShiftRight(vx, vy) => {
                // NOTE: If we have enabled old shift behavior, set vx to vy first
                if self.config.old_shift {
                    self.registers[vx as usize] = self.registers[vy as usize];
                }

                // Capture the right bit before it gets shifted out
                let shifted_out = self.registers[vx as usize] & 0b00000001;
                self.registers[vx as usize] = self.registers[vx as usize] >> 1;
                self.registers[0xF] = shifted_out;
            }
            Instruction::ShiftLeft(vx, vy) => {
                // NOTE: If we have enabled old shift behavior, set vx to vy first
                if self.config.old_shift {
                    self.registers[vx as usize] = self.registers[vy as usize];
                }

                // Capture the right bit before it gets shifted out
                let shifted_out = self.registers[vx as usize] & 0b10000000;
                self.registers[vx as usize] = self.registers[vx as usize] << 1;
                self.registers[0xF] = shifted_out;
            }
            Instruction::SetIndex(value) => {
                self.i = value;
            }
            Instruction::JumpWithRegisterOffset(vx, value) => {
                if self.config.old_jump_with_register_offset {
                    self.pc = self.registers[0] as u16 + value;
                } else {
                    self.pc = self.registers[vx as usize] as u16 + value;
                }
            }
            Instruction::Random(vx, value) => {
                let ran_value: u8 = rng().random();
                self.registers[vx as usize] = ran_value & value;
            }
            Instruction::Draw(vx, vy, n) => {
                let (x, y) = (
                    (self.registers[vx as usize] % 64),
                    (self.registers[vy as usize] % 32),
                );
                // TODO: Improve (remove vec if possible)
                let mut sprite: Vec<u8> = Vec::new();
                for i in 0..n {
                    // get n bytes of the sprite
                    sprite.push(self.ram[(self.i + i as u16) as usize]);
                }
                self.registers[0xF] = self.display.draw((x, y), sprite) as u8;
            }
            Instruction::SkipIfPressed(vx) => {
                let KeyCode::Char(pressed_key) = self.pressed_key else {
                    return;
                };
                let Some(input) = InputKey::get_input_key(pressed_key) else {
                    return;
                };
                if self.registers[vx as usize] == input.into() {
                    self.pc += 2;
                }
            }
            Instruction::SkipIfNotPressed(vx) => {
                if let KeyCode::Char(pressed_key) = self.pressed_key {
                    if let Some(input) = InputKey::get_input_key(pressed_key) {
                        if self.registers[vx as usize] == input.into() {
                            return;
                        }
                    }
                }
                self.pc += 2;
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
            Instruction::AddToIndex(vx) => {
                // NOTE: This is technically not how the COSMAC VIP worked, but no known games rely
                // on previous behavior
                let (new_val, overflowed) =
                    self.i.overflowing_add(self.registers[vx as usize] as u16);
                self.i = new_val;
                self.registers[0xF] = overflowed.into();
            }
            Instruction::GetKey(vx) => {
                let KeyCode::Char(pressed_key) = self.pressed_key else {
                    self.pc -= 2;
                    return;
                };
                let Some(input) = InputKey::get_input_key(pressed_key) else {
                    self.pc -= 2;
                    return;
                };
                self.registers[vx as usize] = input.into();
            }
            Instruction::FontCharacter(vx) => {
                self.i = Self::get_font_addr(self.registers[vx as usize]);
            }
            // TODO: Find out if this needs to be dynamic (if a number is only 2 digits, only
            // replace 2 spots in ram) or if it should always replace, even leading 0s
            Instruction::DecimalConversion(vx) => {
                let x = self.registers[vx as usize];
                let (hundreds, tens, ones) = (x / 100, (x / 10) % 10, x % 10);

                assert!(
                    ((self.i + 2) as usize) < self.ram.len(),
                    "attempted to insert decimal conversion outside of ram"
                );

                self.ram[self.i as usize] = hundreds;
                self.ram[(self.i + 1) as usize] = tens;
                self.ram[(self.i + 2) as usize] = ones;
            }
            Instruction::StoreMemory(vx) => {
                if self.config.old_memory {
                    for i in 0..vx {
                        self.i += 1;
                        self.ram[self.i as usize] = self.registers[i as usize];
                    }
                    self.i += 1;
                } else {
                    for i in 0..vx {
                        self.ram[(self.i + i as u16) as usize] = self.registers[i as usize];
                    }
                }
            }
            Instruction::LoadMemory(vx) => {
                if self.config.old_memory {
                    for i in 0..vx {
                        self.i += 1;
                        self.registers[i as usize] = self.ram[self.i as usize]
                    }
                    self.i += 1;
                } else {
                    for i in 0..vx {
                        self.registers[i as usize] = self.ram[(self.i + i as u16) as usize]
                    }
                }
            }
        }
    }
}

/// Corresponds to the COSMAC VIP keypad
/// 1 2 3 C
/// 4 5 6 D
/// 7 8 9 E
/// A 0 B F
enum InputKey {
    // TODO: Is there a better name for these?
    One,
    Two,
    Three,
    C,
    Four,
    Five,
    Six,
    D,
    Seven,
    Eight,
    Nine,
    E,
    A,
    Zero,
    B,
    F,
}

// The clumsiest impl ever
impl InputKey {
    fn get_input_key(value: char) -> Option<Self> {
        match value {
            '1' => Some(InputKey::One),
            '2' => Some(InputKey::Two),
            '3' => Some(InputKey::Three),
            '4' => Some(InputKey::C),
            'q' => Some(InputKey::Four),
            'w' => Some(InputKey::Five),
            'e' => Some(InputKey::Six),
            'r' => Some(InputKey::D),
            'a' => Some(InputKey::Seven),
            's' => Some(InputKey::Eight),
            'd' => Some(InputKey::Nine),
            'f' => Some(InputKey::E),
            'z' => Some(InputKey::A),
            'x' => Some(InputKey::Zero),
            'c' => Some(InputKey::B),
            'v' => Some(InputKey::F),
            _ => None,
        }
    }
}

impl From<InputKey> for u8 {
    fn from(value: InputKey) -> Self {
        match value {
            InputKey::Zero => 0,
            InputKey::One => 1,
            InputKey::Two => 2,
            InputKey::Three => 3,
            InputKey::Four => 4,
            InputKey::Five => 5,
            InputKey::Six => 6,
            InputKey::Seven => 7,
            InputKey::Eight => 8,
            InputKey::Nine => 9,
            InputKey::A => 10,
            InputKey::B => 11,
            InputKey::C => 12,
            InputKey::D => 13,
            InputKey::E => 14,
            InputKey::F => 15,
        }
    }
}

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

#[derive(Debug)]
enum InstructionError {
    NotImplemented,
}

// TODO: Make this return a result and error in NotImplmented cases to capture more info
fn parse_instruction(bytes: (u8, u8)) -> Result<Instruction, InstructionError> {
    let instruction: String = format!("{:04x}", combine_bytes(bytes));

    println!("Parsing instruction {:?}", instruction);
    // Unwrapping directly because above format string should always have 4 characters
    match instruction.chars().nth(0).unwrap() {
        '0' => {
            if compare_ins_remainder(&instruction, String::from("0e0")) {
                Ok(Instruction::ClearScreen)
            } else if compare_ins_remainder(&instruction, String::from("0ee")) {
                Ok(Instruction::ReturnSubroutine)
            } else {
                Err(InstructionError::NotImplemented)
            }
        }
        '1' => {
            // TODO: Handle unwrap
            Ok(Instruction::Jump(
                u16::from_str_radix(&instruction[1..4], 16).unwrap(),
            ))
        }
        '2' => {
            // TODO: Handle unwrap
            Ok(Instruction::CallSubroutine(
                u16::from_str_radix(&instruction[1..4], 16).unwrap(),
            ))
        }
        '3' => Ok(Instruction::ValueEqualitySkip(
            u8::from_str_radix(&instruction[1..2], 16).unwrap(),
            u8::from_str_radix(&instruction[2..4], 16).unwrap(),
        )),
        '4' => Ok(Instruction::ValueInequalitySkip(
            u8::from_str_radix(&instruction[1..2], 16).unwrap(),
            u8::from_str_radix(&instruction[2..4], 16).unwrap(),
        )),
        '5' => {
            if instruction.chars().nth(3) == Some('0') {
                Ok(Instruction::RegisterEqualitySkip(
                    u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                    u8::from_str_radix(&instruction[2..3], 16).unwrap(),
                ))
            } else {
                Err(InstructionError::NotImplemented)
            }
        }
        '6' => Ok(Instruction::SetRegisterToValue(
            u8::from_str_radix(&instruction[1..2], 16).unwrap(),
            u8::from_str_radix(&instruction[2..4], 16).unwrap(),
        )),
        '7' => Ok(Instruction::AddToRegisterNoFlag(
            u8::from_str_radix(&instruction[1..2], 16).unwrap(),
            u8::from_str_radix(&instruction[2..4], 16).unwrap(),
        )),
        '8' => match instruction.chars().nth(3).unwrap() {
            '0' => Ok(Instruction::SetRegisterToRegister(
                u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                u8::from_str_radix(&instruction[2..3], 16).unwrap(),
            )),
            '1' => Ok(Instruction::OrRegister(
                u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                u8::from_str_radix(&instruction[2..3], 16).unwrap(),
            )),
            '2' => Ok(Instruction::AndRegister(
                u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                u8::from_str_radix(&instruction[2..3], 16).unwrap(),
            )),
            '3' => Ok(Instruction::XorRegister(
                u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                u8::from_str_radix(&instruction[2..3], 16).unwrap(),
            )),
            '4' => Ok(Instruction::AddRegisterToRegisterWithFlag(
                u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                u8::from_str_radix(&instruction[2..3], 16).unwrap(),
            )),
            '5' => Ok(Instruction::SubtractXYRegisterWithFlag(
                u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                u8::from_str_radix(&instruction[2..3], 16).unwrap(),
            )),
            '6' => Ok(Instruction::ShiftRight(
                u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                u8::from_str_radix(&instruction[2..3], 16).unwrap(),
            )),
            '7' => Ok(Instruction::SubtractYXRegisterWithFlag(
                u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                u8::from_str_radix(&instruction[2..3], 16).unwrap(),
            )),
            'e' => Ok(Instruction::ShiftLeft(
                u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                u8::from_str_radix(&instruction[2..3], 16).unwrap(),
            )),
            _ => Err(InstructionError::NotImplemented),
        },
        '9' => {
            if instruction.chars().nth(3) == Some('0') {
                Ok(Instruction::RegisterInequalitySkip(
                    u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                    u8::from_str_radix(&instruction[2..3], 16).unwrap(),
                ))
            } else {
                Err(InstructionError::NotImplemented)
            }
        }
        'a' => Ok(Instruction::SetIndex(
            u16::from_str_radix(&instruction[1..4], 16).unwrap(),
        )),
        'b' => Ok(Instruction::JumpWithRegisterOffset(
            u8::from_str_radix(&instruction[1..2], 16).unwrap(),
            // NOTE: This double parses the second value INTENTIONALLY. Weird instruction
            u16::from_str_radix(&instruction[1..4], 16).unwrap(),
        )),
        'c' => Ok(Instruction::Random(
            u8::from_str_radix(&instruction[1..2], 16).unwrap(),
            u8::from_str_radix(&instruction[2..4], 16).unwrap(),
        )),
        'd' => Ok(Instruction::Draw(
            u8::from_str_radix(&instruction[1..2], 16).unwrap(),
            u8::from_str_radix(&instruction[2..3], 16).unwrap(),
            u8::from_str_radix(&instruction[3..4], 16).unwrap(),
        )),
        'e' => {
            if compare_ins_remainder(&instruction, String::from("9e")) {
                Ok(Instruction::SkipIfPressed(
                    u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                ))
            } else if compare_ins_remainder(&instruction, String::from("a1")) {
                Ok(Instruction::SkipIfNotPressed(
                    u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                ))
            } else {
                Err(InstructionError::NotImplemented)
            }
        }
        'f' => {
            // TODO: Convert to inner switch
            if compare_ins_remainder(&instruction, String::from("07")) {
                Ok(Instruction::GetDelayTimer(
                    u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                ))
            } else if compare_ins_remainder(&instruction, String::from("15")) {
                Ok(Instruction::SetDelayTimer(
                    u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                ))
            } else if compare_ins_remainder(&instruction, String::from("18")) {
                Ok(Instruction::SetSoundTimer(
                    u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                ))
            } else if compare_ins_remainder(&instruction, String::from("1e")) {
                Ok(Instruction::AddToIndex(
                    u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                ))
            } else if compare_ins_remainder(&instruction, String::from("0a")) {
                Ok(Instruction::GetKey(
                    u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                ))
            } else if compare_ins_remainder(&instruction, String::from("29")) {
                Ok(Instruction::FontCharacter(
                    u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                ))
            } else if compare_ins_remainder(&instruction, String::from("33")) {
                Ok(Instruction::DecimalConversion(
                    u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                ))
            } else if compare_ins_remainder(&instruction, String::from("55")) {
                Ok(Instruction::StoreMemory(
                    u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                ))
            } else if compare_ins_remainder(&instruction, String::from("65")) {
                Ok(Instruction::LoadMemory(
                    u8::from_str_radix(&instruction[1..2], 16).unwrap(),
                ))
            } else {
                Err(InstructionError::NotImplemented)
            }
        }

        _ => Err(InstructionError::NotImplemented),
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

    /// (X, NN) BXNN, GOTO XNN + VX AND DIFFERENTLY BNNN, GOTO NNN + V0 -- Allow program to configure this
    JumpWithRegisterOffset(u8, u16),

    /// (X, NN) CXNN, VX = randomInt & NN
    Random(u8, u8),

    /// (X, Y, N) DXYN, Draw sprite of height N located at i to coords (VX, VY)
    Draw(u8, u8, u8),

    /// (X) EX9E, Skip if KEY in VX is pressed
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_instruction() {
        struct TestCase<'a> {
            name: &'a str,
            bytes: (u8, u8),
            expected: Result<Instruction, InstructionError>,
        }
        // NOTE: Downside of table testing this way is that all tests are treated as one and if one
        // fails it does not test the remaining cases
        let test_cases: Vec<TestCase> = vec![
            TestCase {
                name: "00E0 - ClearScreen",
                bytes: (0x00, 0xE0),
                expected: Ok(Instruction::ClearScreen),
            },
            TestCase {
                name: "00EE - ReturnSubroutine",
                bytes: (0x00, 0xEE),
                expected: Ok(Instruction::ReturnSubroutine),
            },
            TestCase {
                name: "1--- - Jump 2 nibbles",
                bytes: (0x10, 123),
                expected: Ok(Instruction::Jump(123)),
            },
            TestCase {
                // Jump takes 3 hex values, so the the leftover 1 from first byte adds to value
                name: "1--- - Jump 3 nibbles",
                bytes: (0x11, 123),
                expected: Ok(Instruction::Jump(379)),
            },
            TestCase {
                name: "2--- - CallSubroutine 2 nibbles",
                bytes: (0x20, 123),
                expected: Ok(Instruction::CallSubroutine(123)),
            },
            TestCase {
                name: "2--- - CallSubroutine 3 nibbles",
                bytes: (0x21, 123),
                expected: Ok(Instruction::CallSubroutine(379)),
            },
            TestCase {
                name: "3--- - ValueEqualitySkip - 5",
                bytes: (0x3F, 0x05),
                expected: Ok(Instruction::ValueEqualitySkip(15, 5)),
            },
            TestCase {
                name: "3--- - ValueEqualitySkip - 21",
                bytes: (0x31, 0x15),
                expected: Ok(Instruction::ValueEqualitySkip(1, 21)),
            },
            TestCase {
                name: "3--- - ValueEqualitySkip - 80",
                bytes: (0x31, 0x50),
                expected: Ok(Instruction::ValueEqualitySkip(1, 80)),
            },
            TestCase {
                name: "3--- - ValueEqualitySkip - 255",
                bytes: (0x31, 0xFF),
                expected: Ok(Instruction::ValueEqualitySkip(1, 255)),
            },
            TestCase {
                name: "4--- - ValueInequalitySkip",
                bytes: (0x41, 0xFF),
                expected: Ok(Instruction::ValueInequalitySkip(1, 255)),
            },
            TestCase {
                name: "5--0 - RegisterEqualitySkip",
                bytes: (0x51, 0xF0),
                expected: Ok(Instruction::RegisterEqualitySkip(1, 15)),
            },
            TestCase {
                name: "5--0 - RegisterEqualitySkip",
                bytes: (0x5A, 0xC0),
                expected: Ok(Instruction::RegisterEqualitySkip(10, 12)),
            },
            TestCase {
                name: "7--- - AddRegisterNoFlag - 1 123",
                bytes: (0x71, 123),
                expected: Ok(Instruction::AddToRegisterNoFlag(1, 123)),
            },
            TestCase {
                name: "7--- - AddRegisterNoFlag - 11 222",
                bytes: (0x7B, 222),
                expected: Ok(Instruction::AddToRegisterNoFlag(11, 222)),
            },
            TestCase {
                name: "9--0 - RegisterInequalitySkip",
                bytes: (0x91, 0xF0),
                expected: Ok(Instruction::RegisterInequalitySkip(1, 15)),
            },
            TestCase {
                name: "9--0 - RegisterInequalitySkip",
                bytes: (0x9A, 0xC0),
                expected: Ok(Instruction::RegisterInequalitySkip(10, 12)),
            },
            TestCase {
                name: "9--0 - RegisterInequalitySkip",
                bytes: (0x91, 0xF0),
                expected: Ok(Instruction::RegisterInequalitySkip(1, 15)),
            },
            TestCase {
                name: "9--0 - RegisterInequalitySkip",
                bytes: (0x9A, 0xC0),
                expected: Ok(Instruction::RegisterInequalitySkip(10, 12)),
            },
            TestCase {
                name: "A--- - SetIndex 2 nibbles",
                bytes: (0xA0, 123),
                expected: Ok(Instruction::SetIndex(123)),
            },
            TestCase {
                name: "A--- - SetIndex 3 nibbles",
                bytes: (0xA1, 123),
                expected: Ok(Instruction::SetIndex(379)),
            },
            TestCase {
                name: "B--- - JumpWithRegisterOffset - 1 123",
                bytes: (0xB1, 123),
                expected: Ok(Instruction::JumpWithRegisterOffset(1, 379)),
            },
            TestCase {
                name: "C--- - Random - 1 123",
                bytes: (0xC1, 123),
                expected: Ok(Instruction::Random(1, 123)),
            },
            TestCase {
                name: "D--- - Draw - 1 12 42",
                bytes: (0xD1, 0xCF),
                expected: Ok(Instruction::Draw(1, 12, 15)),
            },
            TestCase {
                name: "E-9E - SkipIfPressed - 12",
                bytes: (0xEC, 0x9E),
                expected: Ok(Instruction::SkipIfPressed(12)),
            },
            TestCase {
                name: "E-A1 - SkipIfNotPressed - 15",
                bytes: (0xEF, 0xA1),
                expected: Ok(Instruction::SkipIfNotPressed(15)),
            },
            TestCase {
                name: "E-XX - NotImplemented",
                bytes: (0xEF, 0xF1),
                expected: Err(InstructionError::NotImplemented),
            },
            TestCase {
                name: "F-07 - GetDelayTimer - 10",
                bytes: (0xFA, 0x07),
                expected: Ok(Instruction::GetDelayTimer(10)),
            },
            TestCase {
                name: "F-15 - SetDelayTimer - 10",
                bytes: (0xFA, 0x15),
                expected: Ok(Instruction::SetDelayTimer(10)),
            },
            TestCase {
                name: "F-18 - SetSoundTimer - 11",
                bytes: (0xFB, 0x18),
                expected: Ok(Instruction::SetSoundTimer(11)),
            },
            TestCase {
                name: "F-1E - AddToIndex - 7",
                bytes: (0xF7, 0x1E),
                expected: Ok(Instruction::AddToIndex(7)),
            },
            TestCase {
                name: "F-0A - GetKey - 9",
                bytes: (0xF9, 0x0A),
                expected: Ok(Instruction::GetKey(9)),
            },
            TestCase {
                name: "F-29 - FontCharacter - 9",
                bytes: (0xF9, 0x29),
                expected: Ok(Instruction::FontCharacter(9)),
            },
            TestCase {
                name: "F-33 - DecimalConversion - 9",
                bytes: (0xF9, 0x33),
                expected: Ok(Instruction::DecimalConversion(9)),
            },
            TestCase {
                name: "F-55 - StoreMemory - 9",
                bytes: (0xF9, 0x55),
                expected: Ok(Instruction::StoreMemory(9)),
            },
            TestCase {
                name: "F-65 - LoadMemory - 9",
                bytes: (0xF9, 0x65),
                expected: Ok(Instruction::LoadMemory(9)),
            },
            TestCase {
                name: "NotImplemented",
                bytes: (0xFF, 0),
                expected: Err(InstructionError::NotImplemented),
            },
        ];

        for tc in test_cases {
            let parsed = parse_instruction(tc.bytes);
            let equal = match &tc.expected {
                Ok(val) => val == &parsed.unwrap(),
                Err(_) => parsed.is_err(),
            };

            assert!(
                equal,
                "Testing {} with {:?} -- expected {:?}, got {:?}",
                tc.name,
                tc.bytes,
                tc.expected,
                parse_instruction(tc.bytes)
            );
        }
    }

    #[test]
    fn test_execute_instruction() {}
}
