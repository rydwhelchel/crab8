/// Spec of Crab8 largely written as described here
/// https://tobiasvl.github.io/blog/write-a-chip-8-emulator/
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

// NOTE: Shifts are ambiguous see https://tobiasvl.github.io/blog/write-a-chip-8-emulator/#8xy6-and-8xye-shift
// NOTE: JumpWithOffset,              // BNNN, GOTO NNN + V0 -- Allow program to configure this
// NOTE: AddToIndex Flag overflow - https://tobiasvl.github.io/blog/write-a-chip-8-emulator/#fx1e-add-to-index
enum Instructions {
    ExecuteMachineLangaugeRoutine, // 0NNN, Don't implement unless COSMAC VIP, ETI-660, DREAM 6800
    ClearScreen,                   // 00E0, clear display
    Jump,                          // 1NNN, GOTO NNN
    CallSubroutine,                // 2NNN, Push PC to stack, GOTO NNN
    ReturnSubroutine,              // 00EE, PC == stack.pop
    ValueEqualitySkip,             // 3XNN, VX == NN
    ValueInequalitySkip,           // 4XNN, VX != NN
    RegisterEqualitySkip,          // 5XY0, VX == VY
    RegisterInequalitySkip,        // 9XY0, VX != VY
    SetRegisterToValue,            // 6XNN, VX = NN
    AddRegisterNoFlag,             // 7XNN, VX += NN     and do NOT update carry flag
    SetRegisterToRegister,         // 8XY0, VX = VY
    OrRegister,                    // 8XY1, VX |= VY
    AndRegister,                   // 8XY2, VX &= VY
    XorRegister,                   // 8XY3, VX ^= VY
    AddRegisterWithFlag,           // 8XY4, VX += VY     and DO update carry flag
    SubtractXYRegisterWithFlag,    // 8XY5, VX -= VY     and VF = if VX>=VY {1} else {0}
    SubtractYXRegisterWithFlag,    // 8XY7, VX = VY - VX and VF = if VY>=VX {1} else {0}
    ShiftRight,                    // 8XY6, see shift note above
    ShiftLeft,                     // 8XYE, see shift note above
    SetIndex,                      // ANNN, i = NNN
    JumpWithRegisterOffset,        // BXNN, GOTO NNN + VX and see above comment on JumpWithOffset
    Random,                        // CXNN, VX = randomInt & NN
    Draw,                          // DXYN, Draw sprite of height N located at i to coords (VX, VY)
    SkipIfPressed,                 // EX8E, Skip if KEY in VX is pressed
    SkipIfNotPressed,              // EXA1, Skip if KEY in VX is NOT pressed
    GetDelayTimer,                 // FX07, VX = delay_timer
    SetDelayTimer,                 // FX15, delay_timer = VX
    SetSoundTimer,                 // FX18, sound_timer = VX
    AddToIndex,                    // FX1E, i += VX, read about flag overflowing
    FontCharacter,                 // FX29, i = fontLocation(VX)
    DecimalConversion,             // FX33, converted = decimalStr(VX);
    //                                      ram[i..i+len(converted)] = converted[...];
    StoreMemory, // FX55, V0..VX are stored from i..i+X. i isn't touched except for older games
    LoadMemory,  // FX65, V0..VX are loaded from i..i+X. i isn't touched except for older games
}
