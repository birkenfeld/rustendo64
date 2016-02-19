use std::fmt;
use simd;

const NUM_VREGS: usize = 32;

#[allow(dead_code)]
pub struct Cp2 {
    regs:    [simd::i16x8; NUM_VREGS],
    flags:   [simd::i16x8; 6],
    acc:     [simd::i16x8; 3],

    div_out: i16,
    div_in:  i16,
    dp_flag: u8,
}

impl Default for Cp2 {
    fn default() -> Self {
        Cp2 {
            regs:    [simd::i16x8::splat(0); NUM_VREGS],
            flags:   [simd::i16x8::splat(0); 6],
            acc:     [simd::i16x8::splat(0); 3],
            div_out: 0,
            div_in:  0,
            dp_flag: 0,
        }
    }
}

impl fmt::Debug for Cp2 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "...") // TODO
    }
}
