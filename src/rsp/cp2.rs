use std::fmt;
use simd::i16x8;

const NUM_VREGS: usize = 32;

pub const VCO: usize = 0;
pub const VCC: usize = 1;
pub const VCE: usize = 2;
pub const HI:  usize = 0;
pub const LO:  usize = 1;

pub const ACC_HI:  usize = 0;
pub const ACC_MD:  usize = 1;
pub const ACC_LO:  usize = 2;

#[allow(dead_code)]
pub struct Cp2 {
    pub vec:     [i16x8; NUM_VREGS],
    pub flags:   [(i16x8, i16x8); 3],
    pub acc:     [i16x8; 3],

    pub div_out: i16,
    pub div_in:  i16,
    pub dp_flag: u8,
}

impl Default for Cp2 {
    fn default() -> Self {
        Cp2 {
            vec:     [i16x8::splat(0); NUM_VREGS],
            flags:   [(i16x8::splat(0), i16x8::splat(0)); 3],
            acc:     [i16x8::splat(0); 3],
            div_out: 0,
            div_in:  0,
            dp_flag: 0,
        }
    }
}

impl fmt::Debug for Cp2 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for row in 0..8 {
            try!(write!(f, " "));
            for col in 0..4 {
                let i = row + col * 8;
                try!(write!(f, " {:02} = {} |", i, format_vec(self.vec[i])));
            }
            try!(write!(f, "\n"));
        }
        try!(write!(f, " "));
        const N: [&'static str; 3] = ["hi", "md", "lo"];
        for i in 0..3 {
            try!(write!(f, " {} = {} |", N[i], format_vec(self.acc[i])));
        }
        write!(f, "\n")
    }
}

pub fn format_vec(v: i16x8) -> String {
    format!("{:4x} {:4x} {:4x} {:4x} {:4x} {:4x} {:4x} {:4x}",
            v.extract(0), v.extract(1), v.extract(2), v.extract(3),
            v.extract(4), v.extract(5), v.extract(6), v.extract(7))
}
