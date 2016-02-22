use std::fmt;
use simd::i16x8;
use byteorder::{ByteOrder, LittleEndian};

const NUM_VREGS: usize = 32;

pub const VCO: usize = 0;
pub const VCC: usize = 1;
pub const VCE: usize = 2;
pub const HI:  usize = 0;
pub const LO:  usize = 16;

pub const ACC_HI:  usize = 0;
pub const ACC_MD:  usize = 1;
pub const ACC_LO:  usize = 2;

#[allow(dead_code)]
pub struct Cp2 {
    /* TODO: is it faster to store them as SIMD types? */
    pub vec:     [[u8; 16]; NUM_VREGS],
    pub flags:   [[u8; 32]; 3],
    pub acc:     [[u8; 16]; 3],

    pub div_out: i16,
    pub div_in:  i16,
    pub dp_flag: u8,
}

impl Default for Cp2 {
    fn default() -> Self {
        Cp2 {
            vec:     [[0; 16]; NUM_VREGS],
            flags:   [[0; 32]; 3],
            acc:     [[0; 16]; 3],
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
                try!(write!(f, " {:02} = {} |", i, format_vec_array(&self.vec[i])));
            }
            try!(write!(f, "\n"));
        }
        // XXX flags, acc
        write!(f, "")
    }
}

pub fn format_vec_array(v: &[u8; 16]) -> String {
    format!("{:4x} {:4x} {:4x} {:4x} {:4x} {:4x} {:4x} {:4x}",
            LittleEndian::read_u16(&v[0..]),
            LittleEndian::read_u16(&v[2..]),
            LittleEndian::read_u16(&v[4..]),
            LittleEndian::read_u16(&v[6..]),
            LittleEndian::read_u16(&v[8..]),
            LittleEndian::read_u16(&v[10..]),
            LittleEndian::read_u16(&v[12..]),
            LittleEndian::read_u16(&v[14..]))
}

pub fn format_vec(v: i16x8) -> String {
    format!("{:4x} {:4x} {:4x} {:4x} {:4x} {:4x} {:4x} {:4x}",
            v.extract(0), v.extract(1), v.extract(2), v.extract(3),
            v.extract(4), v.extract(5), v.extract(6), v.extract(7))
}
