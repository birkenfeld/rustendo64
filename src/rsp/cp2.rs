use std::fmt;
use byteorder::{ByteOrder, LittleEndian};

const NUM_VREGS: usize = 32;

#[allow(dead_code)]
pub struct Cp2 {
    pub vec:     [[u8; 16]; NUM_VREGS],
    pub flags:   [[u8; 32]; 3],
    pub acc:     [[u8; 16]; 3],

    div_out: i16,
    div_in:  i16,
    dp_flag: u8,
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
            for col in 0..4 {
                let i = row + col * 8;
                try!(write!(f, "  {:02} = {}", i, format_vec(&self.vec[i])));
            }
            try!(write!(f, "\n"));
        }
        // XXX flags, acc
        write!(f, "")
    }
}

fn format_vec(v: &[u8; 16]) -> String {
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
