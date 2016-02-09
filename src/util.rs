//! Miscellaneous utilities for the emulator.

use std::fs::File;
use std::io::Read;
use std::path::Path;

pub fn read_bin<P: AsRef<Path>>(path: P) -> Box<[u8]> {
    let mut file = File::open(path).unwrap();
    let mut file_buf = Vec::new();
    file.read_to_end(&mut file_buf).unwrap();
    file_buf.into_boxed_slice()
}

pub fn mult_64_64_unsigned(a: u64, b: u64) -> (u64, u64) {
    // Extract the high and low 32-bit parts, keeping everything as u64s.
    let ah = a >> 32;
    let al = a & 0xFFFF_FFFF;
    let bh = b >> 32;
    let bl = b & 0xFFFF_FFFF;
    // Multiply them together.
    let ll = al.wrapping_mul(bl);
    let hh = ah.wrapping_mul(bh);
    let m1 = al.wrapping_mul(bh);
    let m2 = ah.wrapping_mul(bl);
    // Add everything up.
    let mm = m1.wrapping_add(m2).wrapping_add(ll >> 32);
    let hi = hh.wrapping_add(mm >> 32);
    let lo = (ll & 0xFFFF_FFFF).wrapping_add(mm << 32);
    (lo, hi)
}

pub fn mult_64_64_signed(a: u64, b: u64) -> (u64, u64) {
    // Extract the high and low 32-bit parts, keeping everything as u64s.
    let ah = (a >> 32) as i32 as i64;
    let al = a as u32 as i64;
    let bh = (b >> 32) as i32 as i64;
    let bl = b as  u32 as i64;
    // Multiply them together.
    let ll = al.wrapping_mul(bl);
    let hh = ah.wrapping_mul(bh);
    let m1 = al.wrapping_mul(bh);
    let m2 = ah.wrapping_mul(bl);
    // Add everything up.
    let mm = m1.wrapping_add(m2).wrapping_add(ll >> 32);
    let hi = hh.wrapping_add(mm >> 32);
    let lo = (ll & 0xFFFF_FFFF).wrapping_add(mm << 32);
    (lo as u64, hi as u64)
}
