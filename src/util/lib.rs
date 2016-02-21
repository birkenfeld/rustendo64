//! Miscellaneous utilities for the emulator.

use std::fs::File;
use std::io::Read;
use std::path::Path;
// use std::{thread, time, process};

/// A guard that will exit the process when dropped.
///
/// Useful for infinite loops that will only end when the thread
/// is panicking.
// pub struct ExitGuard;

// impl Drop for ExitGuard {
//     fn drop(&mut self) {
//         // give other threads time to panic
//         thread::sleep(time::Duration::from_millis(100));
//         println!("Exiting.");
//         process::exit(2);
//     }
// }

pub fn read_bin<P: AsRef<Path>>(path: P) -> Box<[u8]> {
    let mut file = File::open(path).unwrap();
    let mut file_buf = Vec::new();
    file.read_to_end(&mut file_buf).unwrap();
    file_buf.into_boxed_slice()
}

pub fn get_rom_name(rom: &[u8], filename: &str) -> String {
    let name = String::from_utf8_lossy(&rom[0x20..0x34]).to_owned();
    let name = name.trim_matches(|c| c == '\0' || c == ' ').to_owned();
    if name.is_empty() { filename.to_owned() } else { name }
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

#[inline]
pub fn bit_set(value: u32, bit: u32) -> bool {
    value & (1 << bit) != 0
}

// #[inline]
// pub fn set_bit(target: &mut u32, bit: u32) {
//     *target |= 1 << bit;
// }

// #[inline]
// pub fn clear_bit(target: &mut u32, bit: u32) {
//     *target &= !(1 << bit);
// }

#[inline]
pub fn clear_or_set_bit(target: &mut u32, bit: u32,
                        value: u32, clear_bit: u32, set_bit: u32) {
    if value & (1 << clear_bit) != 0 {
        *target &= !(1 << bit);
    } else if value & (1 << set_bit) != 0 {
        *target |= 1 << bit;
    }
}
