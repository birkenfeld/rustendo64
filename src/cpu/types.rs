use std::fmt;
use std::cmp::Ordering;
use byteorder::{BigEndian, ByteOrder};

use cpu::Cpu;
use bus::Bus;

pub trait MemFmt: Copy + fmt::LowerHex {
    fn get_align() -> u64;
    fn load_from(&mut Cpu, &mut Bus, u64) -> Self;
    fn store_to(&mut Cpu, &mut Bus, u64, Self);
}

impl MemFmt for u8 {
    fn get_align() -> u64 { 1 }
    fn load_from(cpu: &mut Cpu, bus: &mut Bus, addr: u64) -> u8 {
        let word = cpu.read_word(bus, addr & !3, false);
        let shift = 8 * (3 - (addr % 4));  // byte 0: shift 24
        (word >> shift) as u8
    }
    fn store_to(cpu: &mut Cpu, bus: &mut Bus, addr: u64, val: u8) {
        let mut word = cpu.read_word(bus, addr & !3, false);
        let shift = 8 * (3 - (addr % 4));
        let mask = !(0xFF << shift);
        word = (word & mask) | ((val as u32) << shift);
        cpu.write_word(bus, addr & !3, word);
    }
}

impl MemFmt for u16 {
    fn get_align() -> u64 { 2 }
    fn load_from(cpu: &mut Cpu, bus: &mut Bus, addr: u64) -> u16 {
        let word = cpu.read_word(bus, addr & !3, false);
        let shift = 8 * (2 - (addr % 4));  // halfword 0: shift 16
        (word >> shift) as u16
    }
    fn store_to(cpu: &mut Cpu, bus: &mut Bus, addr: u64, val: u16) {
        let mut word = cpu.read_word(bus, addr & !3, false);
        let shift = 8 * (2 - (addr % 4));
        let mask = !(0xFFFF << shift);
        word = (word & mask) | ((val as u32) << shift);
        cpu.write_word(bus, addr & !3, word);
    }
}

impl MemFmt for u32 {
    fn get_align() -> u64 { 4 }
    fn load_from(cpu: &mut Cpu, bus: &mut Bus, addr: u64) -> u32 {
        cpu.read_word(bus, addr, false)
    }
    fn store_to(cpu: &mut Cpu, bus: &mut Bus, addr: u64, val: u32) {
        cpu.write_word(bus, addr, val);
    }
}

impl MemFmt for u64 {
    fn get_align() -> u64 { 8 }
    fn load_from(cpu: &mut Cpu, bus: &mut Bus, addr: u64) -> u64 {
        cpu.read_dword(bus, addr)
    }
    fn store_to(cpu: &mut Cpu, bus: &mut Bus, addr: u64, val: u64) {
        cpu.write_dword(bus, addr, val);
    }
}


#[derive(PartialEq, Eq)]
pub enum FpOrd {
    Eq,
    Gt,
    Lt,
    No
}

impl FpOrd {
    pub fn from<T: PartialOrd>(a: T, b: T) -> Self {
        match a.partial_cmp(&b) {
            None                    => FpOrd::No,
            Some(Ordering::Equal)   => FpOrd::Eq,
            Some(Ordering::Greater) => FpOrd::Gt,
            Some(Ordering::Less)    => FpOrd::Lt,
        }
    }
}

// TODO: verify this is the correct offset for half-width
const LO: usize = 4;

pub trait FpFmt: Copy + fmt::Display + PartialOrd {
    fn read_fpr(&[u8; 8]) -> Self;
    fn write_fpr(&mut [u8; 8], value: Self);
    fn is_nan(&self) -> bool { false }
}

impl FpFmt for f32 {
    fn read_fpr(reg: &[u8; 8]) -> f32 {
        BigEndian::read_f32(&reg[LO..])
    }
    fn write_fpr(reg: &mut [u8; 8], value: f32) {
        BigEndian::write_f32(&mut reg[LO..], value);
    }
    fn is_nan(&self) -> bool { f32::is_nan(*self) }
}

impl FpFmt for f64 {
    fn read_fpr(reg: &[u8; 8]) -> f64 {
        BigEndian::read_f64(reg)
    }
    fn write_fpr(reg: &mut [u8; 8], value: f64) {
        BigEndian::write_f64(reg, value);
    }
    fn is_nan(&self) -> bool { f64::is_nan(*self) }
}

impl FpFmt for i32 {
    fn read_fpr(reg: &[u8; 8]) -> i32 {
        BigEndian::read_i32(&reg[LO..])
    }
    fn write_fpr(reg: &mut [u8; 8], value: i32) {
        BigEndian::write_i32(&mut reg[LO..], value);
    }
}

impl FpFmt for i64 {
    fn read_fpr(reg: &[u8; 8]) -> i64 {
        BigEndian::read_i64(reg)
    }
    fn write_fpr(reg: &mut [u8; 8], value: i64) {
        BigEndian::write_i64(reg, value);
    }
}

// For memory loads/stores.
impl FpFmt for u32 {
    fn read_fpr(reg: &[u8; 8]) -> u32 {
        BigEndian::read_u32(&reg[LO..])
    }
    fn write_fpr(reg: &mut [u8; 8], value: u32) {
        BigEndian::write_u32(&mut reg[LO..], value);
    }
}

impl FpFmt for u64 {
    fn read_fpr(reg: &[u8; 8]) -> u64 {
        BigEndian::read_u64(reg)
    }
    fn write_fpr(reg: &mut [u8; 8], value: u64) {
        BigEndian::write_u64(reg, value);
    }
}

pub trait FpRoundExt {
    fn round_to_even(&self) -> Self;
}

macro_rules! impl_round {
    ($ty:ty) => {
        impl FpRoundExt for $ty {
            /// Very naive implementation of round-to-even.
            fn round_to_even(&self) -> Self {
                let i = self.floor();
                let r = self - i;
                match r.partial_cmp(&0.5) {
                    Some(Ordering::Less)                    => i,
                    Some(Ordering::Greater)                 => i + 1.0,
                    Some(Ordering::Equal) if i % 2.0 == 0.0 => i,
                    Some(Ordering::Equal)                   => i + 1.0,
                    None                                    => *self,
                }
            }
        }
    }
}

impl_round!(f32);
impl_round!(f64);
