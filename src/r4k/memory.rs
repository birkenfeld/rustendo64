use std::fmt;

use R4300;

/// Abstracts the different types we can load and store.
pub trait MemFmt<'c, C: R4300<'c>>: Copy + fmt::LowerHex
{
    fn get_align() -> u64;
    fn load_from(&mut C, &C::Bus, u64) -> Self;
    fn store_to(&mut C, &mut C::Bus, u64, Self);
    fn load_unaligned_from(&mut C, &C::Bus, u64) -> Self;
    fn store_unaligned_to(&mut C, &mut C::Bus, u64, Self);
}

impl<'c, C: R4300<'c>> MemFmt<'c, C> for u8 {
    fn get_align() -> u64 { 1 }
    fn load_from(cpu: &mut C, bus: &C::Bus, addr: u64) -> u8 {
        let word = cpu.read_word(bus, addr & !0b11);
        let shift = 8 * (3 - (addr & 0b11));  // byte 0: shift 24
        (word >> shift) as u8
    }
    fn store_to(cpu: &mut C, bus: &mut C::Bus, addr: u64, val: u8) {
        let mut word = cpu.read_word(bus, addr & !0b11);
        let shift = 8 * (3 - (addr & 0b11));
        let mask = !(0xff << shift);
        word = (word & mask) | ((val as u32) << shift);
        cpu.write_word(bus, addr & !0b11, word);
    }
    fn load_unaligned_from(cpu: &mut C, bus: &C::Bus, addr: u64) -> u8 {
        Self::load_from(cpu, bus, addr)
    }
    fn store_unaligned_to(cpu: &mut C, bus: &mut C::Bus, addr: u64, val: u8) {
        Self::store_to(cpu, bus, addr, val)
    }
}

impl<'c, C: R4300<'c>> MemFmt<'c, C> for u16 {
    fn get_align() -> u64 { 2 }
    fn load_from(cpu: &mut C, bus: &C::Bus, addr: u64) -> u16 {
        let word = cpu.read_word(bus, addr & !0b11);
        let shift = 8 * (2 - (addr & 0b11));  // halfword 0: shift 16
        (word >> shift) as u16
    }
    fn store_to(cpu: &mut C, bus: &mut C::Bus, addr: u64, val: u16) {
        let mut word = cpu.read_word(bus, addr & !0b11);
        let shift = 8 * (2 - (addr & 0b11));
        let mask = !(0xffff << shift);
        word = (word & mask) | ((val as u32) << shift);
        cpu.write_word(bus, addr & !0b11, word);
    }
    fn load_unaligned_from(cpu: &mut C, bus: &C::Bus, addr: u64) -> u16 {
        let aligned_addr = addr &!0b11;
        match addr & 0b11 {
            0 => (cpu.read_word(bus, aligned_addr) >> 16) as u16,
            1 => (cpu.read_word(bus, aligned_addr) >> 8) as u16,
            2 => cpu.read_word(bus, aligned_addr) as u16,
            _ => {
                let w1 = cpu.read_word(bus, aligned_addr);
                let w2 = cpu.read_word(bus, aligned_addr + 4);
                (w1 << 8) as u16 | (w2 >> 24) as u16
            }
        }
    }
    fn store_unaligned_to(cpu: &mut C, bus: &mut C::Bus, addr: u64, val: u16) {
        match addr & 0b11 {
            3 => {
                let aligned_addr = addr &!0b11;
                let w1 = cpu.read_word(bus, aligned_addr);
                let w2 = cpu.read_word(bus, aligned_addr + 4);
                cpu.write_word(bus, aligned_addr, (w1 & 0xffffff00) | ((val as u32) >> 8));
                cpu.write_word(bus, aligned_addr + 4, (w2 & 0x00ffffff) | ((val as u32) << 24));
            }
            _ => Self::store_to(cpu, bus, addr, val),
        }
    }
}

impl<'c, C: R4300<'c>> MemFmt<'c, C> for u32 {
    fn get_align() -> u64 { 4 }
    fn load_from(cpu: &mut C, bus: &C::Bus, addr: u64) -> u32 {
        cpu.read_word(bus, addr)
    }
    fn store_to(cpu: &mut C, bus: &mut C::Bus, addr: u64, val: u32) {
        cpu.write_word(bus, addr, val);
    }
    fn load_unaligned_from(cpu: &mut C, bus: &C::Bus, addr: u64) -> u32 {
        let offset = addr & 0b11;
        if offset == 0 {
            cpu.read_word(bus, addr)
        } else {
            let aligned_addr = addr &!0b11;
            let w1 = cpu.read_word(bus, aligned_addr);
            let w2 = cpu.read_word(bus, aligned_addr + 4);
            let shift = offset * 8;
            (w1 << shift) | (w2 >> (32 - shift))
        }
    }
    fn store_unaligned_to(cpu: &mut C, bus: &mut C::Bus, addr: u64, val: u32) {
        let offset = addr & 0b11;
        if offset == 0 {
            cpu.write_word(bus, addr, val)
        } else {
            let aligned_addr = addr &!0b11;
            let shift1 = offset * 8;
            let shift2 = 32 - shift1;
            let mut w1 = cpu.read_word(bus, aligned_addr);
            let mut w2 = cpu.read_word(bus, aligned_addr + 4);
            w1 = (w1 & (0xffffffff << shift2)) | (val >> shift1);
            w2 = (w2 & (0xffffffff >> shift1)) | (val << shift2);
            cpu.write_word(bus, aligned_addr, w1);
            cpu.write_word(bus, aligned_addr + 4, w2);
        }
    }
}

impl<'c, C: R4300<'c>> MemFmt<'c, C> for u64 {
    fn get_align() -> u64 { 8 }
    fn load_from(cpu: &mut C, bus: &C::Bus, addr: u64) -> u64 {
        cpu.read_dword(bus, addr)
    }
    fn store_to(cpu: &mut C, bus: &mut C::Bus, addr: u64, val: u64) {
        cpu.write_dword(bus, addr, val);
    }
    fn load_unaligned_from(cpu: &mut C, bus: &C::Bus, addr: u64) -> u64 {
        let offset = addr & 0b11;  // 4-byte offset is meant here!
        if offset == 0 {
            cpu.read_dword(bus, addr)
        } else {
            let aligned_addr = addr &!0b11;
            let w1 = cpu.read_word(bus, aligned_addr) as u64;
            let w2 = cpu.read_word(bus, aligned_addr + 4) as u64;
            let w3 = cpu.read_word(bus, aligned_addr + 8) as u64;
            let shift = offset * 8;
            (w1 << (32 + shift)) | (w2 << shift) | (w3 >> (32 - shift))
        }
    }
    fn store_unaligned_to(cpu: &mut C, bus: &mut C::Bus, addr: u64, val: u64) {
        let offset = addr & 0b11;
        if offset == 0 {
            cpu.write_dword(bus, addr, val)
        } else {
            let aligned_addr = addr &!0b11;
            let shift1 = offset * 8;
            let shift2 = 32 - shift1;
            let mut w1 = cpu.read_word(bus, aligned_addr);
            // no need to read w2, it will be completely overwritten
            let mut w3 = cpu.read_word(bus, aligned_addr + 8);
            w1 = (w1 & (0xffffffff << shift2)) | (val >> (32 + shift1)) as u32;
            w3 = (w3 & (0xffffffff >> shift1)) | (val << shift2) as u32;
            cpu.write_word(bus, aligned_addr, w1);
            cpu.write_word(bus, aligned_addr + 4, (val >> shift1) as u32);
            cpu.write_word(bus, aligned_addr + 8, w3);
        }
    }
}
