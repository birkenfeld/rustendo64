use std::fmt;

pub trait R4300<'c> {
    type Bus;
    fn read_word(&self, &Self::Bus, u64, bool) -> u32;
    fn write_word(&mut self, &mut Self::Bus, u64, u32);

    fn read_dword(&self, bus: &Self::Bus, virt_addr: u64) -> u64 {
        (self.read_word(bus, virt_addr, false) as u64) << 32 |
        self.read_word(bus, virt_addr + 4, false) as u64
    }

    fn write_dword(&mut self, bus: &mut Self::Bus, virt_addr: u64, dword: u64) {
        self.write_word(bus, virt_addr, (dword >> 32) as u32);
        self.write_word(bus, virt_addr + 4, dword as u32);
    }
}

pub trait MemFmt<'c, C: R4300<'c>>: Copy + fmt::LowerHex {
    fn get_align() -> u64;
    fn load_from(&mut C, &C::Bus, u64) -> Self;
    fn store_to(&mut C, &mut C::Bus, u64, Self);
}

impl<'c, C: R4300<'c>> MemFmt<'c, C> for u8 {
    fn get_align() -> u64 { 1 }
    fn load_from(cpu: &mut C, bus: &C::Bus, addr: u64) -> u8 {
        let word = cpu.read_word(bus, addr & !3, false);
        let shift = 8 * (3 - (addr % 4));  // byte 0: shift 24
        (word >> shift) as u8
    }
    fn store_to(cpu: &mut C, bus: &mut C::Bus, addr: u64, val: u8) {
        let mut word = cpu.read_word(bus, addr & !3, false);
        let shift = 8 * (3 - (addr % 4));
        let mask = !(0xFF << shift);
        word = (word & mask) | ((val as u32) << shift);
        cpu.write_word(bus, addr & !3, word);
    }
}

impl<'c, C: R4300<'c>> MemFmt<'c, C> for u16 {
    fn get_align() -> u64 { 2 }
    fn load_from(cpu: &mut C, bus: &C::Bus, addr: u64) -> u16 {
        let word = cpu.read_word(bus, addr & !3, false);
        let shift = 8 * (2 - (addr % 4));  // halfword 0: shift 16
        (word >> shift) as u16
    }
    fn store_to(cpu: &mut C, bus: &mut C::Bus, addr: u64, val: u16) {
        let mut word = cpu.read_word(bus, addr & !3, false);
        let shift = 8 * (2 - (addr % 4));
        let mask = !(0xFFFF << shift);
        word = (word & mask) | ((val as u32) << shift);
        cpu.write_word(bus, addr & !3, word);
    }
}

impl<'c, C: R4300<'c>> MemFmt<'c, C> for u32 {
    fn get_align() -> u64 { 4 }
    fn load_from(cpu: &mut C, bus: &C::Bus, addr: u64) -> u32 {
        cpu.read_word(bus, addr, false)
    }
    fn store_to(cpu: &mut C, bus: &mut C::Bus, addr: u64, val: u32) {
        cpu.write_word(bus, addr, val);
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
}
