use std::sync::atomic::{AtomicUsize, AtomicBool, Ordering};

use bus::IoResult;
use mem_map::*;
use util::{bit_set, clear_or_set_bit};

// Discriminants are the bits set in reg_intr and reg_intr_mask.
#[repr(u32)]
pub enum Intr {
    SP = 0,
    SI = 1,
    AI = 2,
    VI = 3,
    PI = 4,
    DP = 5,
}

#[derive(Default, Debug)]
pub struct Mi {
    reg_mode:          AtomicUsize,
    reg_intr:          AtomicUsize,
    reg_intr_mask:     AtomicUsize,
    pub has_interrupt: AtomicBool,
}

pub const MI_VERSION: usize = 0x01010101;

pub const ORD: Ordering = Ordering::SeqCst;

impl Mi {
    pub fn read_reg(&self, addr: u32) -> IoResult<u32> {
        Ok(match addr {
            MI_REG_MODE       => self.reg_mode.load(ORD),
            MI_REG_VERSION    => MI_VERSION,
            MI_REG_INTR       => self.reg_intr.load(ORD),
            MI_REG_INTR_MASK  => self.reg_intr_mask.load(ORD),
            _ => return Err("Unsupported MI register")
        } as u32)
    }

    pub fn write_reg(&self, addr: u32, word: u32) -> IoResult<()> {
        Ok(match addr {
            MI_REG_MODE       => {
                let mut reg = self.reg_mode.load(ORD) as u32;
                reg = (reg & !0x7f) | (word & 0x7f);
                clear_or_set_bit(&mut reg, 7, word, 7, 8);
                clear_or_set_bit(&mut reg, 8, word, 9, 10);
                clear_or_set_bit(&mut reg, 9, word, 12, 13);
                self.reg_mode.store(reg as usize, ORD);
                if bit_set(word, 11) {
                    self.clear_interrupt(Intr::DP);
                }
            },
            MI_REG_INTR_MASK  => {
                let mut reg = self.reg_intr_mask.load(ORD) as u32;
                clear_or_set_bit(&mut reg, Intr::SP as u32, word, 0, 1);
                clear_or_set_bit(&mut reg, Intr::SI as u32, word, 2, 3);
                clear_or_set_bit(&mut reg, Intr::AI as u32, word, 4, 5);
                clear_or_set_bit(&mut reg, Intr::VI as u32, word, 6, 7);
                clear_or_set_bit(&mut reg, Intr::PI as u32, word, 8, 9);
                clear_or_set_bit(&mut reg, Intr::DP as u32, word, 10, 11);
                self.reg_intr_mask.store(reg as usize, ORD);
                self.check_interrupts();
            },
            _ => return Err("Unsupported MI register")
        })
    }

    fn check_interrupts(&self) {
        self.has_interrupt.store(self.reg_intr.load(ORD) &
                                 self.reg_intr_mask.load(ORD) != 0, ORD);
    }

    pub fn set_interrupt(&self, intr: Intr) {
        self.reg_intr.fetch_or(1 << intr as usize, ORD);
        self.check_interrupts();
    }

    pub fn clear_interrupt(&self, intr: Intr) {
        self.reg_intr.fetch_and(!(1 << intr as usize), ORD);
        self.check_interrupts();
    }
}
