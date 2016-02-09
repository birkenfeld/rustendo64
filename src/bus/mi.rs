use bus::mem_map::*;
use util::{bit_set, set_bit, clear_bit, clear_or_set_bit};

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
    reg_mode:          u32,
    reg_version:       u32,
    reg_intr:          u32,
    reg_intr_mask:     u32,
    pub has_interrupt: bool,
}

impl Mi {
    pub fn read_reg(&self, addr: u32) -> Result<u32, &'static str> {
        Ok(match addr {
            MI_REG_MODE       => self.reg_mode,
            MI_REG_VERSION    => self.reg_version,
            MI_REG_INTR       => self.reg_intr,
            MI_REG_INTR_MASK  => self.reg_intr_mask,
            _ => return Err("Unsupported MI register")
        })
    }

    pub fn write_reg(&mut self, addr: u32, word: u32) -> Result<(), &'static str> {
        Ok(match addr {
            MI_REG_MODE       => {
                self.reg_mode = (self.reg_mode & !0x7f) | (word & 0x7f);
                clear_or_set_bit(&mut self.reg_mode, 7, word, 7, 8);
                clear_or_set_bit(&mut self.reg_mode, 8, word, 9, 10);
                clear_or_set_bit(&mut self.reg_mode, 9, word, 12, 13);
                if bit_set(word, 11) {
                    self.clear_interrupt(Intr::DP);
                }
            },
            MI_REG_INTR_MASK  => {
                {
                    let reg = &mut self.reg_intr_mask;
                    clear_or_set_bit(reg, Intr::SP as u32, word, 0, 1);
                    clear_or_set_bit(reg, Intr::SI as u32, word, 2, 3);
                    clear_or_set_bit(reg, Intr::AI as u32, word, 4, 5);
                    clear_or_set_bit(reg, Intr::VI as u32, word, 6, 7);
                    clear_or_set_bit(reg, Intr::PI as u32, word, 8, 9);
                    clear_or_set_bit(reg, Intr::DP as u32, word, 10, 11);
                }
                self.check_interrupts();
            },
            _ => return Err("Unsupported MI register")
        })
    }

    fn check_interrupts(&mut self) {
        self.has_interrupt = self.reg_intr & self.reg_intr_mask != 0;
    }

    pub fn set_interrupt(&mut self, intr: Intr) {
        set_bit(&mut self.reg_intr, intr as u32);
        self.check_interrupts();
    }

    pub fn clear_interrupt(&mut self, intr: Intr) {
        clear_bit(&mut self.reg_intr, intr as u32);
        self.check_interrupts();
    }
}
