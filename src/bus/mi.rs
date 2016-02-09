use bus::mem_map::*;

// TODO: enum?
pub const INTR_SP: u32 = 0x01;
pub const INTR_SI: u32 = 0x02;
pub const INTR_AI: u32 = 0x04;
pub const INTR_VI: u32 = 0x08;
pub const INTR_PI: u32 = 0x10;
pub const INTR_DP: u32 = 0x20;

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
                if word & 0x800 != 0 {
                    self.clear_interrupt(INTR_DP);
                }
                /* TODO */
            },
            MI_REG_INTR_MASK  => {
                if word & 0x1 != 0 {
                    self.reg_intr_mask &= !INTR_SP;
                } else if word & 0x2 != 0 {
                    self.reg_intr_mask |= INTR_SP;
                }
                if word & 0x4 != 0 {
                    self.reg_intr_mask &= !INTR_SI;
                } else if word & 0x8 != 0 {
                    self.reg_intr_mask |= INTR_SI;
                }
                if word & 0x10 != 0 {
                    self.reg_intr_mask &= !INTR_AI;
                } else if word & 0x20 != 0 {
                    self.reg_intr_mask |= INTR_AI;
                }
                if word & 0x40 != 0 {
                    self.reg_intr_mask &= !INTR_VI;
                } else if word & 0x80 != 0 {
                    self.reg_intr_mask |= INTR_VI;
                }
                if word & 0x100 != 0 {
                    self.reg_intr_mask &= !INTR_PI;
                } else if word & 0x200 != 0 {
                    self.reg_intr_mask |= INTR_PI;
                }
                if word & 0x400 != 0 {
                    self.reg_intr_mask &= !INTR_DP;
                } else if word & 0x800 != 0 {
                    self.reg_intr_mask |= INTR_DP;
                }
                self.check_interrupts();
            },
            _ => return Err("Unsupported MI register")
        })
    }

    fn check_interrupts(&mut self) {
        self.has_interrupt = self.reg_intr & self.reg_intr_mask != 0;
    }

    pub fn set_interrupt(&mut self, intr: u32) {
        self.reg_intr |= intr;
        self.check_interrupts();
    }

    pub fn clear_interrupt(&mut self, intr: u32) {
        self.reg_intr &= !intr;
        self.check_interrupts();
    }
}
