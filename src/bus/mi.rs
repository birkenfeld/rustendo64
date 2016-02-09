#[derive(Default, Debug)]
pub struct Mi {
    pub reg_mode: u32,
    pub reg_version: u32,
    pub reg_intr: u32,
    pub reg_intr_mask: u32,
}

pub const INTR_SP: u32 = 0x01;
pub const INTR_SI: u32 = 0x02;
pub const INTR_AI: u32 = 0x04;
pub const INTR_VI: u32 = 0x08;
pub const INTR_PI: u32 = 0x10;
pub const INTR_DP: u32 = 0x20;
