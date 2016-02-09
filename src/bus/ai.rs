use bus::mi;
use bus::mem_map::*;

#[derive(Default, Debug)]
pub struct Ai {
    reg_dram_addr: u32,
    reg_len:       u32,
    reg_control:   u32,
    reg_status:    u32,
    reg_dacrate:   u32,
    reg_bitrate:   u32,
}

impl Ai {
    pub fn read_reg(&mut self, addr: u32) -> Result<u32, &'static str> {
        Ok(match addr {
            AI_REG_DRAM_ADDR  => self.reg_dram_addr,
            AI_REG_LEN        => self.reg_len,
            AI_REG_STATUS     => self.reg_status,
            _ => return Err("Unsupported AI register")
        })
    }

    pub fn write_reg(&mut self, addr: u32, word: u32, mi: &mut mi::Mi) -> Result<(), &'static str> {
        Ok(match addr {
            AI_REG_DRAM_ADDR  => self.reg_dram_addr = word & 0xff_ffff,
            AI_REG_LEN        => self.reg_len = word & 0x3_ffff,
            AI_REG_CONTROL    => self.reg_control = word & 0x1,
            AI_REG_STATUS     => {
                mi.clear_interrupt(mi::Intr::AI);
            },
            AI_REG_DACRATE    => self.reg_dacrate = word & 0x3fff,
            AI_REG_BITRATE    => self.reg_bitrate = word & 0xf,
            _ => return Err("Unsupported AI register")
        })
    }
}
