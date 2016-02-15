use bus::IoResult;
use bus::mem_map::*;

#[derive(Default, Debug)]
pub struct Ri {
    // RDRAM interface regs
    reg_mode:            u32,
    reg_config:          u32,
    reg_select:          u32,
    reg_refresh:         u32,
    reg_latency:         u32,
    reg_rerror:          u32,
    // RDRAM regs
    reg_rd_config:       u32,
    reg_rd_device_id:    u32,
    reg_rd_delay:        u32,
    reg_rd_mode:         u32,
    reg_rd_ref_interval: u32,
    reg_rd_ref_row:      u32,
    reg_rd_ras_interval: u32,
    reg_rd_min_interval: u32,
    reg_rd_addr_select:  u32,
    reg_rd_device_manuf: u32,
}

impl Ri {
    pub fn power_on_reset(&mut self) {
        self.reg_mode = 0xE;
        self.reg_config = 0x40;
        self.reg_select = 0x14;
        self.reg_refresh = 0x63634;
    }

    pub fn read_reg(&self, addr: u32) -> IoResult<u32> {
        Ok(match addr {
            RI_REG_MODE            => self.reg_mode,
            RI_REG_CONFIG          => self.reg_config,
            RI_REG_SELECT          => self.reg_select,
            RI_REG_REFRESH         => self.reg_refresh,
            RI_REG_LATENCY         => self.reg_latency,
            RI_REG_RERROR          => self.reg_rerror,
            RDRAM_REG_CONFIG       => self.reg_rd_config,
            RDRAM_REG_DEVICE_ID    => self.reg_rd_device_id,
            RDRAM_REG_DELAY        => self.reg_rd_delay,
            RDRAM_REG_MODE         => self.reg_rd_mode,
            RDRAM_REG_REF_INTERVAL => self.reg_rd_ref_interval,
            RDRAM_REG_REF_ROW      => self.reg_rd_ref_row,
            RDRAM_REG_RAS_INTERVAL => self.reg_rd_ras_interval,
            RDRAM_REG_MIN_INTERVAL => self.reg_rd_min_interval,
            RDRAM_REG_ADDR_SELECT  => self.reg_rd_addr_select,
            RDRAM_REG_DEVICE_MANUF => self.reg_rd_device_manuf,
            _ => return Err("Unsupported RI register"),
        })
    }

    pub fn write_reg(&mut self, addr: u32, word: u32) -> IoResult<()> {
        Ok(match addr {
            RI_REG_MODE            => self.reg_mode = word & 0xf,
            RI_REG_CONFIG          => self.reg_config = word & 0x7f,
            RI_REG_CURRENT_LOAD    => { /* TODO */ },
            RI_REG_SELECT          => self.reg_select = word & 0x3f,
            RI_REG_REFRESH         => self.reg_refresh = word & 0x7_ffff,
            RI_REG_LATENCY         => self.reg_latency = word & 0xf,
            RI_REG_WERROR          => self.reg_rerror = 0,
            RDRAM_REG_CONFIG       => self.reg_rd_config = word,
            RDRAM_REG_DEVICE_ID    => self.reg_rd_device_id = word,
            RDRAM_REG_DELAY        => self.reg_rd_delay = word,
            RDRAM_REG_MODE         => self.reg_rd_mode = word,
            RDRAM_REG_REF_INTERVAL => self.reg_rd_ref_interval = word,
            RDRAM_REG_REF_ROW      => self.reg_rd_ref_row = word,
            RDRAM_REG_RAS_INTERVAL => self.reg_rd_ras_interval = word,
            RDRAM_REG_MIN_INTERVAL => self.reg_rd_min_interval = word,
            RDRAM_REG_ADDR_SELECT  => self.reg_rd_addr_select = word,
            _ => return Err("Unsupported RI register"),
        })
    }
}
