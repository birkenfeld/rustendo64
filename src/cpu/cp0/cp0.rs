use super::reg_status;
use super::reg_cause;
use super::reg_config;

#[derive(Debug, Default)]
pub struct Cp0 {
    pub reg_count:   u32,
    pub reg_compare: u32,

    reg_index:     u32,
    reg_entry_lo0: u64,
    reg_entry_lo1: u64,
    reg_entry_hi:  u64,
    reg_page_mask: u32,

    pub reg_lladdr:    u32,

    pub reg_status:    reg_status::RegStatus,
    pub reg_config:    reg_config::RegConfig,
    pub reg_cause:     reg_cause::RegCause,
    pub reg_epc:       u64,
    pub reg_error_epc: u64,
    pub reg_bad_vaddr: u64,

    reg_tag_lo:   u32,
    reg_watch_lo: u32,
    reg_watch_hi: u32,
}

impl Cp0 {
    pub fn power_on_reset(&mut self) {
        self.reg_config.power_on_reset();
        self.reg_status.power_on_reset();
    }

    pub fn write_reg(&mut self, index: usize, data: u64) {
        match index {
            8  => { self.reg_bad_vaddr = data; }
            9  => { self.reg_count     = data as u32; }
            11 => { self.reg_compare   = data as u32; }
            12 => { self.reg_status    = (data as u32).into(); }
            13 => { self.reg_cause     = (data as u32).into(); }
            14 => { self.reg_epc       = data; }
            16 => { self.reg_config    = (data as u32).into(); }
            17 => { self.reg_lladdr    = data as u32; }
            18 => { self.reg_watch_lo  = data as u32; }
            19 => { self.reg_watch_hi  = data as u32; }
            // Cache tag registers
            28 => { self.reg_tag_lo    = data as u32; }
            29 => if data != 0 { panic!("wrote nonzero to TAG_HI reg"); },
            // TLB related registers
            0  => { self.reg_index     = data as u32; }
            2  => { self.reg_entry_lo0 = data; }
            3  => { self.reg_entry_lo1 = data; }
            10 => { self.reg_entry_hi  = data; }
            5  => { self.reg_page_mask = data as u32; }
            _  => panic!("Unrecognized Cp0 write reg: {}, {:#x}", index, data)
        }
    }

    pub fn read_reg(&self, index: usize) -> u64 {
        match index {
            8  => self.reg_bad_vaddr,
            9  => self.reg_count as u64,
            11 => self.reg_compare as u64,
            12 => self.reg_status.to_u32() as u64,
            13 => self.reg_cause.to_u32() as u64,
            14 => self.reg_epc,
            17 => self.reg_lladdr as u64,
            18 => self.reg_watch_lo as u64,
            19 => self.reg_watch_hi as u64,
            28 => self.reg_tag_lo as u64,
            29 => 0,  // reg_tag_hi is reserved
            30 => self.reg_error_epc,
            0  => self.reg_index as u64,
            2  => self.reg_entry_lo0,
            3  => self.reg_entry_lo1,
            10 => self.reg_entry_hi,
            5  => self.reg_page_mask as u64,
            _  => panic!("Unrecognized Cp0 read reg: {}", index)
        }
    }
}
