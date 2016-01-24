use std::fmt;

const PIF_ROM_SIZE: usize = 2048;

const RAM_SIZE: usize = 4 * 1024 * 1024;

#[derive(Default)]
struct Sp {
    dmem: Vec<u32>,
    imem: Vec<u32>,
    reg_mem_addr: u32,
    reg_dram_addr: u32,
    reg_rd_len: u32,
    reg_wr_len: u32,
    reg_status: u32,
    reg_dma_full: u32,
    reg_dma_busy: u32,
    reg_semaphore: u32,
    reg_pc: u32,
    reg_ibist: u32,
}

#[derive(Default)]
struct Dp {
    // command regs
    reg_start: u32,
    reg_end: u32,
    reg_current: u32,
    reg_status: u32,
    reg_clock: u32,
    reg_bufbusy: u32,
    reg_pipebusy: u32,
    reg_tmem: u32,

    // span regs
    reg_tbist: u32,
    reg_test_mode: u32,
    reg_buftest_addr: u32,
    reg_buftest_data: u32,
}

#[derive(Default)]
struct Mi {
    reg_mode: u32,
    reg_version: u32,
    reg_intr: u32,
    reg_intr_mask: u32,
}

#[derive(Default)]
struct Vi {
    reg_status: u32,
    reg_origin: u32,
    reg_width: u32,
    reg_intr: u32,
    reg_current: u32,
    reg_burst: u32,
    reg_v_sync: u32,
    reg_h_sync: u32,
    reg_leap: u32,
    reg_h_start: u32,
    reg_v_start: u32,
    reg_v_burst: u32,
    reg_x_scale: u32,
    reg_y_scale: u32,
}

#[derive(Default)]
struct Ai {
    reg_dram_addr: u32,
    reg_len: u32,
    reg_control: u32,
    reg_status: u32,
    reg_dacrate: u32,
    reg_bitrate: u32,
}

#[derive(Default)]
struct Pi {
    reg_dram_addr: u32,
    reg_cart_addr: u32,
    reg_rd_len: u32,
    reg_wr_len: u32,
    reg_status: u32,
    reg_bsd_dom1_lat: u32,
    reg_bsd_dom1_pwd: u32,
    reg_bsd_dom1_pgs: u32,
    reg_bsd_dom1_rls: u32,
    reg_bsd_dom2_lat: u32,
    reg_bsd_dom2_pwd: u32,
    reg_bsd_dom2_pgs: u32,
    reg_bsd_dom2_rls: u32,
}

#[derive(Default)]
struct Ri {
    reg_mode: u32,
    reg_config: u32,
    reg_current_load: u32,
    reg_select: u32,
    reg_refresh: u32,
    reg_latency: u32,
    reg_rerror: u32,
    reg_werror: u32,
}

#[derive(Default)]
struct Si {
    reg_dram_addr: u32,
    reg_pif_addr_rd64b: u32,
    reg_pif_addr_wr64b: u32,
    reg_status: u32,
}

pub struct Interconnect {
    pif_rom: Vec<u8>,
    pif_ram: Vec<u32>,
    cart_rom: Vec<u8>,
    ram: Vec<u16>,
    sp: Sp,
    dp: Dp,
    mi: Mi,
    vi: Vi,
    ai: Ai,
    pi: Pi,
    ri: Ri,
    si: Si,
}

impl Interconnect {
    pub fn new(pif_rom: Vec<u8>, cart_rom: Vec<u8>) -> Interconnect {
        Interconnect {
            pif_rom: pif_rom,
            pif_ram: vec![0; 16],
            cart_rom: cart_rom,
            ram: vec![0; RAM_SIZE],
            sp: Sp { dmem: vec![0; 1024], imem: vec![0; 1024], ..Sp::default() },
            dp: Dp::default(),
            mi: Mi::default(),
            vi: Vi::default(),
            ai: Ai::default(),
            pi: Pi::default(),
            ri: Ri::default(),
            si: Si::default(),
        }
    }

    pub fn power_on_reset(&mut self) {
        self.sp.reg_status |= 0x1;
    }

    pub fn read_word(&self, addr: u32) -> u32 {
        match addr {
            // TODO: Replace constants with useful names
            0x0000_0000 ... 0x03ef_ffff => {
                // RAM
                let addr = addr as usize;
                ((self.ram[addr] as u32) << 24) |
                ((self.ram[addr + 1] as u32) << 16) |
                ((self.ram[addr + 2] as u32) << 8) |
                 (self.ram[addr + 3] as u32)
            }
            0x1000_0000 ... 0x1fbf_ffff => {
                // Cartridge ROM
                let rel_addr = addr as usize - 0x1000_0000;
                ((self.cart_rom[rel_addr + 3] as u32) << 24) |
                ((self.cart_rom[rel_addr + 2] as u32) << 16) |
                ((self.cart_rom[rel_addr + 1] as u32) << 8) |
                 (self.cart_rom[rel_addr + 0] as u32)
            }
            0x1fc0_0000 ... 0x1fc0_07bf => {
                // PIF ROM
                let rel_addr = addr as usize - 0x1fc0_0000;
                // TODO: Check out byteorder crate
                ((self.pif_rom[rel_addr] as u32) << 24) |
                ((self.pif_rom[rel_addr + 1] as u32) << 16) |
                ((self.pif_rom[rel_addr + 2] as u32) << 8) |
                 (self.pif_rom[rel_addr + 3] as u32)
            }
            0x1fc0_07c0 ... 0x1fc0_07ff => {
                // PIF RAM
                let rel_addr = addr as usize - 0x1fc0_07c0;
                self.pif_ram[rel_addr / 4]
            }
            0x0400_0000 ... 0x040f_ffff => {
                // SP area
                match addr {
                    0x0400_0000 ... 0x0400_0fff => {
                        self.sp.dmem[(addr as usize - 0x0400_0000) / 4]
                    }
                    0x0400_1000 ... 0x0400_1fff => {
                        self.sp.imem[(addr as usize - 0x0400_1000) / 4]
                    }
                    0x0404_0000 => self.sp.reg_mem_addr,
                    0x0404_0004 => self.sp.reg_dram_addr,
                    0x0404_0008 => self.sp.reg_rd_len,
                    0x0404_000c => self.sp.reg_wr_len,
                    0x0404_0010 => self.sp.reg_status,
                    0x0404_0014 => self.sp.reg_dma_full,
                    0x0404_0018 => self.sp.reg_dma_busy,
                    0x0404_001c => self.sp.reg_semaphore,
                    0x0408_0000 => self.sp.reg_pc,
                    0x0408_0004 => self.sp.reg_ibist,
                    _ => {
                        panic!("Unsupported SP read address: {:#x}", addr);
                    }
                }
            }
            0x0410_0000 ... 0x042f_ffff => {
                // DP area
                match addr {
                    0x0410_0000 => self.dp.reg_start,
                    0x0410_0004 => self.dp.reg_end,
                    0x0410_0008 => self.dp.reg_current,
                    0x0410_000c => self.dp.reg_status,
                    0x0410_0010 => self.dp.reg_clock,
                    0x0410_0014 => self.dp.reg_bufbusy,
                    0x0410_0018 => self.dp.reg_pipebusy,
                    0x0410_001c => self.dp.reg_tmem,
                    0x0420_0000 => self.dp.reg_tbist,
                    0x0420_0004 => self.dp.reg_test_mode,
                    0x0420_0008 => self.dp.reg_buftest_addr,
                    0x0420_000c => self.dp.reg_buftest_data,
                    _ => {
                        panic!("Unsupported DP read address: {:#x}", addr);
                    }
                }
            }
            0x0440_0000 ... 0x044f_ffff => {
                // VI area
                match addr {
                    0x0440_000c => self.vi.reg_intr,
                    0x0440_0010 => self.vi.reg_current,
                    0x0440_0024 => self.vi.reg_h_start,
                    _ => {
                        panic!("Unsupported VI read address: {:#x}", addr);
                    }
                }
            }
            0x0450_0000 ... 0x045f_ffff => {
                // AI area
                match addr {
                    0x0450_0000 => self.ai.reg_dram_addr,
                    0x0450_0004 => self.ai.reg_len,
                    _ => {
                        panic!("Unsupported AI read address: {:#x}", addr);
                    }
                }
            }
            0x0460_0000 ... 0x046f_ffff => {
                // PI area
                match addr {
                    0x0460_0000 => self.pi.reg_dram_addr,
                    0x0460_0004 => self.pi.reg_cart_addr,
                    0x0460_0008 => self.pi.reg_rd_len,
                    0x0460_000c => self.pi.reg_wr_len,
                    0x0460_0010 => self.pi.reg_status,
                    0x0460_0014 => self.pi.reg_bsd_dom1_lat,
                    0x0460_0018 => self.pi.reg_bsd_dom1_pwd,
                    0x0460_001c => self.pi.reg_bsd_dom1_pgs,
                    0x0460_0020 => self.pi.reg_bsd_dom1_rls,
                    0x0460_0024 => self.pi.reg_bsd_dom2_lat,
                    0x0460_0028 => self.pi.reg_bsd_dom2_pwd,
                    0x0460_002c => self.pi.reg_bsd_dom2_pgs,
                    0x0460_0030 => self.pi.reg_bsd_dom2_rls,
                    _ => {
                        panic!("Unsupported PI read address: {:#x}", addr);
                    }
                }
            }
            0x0480_0000 ... 0x048f_ffff => {
                // SI area
                match addr {
                    0x0480_0000 => self.si.reg_dram_addr,
                    0x0480_0004 => self.si.reg_pif_addr_rd64b,
                    0x0480_0010 => self.si.reg_pif_addr_wr64b,
                    0x0480_0018 => self.si.reg_status,
                    _ => {
                        panic!("Unsupported SI read address: {:#x}", addr);
                    }
                }
            }
            _ => {
                // TODO
                panic!("No memory to read at: {:#x}", addr);
            }
        }
    }

    pub fn write_word(&mut self, addr: u32, mut word: u32) {
        match addr {
            0x0000_0000 ... 0x03ef_ffff => {
                let addr = addr as usize;
                // RAM
                self.ram[addr + 3] = word as u16 & 0xff;
                word >>= 8;
                self.ram[addr + 2] = word as u16 & 0xff;
                word >>= 8;
                self.ram[addr + 1] = word as u16 & 0xff;
                word >>= 8;
                self.ram[addr] = word as u16;
            }
            0x1fc0_07c0 ... 0x1fc0_07ff => {
                // PIF RAM
                let rel_addr = addr as usize - 0x1fc0_07c0;
                self.pif_ram[rel_addr / 4] = word;
            }
            0x0400_0000 ... 0x040f_ffff => {
                // SP area
                match addr {
                    0x0400_0000 ... 0x0400_0fff => {
                        self.sp.dmem[(addr as usize - 0x0400_0000) / 4] = word;
                    }
                    0x0400_1000 ... 0x0400_1fff => {
                        self.sp.imem[(addr as usize - 0x0400_1000) / 4] = word;
                    }
                    0x0404_0000 => self.sp.reg_mem_addr = word,
                    0x0404_0004 => self.sp.reg_dram_addr = word,
                    0x0404_0008 => self.sp.reg_rd_len = word,
                    0x0404_000c => self.sp.reg_wr_len = word,
                    0x0404_0010 => self.sp.reg_status = word,
                    0x0404_001c => self.sp.reg_semaphore = word,
                    0x0408_0000 => self.sp.reg_pc = word,
                    0x0408_0004 => self.sp.reg_ibist = word,
                    _ => {
                        panic!("Unsupported SP write address: {:#x}", addr);
                    }
                }
            }
            0x0410_0000 ... 0x042f_ffff => {
                // DP area
                match addr {
                    0x0410_0000 => self.dp.reg_start = word,
                    0x0410_0004 => self.dp.reg_end = word,
                    0x0410_000c => self.dp.reg_status = word,
                    0x0420_0000 => self.dp.reg_tbist = word,
                    0x0420_0004 => self.dp.reg_test_mode = word,
                    0x0420_0008 => self.dp.reg_buftest_addr = word,
                    0x0420_000c => self.dp.reg_buftest_data = word,
                    _ => {
                        panic!("Unsupported DP write address: {:#x}", addr);
                    }
                }
            }
            0x0440_0000 ... 0x044f_ffff => {
                // VI area
                match addr {
                    0x0440_000c => self.vi.reg_intr = word,
                    0x0440_0010 => self.vi.reg_current = word,
                    0x0440_0024 => self.vi.reg_h_start = word,
                    _ => {
                        panic!("Unsupported VI write address: {:#x}", addr);
                    }
                }
            }
            0x0450_0000 ... 0x045f_ffff => {
                // AI area
                match addr {
                    0x0450_0000 => self.ai.reg_dram_addr = word,
                    0x0450_0004 => self.ai.reg_len = word,
                    _ => {
                        panic!("Unsupported AI write address: {:#x}", addr);
                    }
                }
            }
            0x0460_0000 ... 0x046f_ffff => {
                // PI area
                match addr {
                    0x0460_0000 => self.pi.reg_dram_addr = word,
                    0x0460_0004 => self.pi.reg_cart_addr = word,
                    0x0460_0008 => self.pi.reg_rd_len = word,
                    0x0460_000c => self.pi.reg_wr_len = word,
                    0x0460_0010 => self.pi.reg_status = word,
                    0x0460_0014 => self.pi.reg_bsd_dom1_lat = word,
                    0x0460_0018 => self.pi.reg_bsd_dom1_pwd = word,
                    0x0460_001c => self.pi.reg_bsd_dom1_pgs = word,
                    0x0460_0020 => self.pi.reg_bsd_dom1_rls = word,
                    0x0460_0024 => self.pi.reg_bsd_dom2_lat = word,
                    0x0460_0028 => self.pi.reg_bsd_dom2_pwd = word,
                    0x0460_002c => self.pi.reg_bsd_dom2_pgs = word,
                    0x0460_0030 => self.pi.reg_bsd_dom2_rls = word,
                    _ => {
                        panic!("Unsupported PI write address: {:#x}", addr);
                    }
                }
            }
            0x0480_0000 ... 0x048f_ffff => {
                // SI area
                match addr {
                    0x0480_0000 => self.si.reg_dram_addr = word,
                    0x0480_0004 => self.si.reg_pif_addr_rd64b = word,
                    0x0480_0010 => self.si.reg_pif_addr_wr64b = word,
                    0x0480_0018 => self.si.reg_status = word,
                    _ => {
                        panic!("Unsupported SI write address: {:#x}", addr);
                    }
                }
            }
            _ => {
                panic!("No memory to write at: {:#x}", addr);
            }
        }
    }
}

impl fmt::Debug for Interconnect {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "TODO: Impl Debug for Interconnect")
    }
}
