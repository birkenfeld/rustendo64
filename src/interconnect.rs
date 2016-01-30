use super::byteorder::{BigEndian, ByteOrder};

use std::fmt;

const PIF_ROM_SIZE: usize = 2048;

const RAM_SIZE: usize = 8 * 1024 * 1024;

#[derive(Default, Debug)]
struct Rd {
    reg_config: u32,
    reg_device_id: u32,
    reg_delay: u32,
    reg_mode: u32,
    reg_ref_interval: u32,
    reg_ref_row: u32,
    reg_ras_interval: u32,
    reg_min_interval: u32,
    reg_addr_select: u32,
    reg_device_manuf: u32,
}

#[derive(Default)]
struct SpRam {
    dmem: Vec<u32>,
    imem: Vec<u32>,
}

#[derive(Default, Debug)]
struct Sp {
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

#[derive(Default, Debug)]
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

#[derive(Default, Debug)]
struct Mi {
    reg_mode: u32,
    reg_version: u32,
    reg_intr: u32,
    reg_intr_mask: u32,
}

#[derive(Default, Debug)]
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

#[derive(Default, Debug)]
struct Ai {
    reg_dram_addr: u32,
    reg_len: u32,
    reg_control: u32,
    reg_status: u32,
    reg_dacrate: u32,
    reg_bitrate: u32,
}

#[derive(Default, Debug)]
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

#[derive(Default, Debug)]
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

#[derive(Default, Debug)]
struct Si {
    reg_dram_addr: u32,
    reg_pif_addr_rd64b: u32,
    reg_pif_addr_wr64b: u32,
    reg_status: u32,
}

pub struct Interconnect {
    pif_rom: Vec<u8>,
    pif_ram: Vec<u32>,
    pif_status: u32,
    cart_rom: Vec<u8>,
    ram: Vec<u16>,
    spram: SpRam,
    rd: Rd,
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
            pif_status: 0,
            cart_rom: cart_rom,
            ram: vec![0; RAM_SIZE],
            spram: SpRam { dmem: vec![0; 1024], imem: vec![0; 1024] },
            rd: Rd::default(),
            sp: Sp::default(),
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
        // all gleaned from cen64
        self.sp.reg_status |= 0x1;
        // CIC seed for CRC, not for all ROMs!
        // fire demo: 00003f00
        // ocarina: ....91..
//        self.pif_ram[(0x07e4 - 0x7c0) / 4] = 0x0002913f;
        self.pif_ram[(0x07e4 - 0x7c0) / 4] = 0x00003f00;
        // memory size
        self.write_word(0x3f0, 0x800000);
        self.ri.reg_mode = 0xE;
        self.ri.reg_config = 0x40;
        self.ri.reg_select = 0x14;
        self.ri.reg_refresh = 0x63634;
    }

    pub fn read_word(&mut self, addr: u32) -> Result<u32, &'static str> {
        let res = match addr {
            // TODO: Replace constants with useful names
            0x0000_0000 ... 0x03ef_ffff => {
                // RAM
                let addr = addr as usize;
                // Cannot use byteorder: RAM is 16bits
                ((self.ram[addr] as u32) << 24) |
                ((self.ram[addr + 1] as u32) << 16) |
                ((self.ram[addr + 2] as u32) << 8) |
                 (self.ram[addr + 3] as u32)
            }
            0x1000_0000 ... 0x1fbf_ffff => {
                // Cartridge ROM
                let rel_addr = addr as usize - 0x1000_0000;
                BigEndian::read_u32(&self.cart_rom[rel_addr..])
            }
            0x1fc0_0000 ... 0x1fc0_07bf => {
                // PIF ROM
                let rel_addr = addr as usize - 0x1fc0_0000;
                BigEndian::read_u32(&self.pif_rom[rel_addr..])
            }
            0x1fc0_07c0 ... 0x1fc0_07ff => {
                // PIF RAM
                let rel_addr = addr as usize - 0x1fc0_07c0;
                if rel_addr == 0x3c {
                    self.pif_status
                } else {
                    if rel_addr == 0x24 {
                        // hack to avoid looping at the end of the PIF rom
                        self.pif_status = 0x80;
                    }
                    self.pif_ram[rel_addr / 4]
                }
            }
            0x03f0_0000 ... 0x03ff_ffff => {
                // RDRAM register area
                match addr {
                    0x03f0_0000 => self.rd.reg_config,
                    0x03f0_0004 => self.rd.reg_device_id,
                    0x03f0_0008 => self.rd.reg_delay,
                    0x03f0_000c => self.rd.reg_mode,
                    0x03f0_0010 => self.rd.reg_ref_interval,
                    0x03f0_0014 => self.rd.reg_ref_row,
                    0x03f0_0018 => self.rd.reg_ras_interval,
                    0x03f0_001c => self.rd.reg_min_interval,
                    0x03f0_0020 => self.rd.reg_addr_select,
                    0x03f0_0024 => self.rd.reg_device_manuf,
                    _ => {
                        return Err("Unsupported RDreg read address");
                    }
                }
            }
            0x0400_0000 ... 0x040f_ffff => {
                // SP area
                match addr {
                    0x0400_0000 ... 0x0400_0fff => {
                        self.spram.dmem[(addr as usize - 0x0400_0000) / 4]
                    }
                    0x0400_1000 ... 0x0400_1fff => {
                        self.spram.imem[(addr as usize - 0x0400_1000) / 4]
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
                        return Err("Unsupported SP read address");
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
                        return Err("Unsupported DP read address");
                    }
                }
            }
            0x0430_0000 ... 0x043f_ffff => {
                // MI area
                match addr {
                    0x0430_0000 => self.mi.reg_mode,
                    0x0430_0004 => self.mi.reg_version,
                    0x0430_0008 => self.mi.reg_intr,
                    0x0430_000c => self.mi.reg_intr_mask,
                    _ => {
                        return Err("Unsupported MI read address");
                    }
                }
            }
            0x0440_0000 ... 0x044f_ffff => {
                // VI area
                match addr {
                    0x0440_0000 => self.vi.reg_status,
                    0x0440_0004 => self.vi.reg_origin,
                    0x0440_0008 => self.vi.reg_width,
                    0x0440_000c => self.vi.reg_intr,
                    0x0440_0010 => self.vi.reg_current,
                    0x0440_0014 => self.vi.reg_burst,
                    0x0440_0018 => self.vi.reg_v_sync,
                    0x0440_001c => self.vi.reg_h_sync,
                    0x0440_0020 => self.vi.reg_leap,
                    0x0440_0024 => self.vi.reg_h_start,
                    0x0440_0028 => self.vi.reg_v_start,
                    0x0440_002c => self.vi.reg_v_burst,
                    0x0440_0030 => self.vi.reg_x_scale,
                    0x0440_0034 => self.vi.reg_y_scale,
                    _ => {
                        return Err("Unsupported VI read address");
                    }
                }
            }
            0x0450_0000 ... 0x045f_ffff => {
                // AI area
                match addr {
                    0x0450_0000 => self.ai.reg_dram_addr,
                    0x0450_0004 => self.ai.reg_len,
                    0x0450_000c => self.ai.reg_status,
                    _ => {
                        return Err("Unsupported AI read address");
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
                        return Err("Unsupported PI read address");
                    }
                }
            }
            0x0470_0000 ... 0x047f_ffff => {
                // RI area
                match addr {
                    0x0470_0000 => self.ri.reg_mode,
                    0x0470_0004 => self.ri.reg_config,
                    0x0470_0008 => self.ri.reg_current_load,
                    0x0470_000c => self.ri.reg_select,
                    0x0470_0010 => self.ri.reg_refresh,
                    0x0470_0014 => self.ri.reg_latency,
                    0x0470_0018 => self.ri.reg_rerror,
                    _ => {
                        return Err("Unsupported RI read address");
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
                        return Err("Unsupported SI read address");
                    }
                }
            }
            0x0600_0000 ... 0x063f_ffff => {
                // DD IPL ROM, return zeros
                0
            }
            _ => {
                // TODO
                return Err("Unsupported read memory area");
            }
        };
        if addr > 0x03ef_ffff {
            if addr < 0x0400_0000 || addr > 0x0400_1fff {
                if addr < 0x1000_0000 || addr > 0x1fc0_07bf {
                    // Log all reads from non-RAM, non-ROM locations
                    println!("Bus read:  {:#10x} :  {:#10x}", addr, res);
                }
            }
        }
        Ok(res)
    }

    pub fn write_word(&mut self, addr: u32, mut word: u32) -> Result<(), &'static str> {
        if addr > 0x03ef_ffff && (addr < 0x0400_0000 || addr > 0x0400_1fff) {
            // Log all writes to non-RAM locations
            println!("Bus write: {:#10x} <- {:#10x}", addr, word);
        }
        match addr {
            0x0000_0000 ... 0x03ef_ffff => {
                // RAM
                let addr = addr as usize;
                // Cannot use byteorder: RAM is 16bits
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
                // XXX assumes alignment
                self.pif_ram[rel_addr / 4] = word;
            }
            0x03f0_0000 ... 0x03ff_ffff => {
                // RDRAM register area
                match addr {
                    0x03f0_0000 => self.rd.reg_config = word,
                    0x03f0_0004 => self.rd.reg_device_id = word,
                    0x03f0_0008 => self.rd.reg_delay = word,
                    0x03f0_000c => self.rd.reg_mode = word,
                    0x03f0_0010 => self.rd.reg_ref_interval = word,
                    0x03f0_0014 => self.rd.reg_ref_row = word,
                    0x03f0_0018 => self.rd.reg_ras_interval = word,
                    0x03f0_001c => self.rd.reg_min_interval = word,
                    0x03f0_0020 => self.rd.reg_addr_select = word,
                    _ => {
                        return Err("Unsupported RDreg write address");
                    }
                }
            }
            0x0400_0000 ... 0x040f_ffff => {
                // SP area
                match addr {
                    0x0400_0000 ... 0x0400_0fff => {
                        self.spram.dmem[(addr as usize - 0x0400_0000) / 4] = word;
                    }
                    0x0400_1000 ... 0x0400_1fff => {
                        self.spram.imem[(addr as usize - 0x0400_1000) / 4] = word;
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
                        return Err("Unsupported SP write address");
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
                        return Err("Unsupported DP write address");
                    }
                }
            }
            0x0430_0000 ... 0x043f_ffff => {
                // MI area
                match addr {
                    0x0430_0000 => self.mi.reg_mode = word,
                    0x0430_000c => self.mi.reg_intr_mask = word,
                    _ => {
                        return Err("Unsupported MI write address");
                    }
                }
            }
            0x0440_0000 ... 0x044f_ffff => {
                // VI area
                match addr {
                    0x0440_0000 => self.vi.reg_status = word,
                    0x0440_0004 => self.vi.reg_origin = word,
                    0x0440_0008 => self.vi.reg_width = word,
                    0x0440_000c => self.vi.reg_intr = word,
                    0x0440_0010 => self.vi.reg_current = word,
                    0x0440_0014 => self.vi.reg_burst = word,
                    0x0440_0018 => self.vi.reg_v_sync = word,
                    0x0440_001c => self.vi.reg_h_sync = word,
                    0x0440_0020 => self.vi.reg_leap = word,
                    0x0440_0024 => self.vi.reg_h_start = word,
                    0x0440_0028 => self.vi.reg_v_start = word,
                    0x0440_002c => self.vi.reg_v_burst = word,
                    0x0440_0030 => self.vi.reg_x_scale = word,
                    0x0440_0034 => self.vi.reg_y_scale = word,
                    _ => {
                        return Err("Unsupported VI write address");
                    }
                }
            }
            0x0450_0000 ... 0x045f_ffff => {
                // AI area
                match addr {
                    0x0450_0000 => self.ai.reg_dram_addr = word,
                    0x0450_0004 => self.ai.reg_len = word,
                    0x0450_0008 => self.ai.reg_control = word,
                    0x0450_000c => { }, // TODO: clear intr
                    0x0450_0010 => self.ai.reg_dacrate = word,
                    0x0450_0014 => self.ai.reg_bitrate = word,
                    _ => {
                        return Err("Unsupported AI write address");
                    }
                }
            }
            0x0460_0000 ... 0x046f_ffff => {
                // PI area
                match addr {
                    0x0460_0000 => self.pi.reg_dram_addr = word,
                    0x0460_0004 => self.pi.reg_cart_addr = word,
                    0x0460_0008 => self.pi.reg_rd_len = word,
                    0x0460_000c => {
                        self.pi.reg_wr_len = word;
                        // DMA transfer ROM -> main memory
                        let ram_start = self.pi.reg_dram_addr as usize;  // offset is 0
                        let rom_start = self.pi.reg_cart_addr as usize - 0x1000_0000;
                        let length = (word + 1) as usize;
                        println!("DMA transfer: {:#x} bytes from ROM {:#x} to {:#x}",
                                 length, rom_start, ram_start);
                        for i in 0..length {
                            self.ram[ram_start + i] = self.cart_rom[rom_start + i] as u16;
                        }
                    },
                    0x0460_0010 => { }, // TODO: self.pi.reg_status = word,
                    0x0460_0014 => self.pi.reg_bsd_dom1_lat = word,
                    0x0460_0018 => self.pi.reg_bsd_dom1_pwd = word,
                    0x0460_001c => self.pi.reg_bsd_dom1_pgs = word,
                    0x0460_0020 => self.pi.reg_bsd_dom1_rls = word,
                    0x0460_0024 => self.pi.reg_bsd_dom2_lat = word,
                    0x0460_0028 => self.pi.reg_bsd_dom2_pwd = word,
                    0x0460_002c => self.pi.reg_bsd_dom2_pgs = word,
                    0x0460_0030 => self.pi.reg_bsd_dom2_rls = word,
                    _ => {
                        return Err("Unsupported PI write address");
                    }
                }
            }
            0x0470_0000 ... 0x047f_ffff => {
                // RI area
                match addr {
                    0x0470_0000 => self.ri.reg_mode = word,
                    0x0470_0004 => self.ri.reg_config = word,
                    0x0470_0008 => self.ri.reg_current_load = word,
                    0x0470_000c => self.ri.reg_select = word,
                    0x0470_0010 => self.ri.reg_refresh = word,
                    0x0470_0014 => self.ri.reg_latency = word,
                    0x0470_001c => self.ri.reg_werror = word,
                    _ => {
                        return Err("Unsupported RI read address");
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
                        return Err("Unsupported SI write address");
                    }
                }
            }
            _ => {
                return Err("Unsupported memory write area");
            }
        }
        Ok(())
    }
}

impl fmt::Debug for Interconnect {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Interconnect")
         .field("pif_ram", &self.pif_ram)
         .field("pif_status", &self.pif_status)
         .field("rd", &self.rd)
         .field("sp", &self.sp)
         .field("dp", &self.dp)
         .field("mi", &self.mi)
         .field("vi", &self.vi)
         .field("ai", &self.ai)
         .field("pi", &self.pi)
         .field("ri", &self.ri)
         .field("si", &self.si)
         .finish()
    }
}
