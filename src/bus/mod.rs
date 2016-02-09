mod cic;
mod vi;
mod si;
mod pi;
mod mi;
mod ai;
mod ri;
pub mod mem_map;

use std::fmt;
use std::mem::replace;

use byteorder::{BigEndian, ByteOrder};

use self::mem_map::*;
use self::vi::Vi;
use self::si::Si;
use self::pi::Pi;
use self::mi::Mi;
use self::ai::Ai;
use self::ri::{Ri, RdRegs};
use ui::{IfOutput, InterfaceChannel};
use rsp::{Sp, Dp, SpRam};

const PIF_ROM_SIZE: usize = 2048;
const RAM_SIZE: usize = 8 * 1024 * 1024;

pub struct Bus {
    ram: Vec<u32>,
    spram: SpRam,
    rd: RdRegs,
    sp: Sp,
    dp: Dp,
    mi: Mi,
    vi: Vi,
    ai: Ai,
    pi: Pi,
    ri: Ri,
    si: Si,
    interface: InterfaceChannel,
    pub interrupts: u32,
}

impl Bus {
    pub fn new(pif_rom: Vec<u8>, cart_rom: Vec<u8>,
               interface: InterfaceChannel) -> Bus {
        Bus {
            interrupts: 0,
            ram: vec![0; RAM_SIZE / 4],
            spram: SpRam { dmem: vec![0; 1024], imem: vec![0; 1024] },
            interface: interface,
            rd: RdRegs::default(),
            sp: Sp::default(),
            dp: Dp::default(),
            mi: Mi::default(),
            vi: Vi::default(),
            ai: Ai::default(),
            pi: Pi { cart_rom: cart_rom, ..Pi::default() },
            ri: Ri::default(),
            si: Si { pif_rom: pif_rom, pif_ram: vec![0; 64], ..Si::default() },
        }
    }

    pub fn power_on_reset(&mut self) {
        // determine checksum seed and write to PIF ram
        if let Some(seed) = cic::get_cic_seed(&self.pi.cart_rom) {
            BigEndian::write_u32(&mut self.si.pif_ram[0x24..], seed);
        } else {
            println!("Warning: no CIC seed found for this ROM");
        }
        // all gleaned from cen64
        self.sp.reg_status |= 0x1;
        // memory size
        self.write_word(0x3f0, 0x800000).unwrap();
        // RDRAM interface configs
        self.ri.reg_mode = 0xE;
        self.ri.reg_config = 0x40;
        self.ri.reg_select = 0x14;
        self.ri.reg_refresh = 0x63634;
    }

    pub fn read_word(&mut self, addr: u32) -> Result<u32, &'static str> {
        if addr & 0x3 != 0 {
            return Err("unaligned access");
        }
        let res = match addr {
            RDRAM_START ... RDRAM_END => {
                self.ram[addr as usize / 4]
            },
            CART_START ... CART_END => {
                let rel_addr = (addr - CART_START) as usize;
                BigEndian::read_u32(&self.pi.cart_rom[rel_addr..])
            },
            PIF_ROM_START ... PIF_ROM_END => {
                let rel_addr = (addr - PIF_ROM_START) as usize;
                BigEndian::read_u32(&self.si.pif_rom[rel_addr..])
            },
            PIF_RAM_START ... PIF_RAM_END => {
                let rel_addr = (addr - PIF_RAM_START) as usize;
                if rel_addr == 0x3c {
                    self.si.pif_status
                } else {
                    if rel_addr == 0x24 {
                        // hack to avoid looping at the end of the PIF rom
                        self.si.pif_status = 0x80;
                    }
                    BigEndian::read_u32(&self.si.pif_ram[rel_addr..])
                }
            },
            SP_DMEM_START ... SP_DMEM_END => {
                self.spram.dmem[(addr - SP_DMEM_START) as usize / 4]
            }
            SP_IMEM_START ... SP_IMEM_END => {
                self.spram.imem[(addr - SP_IMEM_START) as usize / 4]
            }
            DD_ROM_START ... DD_ROM_END => {
                // DD IPL ROM, return zeros
                0
            }
            DD_REG_START ... DD_REG_END => {
                // DD interface registers
                0
            }
            // RDRAM registers
            RDRAM_REG_CONFIG       => self.rd.reg_config,
            RDRAM_REG_DEVICE_ID    => self.rd.reg_device_id,
            RDRAM_REG_DELAY        => self.rd.reg_delay,
            RDRAM_REG_MODE         => self.rd.reg_mode,
            RDRAM_REG_REF_INTERVAL => self.rd.reg_ref_interval,
            RDRAM_REG_REF_ROW      => self.rd.reg_ref_row,
            RDRAM_REG_RAS_INTERVAL => self.rd.reg_ras_interval,
            RDRAM_REG_MIN_INTERVAL => self.rd.reg_min_interval,
            RDRAM_REG_ADDR_SELECT  => self.rd.reg_addr_select,
            RDRAM_REG_DEVICE_MANUF => self.rd.reg_device_manuf,
            // RI registers
            RI_REG_MODE            => self.ri.reg_mode,
            RI_REG_CONFIG          => self.ri.reg_config,
            RI_REG_SELECT          => self.ri.reg_select,
            RI_REG_REFRESH         => self.ri.reg_refresh,
            RI_REG_LATENCY         => self.ri.reg_latency,
            RI_REG_RERROR          => self.ri.reg_rerror,
            // RSP registers
            SP_REG_MEM_ADDR        => self.sp.reg_mem_addr,
            SP_REG_DRAM_ADDR       => self.sp.reg_dram_addr,
            SP_REG_RD_LEN          => self.sp.reg_rd_len,
            SP_REG_WR_LEN          => self.sp.reg_wr_len,
            SP_REG_STATUS          => self.sp.reg_status,
            SP_REG_DMA_FULL        => self.sp.reg_dma_full,
            SP_REG_DMA_BUSY        => self.sp.reg_dma_busy,
            SP_REG_SEMAPHORE       => replace(&mut self.sp.reg_semaphore, 1),
            SP_REG_PC              => self.sp.reg_pc,
            SP_REG_IBIST           => self.sp.reg_ibist,
            // RDP registers
            DPC_REG_START          => self.dp.reg_start,
            DPC_REG_END            => self.dp.reg_end,
            DPC_REG_CURRENT        => self.dp.reg_current,
            DPC_REG_STATUS         => self.dp.reg_status,
            DPC_REG_CLOCK          => self.dp.reg_clock,
            DPC_REG_BUFBUSY        => self.dp.reg_bufbusy,
            DPC_REG_PIPEBUSY       => self.dp.reg_pipebusy,
            DPC_REG_TMEM           => self.dp.reg_tmem,
            DPS_REG_TBIST          => self.dp.reg_tbist,
            DPS_REG_TEST_MODE      => self.dp.reg_test_mode,
            DPS_REG_BUFTEST_ADDR   => self.dp.reg_buftest_addr,
            DPS_REG_BUFTEST_DATA   => self.dp.reg_buftest_data,
            // MIPS interface
            MI_REG_MODE            => self.mi.reg_mode,
            MI_REG_VERSION         => self.mi.reg_version,
            MI_REG_INTR            => self.mi.reg_intr,
            MI_REG_INTR_MASK       => self.mi.reg_intr_mask,
            // Video interface
            VI_REG_STATUS          => 0, // self.vi.reg_status,
            VI_REG_ORIGIN          => self.vi.reg_origin,
            VI_REG_H_WIDTH         => self.vi.reg_width,
            VI_REG_V_INTR          => self.vi.reg_intr,
            VI_REG_CURRENT         => {
                // TODO
                self.vi.reg_current = (self.vi.reg_current + 1) % 525;
                return Ok(self.vi.reg_current);  // bypass logging
            },
            VI_REG_BURST           => self.vi.reg_burst,
            VI_REG_V_SYNC          => self.vi.reg_v_sync,
            VI_REG_H_SYNC          => self.vi.reg_h_sync,
            VI_REG_LEAP            => self.vi.reg_leap,
            VI_REG_H_START         => self.vi.reg_h_start,
            VI_REG_V_START         => self.vi.reg_v_start,
            VI_REG_V_BURST         => self.vi.reg_v_burst,
            VI_REG_X_SCALE         => self.vi.reg_x_scale,
            VI_REG_Y_SCALE         => self.vi.reg_y_scale,
            // Audio interface
            AI_REG_DRAM_ADDR       => self.ai.reg_dram_addr,
            AI_REG_LEN             => self.ai.reg_len,
            AI_REG_STATUS          => self.ai.reg_status,
            // Peripheral interface
            PI_REG_DRAM_ADDR       => self.pi.reg_dram_addr,
            PI_REG_CART_ADDR       => self.pi.reg_cart_addr,
            PI_REG_RD_LEN          => self.pi.reg_rd_len,
            PI_REG_WR_LEN          => self.pi.reg_wr_len,
            PI_REG_STATUS          => self.pi.reg_status,
            PI_REG_BSD_DOM1_LAT    => self.pi.reg_bsd_dom1_lat,
            PI_REG_BSD_DOM1_PWD    => self.pi.reg_bsd_dom1_pwd,
            PI_REG_BSD_DOM1_PGS    => self.pi.reg_bsd_dom1_pgs,
            PI_REG_BSD_DOM1_RLS    => self.pi.reg_bsd_dom1_rls,
            PI_REG_BSD_DOM2_LAT    => self.pi.reg_bsd_dom2_lat,
            PI_REG_BSD_DOM2_PWD    => self.pi.reg_bsd_dom2_pwd,
            PI_REG_BSD_DOM2_PGS    => self.pi.reg_bsd_dom2_pgs,
            PI_REG_BSD_DOM2_RLS    => self.pi.reg_bsd_dom2_rls,
            // Serial interface
            SI_REG_DRAM_ADDR       => self.si.reg_dram_addr,
            SI_REG_STATUS          => self.si.reg_status,
            _ => {
                // TODO
                return Err("Unsupported read memory area");
            }
        };
        Ok(res)
    }

    pub fn write_word(&mut self, addr: u32, word: u32) -> Result<(), &'static str> {
        if addr & 0x3 != 0 {
            return Err("unaligned access");
        }
        match addr {
            RDRAM_START ... RDRAM_END => {
                self.ram[addr as usize / 4] = word;
            },
            PIF_RAM_START ... PIF_RAM_END => {
                let rel_addr = (addr - PIF_RAM_START) as usize;
                BigEndian::write_u32(&mut self.si.pif_ram[rel_addr..], word);
                self.si.reg_status |= 0x1000;
                self.signal_interrupt(mi::INTR_SI);
            },
            SP_DMEM_START ... SP_DMEM_END => {
                self.spram.dmem[(addr - SP_DMEM_START) as usize / 4] = word;
            },
            SP_IMEM_START ... SP_IMEM_END => {
                self.spram.imem[(addr - SP_IMEM_START) as usize / 4] = word;
            },
            // RDRAM registers
            RDRAM_REG_CONFIG       => self.rd.reg_config = word,
            RDRAM_REG_DEVICE_ID    => self.rd.reg_device_id = word,
            RDRAM_REG_DELAY        => self.rd.reg_delay = word,
            RDRAM_REG_MODE         => self.rd.reg_mode = word,
            RDRAM_REG_REF_INTERVAL => self.rd.reg_ref_interval = word,
            RDRAM_REG_REF_ROW      => self.rd.reg_ref_row = word,
            RDRAM_REG_RAS_INTERVAL => self.rd.reg_ras_interval = word,
            RDRAM_REG_MIN_INTERVAL => self.rd.reg_min_interval = word,
            RDRAM_REG_ADDR_SELECT  => self.rd.reg_addr_select = word,
            // RI registers
            RI_REG_MODE            => self.ri.reg_mode = word & 0xf,
            RI_REG_CONFIG          => self.ri.reg_config = word & 0x7f,
            RI_REG_CURRENT_LOAD    => { /* TODO */ },
            RI_REG_SELECT          => self.ri.reg_select = word & 0x3f,
            RI_REG_REFRESH         => self.ri.reg_refresh = word & 0x7_ffff,
            RI_REG_LATENCY         => self.ri.reg_latency = word & 0xf,
            RI_REG_WERROR          => { /* TODO */ },
            // RSP registers
            SP_REG_MEM_ADDR        => self.sp.reg_mem_addr = word & 0x1fff,
            SP_REG_DRAM_ADDR       => self.sp.reg_dram_addr = word & 0xff_ffff,
            SP_REG_RD_LEN          => self.sp.reg_rd_len = word,
            SP_REG_WR_LEN          => self.sp.reg_wr_len = word,
            SP_REG_STATUS          => {
                if word & 0x1 != 0 {
                    println!("Would start RSP");
                }
                if word & 0x8 != 0 {
                    self.clear_interrupt(mi::INTR_SP);
                }
                if word & 0x10 != 0 {
                    self.signal_interrupt(mi::INTR_SP);
                }
                /* TODO */
            }
            SP_REG_SEMAPHORE       => self.sp.reg_semaphore = 0,
            SP_REG_PC              => self.sp.reg_pc = word & 0xfff,
            SP_REG_IBIST           => self.sp.reg_ibist = word & 0x7,
            // RDP registers
            DPC_REG_START          => self.dp.reg_start = word & 0xff_ffff,
            DPC_REG_END            => self.dp.reg_end = word & 0xff_ffff,
            DPC_REG_STATUS         => { /* TODO */ },
            DPS_REG_TBIST          => self.dp.reg_tbist = word & 0x7,
            DPS_REG_TEST_MODE      => self.dp.reg_test_mode = word & 0x1,
            DPS_REG_BUFTEST_ADDR   => self.dp.reg_buftest_addr = word & 0x7f,
            DPS_REG_BUFTEST_DATA   => self.dp.reg_buftest_data = word,
            // MIPS interface
            MI_REG_MODE            => {
                if word & 0x800 != 0 {
                    self.clear_interrupt(mi::INTR_DP);
                }
                /* TODO */
            },
            MI_REG_INTR_MASK       => {
                if word & 0x1 != 0 {
                    self.mi.reg_intr_mask &= !mi::INTR_SP;
                } else if word & 0x2 != 0 {
                    self.mi.reg_intr_mask |= mi::INTR_SP;
                }
                if word & 0x4 != 0 {
                    self.mi.reg_intr_mask &= !mi::INTR_SI;
                } else if word & 0x8 != 0 {
                    self.mi.reg_intr_mask |= mi::INTR_SI;
                }
                if word & 0x10 != 0 {
                    self.mi.reg_intr_mask &= !mi::INTR_AI;
                } else if word & 0x20 != 0 {
                    self.mi.reg_intr_mask |= mi::INTR_AI;
                }
                if word & 0x40 != 0 {
                    self.mi.reg_intr_mask &= !mi::INTR_VI;
                } else if word & 0x80 != 0 {
                    self.mi.reg_intr_mask |= mi::INTR_VI;
                }
                if word & 0x100 != 0 {
                    self.mi.reg_intr_mask &= !mi::INTR_PI;
                } else if word & 0x200 != 0 {
                    self.mi.reg_intr_mask |= mi::INTR_PI;
                }
                if word & 0x400 != 0 {
                    self.mi.reg_intr_mask &= !mi::INTR_DP;
                } else if word & 0x800 != 0 {
                    self.mi.reg_intr_mask |= mi::INTR_DP;
                }
                self.check_interrupts();
            },
            // Video interface
            VI_REG_STATUS          => {
                self.vi.reg_status = word & 0xffff;
                self.vi.update(&mut self.interface);
            },
            VI_REG_ORIGIN          => {
                self.vi.reg_origin = word & 0xff_ffff;  // only 24 bits
                // println!("VRAM at {:#x}", word);
                self.vi.update_vram();
            },
            VI_REG_H_WIDTH         => {
                self.vi.reg_width = word & 0xfff;
                self.vi.update(&mut self.interface);
            },
            VI_REG_V_INTR          => self.vi.reg_intr = word & 0x3ff,
            VI_REG_CURRENT         => {
                self.clear_interrupt(mi::INTR_VI);
            },
            VI_REG_BURST           => self.vi.reg_burst = word & 0x3fff_ffff,
            VI_REG_V_SYNC          => self.vi.reg_v_sync = word & 0x3ff,
            VI_REG_H_SYNC          => self.vi.reg_h_sync = word & 0x1f_ffff,
            VI_REG_LEAP            => self.vi.reg_leap = word & 0xfff_ffff,
            VI_REG_H_START         => {
                self.vi.reg_h_start = word & 0x3ff_ffff;
                self.vi.update(&mut self.interface);
            },
            VI_REG_V_START         => {
                self.vi.reg_v_start = word & 0x3ff_ffff;
                self.vi.update(&mut self.interface);
            },
            VI_REG_V_BURST         => self.vi.reg_v_burst = word & 0x3ff_ffff,
            VI_REG_X_SCALE         => {
                self.vi.reg_x_scale = word & 0xfff_ffff;
                self.vi.update(&mut self.interface);
            },
            VI_REG_Y_SCALE         => {
                self.vi.reg_y_scale = word & 0xfff_ffff;
                self.vi.update(&mut self.interface);
            },
            // Audio interface
            AI_REG_DRAM_ADDR       => self.ai.reg_dram_addr = word & 0xff_ffff,
            AI_REG_LEN             => self.ai.reg_len = word & 0x3_ffff,
            AI_REG_CONTROL         => self.ai.reg_control = word & 0x1,
            AI_REG_STATUS          => {
                self.clear_interrupt(mi::INTR_AI);
            },
            AI_REG_DACRATE         => self.ai.reg_dacrate = word & 0x3fff,
            AI_REG_BITRATE         => self.ai.reg_bitrate = word & 0xf,
            // Peripheral interface
            PI_REG_DRAM_ADDR       => self.pi.reg_dram_addr = word & 0xff_ffff,
            PI_REG_CART_ADDR       => self.pi.reg_cart_addr = word,
            PI_REG_RD_LEN          => self.pi.reg_rd_len = word & 0xff_ffff,
            PI_REG_WR_LEN          => {
                self.pi.dma_read(&mut self.ram, word);
                self.signal_interrupt(mi::INTR_PI);
            },
            PI_REG_STATUS          => {
                if word & 0x2 != 0 {
                    self.clear_interrupt(mi::INTR_PI);
                }
                /* TODO */
            },
            PI_REG_BSD_DOM1_LAT    => self.pi.reg_bsd_dom1_lat = word & 0xff,
            PI_REG_BSD_DOM1_PWD    => self.pi.reg_bsd_dom1_pwd = word & 0xff,
            PI_REG_BSD_DOM1_PGS    => self.pi.reg_bsd_dom1_pgs = word & 0xf,
            PI_REG_BSD_DOM1_RLS    => self.pi.reg_bsd_dom1_rls = word & 0x3,
            PI_REG_BSD_DOM2_LAT    => self.pi.reg_bsd_dom2_lat = word & 0xff,
            PI_REG_BSD_DOM2_PWD    => self.pi.reg_bsd_dom2_pwd = word & 0xff,
            PI_REG_BSD_DOM2_PGS    => self.pi.reg_bsd_dom2_pgs = word & 0xf,
            PI_REG_BSD_DOM2_RLS    => self.pi.reg_bsd_dom2_rls = word & 0x3,
            // Serial interface
            SI_REG_DRAM_ADDR       => {
                self.si.reg_dram_addr = word & 0xff_ffff;
            },
            SI_REG_PIF_ADDR_RD64B  => {
                self.si.dma_read(&mut self.ram, self.interface.get_input_state());
                self.si.reg_status |= 0x1000;
                self.signal_interrupt(mi::INTR_SI);
            },
            SI_REG_PIF_ADDR_WR64B  => {
                self.si.dma_write(&self.ram);
                self.si.reg_status |= 0x1000;
                self.signal_interrupt(mi::INTR_SI);
            },
            SI_REG_STATUS          => {
                self.si.reg_status &= !0x1000;
                self.clear_interrupt(mi::INTR_SI);
            },
            _ => {
                return Err("Unsupported memory write area");
            }
        }
        Ok(())
    }

    pub fn vi_cycle(&mut self) {
        self.interface.send(IfOutput::Update(
            self.ram[self.vi.vram_start..self.vi.vram_end].to_vec()));
        self.signal_interrupt(mi::INTR_VI);
    }

    fn check_interrupts(&mut self) {
        if self.mi.reg_intr & self.mi.reg_intr_mask != 0 {
            self.interrupts = 0x400; // TODO
        } else {
            self.interrupts = 0;
        }
    }

    pub fn signal_interrupt(&mut self, intr: u32) {
        self.mi.reg_intr |= intr;
        self.check_interrupts();
    }

    pub fn clear_interrupt(&mut self, intr: u32) {
        self.mi.reg_intr &= !intr;
        self.check_interrupts();
    }
}

impl fmt::Debug for Bus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Bus")
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
