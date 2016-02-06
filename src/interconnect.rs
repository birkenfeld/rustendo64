use std::fmt;
use std::mem::replace;

use byteorder::{BigEndian, ByteOrder};

use cic;
use mem_map::*;
use debug::DebugSpecList;
use ui::{IfOutput, InterfaceChannel};
//use rsp::Rsp;

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

    video_width: usize,
    video_height: usize,
    vram_start: usize,
    vram_end: usize,
    vram_pixelsize: usize,
}

impl Vi {
    fn update(&mut self) {
        self.video_width = self.reg_width as usize;
        self.video_height = (self.video_width * 3) / 4;
        self.vram_start = self.reg_origin as usize / 4;
        self.vram_end   = self.vram_start +
            self.video_width * self.video_height * self.vram_pixelsize / 4;
        self.vram_pixelsize = match self.reg_status & 0b11 {
            0b00 => 0,
            0b01 => panic!("using reserved video mode"),
            0b10 => 2,
            0b11 => 4,
            _    => unreachable!()
        };
    }
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
    reg_select: u32,
    reg_refresh: u32,
    reg_latency: u32,
    reg_rerror: u32,
}

#[derive(Default, Debug)]
struct Si {
    pif_ram: Vec<u8>,
    reg_dram_addr: u32,
    reg_status: u32,
}

impl Si {
    fn dma_read(&mut self, ram: &mut [u32]) {
        // transfer 64 bytes PIF ram -> main memory
        let ram_start = self.reg_dram_addr as usize / 4;
        for i in 0..16 {
            ram[ram_start + i] = BigEndian::read_u32(&self.pif_ram[4*i..]);
        }
        // println!("\nPIF read:");
        // for i in 0..8 {
        //     println!("  {:#10x} {:#10x}", ram[ram_start+2*i], ram[ram_start+2*i+1]);
        // }
    }

    fn dma_write(&mut self, ram: &[u32], cstate: u32) {
        // transfer 64 bytes main memory -> PIF ram
        let ram_start = self.reg_dram_addr as usize / 4;
        for i in 0..16 {
            BigEndian::write_u32(&mut self.pif_ram[4*i..], ram[ram_start + i]);
        }
        self.execute(cstate);
    }

    fn execute(&mut self, cstate: u32) {
        // nothing to do?
        if self.pif_ram[63] & 1 == 0 { return; }
        let mut channel = 0;
        let mut start = 0;
        while start <= 60 {  // last possible byte for a command
            let ntrans = self.pif_ram[start] as usize;
            if ntrans == 0xfe { break; }
            if ntrans >= 0x80 { start += 1; continue; }
            channel += 1;
            if ntrans == 0 { start += 1; continue; }
            let nrecv = self.pif_ram[start + 1] as usize;
            let end = start + 2 + ntrans + nrecv;
            if end > 63 {
                // not enough bytes left to read and write
                break;
            }
            let cmdresult = {
                let (input, output) = self.pif_ram[start + 2..end].split_at_mut(ntrans);
                Si::do_command(channel, ntrans, nrecv, input, output, cstate)
            };
            if let Some(err) = cmdresult {
                self.pif_ram[start + 1] |= err;
            }
            start = end;
        }
        self.pif_ram[63] = 0;  // commands executed
    }

    fn do_command(channel: u8, ntrans: usize, nrecv: usize, input: &[u8],
                  output: &mut [u8], cstate: u32) -> Option<u8> {
        if channel != 1 {
            return Some(0x80);  // nothing connected
        }
        match input[0] {  // command
            0 => {  // read status
                if !(ntrans == 1 && nrecv == 3) {
                    return Some(0x40);
                }
                // type: controller, nothing plugged in
                output[0] = 0x05;
                output[1] = 0x0;
                output[2] = 0x02;
            }
            1 => {  // read controller
                if !(ntrans == 1 && nrecv == 4) {
                    return Some(0x40);
                }
                BigEndian::write_u32(output, cstate);
            },
            _ => {
                return Some(0x80);
            }
        }
        None
    }
}

pub struct Interconnect {
    pif_rom: Vec<u8>,
    pif_status: u32,
    cart_rom: Vec<u8>,
    ram: Vec<u32>,
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
    interface: InterfaceChannel,
    pub debug_specs: DebugSpecList,
}

impl Interconnect {
    pub fn new(pif_rom: Vec<u8>, cart_rom: Vec<u8>,
               interface: InterfaceChannel,
               debug: DebugSpecList) -> Interconnect {
        Interconnect {
            pif_rom: pif_rom,
            pif_status: 0,
            cart_rom: cart_rom,
            ram: vec![0; RAM_SIZE / 4],
            spram: SpRam { dmem: vec![0; 1024], imem: vec![0; 1024] },
            interface: interface,
            debug_specs: debug,
            rd: Rd::default(),
            sp: Sp::default(),
            dp: Dp::default(),
            mi: Mi::default(),
            vi: Vi::default(),
            ai: Ai::default(),
            pi: Pi::default(),
            ri: Ri::default(),
            si: Si { pif_ram: vec![0; 64], ..Si::default() },
        }
    }

    pub fn power_on_reset(&mut self) {
        if let Some(seed) = cic::get_cic_seed(&self.cart_rom) {
            BigEndian::write_u32(&mut self.si.pif_ram[0x07e4 - 0x7c0..], seed);
        } else {
            println!("Warning: no CIC seed found for this ROM");
        }
        // all gleaned from cen64
        self.sp.reg_status |= 0x1;
        // memory size
        self.write_word(0x3f0, 0x800000).unwrap();
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
                BigEndian::read_u32(&self.cart_rom[rel_addr..])
            },
            PIF_ROM_START ... PIF_ROM_END => {
                let rel_addr = (addr - PIF_ROM_START) as usize;
                BigEndian::read_u32(&self.pif_rom[rel_addr..])
            },
            PIF_RAM_START ... PIF_RAM_END => {
                let rel_addr = (addr - PIF_RAM_START) as usize;
                if rel_addr == 0x3c {
                    self.pif_status
                } else {
                    if rel_addr == 0x24 {
                        // hack to avoid looping at the end of the PIF rom
                        self.pif_status = 0x80;
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
                if word & 0b1 == 1 {
                    println!("Would start RSP");
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
            MI_REG_MODE            => { /* TODO */ },
            MI_REG_INTR_MASK       => { /* TODO */ },
            // Video interface
            VI_REG_STATUS          => {
                self.vi.reg_status = word & 0xffff;
                println!("Video mode: {:#034b}", word);
                self.vi.update();
                self.interface.send(IfOutput::SetMode(word & 0xffff));
            },
            VI_REG_ORIGIN          => {
                self.vi.reg_origin = word & 0xff_ffff;  // only 24 bits
                // println!("VRAM at {:#x}", word);
                self.vi.update();
                self.interface.send(IfOutput::Update(
                    self.ram[self.vi.vram_start..self.vi.vram_end].to_vec()));
            },
            VI_REG_H_WIDTH         => {
                self.vi.reg_width = word & 0xfff;
                self.vi.update();
                self.interface.send(IfOutput::SetSize(
                    self.vi.video_width, self.vi.video_height));
            },
            VI_REG_V_INTR          => self.vi.reg_intr = word & 0x3ff,
            VI_REG_CURRENT         => self.vi.reg_current = word & 0x3ff,
            VI_REG_BURST           => self.vi.reg_burst = word & 0x3fff_ffff,
            VI_REG_V_SYNC          => self.vi.reg_v_sync = word & 0x3ff,
            VI_REG_H_SYNC          => self.vi.reg_h_sync = word & 0x1f_ffff,
            VI_REG_LEAP            => self.vi.reg_leap = word & 0xfff_ffff,
            VI_REG_H_START         => self.vi.reg_h_start = word & 0x3ff_ffff,
            VI_REG_V_START         => self.vi.reg_v_start = word & 0x3ff_ffff,
            VI_REG_V_BURST         => self.vi.reg_v_burst = word & 0x3ff_ffff,
            VI_REG_X_SCALE         => self.vi.reg_x_scale = word & 0xfff_ffff,
            VI_REG_Y_SCALE         => {
                self.vi.reg_y_scale = word & 0xfff_ffff;
                self.interface.send(IfOutput::Update(
                    self.ram[self.vi.vram_start..self.vi.vram_end].to_vec()));
            },
            // Audio interface
            AI_REG_DRAM_ADDR       => self.ai.reg_dram_addr = word & 0xff_ffff,
            AI_REG_LEN             => self.ai.reg_len = word & 0x3_ffff,
            AI_REG_CONTROL         => self.ai.reg_control = word & 0x1,
            AI_REG_STATUS          => self.ai.reg_status = word,
            AI_REG_DACRATE         => self.ai.reg_dacrate = word & 0x3fff,
            AI_REG_BITRATE         => self.ai.reg_bitrate = word & 0xf,
            // Peripheral interface
            PI_REG_DRAM_ADDR       => self.pi.reg_dram_addr = word & 0xff_ffff,
            PI_REG_CART_ADDR       => self.pi.reg_cart_addr = word,
            PI_REG_RD_LEN          => self.pi.reg_rd_len = word & 0xff_ffff,
            PI_REG_WR_LEN          => {
                self.pi.reg_wr_len = word & 0xff_ffff;
                // DMA transfer ROM -> main memory
                let ram_start = self.pi.reg_dram_addr as usize / 4;
                let rom_start = self.pi.reg_cart_addr as usize - 0x1000_0000;
                let length = (self.pi.reg_wr_len + 1) as usize;
                println!("DMA transfer: {:#x} bytes from ROM {:#x} to {:#x}",
                         length, rom_start, ram_start);
                for i in 0..length/4 {
                    self.ram[ram_start + i] =
                        BigEndian::read_u32(&self.cart_rom[rom_start + 4*i..]);
                }
            },
            PI_REG_STATUS          => { /* TODO */ },
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
            SI_REG_PIF_ADDR_RD64B  => self.si.dma_read(&mut self.ram),
            SI_REG_PIF_ADDR_WR64B  => self.si.dma_write(
                &self.ram, self.interface.get_input_state()),
            SI_REG_STATUS          => { /* TODO */ }
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
