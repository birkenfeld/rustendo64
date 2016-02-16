use std::cmp::min;
use byteorder::{BigEndian, ByteOrder};

use bus::IoResult;
use bus::cic;
use bus::mi;
use bus::mem_map::*;
use util::bit_set;

#[derive(Default, Debug)]
pub struct Pi {
    cart_rom: Box<[u8]>,
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

impl Pi {
    pub fn new(cart_rom: Box<[u8]>) -> Pi {
        Pi { cart_rom: cart_rom, ..Pi::default() }
    }

    pub fn read_reg(&self, addr: u32) -> IoResult<u32> {
        Ok(match addr {
            PI_REG_DRAM_ADDR       => self.reg_dram_addr,
            PI_REG_CART_ADDR       => self.reg_cart_addr,
            PI_REG_RD_LEN          => self.reg_rd_len,
            PI_REG_WR_LEN          => self.reg_wr_len,
            PI_REG_STATUS          => self.reg_status,
            PI_REG_BSD_DOM1_LAT    => self.reg_bsd_dom1_lat,
            PI_REG_BSD_DOM1_PWD    => self.reg_bsd_dom1_pwd,
            PI_REG_BSD_DOM1_PGS    => self.reg_bsd_dom1_pgs,
            PI_REG_BSD_DOM1_RLS    => self.reg_bsd_dom1_rls,
            PI_REG_BSD_DOM2_LAT    => self.reg_bsd_dom2_lat,
            PI_REG_BSD_DOM2_PWD    => self.reg_bsd_dom2_pwd,
            PI_REG_BSD_DOM2_PGS    => self.reg_bsd_dom2_pgs,
            PI_REG_BSD_DOM2_RLS    => self.reg_bsd_dom2_rls,
            _ => return Err("Unsupported PI register")
        })
    }

    pub fn write_reg(&mut self, addr: u32, word: u32, mi: &mut mi::Mi,
                     ram: &mut [u32]) -> IoResult<()>
    {
        Ok(match addr {
            PI_REG_DRAM_ADDR       => self.reg_dram_addr = word & 0xff_ffff,
            PI_REG_CART_ADDR       => self.reg_cart_addr = word,
            PI_REG_RD_LEN          => self.reg_rd_len = word & 0xff_ffff,
            PI_REG_WR_LEN          => {
                self.dma_read(ram, word);
                mi.set_interrupt(mi::Intr::PI);
            },
            PI_REG_STATUS          => {
                if bit_set(word, 0) {
                    /* TODO: reset controller */
                }
                if bit_set(word, 1) {
                    mi.clear_interrupt(mi::Intr::PI);
                }
            },
            PI_REG_BSD_DOM1_LAT    => self.reg_bsd_dom1_lat = word & 0xff,
            PI_REG_BSD_DOM1_PWD    => self.reg_bsd_dom1_pwd = word & 0xff,
            PI_REG_BSD_DOM1_PGS    => self.reg_bsd_dom1_pgs = word & 0xf,
            PI_REG_BSD_DOM1_RLS    => self.reg_bsd_dom1_rls = word & 0x3,
            PI_REG_BSD_DOM2_LAT    => self.reg_bsd_dom2_lat = word & 0xff,
            PI_REG_BSD_DOM2_PWD    => self.reg_bsd_dom2_pwd = word & 0xff,
            PI_REG_BSD_DOM2_PGS    => self.reg_bsd_dom2_pgs = word & 0xf,
            PI_REG_BSD_DOM2_RLS    => self.reg_bsd_dom2_rls = word & 0x3,
            _ => return Err("Unsupported PI register")
        })
    }

    pub fn read_rom(&self, addr: u32) -> IoResult<u32> {
        let rel_addr = (addr - CART_ROM_START) as usize;
        if rel_addr > self.cart_rom.len() - 4 {
            println!("Warning: reading {} bytes beyond end of ROM data, returning zeros.",
                     (4 + rel_addr) - self.cart_rom.len());
            Ok(0)
        } else {
            Ok(BigEndian::read_u32(&self.cart_rom[rel_addr..]))
        }
    }

    pub fn dma_read(&mut self, ram: &mut [u32], word: u32) {
        self.reg_wr_len = word & 0xff_ffff;
        // DMA transfer ROM -> main memory
        let ram_start = self.reg_dram_addr as usize / 4;
        let rom_start = self.reg_cart_addr as usize - 0x1000_0000;
        let length = (self.reg_wr_len + 1) as usize;
        // Some ROMs read past the end of the file...
        let length = min(length, self.cart_rom.len() - rom_start);
        println!("DMA transfer: {:#x} bytes from ROM {:#x} to {:#x}",
                 length, rom_start, ram_start);
        for i in 0..length/4 {
            ram[ram_start + i] =
                BigEndian::read_u32(&self.cart_rom[rom_start + 4*i..]);
        }
    }

    pub fn get_cic_seed(&self) -> Option<u32> {
        cic::get_cic_seed(&self.cart_rom)
    }
}
