use std::cmp::min;
use byteorder::{BigEndian, ByteOrder};

use mi;
use {IoResult, RamAccess};
use mem_map::*;
use util::bit_set;

// From cen64.
const CIC_SEED_NUS_5101: u32 = 0x0000AC00;
const CIC_SEED_NUS_6101: u32 = 0x00063F3F;
const CIC_SEED_NUS_6102: u32 = 0x00023F3F;
const CIC_SEED_NUS_6103: u32 = 0x0002783F;
const CIC_SEED_NUS_6105: u32 = 0x0002913F;
const CIC_SEED_NUS_6106: u32 = 0x0002853F;
const CIC_SEED_NUS_8303: u32 = 0x0000DD00;

const CRC_NUS_5101: u32 = 0x587BD543;
const CRC_NUS_6101: u32 = 0x6170A4A1;
const CRC_NUS_6102: u32 = 0x90BB6CB5;
const CRC_NUS_6103: u32 = 0x0B050EE0;
const CRC_NUS_6105: u32 = 0x98BC2C86;
const CRC_NUS_6106: u32 = 0xACC8580A;
const CRC_NUS_8303: u32 = 0x0E018159;

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

    pub fn write_reg<R>(&mut self, addr: u32, word: u32, mi: &mi::Mi,
                        ram: &mut R) -> IoResult<()>
        where R: RamAccess
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

    pub fn dma_read<R: RamAccess>(&mut self, ram: &mut R, word: u32) {
        self.reg_wr_len = word & 0xff_ffff;
        // DMA transfer ROM -> main memory
        let ram_start = self.reg_dram_addr as usize / 4;
        let rom_start = self.reg_cart_addr as usize - 0x1000_0000;
        let length = (self.reg_wr_len + 1) as usize;
        // Some ROMs read past the end of the file...
        if rom_start >= self.cart_rom.len() { return; }
        let length = min(length, self.cart_rom.len() - rom_start);
        println!("DMA transfer: {:#x} bytes from ROM {:#x} to {:#x}",
                 length, rom_start, ram_start);
        for i in 0..length/4 {
            ram.write_word(ram_start + i,
                           BigEndian::read_u32(&self.cart_rom[rom_start + 4*i..]));
        }
    }

    pub fn get_cic_seed(&self) -> Option<u32> {
        let crc = crc32(&self.cart_rom[0x40..0x1000]);
        let aleck64crc = crc32(&self.cart_rom[0x40..0xc00]);

        if aleck64crc == CRC_NUS_5101 {
            return Some(CIC_SEED_NUS_5101);
        }
        match crc {
            CRC_NUS_6101 => Some(CIC_SEED_NUS_6101),
            CRC_NUS_6102 => Some(CIC_SEED_NUS_6102),
            CRC_NUS_6103 => Some(CIC_SEED_NUS_6103),
            CRC_NUS_6105 => Some(CIC_SEED_NUS_6105),
            CRC_NUS_6106 => Some(CIC_SEED_NUS_6106),
            CRC_NUS_8303 => Some(CIC_SEED_NUS_8303),
            _            => None
        }
    }
}

fn crc32(data: &[u8]) -> u32 {
    // no use precalculating the table, we need it only once
    let mut table = [0u32; 256];
    for n in 0..256 {
        let mut c = n as u32;
        for _ in 0..8 {
            if c & 1 != 0 {
                c = (c >> 1) ^ 0xedb88320;
            } else {
                c >>= 1;
            }
        }
        table[n] = c;
    }
    let mut c = 0 ^ 0xffff_ffff;
    for b in data {
        c = table[(c ^ *b as u32) as usize & 0xff] ^ (c >> 8);
    }
    c ^ 0xffff_ffff
}
