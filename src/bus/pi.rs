use std::cmp::min;
use byteorder::{BigEndian, ByteOrder};

#[derive(Default, Debug)]
pub struct Pi {
    pub cart_rom: Vec<u8>,
    pub reg_dram_addr: u32,
    pub reg_cart_addr: u32,
    pub reg_rd_len: u32,
    pub reg_wr_len: u32,
    pub reg_status: u32,
    pub reg_bsd_dom1_lat: u32,
    pub reg_bsd_dom1_pwd: u32,
    pub reg_bsd_dom1_pgs: u32,
    pub reg_bsd_dom1_rls: u32,
    pub reg_bsd_dom2_lat: u32,
    pub reg_bsd_dom2_pwd: u32,
    pub reg_bsd_dom2_pgs: u32,
    pub reg_bsd_dom2_rls: u32,
}

impl Pi {
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
}
