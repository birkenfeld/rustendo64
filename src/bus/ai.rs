use mi;
use bus::IoResult;
use mem::RamAccess;
use mem_map::*;
use ui::{UiChannel, UiOutput};

#[derive(Default)]
pub struct Ai {
    reg_dram_addr:   u32,
    reg_len:         u32,
    reg_control:     u32,
    reg_status:      u32,
    reg_dacrate:     u32,
    reg_bitrate:     u32,

    // Number of DMA requests that haven't been acknowledged
    // by an interrupt yet.
    pub dma_pending: usize,
}

pub const NTSC_DAC_FREQ: u32 = 48681812; // 48.681812MHz

impl Ai {
    pub fn read_reg(&self, addr: u32) -> IoResult<u32> {
        Ok(match addr {
            AI_REG_DRAM_ADDR  => self.reg_dram_addr,
            AI_REG_LEN        => if self.reg_status & 0x8000_0001 != 0 { self.reg_len } else { 0 },
            AI_REG_STATUS     => self.reg_status,
            _ => return Err("Unsupported AI register")
        })
    }

    pub fn write_reg<R: RamAccess>(&mut self, addr: u32, word: u32, mi: &mi::Mi,
                                   ram: &mut R, ui: &UiChannel) -> IoResult<()> {
        Ok(match addr {
            AI_REG_DRAM_ADDR  => self.reg_dram_addr = word & 0xff_ffff,
            AI_REG_LEN        => {
                self.reg_len = word & 0x3_ffff;
                self.dma_write(ram, ui);
            },
            AI_REG_CONTROL    => self.reg_control = word & 0x1,
            AI_REG_STATUS     => {
                mi.clear_interrupt(mi::Intr::AI);
            },
            AI_REG_DACRATE    => self.reg_dacrate = word & 0x3fff,
            AI_REG_BITRATE    => self.reg_bitrate = word & 0xf,
            _ => return Err("Unsupported AI register")
        })
    }

    pub fn dma_write<R: RamAccess>(&mut self, ram: &mut R, ui: &UiChannel) {
        let length = self.reg_len as usize / 4;
        if length == 0 {
            return;
        }
        self.reg_status = 0x4000_0000;
        let data = ram.read_range(self.reg_dram_addr as usize / 4, length);
        let freq = NTSC_DAC_FREQ / (self.reg_dacrate + 1);
        // println!("Audio DMA write: {} samples from {:#x}, freq={}",
        //          length, self.reg_dram_addr, freq);
        self.reg_status = 0x8000_0001;
        self.dma_pending += 1;
        ui.send(UiOutput::Audio(freq, data));
    }

    pub fn buffer_empty(&mut self, mi: &mi::Mi) {
        if self.dma_pending > 0 {
            self.dma_pending -= 1;
            self.reg_status &= !0x8000_0001;
            mi.set_interrupt(mi::Intr::AI);
        }
    }
}
