use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

use bus::mi;
use bus::mem_map::*;
use bus::{IoResult, RamAccess};
use util::{bit_set, clear_or_set_bit};

#[derive(Default)]
pub struct SpRegs {
    reg_mem_addr:  u32,
    reg_dram_addr: u32,
    reg_rd_len:    u32,
    reg_wr_len:    u32,
    reg_status:    u32,
    reg_dma_full:  u32,
    reg_dma_busy:  u32,
    reg_semaphore: AtomicBool,
    reg_pc:        AtomicUsize,
    reg_ibist:     u32,

    run:           Arc<AtomicBool>,
}

impl SpRegs {
    pub fn new(rsp_sync: Arc<AtomicBool>) -> SpRegs {
        SpRegs { run: rsp_sync, ..SpRegs::default() }
    }

    pub fn power_on_reset(&mut self) {
        self.reg_status |= 0x1;
    }

    pub fn read_reg(&self, addr: u32) -> IoResult<u32> {
        Ok(match addr {
            SP_REG_MEM_ADDR   => self.reg_mem_addr,
            SP_REG_DRAM_ADDR  => self.reg_dram_addr,
            SP_REG_RD_LEN     => self.reg_rd_len,
            SP_REG_WR_LEN     => self.reg_wr_len,
            SP_REG_STATUS     => self.reg_status,
            SP_REG_DMA_FULL   => self.reg_dma_full,
            SP_REG_DMA_BUSY   => self.reg_dma_busy,
            SP_REG_SEMAPHORE  => self.reg_semaphore.swap(true, Ordering::SeqCst) as u32,
            SP_REG_PC         => self.reg_pc.load(Ordering::SeqCst) as u32,
            SP_REG_IBIST      => self.reg_ibist,
            _ => return Err("Unsupported RSP register")
        })
    }

    pub fn write_reg<R, S>(&mut self, addr: u32, word: u32, mi: &mi::Mi,
                           ram: &mut R, spram: &mut S) -> IoResult<()>
        where R: RamAccess, S: RamAccess
    {
        Ok(match addr {
            SP_REG_MEM_ADDR   => self.reg_mem_addr = word & 0x1fff,
            SP_REG_DRAM_ADDR  => self.reg_dram_addr = word & 0xff_ffff,
            SP_REG_RD_LEN     => {
                // DRAM -> SPRAM
                self.reg_rd_len = word;
                println!("RSP: DMA {:#x} bytes from RAM {:#x} to SPRAM {:#x}",
                         word, self.reg_dram_addr, self.reg_mem_addr);
                self.dma(ram, spram, word as usize);
            },
            SP_REG_WR_LEN     => {
                // SPRAM -> DRAM
                self.reg_wr_len = word;
                println!("RSP: DMA {:#x} bytes from SPRAM {:#x} to RAM {:#x}",
                         word, self.reg_mem_addr, self.reg_dram_addr);
                self.dma(spram, ram, word as usize);
            },
            SP_REG_STATUS     => {
                if bit_set(word, 0) {
                    println!("RSP: starting.");
                    self.run.store(true, Ordering::SeqCst);
                }
                if bit_set(word, 1) {
                    self.run.store(false, Ordering::SeqCst);
                }
                // halt
                clear_or_set_bit(&mut self.reg_status, 0, word, 0, 1);
                // break
                // NOTE: bit 31 does nothing in real HW, we use it to set
                // the break bit in a consistent fashion from the RSP
                clear_or_set_bit(&mut self.reg_status, 1, word, 2, 31);
                if bit_set(word, 3) {
                    mi.clear_interrupt(mi::Intr::SP);
                }
                if bit_set(word, 4) {
                    mi.set_interrupt(mi::Intr::SP);
                }
                // single step
                clear_or_set_bit(&mut self.reg_status, 5, word, 5, 6);
                // interrupt on break
                clear_or_set_bit(&mut self.reg_status, 6, word, 7, 8);
                // signals
                for i in 7..15 {
                    clear_or_set_bit(&mut self.reg_status, i, word,
                                     2*i - 5, 2*i - 4);
                }
            }
            SP_REG_SEMAPHORE  => self.reg_semaphore.store(false, Ordering::SeqCst),
            SP_REG_PC         => self.reg_pc.store(word as usize & 0xfff, Ordering::SeqCst),
            SP_REG_IBIST      => self.reg_ibist = word & 0x7,
            _ => return Err("Unsupported RSP register")
        })
    }

    fn dma<R: RamAccess, S: RamAccess>(&mut self, from: &mut R, to: &mut S, spec: usize) {
        let length = ((spec & 0xfff) + 1) / 4;
        let count = ((spec >> 12) & 0xff) + 1;
        let skip = ((spec >> 20) & 0xfff) / 4;
        let mut from_index = self.reg_dram_addr as usize / 4;
        let mut to_index = self.reg_mem_addr as usize / 4;
        // Transfer count blocks of length, skipping skip on the spmem side
        for _ in 0..count {
            let data = from.read_range(from_index, length);
            to.write_range(to_index, &data);
            from_index += length;
            to_index += length + skip;
        }
    }
}

#[derive(Default, Debug)]
pub struct DpRegs {
    // command regs
    reg_start:        u32,
    reg_end:          u32,
    reg_current:      u32,
    reg_status:       u32,
    reg_clock:        u32,
    reg_bufbusy:      u32,
    reg_pipebusy:     u32,
    reg_tmem:         u32,

    // span regs
    reg_tbist:        u32,
    reg_test_mode:    u32,
    reg_buftest_addr: u32,
    reg_buftest_data: u32,
}

impl DpRegs {
    pub fn read_reg(&self, addr: u32) -> IoResult<u32> {
        Ok(match addr {
            DPC_REG_DMA_START      => self.reg_start,
            DPC_REG_DMA_END        => self.reg_end,
            DPC_REG_CURRENT        => self.reg_current,
            DPC_REG_STATUS         => self.reg_status,
            DPC_REG_CLOCK          => self.reg_clock,
            DPC_REG_BUFBUSY        => self.reg_bufbusy,
            DPC_REG_PIPEBUSY       => self.reg_pipebusy,
            DPC_REG_TMEM           => self.reg_tmem,
            DPS_REG_TBIST          => self.reg_tbist,
            DPS_REG_TEST_MODE      => self.reg_test_mode,
            DPS_REG_BUFTEST_ADDR   => self.reg_buftest_addr,
            DPS_REG_BUFTEST_DATA   => self.reg_buftest_data,
            _ => return Err("Unsupported RDP register")
        })
    }

    pub fn write_reg(&mut self, addr: u32, word: u32) -> IoResult<()> {
        Ok(match addr {
            DPC_REG_DMA_START      => self.reg_start = word & 0xff_ffff,
            DPC_REG_DMA_END        => self.reg_end = word & 0xff_ffff,
            DPC_REG_STATUS         => {
                clear_or_set_bit(&mut self.reg_status, 0, word, 0, 1);
                clear_or_set_bit(&mut self.reg_status, 1, word, 2, 3);
                clear_or_set_bit(&mut self.reg_status, 2, word, 4, 5);
                if bit_set(word, 6) {
                    self.reg_tmem = 0;
                }
                if bit_set(word, 7) {
                    self.reg_pipebusy = 0;
                }
                if bit_set(word, 8) {
                    /* TODO */
                }
                if bit_set(word, 9) {
                    self.reg_clock = 0;
                }
            },
            DPS_REG_TBIST          => self.reg_tbist = word & 0x7,
            DPS_REG_TEST_MODE      => self.reg_test_mode = word & 0x1,
            DPS_REG_BUFTEST_ADDR   => self.reg_buftest_addr = word & 0x7f,
            DPS_REG_BUFTEST_DATA   => self.reg_buftest_data = word,
            _ => return Err("Unsupported RDP register")
        })
    }
}
