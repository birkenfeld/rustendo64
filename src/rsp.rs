use std::sync::atomic::{AtomicBool, Ordering};

use bus::mi;
use bus::IoResult;
use bus::mem_map::*;
use util::{bit_set, clear_or_set_bit, clear_bit};

#[derive(Debug)]
pub struct Sp {
    reg_mem_addr:  u32,
    reg_dram_addr: u32,
    reg_rd_len:    u32,
    reg_wr_len:    u32,
    reg_status:    u32,
    reg_dma_full:  u32,
    reg_dma_busy:  u32,
    reg_semaphore: AtomicBool,
    reg_pc:        u32,
    reg_ibist:     u32,
}

impl Default for Sp {
    fn default() -> Sp {
        Sp {
            reg_mem_addr:  0,
            reg_dram_addr: 0,
            reg_rd_len:    0,
            reg_wr_len:    0,
            reg_status:    0,
            reg_dma_full:  0,
            reg_dma_busy:  0,
            reg_semaphore: AtomicBool::default(),
            reg_pc:        0,
            reg_ibist:     0,
        }
    }
}

impl Sp {
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
            SP_REG_PC         => self.reg_pc,
            SP_REG_IBIST      => self.reg_ibist,
            _ => return Err("Unsupported RSP register")
        })
    }

    pub fn write_reg(&mut self, addr: u32, word: u32, mi: &mi::Mi) -> IoResult<()> {
        Ok(match addr {
            SP_REG_MEM_ADDR   => self.reg_mem_addr = word & 0x1fff,
            SP_REG_DRAM_ADDR  => self.reg_dram_addr = word & 0xff_ffff,
            SP_REG_RD_LEN     => self.reg_rd_len = word,
            SP_REG_WR_LEN     => self.reg_wr_len = word,
            SP_REG_STATUS     => {
                if bit_set(word, 0) {
                    println!("Would start RSP");
                }
                // halt
                clear_or_set_bit(&mut self.reg_status, 0, word, 0, 1);
                if bit_set(word, 2) {
                    clear_bit(&mut self.reg_status, 1);
                }
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
            SP_REG_PC         => self.reg_pc = word & 0xfff,
            SP_REG_IBIST      => self.reg_ibist = word & 0x7,
            _ => return Err("Unsupported RSP register")
        })
    }
}

#[derive(Default, Debug)]
pub struct Dp {
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

impl Dp {
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
