#[derive(Default)]
pub struct Rsp;

use std::mem;

use bus::mem_map::*;
use bus::mi;

const SP_RAM_SIZE: usize = 0x1000;

impl Rsp {
    // TODO: Read general regs
    pub fn read_status_reg(&self) -> u32 {
        1 // TODO: Properly implement this
    }
}

#[derive(Debug)]
pub struct Sp {
    dmem:          Box<[u32]>,
    imem:          Box<[u32]>,
    reg_mem_addr:  u32,
    reg_dram_addr: u32,
    reg_rd_len:    u32,
    reg_wr_len:    u32,
    reg_status:    u32,
    reg_dma_full:  u32,
    reg_dma_busy:  u32,
    reg_semaphore: u32,
    reg_pc:        u32,
    reg_ibist:     u32,
}

impl Default for Sp {
    fn default() -> Sp {
        Sp {
            dmem:          vec![0; SP_RAM_SIZE / 4].into_boxed_slice(),
            imem:          vec![0; SP_RAM_SIZE / 4].into_boxed_slice(),
            reg_mem_addr:  0,
            reg_dram_addr: 0,
            reg_rd_len:    0,
            reg_wr_len:    0,
            reg_status:    0,
            reg_dma_full:  0,
            reg_dma_busy:  0,
            reg_semaphore: 0,
            reg_pc:        0,
            reg_ibist:     0,
        }
    }
}

impl Sp {
    pub fn power_on_reset(&mut self) {
        self.reg_status |= 0x1;
    }

    pub fn read_reg(&mut self, addr: u32) -> Result<u32, &'static str> {
        Ok(match addr {
            SP_REG_MEM_ADDR   => self.reg_mem_addr,
            SP_REG_DRAM_ADDR  => self.reg_dram_addr,
            SP_REG_RD_LEN     => self.reg_rd_len,
            SP_REG_WR_LEN     => self.reg_wr_len,
            SP_REG_STATUS     => self.reg_status,
            SP_REG_DMA_FULL   => self.reg_dma_full,
            SP_REG_DMA_BUSY   => self.reg_dma_busy,
            SP_REG_SEMAPHORE  => mem::replace(&mut self.reg_semaphore, 1),
            SP_REG_PC         => self.reg_pc,
            SP_REG_IBIST      => self.reg_ibist,
            _ => return Err("Unsupported RSP register")
        })
    }

    pub fn write_reg(&mut self, addr: u32, word: u32, mi: &mut mi::Mi) -> Result<(), &'static str> {
        Ok(match addr {
            SP_REG_MEM_ADDR   => self.reg_mem_addr = word & 0x1fff,
            SP_REG_DRAM_ADDR  => self.reg_dram_addr = word & 0xff_ffff,
            SP_REG_RD_LEN     => self.reg_rd_len = word,
            SP_REG_WR_LEN     => self.reg_wr_len = word,
            SP_REG_STATUS     => {
                if word & 0x1 != 0 {
                    println!("Would start RSP");
                }
                if word & 0x8 != 0 {
                    mi.clear_interrupt(mi::INTR_SP);
                }
                if word & 0x10 != 0 {
                    mi.set_interrupt(mi::INTR_SP);
                }
                /* TODO */
            }
            SP_REG_SEMAPHORE  => self.reg_semaphore = 0,
            SP_REG_PC         => self.reg_pc = word & 0xfff,
            SP_REG_IBIST      => self.reg_ibist = word & 0x7,
            _ => return Err("Unsupported RSP register")
        })
    }

    pub fn read_dmem(&self, addr: u32) -> Result<u32, &'static str> {
        Ok(self.dmem[(addr - SP_DMEM_START) as usize / 4])
    }

    pub fn read_imem(&self, addr: u32) -> Result<u32, &'static str> {
        Ok(self.imem[(addr - SP_IMEM_START) as usize / 4])
    }

    pub fn write_dmem(&mut self, addr: u32, word: u32) -> Result<(), &'static str> {
        self.dmem[(addr - SP_DMEM_START) as usize / 4] = word;
        Ok(())
    }

    pub fn write_imem(&mut self, addr: u32, word: u32) -> Result<(), &'static str> {
        self.imem[(addr - SP_IMEM_START) as usize / 4] = word;
        Ok(())
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
    pub fn read_reg(&mut self, addr: u32) -> Result<u32, &'static str> {
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

    pub fn write_reg(&mut self, addr: u32, word: u32) -> Result<(), &'static str> {
        Ok(match addr {
            DPC_REG_DMA_START      => self.reg_start = word & 0xff_ffff,
            DPC_REG_DMA_END        => self.reg_end = word & 0xff_ffff,
            DPC_REG_STATUS         => { /* TODO */ },
            DPS_REG_TBIST          => self.reg_tbist = word & 0x7,
            DPS_REG_TEST_MODE      => self.reg_test_mode = word & 0x1,
            DPS_REG_BUFTEST_ADDR   => self.reg_buftest_addr = word & 0x7f,
            DPS_REG_BUFTEST_DATA   => self.reg_buftest_data = word,
            _ => return Err("Unsupported RDP register")
        })
    }
}
