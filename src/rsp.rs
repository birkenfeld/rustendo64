#[derive(Default)]
pub struct Rsp;

impl Rsp {
    // TODO: Read general regs
    pub fn read_status_reg(&self) -> u32 {
        1 // TODO: Properly implement this
    }
}

#[derive(Default)]
pub struct SpRam {
    pub dmem: Vec<u32>,
    pub imem: Vec<u32>,
}

#[derive(Default, Debug)]
pub struct Sp {
    pub reg_mem_addr:  u32,
    pub reg_dram_addr: u32,
    pub reg_rd_len:    u32,
    pub reg_wr_len:    u32,
    pub reg_status:    u32,
    pub reg_dma_full:  u32,
    pub reg_dma_busy:  u32,
    pub reg_semaphore: u32,
    pub reg_pc:        u32,
    pub reg_ibist:     u32,
}

#[derive(Default, Debug)]
pub struct Dp {
    // command regs
    pub reg_start:        u32,
    pub reg_end:          u32,
    pub reg_current:      u32,
    pub reg_status:       u32,
    pub reg_clock:        u32,
    pub reg_bufbusy:      u32,
    pub reg_pipebusy:     u32,
    pub reg_tmem:         u32,

    // span regs
    pub reg_tbist:        u32,
    pub reg_test_mode:    u32,
    pub reg_buftest_addr: u32,
    pub reg_buftest_data: u32,
}
