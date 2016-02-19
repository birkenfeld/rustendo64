use std::fmt;
use std::sync::{Arc, RwLock};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::thread;
use std::time::Duration;
#[cfg(debug_assertions)]
use ansi_term;

use bus::mi;
use bus::Bus;
use bus::{IoResult, RamAccess};
use bus::mem_map::*;
use rsp::cp2::Cp2;
use vr4k::instruction::*;
use vr4k::types::*;
use debug::DebugSpecList;
use util::{bit_set, clear_or_set_bit};


#[derive(Default)]
pub struct Rsp {
    regs:   R4300Common,
    run:    Arc<AtomicBool>,
    cp2:    Cp2,
    broke:  bool,
}

pub type RspBus<'c> = Bus<'c, &'c RwLock<Box<[u32]>>, &'c mut [u32]>;


impl fmt::Debug for Rsp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for row in 0..8 {
            for col in 0..4 {
                let i = row + col * 8;
                try!(write!(f, "  {:2}:{} = {:016x}", i, REG_NAMES[i], self.regs.gpr[i]));
            }
            try!(write!(f, "\n"));
        }
        write!(f, "     pc = {:016x}\n", self.regs.pc)
    }
}

impl<'c> R4300<'c> for Rsp {
    type Bus = RspBus<'c>;

    fn read_instr(&self, bus: &RspBus, virt_addr: u64) -> u32 {
        let phys_addr = self.translate_addr(virt_addr + 0x1000);
        match bus.read_word(phys_addr as u32) {
            Ok(res) => res,
            Err(desc) => {
                self.bug(format!("{}: ({:#x}) {:#x}", desc, virt_addr, phys_addr));
            }
        }
    }

    fn read_word(&self, bus: &RspBus, virt_addr: u64) -> u32 {
        let phys_addr = self.translate_addr(virt_addr);
        match bus.read_word(phys_addr as u32) {
            Ok(res) => {
                self.debug_read(phys_addr, res);
                res
            }
            Err(desc) => {
                self.bug(format!("{}: ({:#x}) {:#x}", desc, virt_addr, phys_addr));
            }
        }
    }

    fn write_word(&mut self, bus: &mut RspBus, virt_addr: u64, word: u32) {
        let phys_addr = self.translate_addr(virt_addr);
        self.debug_write(phys_addr, word);
        if let Err(desc) = bus.write_word(phys_addr as u32, word) {
            self.bug(format!("{}: ({:#x}) {:#x}", desc, virt_addr, phys_addr));
        }
    }

    fn translate_addr(&self, virt_addr: u64) -> u64 {
        if virt_addr >= 0x2000 {  // TODO: limits
            self.bug(format!("Cannot access memory at {:#x} from RSP", virt_addr));
        }
        virt_addr + 0x0400_0000
    }

    fn check_interrupts(&mut self, _: &mut RspBus) { }

    fn ll_handler(&mut self, _: u64) {
        self.bug(format!("#UD: LL operation undefined for RSP"))
    }

    fn sc_handler<T: MemFmt<'c, Self>>(&mut self, _: &mut RspBus, _: &Instruction, _: u64, _: T) {
        self.bug(format!("#UD: SC operation undefined for RSP"))
    }

    fn get_desc(&self) -> &'static str { "RSP" }
    fn get_regs(&self) -> &R4300Common { &self.regs }
    fn mut_regs(&mut self) -> &mut R4300Common { &mut self.regs }
    fn get_pc_mask(&self) -> u64 { 0xfff }

    #[cfg(debug_assertions)]
    fn get_debug_color(&self) -> ansi_term::Colour {
        ansi_term::Colour::Green
    }

    #[cfg(debug_assertions)]
    fn cp0_dump(&self) {
    }

    #[cfg(debug_assertions)]
    fn cp1_dump(&self) {
    }

    #[cfg(debug_assertions)]
    fn cp2_dump(&self) {
        println!("{:#?}", self.cp2);
    }

    fn dispatch_op(&mut self, _: &mut RspBus, instr: &Instruction) {
        match instr.opcode() {
            // LWC2   => self.mem_load_vec::<u32> (bus, instr),
            // SWC2   => self.mem_store_vec::<u32>(bus, instr),
            _      => self.bug(format!("#UD: I {:#b} -- {:?}", instr.0, instr))
        }
    }

    fn dispatch_special_op(&mut self, bus: &mut RspBus, instr: &Instruction) {
        match instr.special_op() {
            BREAK => {
                let cur_status = bus.read_word(SP_REG_STATUS).unwrap();
                let mut write_status = 0_u32;
                // set interrupt if intr on break is enabled
                if bit_set(cur_status, 6) {
                    write_status |= 1 << 4;
                }
                // set halt
                write_status |= 1 << 1;
                // set break
                write_status |= 1 << 31;
                bus.write_word(SP_REG_STATUS, write_status).unwrap();
                println!("RSP: break.");
                self.broke = true;
            }
            _     => self.bug(format!("#UD: I {:#b} -- {:?}", instr.0, instr))
        }
    }

    fn dispatch_cop0_op(&mut self, bus: &mut RspBus, instr: &Instruction) {
        match instr.cop_op() {
            MF => {
                let reg_addr = COP0_REG_MAP[instr.rd()];  // TODO: check range
                let data = match bus.read_word(reg_addr) {
                    Ok(res) => {
                        self.debug_read(reg_addr as u64, res);
                        res
                    }
                    Err(desc) => {
                        self.bug(format!("{}: {:#x}", desc, reg_addr));
                    }
                };
                self.write_gpr(instr.rt(), data as i32 as u64);
            }
            MT => {
                let reg_addr = COP0_REG_MAP[instr.rd()];  // TODO: check range
                let data = self.read_gpr(instr.rt()) as u32;
                self.debug_write(reg_addr as u64, data);
                if let Err(desc) = bus.write_word(reg_addr, data) {
                    self.bug(format!("{}: {:#x}", desc, reg_addr));
                }
            }
            _  => self.bug(format!("#CU CP0: I {:#b} -- {:?}", instr.0, instr))
        }
    }

    fn dispatch_cop1_op(&mut self, _: &mut RspBus, instr: &Instruction) {
        self.bug(format!("#CU CP1: I {:#b} -- {:?}", instr.0, instr))
    }

    fn dispatch_cop2_op(&mut self, _: &mut RspBus, instr: &Instruction) {
        self.bug(format!("#CU CP2: I {:#b} -- {:?}", instr.0, instr))
    }
}

impl Rsp {
    #[cfg(debug_assertions)]
    pub fn new(debug: DebugSpecList, run: Arc<AtomicBool>) -> Self {
        let mut rsp = Self::default();
        rsp.run = run;
        rsp.mut_regs().debug_specs = debug;
        rsp
    }

    #[cfg(not(debug_assertions))]
    pub fn new(_: DebugSpecList, run: Arc<AtomicBool>) -> Self {
        Rsp { run: run, ..Self::default() }
    }

    pub fn wait_for_start(&self) {
        /* TODO: make this a condvar? */
        while !self.run.load(Ordering::SeqCst) {
            thread::sleep(Duration::new(0, 1_000));
        }
    }

    pub fn run_sequence(&mut self, bus: &mut RspBus, n: usize) {
        self.regs.pc = (bus.read_word(SP_REG_PC).unwrap() & 0xfff) as u64;
        self.broke = false;
        for _ in 0..n {
            if self.broke { break; }
            self.run_instruction(bus);
        }
    }
}

pub const COP0_REG_MAP: [u32; 16] = [
    SP_REG_MEM_ADDR,
    SP_REG_DRAM_ADDR,
    SP_REG_RD_LEN,
    SP_REG_WR_LEN,
    SP_REG_STATUS,
    SP_REG_DMA_FULL,
    SP_REG_DMA_BUSY,
    SP_REG_SEMAPHORE,
    DPC_REG_DMA_START,
    DPC_REG_DMA_END,
    DPC_REG_CURRENT,
    DPC_REG_STATUS,
    DPC_REG_CLOCK,
    DPC_REG_BUFBUSY,
    DPC_REG_PIPEBUSY,
    DPC_REG_TMEM,
];


#[derive(Default)]
pub struct Sp {
    reg_mem_addr:  u32,
    reg_dram_addr: u32,
    reg_rd_len:    u32,
    reg_wr_len:    u32,
    reg_status:    u32,
    reg_dma_full:  u32,
    reg_dma_busy:  u32,
    reg_semaphore: AtomicBool,
    pub reg_pc:    AtomicUsize,
    reg_ibist:     u32,
    pub run:       Arc<AtomicBool>,
}

impl Sp {
    pub fn new(rsp_sync: Arc<AtomicBool>) -> Sp {
        Sp { run: rsp_sync, ..Sp::default() }
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
