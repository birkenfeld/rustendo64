use std::fmt;
use std::sync::{Arc, Condvar, Mutex, RwLock};
use std::sync::atomic::AtomicBool;
#[cfg(debug_assertions)]
use ansi_term;

use bus::{Bus, RamAccess};
use bus::mem_map::*;
use rsp::cp2::Cp2;
use vr4k::instruction::*;
use vr4k::types::*;
use debug::DebugSpecList;
use util::bit_set;

/// Maps RSP CP0 register indices to bus addresses.
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


pub struct Rsp {
    regs:      R4300Common,
    cp2:       Cp2,
    broke:     bool,
    #[allow(dead_code)] run_bit:   Arc<AtomicBool>,
    run_cond:  Arc<Condvar>,
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
        self.read_word_raw(bus, phys_addr as u32)
    }

    fn read_word(&self, bus: &RspBus, virt_addr: u64) -> u32 {
        let phys_addr = self.translate_addr(virt_addr);
        let res = self.read_word_raw(bus, phys_addr as u32);
        self.debug_read(phys_addr, res);
        res
    }

    fn write_word(&mut self, bus: &mut RspBus, virt_addr: u64, word: u32) {
        let phys_addr = self.translate_addr(virt_addr);
        self.write_word_raw(bus, phys_addr as u32, word);
    }

    fn translate_addr(&self, virt_addr: u64) -> u64 {
        if virt_addr >= 0x2000 {
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
                let reg_addr = COP0_REG_MAP[instr.rd()];
                let data = self.read_word_raw(bus, reg_addr);
                self.debug_read(reg_addr as u64, data);
                self.write_gpr(instr.rt(), data as i32 as u64);
            }
            MT => {
                let reg_addr = COP0_REG_MAP[instr.rd()];
                let data = self.read_gpr(instr.rt()) as u32;
                self.write_word_raw(bus, reg_addr, data);
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
    pub fn new(debug: DebugSpecList, run_bit: Arc<AtomicBool>, run_cond: Arc<Condvar>) -> Self {
        let mut rsp = Rsp {
            regs:      R4300Common::default(),
            cp2:       Cp2::default(),
            broke:     false,
            run_bit:   run_bit,
            run_cond:  run_cond,
        };
        rsp.mut_regs().debug_specs = debug;
        rsp
    }

    #[cfg(not(debug_assertions))]
    pub fn new(_: DebugSpecList, run_bit: Arc<AtomicBool>, run_cond: Arc<Condvar>) -> Self {
        Rsp {
            regs:      R4300Common::default(),
            cp2:       Cp2::default(),
            broke:     false,
            run_bit:   run_bit,
            run_cond:  run_cond,
        }
    }

    pub fn wait_for_start(&self) {
        let mutex = Mutex::new(());
        let _ = self.run_cond.wait(mutex.lock().unwrap()).unwrap();
    }

    pub fn run_sequence(&mut self, bus: &mut RspBus, n: usize) {
        self.regs.pc = (bus.read_word(SP_REG_PC).unwrap() & 0xfff) as u64;
        self.broke = false;
        for _ in 0..n {
            if self.broke { break; }
            self.run_instruction(bus);
        }
    }

    // Helpers

    fn read_word_raw(&self, bus: &RspBus, phys_addr: u32) -> u32 {
        match bus.read_word(phys_addr) {
            Ok(res) => res,
            Err(desc) => {
                self.bug(format!("{}: {:#x}", desc, phys_addr));
            }
        }
    }

    fn write_word_raw(&self, bus: &mut RspBus, phys_addr: u32, data: u32) {
        self.debug_write(phys_addr as u64, data);
        if let Err(desc) = bus.write_word(phys_addr, data) {
            self.bug(format!("{}: {:#x}", desc, phys_addr));
        }
    }
}
