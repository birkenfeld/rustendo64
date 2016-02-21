use std::sync::{Arc, Condvar, RwLock};
use std::sync::atomic::{AtomicBool, Ordering};

use mem::RamAccess;
use mem_map::*;
use mi;
use vi::Vi;
use si::Si;
use pi::Pi;
use mi::Mi;
use ai::Ai;
use ri::Ri;
use rcp::{SpRegs, DpRegs};
use ui::{UiOutput, UiChannel};

macro_rules! lr {
    ($what:expr) => { $what.read().unwrap() };
}

macro_rules! lw {
    ($what:expr) => { $what.write().unwrap() };
}

pub type IoResult<T> = Result<T, &'static str>;

pub struct BusInterfaces {
    // The MI is unlocked here since it is used by everyone for interrupts,
    // so it only stores registers and uses atomics for them.
    mi: Mi,
    // All other interfaces are locked, and so can have any kind of data.
    sp: RwLock<SpRegs>,
    dp: RwLock<DpRegs>,
    vi: RwLock<Vi>,
    ai: RwLock<Ai>,
    ri: RwLock<Ri>,  // includes RDRAM regs
    pi: RwLock<Pi>,
    si: RwLock<Si>,
    // This can be unlocked.
    sp_semaphore: AtomicBool,
}

impl BusInterfaces {
    pub fn new(pif_rom: Box<[u8]>, cart_rom: Box<[u8]>,
               rsp_run_bit: Arc<AtomicBool>, rsp_run_cond: Arc<Condvar>)
               -> BusInterfaces
    {
        BusInterfaces {
            mi: Mi::default(),
            sp: RwLock::new(SpRegs::new(rsp_run_bit, rsp_run_cond)),
            dp: RwLock::new(DpRegs::default()),
            vi: RwLock::new(Vi::default()),
            ai: RwLock::new(Ai::default()),
            ri: RwLock::new(Ri::default()),
            pi: RwLock::new(Pi::new(cart_rom)),
            si: RwLock::new(Si::new(pif_rom)),
            sp_semaphore: AtomicBool::new(false),
        }
    }

    pub fn power_on_reset(&mut self) {
        // determine checksum seed and write to PIF ram
        if let Some(seed) = lr!(self.pi).get_cic_seed() {
            lw!(self.si).set_cic_seed(seed);
        } else {
            println!("Warning: no CIC seed found for this ROM");
        }
        // power-on reset configs from cen64
        lw!(self.sp).power_on_reset();
        lw!(self.ri).power_on_reset();
    }
}

#[derive(Clone)]
pub struct Bus<'i, R, S> where R: RamAccess, S: RamAccess {
    ui: UiChannel,
    ifs: &'i BusInterfaces,
    ram: R,
    spram: S,
}

impl<'i, R: RamAccess, S: RamAccess> Bus<'i, R, S> {
    pub fn new(ui: UiChannel, ifs: &'i BusInterfaces, ram: R, spram: S) -> Bus<'i, R, S> {
        Bus {
            ui: ui,
            ifs: ifs,
            ram: ram,
            spram: spram,
        }
    }

    pub fn read_word(&self, addr: u32) -> IoResult<u32> {
        if addr & 0x3 != 0 {
            return Err("unaligned access");
        }
        match addr {
            RDRAM_START     ... RDRAM_END     => Ok(self.ram.read_word(addr as usize / 4)),
            CART_ROM_START  ... CART_ROM_END  => lr!(self.ifs.pi).read_rom(addr),
            PIF_ROM_START   ... PIF_ROM_END   => lr!(self.ifs.si).read_pif_rom(addr),
            PIF_RAM_START   ... PIF_RAM_END   => lr!(self.ifs.si).read_pif_ram(addr),
            SP_DMEM_START   ... SP_IMEM_END   =>
                Ok(self.spram.read_word((addr - SP_DMEM_START) as usize / 4)),
            SP_REG_SEMAPHORE  => Ok(self.ifs.sp_semaphore.swap(true, Ordering::SeqCst) as u32),
            SP_REG_START    ... SP_REG_END    => lr!(self.ifs.sp).read_reg(addr),
            DP_REG_START    ... DP_REG_END    => lr!(self.ifs.dp).read_reg(addr),
            SI_REG_START    ... SI_REG_END    => lr!(self.ifs.si).read_reg(addr),
            MI_REG_START    ... MI_REG_END    => self.ifs.mi.read_reg(addr),
            VI_REG_START    ... VI_REG_END    => lr!(self.ifs.vi).read_reg(addr),
            AI_REG_START    ... AI_REG_END    => lr!(self.ifs.ai).read_reg(addr),
            PI_REG_START    ... PI_REG_END    => lr!(self.ifs.pi).read_reg(addr),
            RI_REG_START    ... RI_REG_END    |
            RDRAM_REG_START ... RDRAM_REG_END => lr!(self.ifs.ri).read_reg(addr),
            // Just some empty areas that nobody knows exactly what
            // could be there, but ROMs try to read from them.
            DD_ROM_START    ... DD_ROM_END    => Ok(0),
            DD_REG_START    ... DD_REG_END    => Ok(0),
            CART_SRAM_START ... CART_SRAM_END => Ok(0),
            CART_DOM2_START ... CART_DOM2_END => Ok(0),
            CART_DOM3_START ... CART_DOM3_END => Ok(0),
            _ => Err("Unsupported read memory area")
        }
    }

    pub fn write_word(&mut self, addr: u32, word: u32) -> IoResult<()> {
        if addr & 0x3 != 0 {
            return Err("unaligned access");
        }
        match addr {
            RDRAM_START     ... RDRAM_END     =>
                Ok(self.ram.write_word(addr as usize / 4, word)),
            PIF_RAM_START   ... PIF_RAM_END   =>
                lw!(self.ifs.si).write_pif_ram(addr, word, &self.ifs.mi),
            SP_DMEM_START   ... SP_IMEM_END   =>
                Ok(self.spram.write_word((addr - SP_DMEM_START) as usize / 4, word)),
            SP_REG_SEMAPHORE  => Ok(self.ifs.sp_semaphore.store(false, Ordering::SeqCst)),
            SP_REG_START    ... SP_REG_END    =>
                lw!(self.ifs.sp).write_reg(addr, word, &self.ifs.mi, &mut self.ram,
                                           &mut self.spram),
            DP_REG_START    ... DP_REG_END    =>
                lw!(self.ifs.dp).write_reg(addr, word, &self.ifs.mi, &mut self.ram,
                                           &mut self.spram),
            SI_REG_START    ... SI_REG_END    =>
                lw!(self.ifs.si).write_reg(addr, word, &self.ifs.mi, &mut self.ram,
                                           &self.ui),
            MI_REG_START    ... MI_REG_END    =>
                self.ifs.mi.write_reg(addr, word),
            VI_REG_START    ... VI_REG_END    =>
                lw!(self.ifs.vi).write_reg(addr, word, &self.ifs.mi, &self.ui),
            AI_REG_START    ... AI_REG_END    =>
                lw!(self.ifs.ai).write_reg(addr, word, &self.ifs.mi, &mut self.ram,
                                           &self.ui),
            PI_REG_START    ... PI_REG_END    =>
                lw!(self.ifs.pi).write_reg(addr, word, &self.ifs.mi, &mut self.ram),
            RI_REG_START    ... RI_REG_END    |
            RDRAM_REG_START ... RDRAM_REG_END =>
                lw!(self.ifs.ri).write_reg(addr, word),
            CART_SRAM_START ... CART_SRAM_END => Ok(()),
            _ => Err("Unsupported memory write area")
        }
    }

    pub fn vi_cycle(&mut self) {
        let (s, l) = {
            let vi = lr!(self.ifs.vi);
            (vi.vram_start, vi.vram_end - vi.vram_start)
        };
        self.ui.send(UiOutput::Update(self.ram.read_range(s, l)));
        self.ifs.mi.set_interrupt(mi::Intr::VI);
    }

    pub fn ai_cycle(&mut self) {
        let samples = self.ui.get_pending_audio();
        if samples < 5000 {
            let mut ai = lw!(self.ifs.ai);
            ai.buffer_empty(&self.ifs.mi);
        }
    }

    pub fn has_interrupt(&self) -> bool {
        self.ifs.mi.has_interrupt.load(Ordering::SeqCst)
    }

    pub fn into_ui(self) -> UiChannel {
        self.ui
    }
}
