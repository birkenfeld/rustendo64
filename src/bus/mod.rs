mod vi;
mod si;
mod pi;
mod ai;
mod ri;
pub mod cic;
pub mod mi;
pub mod mem_map;

use std::fmt;

use self::mem_map::*;
use self::vi::Vi;
use self::si::Si;
use self::pi::Pi;
use self::mi::Mi;
use self::ai::Ai;
use self::ri::{Ri, RdRegs};
use ui::{IfOutput, InterfaceChannel};
use rsp::{Sp, Dp};

const PIF_ROM_SIZE: usize = 0x800;
const RAM_SIZE: usize = 8 * 0x100000;

pub struct Bus {
    interface: InterfaceChannel,
    ram: Box<[u32]>,
    rd: RdRegs,
    sp: Sp,
    dp: Dp,
    mi: Mi,
    vi: Vi,
    ai: Ai,
    ri: Ri,
    pi: Pi,
    si: Si,
}

impl Bus {
    pub fn new(pif_rom: Box<[u8]>, cart_rom: Box<[u8]>,
               interface: InterfaceChannel) -> Bus {
        Bus {
            interface: interface,
            ram: vec![0; RAM_SIZE / 4].into_boxed_slice(),
            rd: RdRegs::default(),
            sp: Sp::default(),
            dp: Dp::default(),
            mi: Mi::default(),
            vi: Vi::default(),
            ai: Ai::default(),
            ri: Ri::default(),
            pi: Pi::new(cart_rom),
            si: Si::new(pif_rom),
        }
    }

    pub fn power_on_reset(&mut self) {
        // determine checksum seed and write to PIF ram
        if let Some(seed) = self.pi.get_cic_seed() {
            self.si.set_cic_seed(seed);
        } else {
            println!("Warning: no CIC seed found for this ROM");
        }
        // memory size
        self.write_word(0x3f0, 0x800000).unwrap();
        // power-on reset configs from cen64
        self.sp.power_on_reset();
        self.ri.power_on_reset();
    }

    pub fn read_word(&mut self, addr: u32) -> Result<u32, &'static str> {
        if addr & 0x3 != 0 {
            return Err("unaligned access");
        }
        match addr {
            RDRAM_START     ... RDRAM_END     => Ok(self.ram[addr as usize / 4]),
            CART_ROM_START  ... CART_ROM_END  => self.pi.read_rom(addr),
            PIF_ROM_START   ... PIF_ROM_END   => self.si.read_pif_rom(addr),
            PIF_RAM_START   ... PIF_RAM_END   => self.si.read_pif_ram(addr),
            SP_DMEM_START   ... SP_DMEM_END   => self.sp.read_dmem(addr),
            SP_IMEM_START   ... SP_IMEM_END   => self.sp.read_imem(addr),
            SP_REG_START    ... SP_REG_END    => self.sp.read_reg(addr),
            DP_REG_START    ... DP_REG_END    => self.dp.read_reg(addr),
            SI_REG_START    ... SI_REG_END    => self.si.read_reg(addr),
            MI_REG_START    ... MI_REG_END    => self.mi.read_reg(addr),
            VI_REG_START    ... VI_REG_END    => self.vi.read_reg(addr),
            AI_REG_START    ... AI_REG_END    => self.ai.read_reg(addr),
            PI_REG_START    ... PI_REG_END    => self.pi.read_reg(addr),
            RI_REG_START    ... RI_REG_END    => self.ri.read_reg(addr),
            RDRAM_REG_START ... RDRAM_REG_END => self.rd.read_reg(addr),
            DD_ROM_START    ... DD_ROM_END    => Ok(0),
            DD_REG_START    ... DD_REG_END    => Ok(0),
            _ => Err("Unsupported read memory area")
        }
    }

    pub fn write_word(&mut self, addr: u32, word: u32) -> Result<(), &'static str> {
        if addr & 0x3 != 0 {
            return Err("unaligned access");
        }
        match addr {
            RDRAM_START     ... RDRAM_END     =>
                Ok(self.ram[addr as usize / 4] = word),
            PIF_RAM_START   ... PIF_RAM_END   =>
                self.si.write_pif_ram(addr, word, &mut self.mi),
            SP_DMEM_START   ... SP_DMEM_END   =>
                self.sp.write_dmem(addr, word),
            SP_IMEM_START   ... SP_IMEM_END   =>
                self.sp.write_imem(addr, word),
            SP_REG_START    ... SP_REG_END    =>
                self.sp.write_reg(addr, word, &mut self.mi),
            DP_REG_START    ... DP_REG_END    =>
                self.dp.write_reg(addr, word),
            SI_REG_START    ... SI_REG_END    =>
                self.si.write_reg(addr, word, &mut self.mi, &mut self.ram,
                                  &mut self.interface),
            MI_REG_START    ... MI_REG_END    =>
                self.mi.write_reg(addr, word),
            VI_REG_START    ... VI_REG_END    =>
                self.vi.write_reg(addr, word, &mut self.mi, &mut self.interface),
            AI_REG_START    ... AI_REG_END    =>
                self.ai.write_reg(addr, word, &mut self.mi),
            PI_REG_START    ... PI_REG_END    =>
                self.pi.write_reg(addr, word, &mut self.mi, &mut self.ram),
            RI_REG_START    ... RI_REG_END    =>
                self.ri.write_reg(addr, word),
            RDRAM_REG_START ... RDRAM_REG_END =>
                self.rd.write_reg(addr, word),
            _ => Err("Unsupported memory write area")
        }
    }

    pub fn vi_cycle(&mut self) {
        self.interface.send(IfOutput::Update(
            self.ram[self.vi.vram_start..self.vi.vram_end].to_vec()));
        self.mi.set_interrupt(mi::Intr::VI);
    }

    pub fn has_interrupt(&self) -> bool {
        self.mi.has_interrupt
    }
}

impl fmt::Debug for Bus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Bus")
         .field("rd", &self.rd)
         .field("sp", &self.sp)
         .field("dp", &self.dp)
         .field("mi", &self.mi)
         .field("vi", &self.vi)
         .field("ai", &self.ai)
         .field("pi", &self.pi)
         .field("ri", &self.ri)
         .field("si", &self.si)
         .finish()
    }
}
