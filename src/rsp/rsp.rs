use std::cmp::min;
use std::fmt;
use std::mem;
use std::sync::{Arc, Condvar, Mutex, RwLock};
use std::sync::atomic::{AtomicBool, Ordering};
use simd::{u8x16, i16x8};
use simd::x86::sse2::{Sse2I8x16, Sse2I16x8, Sse2I32x4};
use simd::x86::ssse3::Ssse3U8x16;
use byteorder::{ByteOrder, BigEndian, LittleEndian};
#[cfg(debug_assertions)]
use ansi_term;

use vops;
use cp2::{Cp2, format_vec, HI, LO, ACC_HI, ACC_MD, ACC_LO};
use tables::SimdTables;
use bus::{Bus, RamAccess};
use bus::mem_map::*;
use r4k::{R4300, R4300Common, MemFmt};
use r4k::instruction::*;
use r4k::debug::DebugSpecList;
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
    run_bit:   Arc<AtomicBool>,
    run_cond:  Arc<Condvar>,
    tables:    SimdTables,
}

pub type RspBus<'c> = Bus<'c, &'c RwLock<Box<[u32]>>, &'c mut [u32]>;

#[cfg(debug_assertions)]
pub const INDENT: &'static str = "                                       ";


impl fmt::Debug for Rsp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for row in 0..8 {
            for col in 0..4 {
                let i = row + col * 8;
                try!(write!(f, "  {:2}:{} = {:016x}", i, REG_NAMES[i], self.regs.gpr[i]));
            }
            try!(write!(f, "\n"));
        }
        try!(write!(f, "     pc = {:016x}     ic = {:16}\n\n", self.regs.pc, self.regs.instr_ctr));
        write!(f, "{:?}", self.cp2)
    }
}

impl<'c> R4300<'c> for Rsp {
    type Bus = RspBus<'c>;

    fn read_instr(&self, bus: &RspBus, virt_addr: u64) -> u32 {
        let phys_addr = (virt_addr & 0xfff) | 0x0400_1000;
        self.read_word_raw(bus, phys_addr as u32)
    }

    fn read_word(&self, bus: &RspBus, virt_addr: u64) -> u32 {
        // Memory wraps around: all addresses are allowed.
        // XXX: Check this.
        let phys_addr = (virt_addr & 0xfff) | 0x0400_0000;
        let res = self.read_word_raw(bus, phys_addr as u32);
        self.debug_read(phys_addr, res);
        res
    }

    fn write_word(&mut self, bus: &mut RspBus, virt_addr: u64, word: u32) {
        let phys_addr = (virt_addr & 0xfff) | 0x0400_0000;
        self.write_word_raw(bus, phys_addr as u32, word);
    }

    fn aligned_offset(&self, instr: Instruction, _: u64) -> u64 {
        // No alignment is enforced or necessary.
        self.read_gpr(instr.base()).wrapping_add(instr.imm_sign_ext())
    }

    fn load_mem<T: MemFmt<'c, Self>>(&mut self, bus: &Self::Bus, addr: u64) -> T {
        T::load_unaligned_from(self, bus, addr)
    }

    fn store_mem<T: MemFmt<'c, Self>>(&mut self, bus: &mut Self::Bus, addr: u64, data: T) {
        T::store_unaligned_to(self, bus, addr, data)
    }

    fn check_interrupts(&mut self, _: &mut RspBus) { }

    fn ll_handler(&mut self, _: u64) {
        self.bug(format!("#UD: LL operation undefined for RSP"))
    }

    fn sc_handler<T: MemFmt<'c, Self>>(&mut self, _: &mut RspBus, _: Instruction, _: u64, _: T) {
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
    }

    fn dispatch_op(&mut self, bus: &mut RspBus, instr: Instruction) {
        match instr.opcode() {
            LWC2   => self.mem_load_vec(bus, instr),
            SWC2   => self.mem_store_vec(bus, instr),
            _      => self.bug(format!("#UD: I {:#b} -- {:?}", instr.0, instr))
        }
    }

    fn dispatch_special_op(&mut self, bus: &mut RspBus, instr: Instruction) {
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
                // reset PC to zero
                bus.write_word(SP_REG_PC, 0).unwrap();
                // println!("RSP: break.");
                self.broke = true;
            }
            _     => self.bug(format!("#UD: I {:#b} -- {:?}", instr.0, instr))
        }
    }

    fn dispatch_cop0_op(&mut self, bus: &mut RspBus, instr: Instruction) {
        match instr.cop_op() {
            MF => {
                let reg_addr = COP0_REG_MAP[instr.rd()];
                let data = self.read_word_raw(bus, reg_addr);
                dprintln!(self, "{} cp0[{:02}] :  {:#18x}", INDENT, instr.rd(), data);
                self.debug_read(reg_addr as u64, data);
                self.write_gpr(instr.rt(), data as i32 as u64);
            }
            MT => {
                let reg_addr = COP0_REG_MAP[instr.rd()];
                let data = self.read_gpr(instr.rt()) as u32;
                dprintln!(self, "{} cp0[{:02}] <- {:#18x}", INDENT, instr.rd(), data);
                self.write_word_raw(bus, reg_addr, data);
            }
            _  => self.bug(format!("#UD CP0: I {:#b} -- {:?}", instr.0, instr))
        }
    }

    fn dispatch_cop1_op(&mut self, _: &mut RspBus, instr: Instruction) {
        self.bug(format!("#CU CP1: I {:#b} -- {:?}", instr.0, instr))
    }

    fn dispatch_cop2_op(&mut self, _: &mut RspBus, instr: Instruction) {
        match instr.cop_op() {
            CF => {
                // TODO: check rd range (0-2)
                let flo = self.read_flags(instr.rd(), LO);
                let fhi = self.read_flags(instr.rd(), HI);
                let fcomb = flo.packs(fhi);
                let res = fcomb.move_mask() as i32 as u64;
                self.write_gpr(instr.rt(), res);
            },
            MF => {
                let vec = self.read_vec(instr.vs());
                let element = instr.vel_ls() as u32;
                let lo = element >> 1;

                let res = if element & 1 != 0 {
                    let hi = (element + 1) >> 1;
                    let high = vec.extract(lo) << 8;
                    let low = vec.extract(hi) >> 8;
                    (high | low) as u64
                } else {
                    vec.extract(lo) as u64
                };
                dprintln!(self, "{} $v{:02}[{:02}] :   {:#6x}", INDENT, instr.vs(), element, res);
                self.write_gpr(instr.rt(), res);
            },
            MT => {
                let reg = self.read_gpr(instr.rt()) as i16;
                let mut vec = self.read_vec(instr.vs());
                let element = instr.vel_ls() as u32;
                let lo = element >> 1;

                if element & 1 != 0 {
                    let hi = (element + 1) >> 1;
                    vec = vec.replace(lo, (vec.extract(lo) & !0x00ff) | (reg >> 8 & 0xff));
                    vec = vec.replace(hi, (vec.extract(hi) & 0x00ff)  | ((reg & 0xff) << 8));
                } else {
                    vec = vec.replace(lo, reg);
                }
                dprintln!(self, "{} $v{:02}[{:02}] <-  {:#6x}", INDENT, instr.vs(), element, reg);
                self.write_vec(instr.vs(), vec);
            },
            c  => {
                if c & 0b10000 == 0 {
                    self.bug(format!("#UD CP2: I {:#b} -- {:?}", instr.0, instr));
                }
                let op = instr.special_op();
                match op {
                    VNOP | VNULL => self.vec_binop(instr, vops::vnop),
                    VADD  => self.vec_binop(instr, vops::vadd),
                    VADDC => self.vec_binop(instr, vops::vaddc),
                    VSUB  => self.vec_binop(instr, vops::vsub),
                    VSUBC => self.vec_binop(instr, vops::vsubc),
                    VABS  => self.vec_binop(instr, vops::vabs),
                    VAND  => self.vec_binop(instr, vops::vand),
                    VNAND => self.vec_binop(instr, vops::vnand),
                    VOR   => self.vec_binop(instr, vops::vor),
                    VNOR  => self.vec_binop(instr, vops::vnor),
                    VXOR  => self.vec_binop(instr, vops::vxor),
                    VNXOR => self.vec_binop(instr, vops::vnxor),
                    VMULU | VMULF => self.vec_binop(instr, vops::vmulx),
                    VMADN | VMUDN => self.vec_binop(instr, vops::vmxdn),
                    VMADH | VMUDH => self.vec_binop(instr, vops::vmxdh),
                    VMADL | VMUDL => self.vec_binop(instr, vops::vmxdl),
                    VMADM | VMUDM => self.vec_binop(instr, vops::vmxdm),
                    VMACU | VMACF => self.vec_binop(instr, vops::vmacx),
                    VCH   => self.vec_binop(instr, vops::vch),
                    VCL   => self.vec_binop(instr, vops::vcl),
                    VCR   => self.vec_binop(instr, vops::vcr),
                    VMRG  => self.vec_binop(instr, vops::vmrg),
                    VMOV  => self.vec_elop(instr, vops::vmov),
                    VRCPH | VRSQH => self.vec_elop(instr, vops::vrcph_vrsqh),
                    VRCP | VRCPL | VRSQ | VRSQL => self.vec_elop(instr, vops::vrcp_vrsqh),
                    VEQ | VGE | VLT | VNE => self.vec_binop(instr, vops::vcmp),
                    VSAR  => {
                        match instr.vel() {
                            8  => self.cp2.vec[instr.vd()] = self.cp2.acc[ACC_HI],
                            9  => self.cp2.vec[instr.vd()] = self.cp2.acc[ACC_MD],
                            10 => self.cp2.vec[instr.vd()] = self.cp2.acc[ACC_LO],
                            _  => self.cp2.vec[instr.vd()] = [0; 16],
                        }
                    },
                    _     => self.bug(format!("#UD CP2: I {:#b} -- {:?}", instr.0, instr))
                }
            }
        }
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
            tables:    SimdTables::new()
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
            tables:    SimdTables::new()
        }
    }

    pub fn wait_for_start(&self) {
        while !self.run_bit.load(Ordering::SeqCst) {
            let mutex = Mutex::new(());
            let _ = self.run_cond.wait(mutex.lock().unwrap()).unwrap();
        }
    }

    pub fn run_sequence(&mut self, bus: &mut RspBus) {
        self.regs.pc = (bus.read_word(SP_REG_PC).unwrap() & 0xfff) as u64;
        self.broke = false;
        while !self.broke {
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

    // Load/store implementations

    fn aligned_offset_shift(&self, instr: Instruction, shift: u64, align: u64) -> u64 {
        let addr = self.read_gpr(instr.base()).wrapping_add(instr.voff() << shift);
        if addr & (align - 1) != 0 {
            self.bug(format!("Address not aligned to {} bytes: {:#x}", align, addr));
        }
        addr
    }

    fn restricted_vel_ls(&self, instr: Instruction, modulus: usize) -> usize {
        let vel_ls = instr.vel_ls();
        if vel_ls % modulus != 0 {
            self.bug(format!("Element spec not divisible by {}: {}", modulus, vel_ls));
        }
        vel_ls
    }

    fn mem_load_vec(&mut self, bus: &RspBus, instr: Instruction) {
        let vf = instr.vec_fmt();
        match vf {
            VLF_B | VLF_S | VLF_L | VLF_D => {
                // Load byte/hword/word/dword into part of the vector
                // indexed by element.  Other parts are unaffected.
                let shift = vf as u64 & 0b11;
                let addr = self.aligned_offset_shift(instr, shift, 1);
                // NOTE: the patent says this is restricted to 1 << shift.
                // Games seem to use any values.
                let index = self.restricted_vel_ls(instr, 1);
                /* TODO: optimize these with SIMD operations! */
                match vf {
                    VLF_B => {
                        let data = self.load_mem(bus, addr);
                        dprintln!(self, "{} $v{:02}[{:02}] <- {:#18x} :  mem @ {:#x}",
                                  INDENT, instr.vt(), index, data, addr);
                        self.cp2.vec[instr.vt()][index ^ 1] = data;
                    }
                    VLF_S => {
                        let data = self.load_mem(bus, addr);
                        dprintln!(self, "{} $v{:02}[{:02}] <- {:#18x} :  mem @ {:#x}",
                                  INDENT, instr.vt(), index, data, addr);
                        LittleEndian::write_u16(&mut self.cp2.vec[instr.vt()][index..], data);
                    }
                    VLF_L => {
                        let data: u32 = self.load_mem(bus, addr);
                        dprintln!(self, "{} $v{:02}[{:02}] <- {:#18x} :  mem @ {:#x}",
                                  INDENT, instr.vt(), index, data, addr);
                        LittleEndian::write_u16(&mut self.cp2.vec[instr.vt()][index..], (data >> 16) as u16);
                        LittleEndian::write_u16(&mut self.cp2.vec[instr.vt()][index+2..], data as u16);
                    }
                    VLF_D => {
                        let data: u64 = self.load_mem(bus, addr);
                        dprintln!(self, "{} $v{:02}[{:02}] <- {:#18x} :  mem @ {:#x}",
                                  INDENT, instr.vt(), index, data, addr);
                        LittleEndian::write_u16(&mut self.cp2.vec[instr.vt()][index..], (data >> 48) as u16);
                        LittleEndian::write_u16(&mut self.cp2.vec[instr.vt()][index+2..], (data >> 32) as u16);
                        LittleEndian::write_u16(&mut self.cp2.vec[instr.vt()][index+4..], (data >> 16) as u16);
                        LittleEndian::write_u16(&mut self.cp2.vec[instr.vt()][index+6..], data as u16);
                    }
                    _ => unreachable!()
                }
            },
            VLF_Q | VLF_R => {
                // Load left/right part of quadword into part of the vector.
                let addr = self.aligned_offset_shift(instr, 4, 1);
                self.restricted_vel_ls(instr, 16);  // ensure zero
                let offset = addr & 0b1111;
                let aligned_addr = addr & !0b1111;
                let mut buffer = [0_u8; 16];
                // XXX: implement load_from for u8x16?
                BigEndian::write_u64(&mut buffer[0..], self.load_mem(bus, aligned_addr));
                BigEndian::write_u64(&mut buffer[8..], self.load_mem(bus, aligned_addr + 8));
                let val = u8x16::load(&buffer, 0);
                let reg = u8x16::load(&self.cp2.vec[instr.vt()], 0);
                let result = if vf == VLF_Q {
                    let mask = self.tables.keep_swap_r[offset as usize];
                    let shift = self.tables.shift_swap_l[offset as usize];
                    (reg & mask) | val.shuffle_bytes(shift)
                } else {
                    // XXX: panics when offset == 0
                    let mask = self.tables.keep_swap_l[16 - offset as usize];
                    let shift = self.tables.shift_swap_r[16 - offset as usize];
                    (reg & mask) | val.shuffle_bytes(shift)
                };
                result.store(&mut self.cp2.vec[instr.vt()], 0);
                dprintln!(self, "{} $v{:02}     <- {} :  mem @ {:#x}",
                          INDENT, instr.vt(), format_vec(self.read_vec(instr.vt())), addr);
            },
            VLF_P | VLF_U => {
                // Load packed 8-bit signed/unsigned into 16-bit vectors.
                let addr = self.aligned_offset_shift(instr, 3, 1);
                self.restricted_vel_ls(instr, 16);  // ensure zero
                let dword: u64 = self.load_mem(bus, addr);
                let result = if vf == VLF_P {
                    i16x8::new(
                        ((dword >> 48) & 0xff00) as i16,
                        ((dword >> 40) & 0xff00) as i16,
                        ((dword >> 32) & 0xff00) as i16,
                        ((dword >> 24) & 0xff00) as i16,
                        ((dword >> 16) & 0xff00) as i16,
                        ((dword >> 8)  & 0xff00) as i16,
                        ( dword        & 0xff00) as i16,
                        ((dword << 8)  & 0xff00) as i16,
                    )
                } else {
                    i16x8::new(
                        ((dword >> 49) & 0x7f80) as i16,
                        ((dword >> 41) & 0x7f80) as i16,
                        ((dword >> 33) & 0x7f80) as i16,
                        ((dword >> 25) & 0x7f80) as i16,
                        ((dword >> 17) & 0x7f80) as i16,
                        ((dword >> 9)  & 0x7f80) as i16,
                        ((dword >> 1)  & 0x7f80) as i16,
                        ((dword << 7)  & 0x7f80) as i16,
                    )
                };
                self.write_vec(instr.vt(), result);
            },
            VLF_H => {
                // Load packed bytes from 2 dwords with 2-byte stride into 16-bit vectors.
                let addr = self.aligned_offset_shift(instr, 4, 1);
                // unaligned is allowed, but we load from aligned
                let aligned_addr = addr & !15;
                self.restricted_vel_ls(instr, 16);  // ensure zero
                let dw1: u64 = self.load_mem(bus, aligned_addr);
                let dw2: u64 = self.load_mem(bus, aligned_addr + 8);
                let result = if addr & 1 == 0 {
                    i16x8::new(
                        ((dw1 >> 49) & 0x7f80) as i16,
                        ((dw1 >> 33) & 0x7f80) as i16,
                        ((dw1 >> 17) & 0x7f80) as i16,
                        ((dw1 >> 1)  & 0x7f80) as i16,
                        ((dw2 >> 49) & 0x7f80) as i16,
                        ((dw2 >> 33) & 0x7f80) as i16,
                        ((dw2 >> 17) & 0x7f80) as i16,
                        ((dw2 >> 1)  & 0x7f80) as i16
                    )
                } else {
                    i16x8::new(
                        ((dw1 >> 41) & 0x7f80) as i16,
                        ((dw1 >> 25) & 0x7f80) as i16,
                        ((dw1 >> 9)  & 0x7f80) as i16,
                        ((dw1 << 7)  & 0x7f80) as i16,
                        ((dw2 >> 41) & 0x7f80) as i16,
                        ((dw2 >> 25) & 0x7f80) as i16,
                        ((dw2 >> 9)  & 0x7f80) as i16,
                        ((dw2 << 7)  & 0x7f80) as i16
                    )
                };
                self.write_vec(instr.vt(), result);
            },
            VLF_F => {
                // Load bytes from 2 dwords with 4-byte stride into 16-bit vectors.
                let addr = self.aligned_offset_shift(instr, 4, 1);
                let aligned_addr = addr & !15;
                let el = self.restricted_vel_ls(instr, 8);  // ensure 0 or 8
                let dw1: u64 = self.load_mem(bus, aligned_addr);
                let dw2: u64 = self.load_mem(bus, aligned_addr + 8);
                let v1;
                let v2;
                let v3;
                let v4;
                match addr & 3 {
                    0 => {
                        v1 = ((dw1 >> 49) & 0x7f80) as i16;
                        v2 = ((dw1 >> 17) & 0x7f80) as i16;
                        v3 = ((dw2 >> 49) & 0x7f80) as i16;
                        v4 = ((dw2 >> 17) & 0x7f80) as i16;
                    },
                    1 => {
                        v1 = ((dw1 >> 41) & 0x7f80) as i16;
                        v2 = ((dw1 >> 9)  & 0x7f80) as i16;
                        v3 = ((dw2 >> 41) & 0x7f80) as i16;
                        v4 = ((dw2 >> 9)  & 0x7f80) as i16;
                    },
                    2 => {
                        v1 = ((dw1 >> 33) & 0x7f80) as i16;
                        v2 = ((dw1 >> 1)  & 0x7f80) as i16;
                        v3 = ((dw2 >> 33) & 0x7f80) as i16;
                        v4 = ((dw2 >> 1)  & 0x7f80) as i16;
                    },
                    _ => {
                        v1 = ((dw1 >> 25) & 0x7f80) as i16;
                        v2 = ((dw1 << 7)  & 0x7f80) as i16;
                        v3 = ((dw2 >> 25) & 0x7f80) as i16;
                        v4 = ((dw2 << 7)  & 0x7f80) as i16;
                    }
                }
                let result = if el == 0 {
                    i16x8::new(0, 0, 0, 0, v1, v2, v3, v4)
                } else {
                    i16x8::new(v1, v2, v3, v4, 0, 0, 0, 0)
                };
                self.write_vec(instr.vt(), result);
            },
            VLF_T => {
                // Load 16-bit elements into up to 8 different vectors
                let addr = self.aligned_offset_shift(instr, 4, 16);
                let el = self.restricted_vel_ls(instr, 2);
                let vel = el as u32 >> 1;  // element in the vector (0..7)
                let mut buffer = [0_u8; 16];
                BigEndian::write_u64(&mut buffer[0..], self.load_mem(bus, addr));
                BigEndian::write_u64(&mut buffer[8..], self.load_mem(bus, addr + 8));
                // if the first vector is e.g. 30, we only do two elements
                // (others require vt to be divisible by 8)
                let vt = instr.vt();
                for i in 0..min(8, 32 - vt) {
                    let vec = self.read_vec(vt + i);
                    self.write_vec(vt + i, vec.replace((i as u32 + 8 - vel) % 8,
                                                       // This is actually ok!
                                                       BigEndian::read_i16(&buffer[i*2..])));
                }
            }
            _ => self.bug(format!("Unimplemented vector load format: {}", vf))
        }
    }

    fn mem_store_vec(&mut self, bus: &mut RspBus, instr: Instruction) {
        let vf = instr.vec_fmt();
        match vf {
            VLF_B | VLF_S | VLF_L | VLF_D => {
                // Store byte/hword/word/dword part of the vector
                // indexed by element.  Other parts are unaffected.
                let shift = vf as u64 & 0b11;
                let addr = self.aligned_offset_shift(instr, shift, 1);
                // NOTE: the patent says this is restricted to 1 << shift.
                // Games seem to use any values.
                let index = self.restricted_vel_ls(instr, 1);
                match vf {
                    VLF_B => {
                        let data = self.cp2.vec[instr.vt()][index ^ 1];
                        dprintln!(self, "{} $v{:02}[{:02}] :  {:#18x} -> mem @ {:#x}",
                                  INDENT, instr.vt(), index, data, addr);
                        self.store_mem(bus, addr, data);
                    }
                    VLF_S => {
                        let data = LittleEndian::read_u16(&self.cp2.vec[instr.vt()][index..]);
                        dprintln!(self, "{} $v{:02}[{:02}] :  {:#18x} -> mem @ {:#x}",
                                  INDENT, instr.vt(), index, data, addr);
                        self.store_mem(bus, addr, data);
                    }
                    VLF_L => {
                        let data = (LittleEndian::read_u16(&self.cp2.vec[instr.vt()][index..]) as u32) << 16 |
                                    LittleEndian::read_u16(&self.cp2.vec[instr.vt()][index+2..]) as u32;
                        dprintln!(self, "{} $v{:02}[{:02}] :  {:#18x} -> mem @ {:#x}",
                                  INDENT, instr.vt(), index, data, addr);
                        self.store_mem(bus, addr, data);
                    }
                    VLF_D => {
                        let data = (LittleEndian::read_u16(&self.cp2.vec[instr.vt()][index..]) as u64) << 48 |
                                   (LittleEndian::read_u16(&self.cp2.vec[instr.vt()][index+2..]) as u64) << 32 |
                                   (LittleEndian::read_u16(&self.cp2.vec[instr.vt()][index+4..]) as u64) << 16 |
                                    LittleEndian::read_u16(&self.cp2.vec[instr.vt()][index+6..]) as u64;
                        dprintln!(self, "{} $v{:02}[{:02}] :  {:#18x} -> mem @ {:#x}",
                                  INDENT, instr.vt(), index, data, addr);
                        self.store_mem(bus, addr, data);
                    }
                    _    => unreachable!()
                }
            },
            VLF_Q | VLF_R => {
                // Store part of the vector into left/right part of quadword.
                let addr = self.aligned_offset_shift(instr, 4, 1);
                self.restricted_vel_ls(instr, 16);  // ensure zero
                let offset = addr & 0b1111;
                let aligned_addr = addr & !0b1111;
                let mut buffer = [0_u8; 16];
                BigEndian::write_u64(&mut buffer[0..], self.load_mem(bus, aligned_addr));
                BigEndian::write_u64(&mut buffer[8..], self.load_mem(bus, aligned_addr + 8));
                let val = u8x16::load(&buffer, 0);
                let reg = u8x16::load(&self.cp2.vec[instr.vt()], 0);
                let result = if vf == VLF_Q {
                    let mask = self.tables.keep_l[offset as usize];
                    let shift = self.tables.shift_r[offset as usize];
                    // XXX first shift or first swap?
                    (val & mask) | reg.shuffle_bytes(self.tables.bswap).shuffle_bytes(shift)
                } else {
                    // XXX: panics when offset == 0
                    let mask = self.tables.keep_r[16 - offset as usize];
                    let shift = self.tables.shift_l[16 - offset as usize];
                    (val & mask) | reg.shuffle_bytes(self.tables.bswap).shuffle_bytes(shift)
                };
                result.store(&mut buffer, 0);
                let dw1 = BigEndian::read_u64(&buffer[0..]);
                let dw2 = BigEndian::read_u64(&buffer[8..]);
                dprintln!(self, "{} $v{:02}     :  {:#18x}{:016x} -> mem @ {:#x}",
                          INDENT, instr.vt(), dw1, dw2, addr);
                self.store_mem(bus, aligned_addr, dw1);
                self.store_mem(bus, aligned_addr + 8, dw2);
            },
            VLF_P | VLF_U => {
                // Store packed 8-bit signed/unsigned from 16-bit vectors.
                let addr = self.aligned_offset_shift(instr, 3, 1);
                self.restricted_vel_ls(instr, 16);  // ensure zero
                let vec = self.read_vec(instr.vt());
                let dword = if vf == VLF_P {
                    ((vec.extract(0) as u64) & 0xff00) << 48 |
                    ((vec.extract(1) as u64) & 0xff00) << 40 |
                    ((vec.extract(2) as u64) & 0xff00) << 32 |
                    ((vec.extract(3) as u64) & 0xff00) << 24 |
                    ((vec.extract(4) as u64) & 0xff00) << 16 |
                    ((vec.extract(5) as u64) & 0xff00) << 8  |
                    ((vec.extract(6) as u64) & 0xff00)       |
                    ((vec.extract(7) as u64) & 0xff00) >> 8
                } else {
                    ((vec.extract(0) as u64) & 0x7f80) << 49 |
                    ((vec.extract(1) as u64) & 0x7f80) << 41 |
                    ((vec.extract(2) as u64) & 0x7f80) << 33 |
                    ((vec.extract(3) as u64) & 0x7f80) << 25 |
                    ((vec.extract(4) as u64) & 0x7f80) << 17 |
                    ((vec.extract(5) as u64) & 0x7f80) << 9  |
                    ((vec.extract(6) as u64) & 0x7f80) << 1  |
                    ((vec.extract(7) as u64) & 0x7f80) >> 7
                };
                self.store_mem(bus, addr, dword);
            },
            VLF_H => {
                // Store bytes to 2 dwords with 2-byte stride from 16-bit vectors.
                let addr = self.aligned_offset_shift(instr, 4, 1);
                // unaligned is allowed, but we load from aligned
                let aligned_addr = addr & !15;
                self.restricted_vel_ls(instr, 16);  // ensure zero
                let vec = self.read_vec(instr.vt());
                let (dw1, dw2) = if addr & 1 == 0 {
                    (
                        ((vec.extract(0) as u64) & 0x7f80) << 49 |
                        ((vec.extract(1) as u64) & 0x7f80) << 33 |
                        ((vec.extract(2) as u64) & 0x7f80) << 17 |
                        ((vec.extract(3) as u64) & 0x7f80) << 1,
                        ((vec.extract(4) as u64) & 0x7f80) << 49 |
                        ((vec.extract(5) as u64) & 0x7f80) << 33 |
                        ((vec.extract(6) as u64) & 0x7f80) << 17 |
                        ((vec.extract(7) as u64) & 0x7f80) << 1
                    )
                } else {
                    (
                        ((vec.extract(0) as u64) & 0x7f80) << 41 |
                        ((vec.extract(1) as u64) & 0x7f80) << 25 |
                        ((vec.extract(2) as u64) & 0x7f80) << 9  |
                        ((vec.extract(3) as u64) & 0x7f80) >> 7,
                        ((vec.extract(4) as u64) & 0x7f80) << 41 |
                        ((vec.extract(5) as u64) & 0x7f80) << 25 |
                        ((vec.extract(6) as u64) & 0x7f80) << 9  |
                        ((vec.extract(7) as u64) & 0x7f80) >> 7
                    )
                };
                self.store_mem(bus, aligned_addr, dw1);
                self.store_mem(bus, aligned_addr + 8, dw2);
            },
            VLF_F => {
                // Store bytes to 2 dwords with 4-byte stride from 16-bit vectors.
                let addr = self.aligned_offset_shift(instr, 4, 1);
                let aligned_addr = addr & !15;
                let el = self.restricted_vel_ls(instr, 8);  // ensure 0 or 8
                let vec = self.read_vec(instr.vt());
                let mut dw1: u64 = self.load_mem(bus, aligned_addr);
                let mut dw2: u64 = self.load_mem(bus, aligned_addr + 8);
                let v1;
                let v2;
                let v3;
                let v4;
                if el == 0 {
                    v1 = vec.extract(4);
                    v2 = vec.extract(5);
                    v3 = vec.extract(6);
                    v4 = vec.extract(7);
                } else {
                    v1 = vec.extract(0);
                    v2 = vec.extract(1);
                    v3 = vec.extract(2);
                    v4 = vec.extract(3);
                }
                match addr & 3 {
                    0 => {
                        dw1 = (dw1 & 0x00ffffff_00ffffff) |
                              (((v1 as u64) & 0x7f80) << 49) |
                              (((v2 as u64) & 0x7f80) << 17);
                        dw2 = (dw2 & 0x00ffffff_00ffffff) |
                              (((v3 as u64) & 0x7f80) << 49) |
                              (((v4 as u64) & 0x7f80) << 17);
                    },
                    1 => {
                        dw1 = (dw1 & 0xff00ffff_ff00ffff) |
                              (((v1 as u64) & 0x7f80) << 41) |
                              (((v2 as u64) & 0x7f80) << 9);
                        dw2 = (dw2 & 0xff00ffff_ff00ffff) |
                              (((v3 as u64) & 0x7f80) << 41) |
                              (((v4 as u64) & 0x7f80) << 9);
                    },
                    2 => {
                        dw1 = (dw1 & 0xffff00ff_ffff00ff) |
                              (((v1 as u64) & 0x7f80) << 33) |
                              (((v2 as u64) & 0x7f80) << 1);
                        dw2 = (dw2 & 0xffff00ff_ffff00ff) |
                              (((v3 as u64) & 0x7f80) << 33) |
                              (((v4 as u64) & 0x7f80) << 1);
                    },
                    _ => {
                        dw1 = (dw1 & 0xffffff00_ffffff00) |
                              (((v1 as u64) & 0x7f80) << 25) |
                              (((v2 as u64) & 0x7f80) >> 7);
                        dw2 = (dw2 & 0xffffff00_ffffff00) |
                              (((v3 as u64) & 0x7f80) << 25) |
                              (((v4 as u64) & 0x7f80) >> 7);
                    }
                }
                self.store_mem(bus, aligned_addr, dw1);
                self.store_mem(bus, aligned_addr + 8, dw2);
            },
            VLF_W => {
                // Store bytes from vector, starting at element and wrapping around.
                let addr = self.aligned_offset_shift(instr, 4, 1);
                let el = self.restricted_vel_ls(instr, 1);  // any value allowed
                let vec = u8x16::load(&self.cp2.vec[instr.vt()], 0);
                let vec = vec.shuffle_bytes(self.tables.rot_swap_l[el]);
                let mut buffer = [0_u8; 16];
                vec.store(&mut buffer, 0);
                self.store_mem(bus, addr, BigEndian::read_u64(&buffer[0..]));
                self.store_mem(bus, addr + 8, BigEndian::read_u64(&buffer[8..]));
            },
            VLF_T => {
                // Store 16-bit elements from up to 8 different vectors
                let addr = self.aligned_offset_shift(instr, 4, 1);
                let el = self.restricted_vel_ls(instr, 1);  // any value allowed
                let vel = el as u32 >> 1;  // element in the vector (0..7)
                let mut buffer = [0_u8; 16];
                BigEndian::write_u64(&mut buffer[0..], self.load_mem(bus, addr));
                BigEndian::write_u64(&mut buffer[8..], self.load_mem(bus, addr + 8));
                // if the first vector is e.g. 30, we only do two elements
                let vt = instr.vt();
                for i in 0..min(8, 32 - vt) {
                    let vec = self.read_vec(vt + i);
                    BigEndian::write_i16(&mut buffer[i*2..], vec.extract((i as u32 + 8 - vel) % 8));
                }
                self.store_mem(bus, addr, BigEndian::read_u64(&buffer[0..]));
                self.store_mem(bus, addr + 8, BigEndian::read_u64(&buffer[8..]));
            }
            _ => self.bug(format!("Unimplemented vector store format: {}", vf))
        }
    }

    fn vec_binop<F>(&mut self, instr: Instruction, func: F)
        where F: Fn(i16x8, i16x8, u32, &mut Self) -> i16x8
    {
        let vs = self.read_vec(instr.vs());
        let vt = self.read_and_shuffle_vec(instr.vt(), instr.vel());
        dprintln!(self, "{} $v{:02}     :  {}", INDENT, instr.vs(), format_vec(vs));
        dprintln!(self, "{} $v{:02}{:-4} :  {}", INDENT, instr.vt(),
                  VEC_EL_SPEC[instr.vel()], format_vec(vt));
        let res = func(vs, vt, instr.special_op(), self);
        dprintln!(self, "{} $v{:02}     <- {}", INDENT, instr.vd(), format_vec(res));
        self.write_vec(instr.vd(), res);
    }

    fn vec_elop<F>(&mut self, instr: Instruction, func: F)
        where F: Fn(i16x8, i16, u32, &mut Self) -> i16
    {
        let vt_el = self.read_vec(instr.vt()).extract(instr.vel() as u32 & 0x7);
        let vt = self.read_and_shuffle_vec(instr.vt(), instr.vel());  // XXX really?
        let vd = self.read_vec(instr.vd());
        dprintln!(self, "{} $v{:02}[{:02}] :  {}", INDENT, instr.vt(), instr.vel(), vt_el);
        let res_el = func(vt, vt_el, instr.special_op(), self);
        let res = vd.replace(instr.vdel() as u32 & 0x7, res_el);
        dprintln!(self, "{} $v{:02}[{:02}] <- {}", INDENT, instr.vd(), instr.vdel(), res_el);
        self.write_vec(instr.vd(), res);
    }

    fn read_vec(&self, index: usize) -> i16x8 {
        unsafe { mem::transmute(u8x16::load(&self.cp2.vec[index], 0)) }
    }

    fn read_and_shuffle_vec(&self, index: usize, elements: usize) -> i16x8 {
        let vec = u8x16::load(&self.cp2.vec[index], 0);
        let vec = vec.shuffle_bytes(self.tables.el_shuf[elements]);
        unsafe { mem::transmute(vec) }
    }

    fn write_vec(&mut self, index: usize, value: i16x8) {
        let res: u8x16 = unsafe {  mem::transmute(value) };
        res.store(&mut self.cp2.vec[index], 0);
    }

    pub fn read_acc(&self, index: usize) -> i16x8 {
        unsafe { mem::transmute(u8x16::load(&self.cp2.acc[index], 0)) }
    }

    pub fn write_acc(&mut self, index: usize, value: i16x8) {
        let res: u8x16 = unsafe {  mem::transmute(value) };
        res.store(&mut self.cp2.acc[index], 0);
    }

    pub fn read_flags(&self, index: usize, offset: usize) -> i16x8 {
        unsafe { mem::transmute(u8x16::load(&self.cp2.flags[index], offset)) }
    }

    pub fn write_flags(&mut self, index: usize, offset: usize, value: i16x8) {
        let res: u8x16 = unsafe {  mem::transmute(value) };
        res.store(&mut self.cp2.flags[index], offset);
    }

    pub fn get_cp2(&mut self) -> &mut Cp2 {
        &mut self.cp2
    }
}
