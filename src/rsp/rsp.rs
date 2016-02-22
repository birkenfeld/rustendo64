use std::cmp::min;
use std::fmt;
use std::mem;
use std::sync::{Arc, Condvar, Mutex, RwLock};
use std::sync::atomic::{AtomicBool, Ordering};
use simd::{u8x16, i16x8};
use simd::x86::sse2::{Sse2I8x16, Sse2I16x8, Sse2I32x4};
use simd::x86::ssse3::Ssse3U8x16;
use byteorder::{ByteOrder, BigEndian};
#[cfg(debug_assertions)]
use ansi_term;

use vops;
use mops;
use cp2::{Cp2, ACC_HI, ACC_MD, ACC_LO, HI, LO};
use tables::SimdTables;
use bus::{Bus, RamAccess};
use bus::mem_map::*;
use r4k::{R4300, R4300Common, MemFmt};
use r4k::instruction::*;
use r4k::debug::DebugSpecList;
use util::bit_set;
#[cfg(debug_assertions)]
use cp2::format_vec;

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
                if instr.rd() < 3 {
                    let flo = self.read_flags(instr.rd(), LO);
                    let fhi = self.read_flags(instr.rd(), HI);
                    let fcomb = flo.packs(fhi);
                    let res = fcomb.move_mask() as i32 as u64;
                    self.write_gpr(instr.rt(), res);
                }
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
                    VSAR  => match instr.vel() {
                        8  => self.cp2.vec[instr.vd()] = self.cp2.acc[ACC_HI],
                        9  => self.cp2.vec[instr.vd()] = self.cp2.acc[ACC_MD],
                        10 => self.cp2.vec[instr.vd()] = self.cp2.acc[ACC_LO],
                        _  => self.cp2.vec[instr.vd()] = i16x8::splat(0),
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
        let vf = instr.vls_fmt();
        match vf {
            VLS_B | VLS_S | VLS_L | VLS_D => {
                // Load byte/hword/word/dword into part of the vector
                // indexed by element.  Other parts are unaffected.
                let shift = vf as u64 & 0b11;
                let addr = self.aligned_offset_shift(instr, shift, 1);
                // NOTE: the patent says this is restricted to 1 << shift.
                // Games seem to use any value.  This will panic when
                // index + size_of_load > 16.
                let el = self.restricted_vel_ls(instr, 1);
                let vel = el as u32 >> 1;
                let mut vec = self.cp2.vec[instr.vt()];
                match vf {
                    VLS_B => {
                        let byte: u8 = self.load_mem(bus, addr);
                        dprintln!(self, "{} $v{:02}[{:02}] <- {:#18x} :  mem @ {:#x}",
                                  INDENT, instr.vt(), el, byte, addr);
                        let data = if el & 1 == 0 {
                            (vec.extract(vel) as u16 & 0x00ff) | (byte as u16) << 8
                        } else {
                            (vec.extract(vel) as u16 & 0xff00) | (byte as u16)
                        };
                        vec = vec.replace(vel, data as i16);
                    }
                    VLS_S => {
                        let data: u16 = self.load_mem(bus, addr);
                        dprintln!(self, "{} $v{:02}[{:02}] <- {:#18x} :  mem @ {:#x}",
                                  INDENT, instr.vt(), el, data, addr);
                        vec = vec.replace(vel, data as i16);
                    }
                    VLS_L => {
                        let data: u32 = self.load_mem(bus, addr);
                        dprintln!(self, "{} $v{:02}[{:02}] <- {:#18x} :  mem @ {:#x}",
                                  INDENT, instr.vt(), el, data, addr);
                        vec = vec.replace(vel,     (data >> 16) as i16)
                                 .replace(vel + 1, data as i16);
                    }
                    VLS_D => {
                        let data: u64 = self.load_mem(bus, addr);
                        dprintln!(self, "{} $v{:02}[{:02}] <- {:#18x} :  mem @ {:#x}",
                                  INDENT, instr.vt(), el, data, addr);
                        vec = vec.replace(vel,     (data >> 48) as i16)
                                 .replace(vel + 1, (data >> 32) as i16)
                                 .replace(vel + 2, (data >> 16) as i16)
                                 .replace(vel + 3, data as i16);
                    }
                    _ => unreachable!()
                }
                self.cp2.vec[instr.vt()] = vec;
            },
            VLS_Q | VLS_R => {
                // Load left/right part of quadword into part of the vector.
                let addr = self.aligned_offset_shift(instr, 4, 1);
                self.restricted_vel_ls(instr, 16);  // ensure zero
                let offset = addr & 0b1111;
                let aligned_addr = addr & !0b1111;
                let mut buffer = [0_u8; 16];
                BigEndian::write_u64(&mut buffer[0..], self.load_mem(bus, aligned_addr));
                BigEndian::write_u64(&mut buffer[8..], self.load_mem(bus, aligned_addr + 8));
                let val = u8x16::load(&buffer, 0);
                let reg = self.read_vec_u8(instr.vt());
                let result = if vf == VLS_Q {
                    let mask = self.tables.keep_swap_r[offset as usize];
                    let shift = self.tables.shift_swap_l[offset as usize];
                    (reg & mask) | val.shuffle_bytes(shift)
                } else {
                    // XXX: panics when offset == 0
                    let mask = self.tables.keep_swap_l[16 - offset as usize];
                    let shift = self.tables.shift_swap_r[16 - offset as usize];
                    (reg & mask) | val.shuffle_bytes(shift)
                };
                self.write_vec_u8(instr.vt(), result);
                dprintln!(self, "{} $v{:02}     <- {} :  mem @ {:#x}",
                          INDENT, instr.vt(), format_vec(self.read_vec(instr.vt())), addr);
            },
            VLS_P | VLS_U => {
                // Load packed 8-bit signed/unsigned into 16-bit vectors.
                let addr = self.aligned_offset_shift(instr, 3, 1);
                self.restricted_vel_ls(instr, 16);  // ensure zero
                let dword = self.load_mem(bus, addr);
                let result = if vf == VLS_P {
                    mops::pack_signed(dword)
                } else {
                    mops::pack_unsigned(dword)
                };
                self.write_vec(instr.vt(), result);
            },
            VLS_H => {
                // Load packed bytes from 2 dwords with 2-byte stride into 16-bit vectors.
                let addr = self.aligned_offset_shift(instr, 4, 1);
                // unaligned is allowed, but we load from aligned
                let aligned_addr = addr & !15;
                self.restricted_vel_ls(instr, 16);  // ensure zero
                let dw1 = self.load_mem(bus, aligned_addr);
                let dw2 = self.load_mem(bus, aligned_addr + 8);
                let result = mops::pack_unsigned_alternate(dw1, dw2, addr & 1);
                self.write_vec(instr.vt(), result);
            },
            VLS_F => {
                // Load bytes from 2 dwords with 4-byte stride into 16-bit vectors.
                let addr = self.aligned_offset_shift(instr, 4, 1);
                let aligned_addr = addr & !15;
                let el = self.restricted_vel_ls(instr, 8);  // ensure 0 or 8
                let dw1 = self.load_mem(bus, aligned_addr);
                let dw2 = self.load_mem(bus, aligned_addr + 8);
                let result = mops::pack_unsigned_fourths(dw1, dw2, addr & 3, el == 0);
                self.write_vec(instr.vt(), result);
            },
            VLS_T => {
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
                                                       // BigEndian is actually correct!
                                                       BigEndian::read_i16(&buffer[i*2..])));
                }
            }
            _ => self.bug(format!("Unimplemented vector load format: {}", vf))
        }
    }

    fn mem_store_vec(&mut self, bus: &mut RspBus, instr: Instruction) {
        let vf = instr.vls_fmt();
        match vf {
            VLS_B | VLS_S | VLS_L | VLS_D => {
                // Store byte/hword/word/dword part of the vector
                // indexed by element.  Other parts are unaffected.
                let shift = vf as u64 & 0b11;
                let addr = self.aligned_offset_shift(instr, shift, 1);
                let el = self.restricted_vel_ls(instr, 1);
                let vel = el as u32 >> 1;
                let vec = self.cp2.vec[instr.vt()].to_u16();
                match vf {
                    VLS_B => {
                        let byte = if el & 1 == 0 {
                            (vec.extract(vel) >> 8) as u8
                        } else {
                            (vec.extract(vel) as u8)
                        };
                        dprintln!(self, "{} $v{:02}[{:02}] :  {:#18x} -> mem @ {:#x}",
                                  INDENT, instr.vt(), el, byte, addr);
                        self.store_mem(bus, addr, byte);
                    }
                    VLS_S => {
                        let data = vec.extract(vel);
                        dprintln!(self, "{} $v{:02}[{:02}] :  {:#18x} -> mem @ {:#x}",
                                  INDENT, instr.vt(), el, data, addr);
                        self.store_mem(bus, addr, data);
                    }
                    VLS_L => {
                        let data = (vec.extract(vel)     as u32) << 16 |
                                    vec.extract(vel + 1) as u32;
                        dprintln!(self, "{} $v{:02}[{:02}] :  {:#18x} -> mem @ {:#x}",
                                  INDENT, instr.vt(), el, data, addr);
                        self.store_mem(bus, addr, data);
                    }
                    VLS_D => {
                        let data = (vec.extract(vel)     as u64) << 48 |
                                   (vec.extract(vel + 1) as u64) << 32 |
                                   (vec.extract(vel + 2) as u64) << 16 |
                                    vec.extract(vel + 3) as u64;
                        dprintln!(self, "{} $v{:02}[{:02}] :  {:#18x} -> mem @ {:#x}",
                                  INDENT, instr.vt(), el, data, addr);
                        self.store_mem(bus, addr, data);
                    }
                    _    => unreachable!()
                }
            },
            VLS_Q | VLS_R => {
                // Store part of the vector into left/right part of quadword.
                let addr = self.aligned_offset_shift(instr, 4, 1);
                self.restricted_vel_ls(instr, 16);  // ensure zero
                let offset = addr & 0b1111;
                let aligned_addr = addr & !0b1111;
                let mut buffer = [0_u8; 16];
                BigEndian::write_u64(&mut buffer[0..], self.load_mem(bus, aligned_addr));
                BigEndian::write_u64(&mut buffer[8..], self.load_mem(bus, aligned_addr + 8));
                let val = u8x16::load(&buffer, 0);
                let reg = self.read_vec_u8(instr.vt());
                let result = if vf == VLS_Q {
                    let mask = self.tables.keep_l[offset as usize];
                    let shift = self.tables.shift_r[offset as usize];
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
            VLS_P | VLS_U => {
                // Store packed 8-bit signed/unsigned from 16-bit vectors.
                let addr = self.aligned_offset_shift(instr, 3, 1);
                self.restricted_vel_ls(instr, 16);  // ensure zero
                let vec = self.read_vec(instr.vt());
                let dword = if vf == VLS_P {
                    mops::unpack_signed(vec)
                } else {
                    mops::unpack_unsigned(vec)
                };
                self.store_mem(bus, addr, dword);
            },
            VLS_H => {
                // Store bytes to 2 dwords with 2-byte stride from 16-bit vectors.
                let addr = self.aligned_offset_shift(instr, 4, 1);
                // unaligned is allowed, but we load from aligned
                let aligned_addr = addr & !15;
                self.restricted_vel_ls(instr, 16);  // ensure zero
                let vec = self.read_vec(instr.vt());
                let (dw1, dw2) = mops::unpack_unsigned_alternate(vec, addr & 1);
                self.store_mem(bus, aligned_addr, dw1);
                self.store_mem(bus, aligned_addr + 8, dw2);
            },
            VLS_F => {
                // Store bytes to 2 dwords with 4-byte stride from 16-bit vectors.
                let addr = self.aligned_offset_shift(instr, 4, 1);
                let aligned_addr = addr & !15;
                let el = self.restricted_vel_ls(instr, 8);  // ensure 0 or 8
                let vec = self.read_vec(instr.vt());
                let dw1 = self.load_mem(bus, aligned_addr);
                let dw2 = self.load_mem(bus, aligned_addr + 8);
                let (dw1, dw2) = mops::unpack_unsigned_fourths(vec, dw1, dw2, addr & 3, el == 0);
                self.store_mem(bus, aligned_addr, dw1);
                self.store_mem(bus, aligned_addr + 8, dw2);
            },
            VLS_W => {
                // Store bytes from vector, starting at element and wrapping around.
                let addr = self.aligned_offset_shift(instr, 4, 1);
                let el = self.restricted_vel_ls(instr, 1);  // any value allowed
                let vec = self.read_vec_u8(instr.vt());
                let vec = vec.shuffle_bytes(self.tables.rot_swap_l[el]);
                let mut buffer = [0_u8; 16];
                vec.store(&mut buffer, 0);
                self.store_mem(bus, addr, BigEndian::read_u64(&buffer[0..]));
                self.store_mem(bus, addr + 8, BigEndian::read_u64(&buffer[8..]));
            },
            VLS_T => {
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
        self.cp2.vec[index]
    }

    fn read_vec_u8(&self, index: usize) -> u8x16 {
        unsafe { mem::transmute_copy(&self.cp2.vec[index]) }
    }

    fn read_and_shuffle_vec(&self, index: usize, elements: usize) -> i16x8 {
        let vec = self.read_vec_u8(index).shuffle_bytes(self.tables.el_shuf[elements]);
        unsafe { mem::transmute(vec) }
    }

    fn write_vec(&mut self, index: usize, value: i16x8) {
        self.cp2.vec[index] = value;
    }

    fn write_vec_u8(&mut self, index: usize, value: u8x16) {
        self.cp2.vec[index] = unsafe { mem::transmute(value) };
    }

    pub fn read_acc(&self, index: usize) -> i16x8 {
        self.cp2.acc[index]
    }

    pub fn write_acc(&mut self, index: usize, value: i16x8) {
        self.cp2.acc[index] = value;
    }

    pub fn read_flags(&self, index: usize, offset: usize) -> i16x8 {
        if offset == 0 {
            self.cp2.flags[index].0
        } else {
            self.cp2.flags[index].1
        }
    }

    pub fn write_flags(&mut self, index: usize, offset: usize, value: i16x8) {
        if offset == 0 {
            self.cp2.flags[index].0 = value;
        } else {
            self.cp2.flags[index].1 = value;
        }
    }

    pub fn get_cp2(&mut self) -> &mut Cp2 {
        &mut self.cp2
    }
}
