use std::fmt;
use std::mem;
use std::sync::{Arc, Condvar, Mutex, RwLock};
use std::sync::atomic::AtomicBool;
use simd::{u8x16, u16x8, i16x8, bool16ix8, i32x4};
use simd::x86::sse2::{Sse2I8x16, Sse2I16x8, Sse2U16x8, Sse2I32x4};
use simd::x86::ssse3::Ssse3U8x16;
use byteorder::{ByteOrder, BigEndian, LittleEndian};
#[cfg(debug_assertions)]
use ansi_term;

use cp2::Cp2;
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
    #[allow(dead_code)] run_bit:   Arc<AtomicBool>,  /* TODO: remove if unneeded */
    run_cond:  Arc<Condvar>,
    tables:    SimdTables,
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
        try!(write!(f, "     pc = {:016x}\n\n", self.regs.pc));
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

    fn aligned_offset(&self, instr: &Instruction, _: u64) -> u64 {
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
    }

    fn dispatch_op(&mut self, bus: &mut RspBus, instr: &Instruction) {
        match instr.opcode() {
            LWC2   => self.mem_load_vec(bus, instr),
            SWC2   => self.mem_store_vec(bus, instr),
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
            _  => self.bug(format!("#UD CP0: I {:#b} -- {:?}", instr.0, instr))
        }
    }

    fn dispatch_cop1_op(&mut self, _: &mut RspBus, instr: &Instruction) {
        self.bug(format!("#CU CP1: I {:#b} -- {:?}", instr.0, instr))
    }

    fn dispatch_cop2_op(&mut self, _: &mut RspBus, instr: &Instruction) {
        // println!("{:?}", instr);
        match instr.cop_op() {
            CF => {
                // TODO: check rd range (0-2)
                let flo = self.read_flags(instr.rd(), LO);
                let fhi = self.read_flags(instr.rd(), HI);
                let fcomb = flo.packs(fhi);
                let res = fcomb.move_mask() as i32 as u64;
                self.write_gpr(instr.rt(), res);
            },
            c  => {
                if c & 0b10000 == 0 {
                    self.bug(format!("#UD CP2: I {:#b} -- {:?}", instr.0, instr));
                }
                let op = instr.special_op();
                // These operations are basically just translated from CEN64's
                // C code.  Didn't bother copying the comments.
                match op {
                    VSAR  => {
                        match instr.vel() {
                            8  => self.cp2.vec[instr.vd()] = self.cp2.acc[ACC_HI],
                            9  => self.cp2.vec[instr.vd()] = self.cp2.acc[ACC_MD],
                            10 => self.cp2.vec[instr.vd()] = self.cp2.acc[ACC_LO],
                            _  => self.cp2.vec[instr.vd()] = [0; 16],
                        }
                    },
                    VADD  => self.vec_binop(instr, |vs, vt, cpu| {
                        // Signed sum of  carry + source1 + source2.
                        let carry = cpu.read_flags(VCO, LO);
                        let vd = vs + vt;
                        cpu.write_acc(ACC_LO, vd - carry);
                        let min = vs.min(vt);
                        let max = vs.max(vt);
                        let min = min.subs(carry);
                        cpu.write_flags(VCO, HI, zero());
                        cpu.write_flags(VCO, LO, zero());
                        min.adds(max)
                    }),
                    VADDC => self.vec_binop(instr, |vs, vt, cpu| {
                        let sat_sum = vs.to_u16().adds(vt.to_u16()).to_i16();
                        let unsat_sum = vs + vt;
                        let sn = frombool(sat_sum.ne(unsat_sum));
                        cpu.write_flags(VCO, HI, zero());
                        cpu.write_flags(VCO, LO, sn);
                        cpu.write_acc(ACC_LO, unsat_sum);
                        unsat_sum
                    }),
                    VSUB  => self.vec_binop(instr, |vs, vt, cpu| {
                        let carry = cpu.read_flags(VCO, LO);
                        let unsat_diff = vt - carry;
                        let sat_diff = vt.subs(carry);
                        cpu.write_acc(ACC_LO, vs - unsat_diff);
                        let vd = vs.subs(sat_diff);
                        let overflow = frombool(sat_diff.gt(unsat_diff));
                        cpu.write_flags(VCO, HI, zero());
                        cpu.write_flags(VCO, LO, zero());
                        vd.adds(overflow)
                    }),
                    VSUBC => self.vec_binop(instr, |vs, vt, cpu| {
                        let sat_udiff = vs.to_u16().subs(vt.to_u16()).to_i16();
                        let equal = frombool(vs.eq(vt));
                        let sat_udiff_zero = frombool(sat_udiff.eq(zero()));
                        let eq = frombool(equal.eq(zero()));
                        let sn = !equal & sat_udiff_zero;
                        let result = vs - vt;
                        cpu.write_flags(VCO, HI, eq);
                        cpu.write_flags(VCO, LO, sn);
                        cpu.write_acc(ACC_LO, result);
                        result
                    }),
                    VMULU | VMULF => self.vec_binop(instr, |vs, vt, cpu| {
                        let lo = vs * vt;
                        let sign1 = ((lo.to_u16() >> 15) as u16x8).to_i16();
                        let lo = lo + lo;
                        let round = i16x8::splat(1 << 15);
                        let hi = vs.mulhi(vt);
                        let sign2 = ((lo.to_u16() >> 15) as u16x8).to_i16();
                        cpu.write_acc(ACC_LO, round + lo);
                        let sign1 = sign1 + sign2;

                        let hi = hi << 1;
                        let eq = frombool(vs.eq(vt));
                        let neq = eq;
                        let accmd = sign1 + hi;
                        cpu.write_acc(ACC_MD, accmd);
                        let neg = accmd >> 15;

                        if op == VMULU {
                            let acchi = !eq & neg;
                            cpu.write_acc(ACC_HI, acchi);
                            let hi = accmd | neg;
                            !acchi & hi
                        } else {
                            let eq = eq & neg;
                            let acchi = !neq & neg;
                            cpu.write_acc(ACC_HI, acchi);
                            accmd + eq
                        }
                    }),
                    VMADN | VMUDN => self.vec_binop(instr, |vs, vt, cpu| {
                        let lo = vs * vt;
                        let hi = vs.to_u16().mulhi(vt.to_u16()).to_i16();
                        let sign = vt >> 15;
                        let vs = vs & sign;
                        let hi = hi - vs;

                        if op == VMADN {
                            let acc_lo = cpu.read_acc(ACC_LO);
                            let acc_md = cpu.read_acc(ACC_MD);
                            let acc_hi = cpu.read_acc(ACC_HI);

                            let overflow_mask = acc_lo.to_u16().adds(lo.to_u16()).to_i16();
                            let acc_lo = acc_lo + lo;

                            let overflow_mask = frombool(acc_lo.ne(overflow_mask));

                            let hi = hi - overflow_mask;

                            let overflow_mask = acc_md.to_u16().adds(hi.to_u16()).to_i16();
                            let acc_md = acc_md + hi;

                            let overflow_mask = frombool(acc_md.ne(overflow_mask));

                            let acc_hi = acc_hi + (hi >> 15);
                            let acc_hi = acc_hi - overflow_mask;

                            cpu.write_acc(ACC_LO, acc_lo);
                            cpu.write_acc(ACC_MD, acc_md);
                            cpu.write_acc(ACC_HI, acc_hi);
                            uclamp_acc(acc_lo, acc_md, acc_hi)
                        } else {
                            cpu.write_acc(ACC_LO, lo);
                            cpu.write_acc(ACC_MD, hi);
                            cpu.write_acc(ACC_HI, hi >> 15);
                            lo
                        }
                    }),
                    VMADH | VMUDH => self.vec_binop(instr, |vs, vt, cpu| {
                        let lo = vs * vt;
                        let hi = vs.mulhi(vt);

                        let (acc_md, acc_hi) = if op == VMADH {
                            let acc_md = cpu.read_acc(ACC_MD);
                            let acc_hi = cpu.read_acc(ACC_HI);

                            let overflow_mask = acc_md.to_u16().adds(lo.to_u16()).to_i16();
                            let acc_md = acc_md + lo;

                            let overflow_mask = frombool(acc_md.ne(overflow_mask));

                            let hi = hi - overflow_mask;
                            let acc_hi = acc_hi + hi;

                            cpu.write_acc(ACC_MD, acc_md);
                            cpu.write_acc(ACC_HI, acc_hi);
                            (acc_md, acc_hi)
                        } else {
                            cpu.write_acc(ACC_LO, zero());
                            cpu.write_acc(ACC_MD, lo);
                            cpu.write_acc(ACC_HI, hi);
                            (lo, hi)
                        };
                        sclamp_acc_tomd(acc_md, acc_hi)
                    }),
                    VMADL | VMUDL => self.vec_binop(instr, |vs, vt, cpu| {
                        let hi = vs.to_u16().mulhi(vt.to_u16()).to_i16();

                        if op == VMADL {
                            let acc_lo = cpu.read_acc(ACC_LO);
                            let acc_md = cpu.read_acc(ACC_MD);
                            let acc_hi = cpu.read_acc(ACC_HI);

                            let overflow_mask = acc_lo.to_u16().adds(hi.to_u16()).to_i16();
                            let acc_lo = acc_lo + hi;

                            let overflow_mask = frombool(acc_lo.ne(overflow_mask));
                            let hi = zero() - overflow_mask;

                            let overflow_mask = acc_md.to_u16().adds(hi.to_u16()).to_i16();
                            let acc_md = acc_md + hi;

                            let overflow_mask = frombool(acc_md.ne(overflow_mask));
                            let acc_hi = acc_hi - overflow_mask;

                            cpu.write_acc(ACC_LO, acc_lo);
                            cpu.write_acc(ACC_MD, acc_md);
                            cpu.write_acc(ACC_HI, acc_hi);
                            uclamp_acc(acc_lo, acc_md, acc_hi)
                        } else {
                            cpu.write_acc(ACC_LO, hi);
                            cpu.write_acc(ACC_MD, zero());
                            cpu.write_acc(ACC_HI, zero());
                            hi
                        }
                    }),
                    VMADM | VMUDM => self.vec_binop(instr, |vs, vt, cpu| {
                        let lo = vs * vt;
                        let hi = vs.to_u16().mulhi(vt.to_u16()).to_i16();
                        let sign = vs >> 15;
                        let vt = vt & sign;
                        let hi = hi - vt;

                        if op == VMADM {
                            let acc_lo = cpu.read_acc(ACC_LO);
                            let acc_md = cpu.read_acc(ACC_MD);
                            let acc_hi = cpu.read_acc(ACC_HI);

                            let overflow_mask = acc_lo.to_u16().adds(lo.to_u16()).to_i16();
                            let acc_lo = acc_lo + lo;

                            let overflow_mask = frombool(acc_lo.ne(overflow_mask));

                            let hi = hi - overflow_mask;

                            let overflow_mask = acc_md.to_u16().adds(hi.to_u16()).to_i16();
                            let acc_md = acc_md + hi;

                            let overflow_mask = frombool(acc_md.ne(overflow_mask));

                            let acc_hi = acc_hi + (hi >> 15);
                            let acc_hi = acc_hi - overflow_mask;

                            cpu.write_acc(ACC_LO, acc_lo);
                            cpu.write_acc(ACC_MD, acc_md);
                            cpu.write_acc(ACC_HI, acc_hi);
                            sclamp_acc_tomd(acc_md, acc_hi)
                        } else {
                            cpu.write_acc(ACC_LO, lo);
                            cpu.write_acc(ACC_MD, hi);
                            cpu.write_acc(ACC_HI, hi >> 15);
                            hi
                        }
                    }),
                    VMACU | VMACF => self.vec_binop(instr, |vs, vt, cpu| {
                        let acc_lo = cpu.read_acc(ACC_LO);
                        let acc_md = cpu.read_acc(ACC_MD);
                        let acc_hi = cpu.read_acc(ACC_HI);

                        let lo = vs * vt;
                        let hi = vs.mulhi(vt);

                        let md = hi << 1;
                        let carry = ((lo.to_u16() >> 15) as u16x8).to_i16();
                        let hi = hi >> 15;
                        let md = md | carry;
                        let lo: i16x8 = lo << 1;

                        let overflow_mask = acc_lo.to_u16().adds(lo.to_u16()).to_i16();
                        let acc_lo = acc_lo + lo;

                        let overflow_mask = frombool(acc_lo.ne(overflow_mask));

                        let md: i16x8 = md - overflow_mask;
                        let carry = frombool(md.eq(zero()));
                        let carry = carry & overflow_mask;
                        let hi = hi - carry;

                        let overflow_mask = acc_md.to_u16().adds(md.to_u16()).to_i16();
                        let acc_md = acc_md + md;

                        let overflow_mask = frombool(acc_md.ne(overflow_mask));

                        let acc_hi = acc_hi + hi;
                        let acc_hi = acc_hi - overflow_mask;

                        let result = if op == VMACU {
                            let overflow_hi_mask: i16x8 = acc_hi >> 15;
                            let overflow_md_mask = acc_md >> 15;
                            let md = overflow_md_mask | acc_md;
                            let overflow_mask = frombool(acc_hi.gt(zero()));
                            let md = !overflow_hi_mask & md;
                            overflow_mask | md
                        } else {
                            sclamp_acc_tomd(acc_md, acc_hi)
                        };
                        cpu.write_acc(ACC_LO, acc_lo);
                        cpu.write_acc(ACC_MD, acc_md);
                        cpu.write_acc(ACC_HI, acc_hi);
                        result
                    }),
                    VABS  => self.vec_binop(instr, |vs, vt, cpu| {
                        let vs_zero = frombool(vs.eq(zero()));
                        let sign_lt = vs >> 15;
                        let vd = !vs_zero & vt;
                        let vd = vd ^ sign_lt;
                        cpu.write_acc(ACC_LO, vd - sign_lt);
                        vd.subs(sign_lt)
                    }),
                    VMOV  => self.vec_binop(instr, |_, vt, cpu| {
                        cpu.write_acc(ACC_LO, vt);
                        let data_el = vt.extract(instr.vs() as u32 & 0x7);
                        let reg = cpu.read_vec(instr.vd());
                        reg.replace(instr.vel() as u32 & 0x7, data_el)
                    }),
                    VAND  => self.vec_binop(instr, |vs, vt, cpu| {
                        let result = vs & vt;
                        cpu.write_acc(ACC_LO, result);
                        result
                    }),
                    VNAND => self.vec_binop(instr, |vs, vt, cpu| {
                        let result = !(vs & vt);
                        cpu.write_acc(ACC_LO, result);
                        result
                    }),
                    VOR   => self.vec_binop(instr, |vs, vt, cpu| {
                        let result = vs | vt;
                        cpu.write_acc(ACC_LO, result);
                        result
                    }),
                    VNOR  => self.vec_binop(instr, |vs, vt, cpu| {
                        let result = !(vs | vt);
                        cpu.write_acc(ACC_LO, result);
                        result
                    }),
                    VXOR  => self.vec_binop(instr, |vs, vt, cpu| {
                        let result = vs ^ vt;
                        cpu.write_acc(ACC_LO, result);
                        result
                    }),
                    VNXOR => self.vec_binop(instr, |vs, vt, cpu| {
                        let result = !(vs ^ vt);
                        cpu.write_acc(ACC_LO, result);
                        result
                    }),
                    VNOP | VNULL => self.vec_binop(instr, |vs, _, _| {
                        vs
                    }),
                    _     => self.bug(format!("#UD CP2: I {:#b} -- {:?}", instr.0, instr))
                }
            }
        }
    }
}

fn sclamp_acc_tomd(acc_md: i16x8, acc_hi: i16x8) -> i16x8 {
    let acc_md = acc_md.to_u16();
    let acc_hi = acc_hi.to_u16();
    // unpack intrinsics are missing...
    let l0 = acc_md.extract(0) as i32 | ((acc_hi.extract(0) as i32) << 16);
    let l1 = acc_md.extract(1) as i32 | ((acc_hi.extract(1) as i32) << 16);
    let l2 = acc_md.extract(2) as i32 | ((acc_hi.extract(2) as i32) << 16);
    let l3 = acc_md.extract(3) as i32 | ((acc_hi.extract(3) as i32) << 16);
    let h0 = acc_md.extract(4) as i32 | ((acc_hi.extract(4) as i32) << 16);
    let h1 = acc_md.extract(5) as i32 | ((acc_hi.extract(5) as i32) << 16);
    let h2 = acc_md.extract(6) as i32 | ((acc_hi.extract(6) as i32) << 16);
    let h3 = acc_md.extract(7) as i32 | ((acc_hi.extract(7) as i32) << 16);
    i32x4::new(l0, l1, l2, l3).packs(i32x4::new(h0, h1, h2, h3))
}

fn uclamp_acc(val: i16x8, acc_md: i16x8, acc_hi: i16x8) -> i16x8 {
    p128("val", val);
    p128("md", acc_md);
    p128("hi", acc_hi);
    let hi_negative: i16x8 = acc_hi >> 15;
    let md_negative = acc_md >> 15;
    let hi_sign_check = hi_negative.eq(acc_hi);
    let md_sign_check = hi_negative.eq(md_negative);
    let clamp_mask = hi_sign_check & md_sign_check;
    p128("cm", frombool(clamp_mask));

    let clamped_val = frombool(hi_negative.eq(zero()));
    let res = clamp_mask.select(val, clamped_val);
    p128("uclamp res", res);
    res
}


pub const VCO: usize = 0;
//pub const VCC: usize = 1;
//pub const VCE: usize = 2;
pub const HI:  usize = 0;
pub const LO:  usize = 16;

pub const ACC_HI:  usize = 0;
pub const ACC_MD:  usize = 1;
pub const ACC_LO:  usize = 2;


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

    // Load/store implementations

    fn aligned_shift_addr(&self, instr: &Instruction, shift: u64, align: u64) -> u64 {
        let addr = self.read_gpr(instr.base()).wrapping_add(instr.voff() << shift);
        if addr & (align - 1) != 0 {
            self.bug(format!("Address not aligned to {} bytes: {:#x}", align, addr));
        }
        addr
    }

    fn restricted_vdel(&self, instr: &Instruction, modulus: usize) -> usize {
        let vdel = instr.vdel();
        if vdel % modulus != 0 {
            self.bug(format!("Element spec not divisible by {}: {}", modulus, vdel));
        }
        vdel
    }

    // fn read_vec(&self, index: usize) -> i16x8 {
    //     self.cp2.regs[index]
    // }

    fn mem_load_vec(&mut self, bus: &RspBus, instr: &Instruction) {
        let vf = instr.vec_fmt();
        match vf {
            VLF_B | VLF_S | VLF_L | VLF_D => {
                // Load byte/hword/word/dword into part of the vector
                // indexed by element.  Other parts are unaffected.
                let shift = vf as u64 & 0b11;
                let addr = self.aligned_shift_addr(instr, shift, 1);
                let index = self.restricted_vdel(instr, 1 << shift);
                match vf {
                    VLF_B => {
                        let data = self.load_mem(bus, addr);
                        self.cp2.vec[instr.vt()][index] = data;
                    }
                    VLF_S => {
                        let data = self.load_mem(bus, addr);
                        LittleEndian::write_u16(&mut self.cp2.vec[instr.vt()][index..], data);
                    }
                    VLF_L => {
                        let data = self.load_mem(bus, addr);
                        LittleEndian::write_u32(&mut self.cp2.vec[instr.vt()][index..], data);
                    }
                    VLF_D => {
                        let data = self.load_mem(bus, addr);
                        LittleEndian::write_u64(&mut self.cp2.vec[instr.vt()][index..], data);
                    }
                    _    => unreachable!()
                }
            },
            VLF_Q | VLF_R => {
                // Load left/right part of quadword into part of the vector.
                let addr = self.aligned_shift_addr(instr, 4, 1);
                self.restricted_vdel(instr, 16);  // ensure zero
                let offset = addr & 0b1111;
                let aligned_addr = addr & !0b1111;
                let mut buffer = [0_u8; 16];
                // XXX: implement load_from for u8x16
                BigEndian::write_u64(&mut buffer[0..], self.load_mem(bus, aligned_addr));
                BigEndian::write_u64(&mut buffer[8..], self.load_mem(bus, aligned_addr + 8));
                let val = u8x16::load(&buffer, 0);
                let mut reg = u8x16::load(&self.cp2.vec[instr.vt()], 0);
                if vf == VLF_Q {
                    // XXX: precombine tables
                    let mask = self.tables.keep_r[offset as usize].shuffle_bytes(self.tables.bswap);
                    let shift = self.tables.shift_l[offset as usize].shuffle_bytes(self.tables.bswap);
                    reg = reg & mask;
                    reg = reg | val.shuffle_bytes(shift);
                } else {
                    // XXX: bug when offset == 0
                    // XXX: bswap
                    reg = reg & self.tables.keep_l[16 - offset as usize];
                    reg = reg | val.shuffle_bytes(self.tables.shift_r[16 - offset as usize]);
                }
                reg.store(&mut self.cp2.vec[instr.vt()], 0);
            }
            _ => self.bug(format!("Unimplemented vector load format: {}", vf))
        }
    }

    fn mem_store_vec(&mut self, bus: &mut RspBus, instr: &Instruction) {
        let vf = instr.vec_fmt();
        match vf {
            VLF_B | VLF_S | VLF_L | VLF_D => {
                // Store byte/hword/word/dword part of the vector
                // indexed by element.  Other parts are unaffected.
                let shift = vf as u64 & 0b11;
                let addr = self.aligned_shift_addr(instr, shift, 1);
                let index = self.restricted_vdel(instr, 1 << shift);
                match vf {
                    VLF_B => {
                        let data = self.cp2.vec[instr.vt()][index];
                        self.store_mem(bus, addr, data);
                    }
                    VLF_S => {
                        let data = LittleEndian::read_u16(&self.cp2.vec[instr.vt()][index..]);
                        self.store_mem(bus, addr, data);
                    }
                    VLF_L => {
                        let data = LittleEndian::read_u32(&self.cp2.vec[instr.vt()][index..]);
                        self.store_mem(bus, addr, data);
                    }
                    VLF_D => {
                        let data = LittleEndian::read_u64(&self.cp2.vec[instr.vt()][index..]);
                        self.store_mem(bus, addr, data);
                    }
                    _    => unreachable!()
                }
            },
            VLF_Q | VLF_R => {
                // Store part of the vector into left/right part of quadword.
                let addr = self.aligned_shift_addr(instr, 4, 1);
                self.restricted_vdel(instr, 16);  // ensure zero
                let offset = addr & 0b1111;
                let aligned_addr = addr & !0b1111;
                let mut buffer = [0_u8; 16];
                BigEndian::write_u64(&mut buffer[0..], self.load_mem(bus, aligned_addr));
                BigEndian::write_u64(&mut buffer[8..], self.load_mem(bus, aligned_addr + 8));
                let mut val = u8x16::load(&buffer, 0);
                let reg = u8x16::load(&self.cp2.vec[instr.vt()], 0);
                if vf == VLF_Q {
                    let mask = self.tables.keep_l[offset as usize];
                    let shift = self.tables.shift_r[offset as usize];
                    val = val & mask;
                    // XXX first shift or first swap?
                    val = val | reg.shuffle_bytes(self.tables.bswap).shuffle_bytes(shift);
                } else {
                    // XXX: bswap
                    // XXX: bug when offset == 0
                    val = val & self.tables.keep_r[16 - offset as usize];
                    val = val | reg.shuffle_bytes(self.tables.shift_l[16 - offset as usize]);
                }
                val.store(&mut buffer, 0);
                self.store_mem(bus, aligned_addr, BigEndian::read_u64(&buffer[0..]));
                self.store_mem(bus, aligned_addr + 8, BigEndian::read_u64(&buffer[8..]));
            }
            _ => self.bug(format!("Unimplemented vector load format: {}", vf))
        }
    }

    fn vec_binop<F>(&mut self, instr: &Instruction, func: F)
        where F: Fn(i16x8, i16x8, &mut Self) -> i16x8
    {
        let res: u8x16 = unsafe {
            let s1 = self.read_vec(instr.vs());
            let s2 = u8x16::load(&self.cp2.vec[instr.vt()], 0);
            let s2 = mem::transmute(s2.shuffle_bytes(self.tables.el_shuf[instr.vel()]));
            p128("vs", s1);
            p128("vt", s2);
            let res = func(s1, s2, self);
            p128("vres", res);
            mem::transmute(res)
        };
        res.store(&mut self.cp2.vec[instr.vd()], 0);
    }

    fn read_vec(&self, index: usize) -> i16x8 {
        unsafe {
            mem::transmute(u8x16::load(&self.cp2.vec[index], 0))
        }
    }

    fn read_acc(&self, index: usize) -> i16x8 {
        unsafe {
            mem::transmute(u8x16::load(&self.cp2.acc[index], 0))
        }
    }

    fn write_acc(&mut self, index: usize, value: i16x8) {
        let res: u8x16 = unsafe {  mem::transmute(value) };
        res.store(&mut self.cp2.acc[index], 0);
    }

    fn read_flags(&self, index: usize, offset: usize) -> i16x8 {
        unsafe {
            mem::transmute(u8x16::load(&self.cp2.flags[index], offset))
        }
    }

    fn write_flags(&mut self, index: usize, offset: usize, value: i16x8) {
        let res: u8x16 = unsafe {  mem::transmute(value) };
        res.store(&mut self.cp2.flags[index], offset);
    }
}

#[inline(always)]
fn zero() -> i16x8 {
    i16x8::splat(0)
}

#[inline(always)]
fn frombool(x: bool16ix8) -> i16x8 {
    x.select(i16x8::splat(-1), i16x8::splat(0))
}

fn p128(s: &'static str, v: i16x8) {
    println!("{}: {:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {:04x}",
             s,
             v.extract(0), v.extract(1), v.extract(2), v.extract(3),
             v.extract(4), v.extract(5), v.extract(6), v.extract(7));
}

// fn pu8(s: &'static str, v: u8x16) {
//     println!("{}: {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} \
//               {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}",
//              s,
//              v.extract(0), v.extract(1), v.extract(2), v.extract(3),
//              v.extract(4), v.extract(5), v.extract(6), v.extract(7),
//              v.extract(8), v.extract(9), v.extract(10), v.extract(11),
//              v.extract(12), v.extract(13), v.extract(14), v.extract(15));
// }
