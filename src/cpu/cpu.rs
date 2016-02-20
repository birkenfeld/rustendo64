use std::cmp::Ordering;
use std::fmt;
use std::sync::RwLock;
use byteorder::{BigEndian, ByteOrder};
#[cfg(debug_assertions)]
use ansi_term;

use bus::Bus;
use bus::mem_map::*;
use r4k::instruction::*;
use r4k::{R4300, R4300Common, MemFmt};
use r4k::debug::DebugSpecList;
use util::{mult_64_64_unsigned, mult_64_64_signed};

use cp0::Cp0;
use exception::*;

pub const NUM_FPR: usize = 32;

#[derive(Default)]
pub struct Cpu {
    // Common members
    regs:        R4300Common,

    // Subcomponents
    cp0:         Cp0,

    // CPU-only registers
    reg_fpr:     [[u8; 8]; NUM_FPR],
    reg_hi:      u64,
    reg_lo:      u64,
    reg_llbit:   bool,
    reg_fcr31:   u32,
}

pub type CpuBus<'c> = Bus<'c, &'c mut [u32], &'c RwLock<Box<[u32]>>>;

#[cfg(debug_assertions)]
pub const INDENT: &'static str = "                                       ";


impl fmt::Debug for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for row in 0..8 {
            for col in 0..4 {
                let i = row + col * 8;
                try!(write!(f, "  {:2}:{} = {:016x}", i, REG_NAMES[i], self.regs.gpr[i]));
            }
            try!(write!(f, "\n"));
        }
        try!(write!(f, "     pc = {:016x}", self.regs.pc));
        try!(write!(f, "     hi = {:016x}     lo = {:016x}", self.reg_hi, self.reg_lo));
        write!(f, "     ll = {:16}\n", self.reg_llbit)
    }
}

impl<'c> R4300<'c> for Cpu {
    type Bus = CpuBus<'c>;

    fn read_instr(&self, bus: &CpuBus, virt_addr: u64) -> u32 {
        let phys_addr = self.translate_addr(virt_addr);
        match bus.read_word(phys_addr as u32) {
            Ok(res) => res,
            Err(desc) => {
                self.bug(format!("{}: ({:#x}) {:#x}", desc, virt_addr, phys_addr));
            }
        }
    }

    fn read_word(&self, bus: &CpuBus, virt_addr: u64) -> u32 {
        let phys_addr = self.translate_addr(virt_addr);
        match bus.read_word(phys_addr as u32) {
            Ok(res) => {
                self.debug_read(phys_addr, res);
                res
            },
            Err(desc) => {
                self.bug(format!("{}: ({:#x}) {:#x}", desc, virt_addr, phys_addr));
            }
        }
    }

    fn write_word(&mut self, bus: &mut CpuBus, virt_addr: u64, word: u32) {
        let phys_addr = self.translate_addr(virt_addr);
        if (phys_addr as u32) & !0xF == self.cp0.reg_lladdr {
            self.reg_llbit = false;
        }
        self.debug_write(phys_addr, word);
        if let Err(desc) = bus.write_word(phys_addr as u32, word) {
            self.bug(format!("{}: ({:#x}) {:#x}", desc, virt_addr, phys_addr));
        }
    }

    fn aligned_offset(&self, instr: &Instruction, align: u64) -> u64 {
        let addr = self.read_gpr(instr.base()).wrapping_add(instr.imm_sign_ext());
        if addr & (align - 1) != 0 {
            self.bug(format!("Address not aligned to {} bytes: {:#x}", align, addr));
        }
        addr
    }

    fn load_mem<T: MemFmt<'c, Self>>(&mut self, bus: &Self::Bus, addr: u64) -> T {
        T::load_from(self, bus, addr)
    }

    fn store_mem<T: MemFmt<'c, Self>>(&mut self, bus: &mut Self::Bus, addr: u64, data: T) {
        T::store_to(self, bus, addr, data)
    }

    fn check_interrupts(&mut self, bus: &mut CpuBus) {
        // Process interrupts from interconnect.
        if bus.has_interrupt() {
            self.flag_exception(Exception::Interrupt(Intr::Ext(0)));
        }

        // Timer interrupt?
        self.cp0.reg_count = self.cp0.reg_count.wrapping_add(1);
        if self.cp0.reg_compare == self.cp0.reg_count {
            self.flag_exception(Exception::Interrupt(Intr::Timer));
        }
    }

    fn ll_handler(&mut self, virt_addr: u64) {
        self.cp0.reg_lladdr = (self.translate_addr(virt_addr) as u32) & !0xF;
        self.reg_llbit = true;
    }

    fn sc_handler<T: MemFmt<'c, Self>>(&mut self, bus: &mut CpuBus<'c>, instr: &Instruction,
                                       virt_addr: u64, data: T) {
        let llbit = self.reg_llbit;
        if llbit {
            self.store_mem(bus, virt_addr, data);
        }
        self.write_gpr(instr.rt(), llbit as u64);
    }

    fn get_desc(&self) -> &'static str { "CPU" }
    fn get_regs(&self) -> &R4300Common { &self.regs }
    fn mut_regs(&mut self) -> &mut R4300Common { &mut self.regs }
    fn get_pc_mask(&self) -> u64 { !0 }

    #[cfg(debug_assertions)]
    fn get_debug_color(&self) -> ansi_term::Colour {
        ansi_term::Colour::Blue
    }

    #[cfg(debug_assertions)]
    fn cp0_dump(&self) {
        println!("{:#?}", self.cp0);
    }

    #[cfg(debug_assertions)]
    fn cp1_dump(&self) {
        for i in 0..32 {
            println!("  $f{:02} = {:12.6}s {:16.10}d {:11}w {:21}l", i,
                     f32::read_fpr(&self.reg_fpr[i]),
                     f64::read_fpr(&self.reg_fpr[i]),
                     i32::read_fpr(&self.reg_fpr[i]),
                     i64::read_fpr(&self.reg_fpr[i]));
        }
        println!(" fcr31 = {:#034b}", self.reg_fcr31);
        println!("           -------FC-----EVZOUIVZOUIVZOUIRM");
        println!("                         Cause Enab Flag   ");
        println!("");
    }

    #[cfg(debug_assertions)]
    fn cp2_dump(&self) {
    }

    fn dispatch_op(&mut self, bus: &mut CpuBus, instr: &Instruction) {
        match instr.opcode() {
            DADDI  => self.binary_imm(instr, |rs| rs.wrapping_add(instr.imm_sign_ext())),
            DADDIU => self.binary_imm(instr, |rs| rs.wrapping_add(instr.imm_sign_ext())),
            LL     => self.mem_load (bus, instr, true, |data: u32| data as i32 as u64),
            LLD    => self.mem_load (bus, instr, true, |data: u64| data),
            SC     => self.mem_store(bus, instr, true, |data| data as u32),
            SCD    => self.mem_store(bus, instr, true, |data| data),
            LDC1   => self.mem_load_fp::<u64> (bus, instr),
            LWC1   => self.mem_load_fp::<u32> (bus, instr),
            SDC1   => self.mem_store_fp::<u64>(bus, instr),
            SWC1   => self.mem_store_fp::<u32>(bus, instr),
            LWL    => self.mem_load_leftright (bus, instr, false, |data: u32| data as i32 as u64),
            LWR    => self.mem_load_leftright (bus, instr, true,  |data: u32| data as i32 as u64),
            LDL    => self.mem_load_leftright (bus, instr, false, |data: u64| data),
            LDR    => self.mem_load_leftright (bus, instr, true,  |data: u64| data),
            SWL    => self.mem_store_leftright(bus, instr, false, |mask, data: u32, reg|
                                               ((data as u64 & !mask) | (reg & mask)) as u32),
            SWR    => self.mem_store_leftright(bus, instr, true,  |mask, data: u32, reg|
                                               ((data as u64 & !mask) | (reg & mask)) as u32),
            SDL    => self.mem_store_leftright(bus, instr, false, |mask, data: u64, reg|
                                               (data & !mask) | (reg & mask)),
            SDR    => self.mem_store_leftright(bus, instr, true,  |mask, data: u64, reg|
                                               (data & !mask) | (reg & mask)),
            CACHE  => {
                /* TODO: do we need to implement this? */
            }
            _      => self.bug(format!("#UD: I {:#b} -- {:?}", instr.0, instr))
        }
    }

    fn dispatch_special_op(&mut self, _: &mut CpuBus, instr: &Instruction) {
        match instr.special_op() {
            // TODO: Overflow exception
            DADD   => self.binary(instr, |rs, rt| rs.wrapping_add(rt)),
            DADDU  => self.binary(instr, |rs, rt| rs.wrapping_add(rt)),
            // TODO: Overflow exception
            DSUB   => self.binary(instr, |rs, rt| rs.wrapping_sub(rt)),
            DSUBU  => self.binary(instr, |rs, rt| rs.wrapping_sub(rt)),
            DSLLV  => self.binary(instr, |rs, rt| rt << (rs & 0b111111)),
            DSRAV  => self.binary(instr, |rs, rt| (rt as i64 >> (rs & 0b111111)) as u64),
            DSRLV  => self.binary(instr, |rs, rt| rt >> (rs & 0b111111)),
            DSLL   => self.unary(instr, |rt| rt << instr.sa()),
            DSLL32 => self.unary(instr, |rt| rt << (instr.sa() + 32)),
            DSRA   => self.unary(instr, |rt| (rt as i64 >> instr.sa()) as u64),
            DSRA32 => self.unary(instr, |rt| (rt as i64 >> (instr.sa() + 32)) as u64),
            DSRL   => self.unary(instr, |rt| rt >> instr.sa()),
            DSRL32 => self.unary(instr, |rt| rt >> (instr.sa() + 32)),
            MFHI   => { let val = self.reg_hi; self.write_gpr(instr.rd(), val); }
            MFLO   => { let val = self.reg_lo; self.write_gpr(instr.rd(), val); }
            MTHI   => { let val = self.read_gpr(instr.rs()); self.reg_hi = val; }
            MTLO   => { let val = self.read_gpr(instr.rs()); self.reg_lo = val; }
            MULT   => self.binary_hilo(instr, |a, b| {
                let res = (a as i32 as i64) * (b as i32 as i64);
                (res as i32 as u64, (res >> 32) as i32 as u64) }),
            MULTU  => self.binary_hilo(instr, |a, b| {
                let res = (a & 0xffff_ffff) * (b & 0xffff_ffff);
                (res as i32 as u64, (res >> 32) as i32 as u64) }),
            DIV    => self.binary_hilo(instr, |a, b| ((a as i32 / b as i32) as u64,
                                                       (a as i32 % b as i32) as u64)),
            DIVU   => self.binary_hilo(instr, |a, b| ((a as u32 / b as u32) as u64,
                                                       (a as u32 % b as u32) as u64)),
            DMULT  => self.binary_hilo(instr, |a, b| mult_64_64_signed(a, b)),
            DMULTU => self.binary_hilo(instr, |a, b| mult_64_64_unsigned(a, b)),
            DDIV   => self.binary_hilo(instr, |a, b| ((a as i64 / b as i64) as u64,
                                                       (a as i64 % b as i64) as u64)),
            DDIVU  => self.binary_hilo(instr, |a, b| (a / b, a % b)),
            // TEQ, TGE, TGEU, TLT, TLTU, TNE
            SYNC   => { }
            // SYSCALL, BREAK
            _      => self.bug(format!("#UD: I {:#b} -- {:?}", instr.0, instr))
        }
    }

    fn dispatch_cop0_op(&mut self, _: &mut CpuBus, instr: &Instruction) {
        match instr.cop_op() {
            // TODO: do these really transfer 64 bits?
            MF => {
                let data = self.cp0.read_reg(instr.rd());
                dprintln!(self, "{} cp0[{:2}] :  {:#18x}, {:#066b}",
                          INDENT, instr.rd(), data, data);
                self.write_gpr(instr.rt(), data);
            }
            MT => {
                let data = self.read_gpr(instr.rt());
                dprintln!(self, "{} cp0[{:2}] <- {:#18x}, {:#066b}",
                          INDENT, instr.rd(), data, data);
                self.cp0.write_reg(instr.rd(), data);
            }
            // DMF, DMT
            CO => match instr.special_op() {
                ERET  => {
                    self.reg_llbit = false;
                    if self.cp0.reg_status.error_level {
                        dprintln!(self, "{} return from errorlevel to {:#x}",
                                  INDENT, self.cp0.reg_error_epc);
                        self.regs.next_pc = Some(self.cp0.reg_error_epc);
                        self.cp0.reg_status.error_level = false;
                    } else if self.cp0.reg_status.exception_level {
                        dprintln!(self, "{} return from exception to {:#x}",
                                  INDENT, self.cp0.reg_epc);
                        self.regs.next_pc = Some(self.cp0.reg_epc);
                        self.cp0.reg_status.exception_level = false;
                    } else {
                        self.bug(format!("ERET without error/exception bit set"));
                    }
                }
                TLBP  => { }
                TLBR  => { }
                TLBWI => { }
                TLBWR => { }
                _ => self.bug(format!("#UD: I {:#b} -- {:?}", instr.0, instr))
            },
            _ => self.bug(format!("#UD: I {:#b} -- {:?}", instr.0, instr))
        }
    }

    fn dispatch_cop1_op(&mut self, bus: &mut CpuBus, instr: &Instruction) {
        match instr.cop_op() {
            MF  => self.reg_store_fp(instr, |v: i32| v as u64),
            DMF => self.reg_store_fp(instr, |v: u64| v),
            MT  => self.reg_load_fp (instr, |v| v as u32),
            DMT => self.reg_load_fp (instr, |v| v),
            CF  => {
                let data = match instr.fs() {
                    31 => self.reg_fcr31,
                    0  => 0xB << 8 | 0,  // TODO: constant
                    _  => self.bug(format!("#RG: invalid read fp control reg {}", instr.fs()))
                };
                dprintln!(self, "{} cp1[{:2}] :  {:#18x}, {:#066b}",
                          INDENT, instr.fs(), data, data);
                self.write_gpr(instr.rt(), data as u64);
            }
            CT  => {
                let data = self.read_gpr(instr.rt());
                dprintln!(self, "{} cp1[{:2}] <- {:#18x}, {:#066b}",
                          INDENT, instr.fs(), data, data);
                if instr.fs() == 31 {
                    self.reg_fcr31 = data as u32;
                } else {
                    self.bug(format!("#RG: invalid write fp control reg {}", instr.fs()));
                }
            }
            BC  => match instr.regimm_op() {
                BCF  => self.branch(bus, instr, false, false, |cpu| cpu.reg_fcr31 & 0x80_0000 == 0),
                BCFL => self.branch(bus, instr, true,  false, |cpu| cpu.reg_fcr31 & 0x80_0000 == 0),
                BCT  => self.branch(bus, instr, false, false, |cpu| cpu.reg_fcr31 & 0x80_0000 != 0),
                BCTL => self.branch(bus, instr, true,  false, |cpu| cpu.reg_fcr31 & 0x80_0000 != 0),
                _    => self.bug(format!("#UD: {:#b} -- {:?}", instr.0, instr))
            },
            _   => match instr.fp_op() {
                // TODO: exceptions
                // Binary operations
                FADD    => self.fp_binary_2way(instr, |a, b| a + b, |a, b| a + b),
                FSUB    => self.fp_binary_2way(instr, |a, b| a - b, |a, b| a - b),
                FMUL    => self.fp_binary_2way(instr, |a, b| a * b, |a, b| a * b),
                FDIV    => self.fp_binary_2way(instr, |a, b| a / b, |a, b| a / b),
                // Unary operations
                FMOV    => self.fp_unary_2way(instr, |a| a,        |a| a),
                FNEG    => self.fp_unary_2way(instr, |a| -a,       |a| -a),
                FABS    => self.fp_unary_2way(instr, |a| a.abs(),  |a| a.abs()),
                FSQRT   => self.fp_unary_2way(instr, |a| a.sqrt(), |a| a.sqrt()),
                // Conversions
                FCVTS   => match instr.fp_fmt() {
                    FMT_D => self.fp_convert::<f64, _, _>(instr, |a| a as f32),
                    FMT_W => self.fp_convert::<i32, _, _>(instr, |a| a as f32),
                    FMT_L => self.fp_convert::<i64, _, _>(instr, |a| a as f32),
                    _     => self.bug(format!("invalid FP source format: {:?}", instr))
                },
                FCVTD   => match instr.fp_fmt() {
                    FMT_S => self.fp_convert::<f32, _, _>(instr, |a| a as f64),
                    FMT_W => self.fp_convert::<i32, _, _>(instr, |a| a as f64),
                    FMT_L => self.fp_convert::<i64, _, _>(instr, |a| a as f64),
                    _     => self.bug(format!("invalid FP source format: {:?}", instr))
                },
                FCVTW   => self.fp_convert_2way(instr, |a| a.round() as i32, |a| a.round() as i32),
                FCVTL   => self.fp_convert_2way(instr, |a| a.round() as i64, |a| a.round() as i64),
                FCEILW  => self.fp_convert_2way(instr, |a| a.ceil()  as i32, |a| a.ceil()  as i32),
                FCEILL  => self.fp_convert_2way(instr, |a| a.ceil()  as i64, |a| a.ceil()  as i64),
                FFLOORW => self.fp_convert_2way(instr, |a| a.floor() as i32, |a| a.floor() as i32),
                FFLOORL => self.fp_convert_2way(instr, |a| a.floor() as i64, |a| a.floor() as i64),
                FROUNDW => self.fp_convert_2way(instr, |a| a.round_to_even() as i32, |a| a.round_to_even() as i32),
                FROUNDL => self.fp_convert_2way(instr, |a| a.round_to_even() as i64, |a| a.round_to_even() as i64),
                FTRUNCW => self.fp_convert_2way(instr, |a| a.trunc() as i32, |a| a.trunc() as i32),
                FTRUNCL => self.fp_convert_2way(instr, |a| a.trunc() as i64, |a| a.trunc() as i64),
                // Compares
                FCF     => self.fp_compare_2way(instr, false, |_| false),
                FCUN    => self.fp_compare_2way(instr, false, |r| r == FpOrd::No),
                FCEQ    => self.fp_compare_2way(instr, false, |r| r == FpOrd::Eq),
                FCUEQ   => self.fp_compare_2way(instr, false, |r| r == FpOrd::Eq || r == FpOrd::No),
                FCOLT   => self.fp_compare_2way(instr, false, |r| r == FpOrd::Lt),
                FCULT   => self.fp_compare_2way(instr, false, |r| r == FpOrd::Lt || r == FpOrd::No),
                FCOLE   => self.fp_compare_2way(instr, false, |r| r != FpOrd::Gt && r != FpOrd::No),
                FCULE   => self.fp_compare_2way(instr, false, |r| r != FpOrd::Gt),
                FCSF    => self.fp_compare_2way(instr, true,  |_| false),
                FCNGLE  => self.fp_compare_2way(instr, true,  |r| r == FpOrd::No),
                FCSEQ   => self.fp_compare_2way(instr, true,  |r| r == FpOrd::Eq),
                FCNGL   => self.fp_compare_2way(instr, true,  |r| r == FpOrd::Eq || r == FpOrd::No),
                FCLT    => self.fp_compare_2way(instr, true,  |r| r == FpOrd::Lt),
                FCNGE   => self.fp_compare_2way(instr, true,  |r| r == FpOrd::Lt || r == FpOrd::No),
                FCLE    => self.fp_compare_2way(instr, true,  |r| r != FpOrd::Gt && r != FpOrd::No),
                FCNGT   => self.fp_compare_2way(instr, true,  |r| r != FpOrd::Gt),
                _       => self.bug(format!("#UD: I {:#b} -- {:?}", instr.0, instr))
            }
        }
    }

    fn dispatch_cop2_op(&mut self, _: &mut CpuBus, instr: &Instruction) {
        self.bug(format!("#CU CP2: I {:#b} -- {:?}", instr.0, instr))
    }
}

impl Cpu {
    #[cfg(debug_assertions)]
    pub fn new(debug: DebugSpecList) -> Self {
        let mut cpu = Self::default();
        cpu.mut_regs().debug_specs = debug;
        cpu
    }

    #[cfg(not(debug_assertions))]
    pub fn new(_: DebugSpecList) -> Self {
        Self::default()
    }

    pub fn power_on_reset(&mut self) {
        self.cp0.power_on_reset();
        self.regs.pc = RESET_VECTOR;
    }

    pub fn run_sequence(&mut self, bus: &mut CpuBus, n: usize) {
        /* TODO: tweak this */
        for _ in 0..n {
            self.run_instruction(bus);
        }
    }

    fn translate_addr(&self, virt_addr: u64) -> u64 {
        // See Table 5-3 in the VR4300 User's Manual
        match (virt_addr >> 29) & 0b111 {
            // kseg0 (cached)
            0b100 => virt_addr - KSEG0_START,
            // kseg1 (uncached)
            0b101 => virt_addr - KSEG1_START,
            // TODO
            _     => self.bug(format!("Unrecognized virtual address: {:#x}", virt_addr))
        }
    }

    // EXCEPTION HANDLING

    fn flag_exception(&mut self, exc: Exception) {
        if !self.cp0.reg_status.exception_level && exc.is_enabled(&self.cp0) {
            dprintln!(self, "Starting exception processing: {:?}", exc);
            self.cp0.reg_epc = self.regs.pc;
            if self.regs.in_branch_delay {
                self.cp0.reg_epc -= 4;
            }
            self.cp0.reg_cause.exc_in_delay_slot = self.regs.in_branch_delay;
            self.cp0.reg_cause.coprocessor = exc.coprocessor();
            self.cp0.reg_cause.exception_code = exc.code();
            self.cp0.reg_cause.interrupts_pending = exc.interrupt_mask();
            self.cp0.reg_status.exception_level = true;
            self.regs.pc = exc.vector_location(self.cp0.reg_status.is_bootstrap());
            self.regs.next_pc = Some(self.regs.pc);
        }
    }

    // MULT/DIV OPERATIONS

    fn binary_hilo<F>(&mut self, instr: &Instruction, func: F)
        where F: Fn(u64, u64) -> (u64, u64)
    {
        let a = self.read_gpr(instr.rs());
        let b = self.read_gpr(instr.rt());
        let (rlo, rhi) = func(a, b);
        dprintln!(self, "{} {} :  {:#18x}", INDENT, REG_NAMES[instr.rs()], a);
        dprintln!(self, "{} {} :  {:#18x}", INDENT, REG_NAMES[instr.rt()], b);
        dprintln!(self, "{} lo <- {:#18x}", INDENT, rlo);
        dprintln!(self, "{} hi <- {:#18x}", INDENT, rhi);
        self.reg_lo = rlo;
        self.reg_hi = rhi;
    }

    // LEFT/RIGHT LOADS/STORES

    fn mem_load_leftright<'c, T, F>(&mut self, bus: &CpuBus<'c>, instr: &Instruction,
                                    right: bool, func: F)
        where T: MemFmt<'c, Self>, F: Fn(T) -> u64
    {
        let addr = self.aligned_offset(&instr, 1);
        let align = T::get_align();
        let amask = align - 1;
        let aligned_addr = addr & !amask;
        let offset = addr & amask;
        let shift = if right { (amask - offset) * 8 } else { offset * 8 };
        let data = func(self.load_mem(bus, aligned_addr));
        let sh_data = if right { data >> shift } else { data << shift };
        let mask = if align == 8 { // XXX: can this be written easier?
            if right { !0 >> shift } else { !0 << shift }
        } else {
            if right { ((1 << (8 * align)) - 1) >> shift } else { !0 << shift }
        };
        let orig_reg = self.read_gpr(instr.rt());
        let reg = (orig_reg & !mask) | (sh_data & mask);
        dprintln!(self, "{}       {:#18x} :  mem @ {:#x}", INDENT, data, aligned_addr);
        dprintln!(self, "{} {} <- {:#18x} :  mem @ {:#x}",
                  INDENT, REG_NAMES[instr.rt()], reg, addr);
        self.write_gpr(instr.rt(), reg);
    }

    fn mem_store_leftright<'c, T, F>(&mut self, bus: &mut CpuBus<'c>, instr: &Instruction,
                                     right: bool, func: F)
        where T: MemFmt<'c, Self>, F: Fn(u64, T, u64) -> T
    {
        let addr = self.aligned_offset(&instr, 1);
        let align = T::get_align();
        let amask = align - 1;
        let aligned_addr = addr & !amask;
        let offset = addr & amask;
        let shift = if right { (amask - offset) * 8 } else { offset * 8 };
        let reg = self.read_gpr(instr.rt());
        let sh_reg = if right { reg << shift } else { reg >> shift };
        let mask = if right { !0 << shift } else { !0 >> shift };
        let orig_data = self.load_mem(bus, aligned_addr);
        let data = func(mask, orig_data, sh_reg);
        dprintln!(self, "{}       {:#18x} :  mem @ {:#x}",
                  INDENT, orig_data, aligned_addr);
        dprintln!(self, "{}       {:#18x} -> mem @ {:#x}", INDENT, data, addr);
        self.store_mem(bus, aligned_addr, data);
    }

    // FLOATING-POINT OPS

    fn fp_unary<T: FpFmt, F>(&mut self, instr: &Instruction, f: F)
        where F: Fn(T) -> T
    {
        let a = T::read_fpr(&self.reg_fpr[instr.fs()]);
        let res = f(a);
        dprintln!(self, "{} $f{:02} :  {:16.8}", INDENT, instr.fs(), a);
        dprintln!(self, "{} $f{:02} <- {:16.8}", INDENT, instr.fd(), res);
        T::write_fpr(&mut self.reg_fpr[instr.fd()], res);
    }

    fn fp_unary_2way<F, G>(&mut self, instr: &Instruction, f: F, g: G)
        where F: Fn(f32) -> f32, G: Fn(f64) -> f64
    {
        match instr.fp_fmt() {
            FMT_S => self.fp_unary(&instr, f),
            FMT_D => self.fp_unary(&instr, g),
            _     => self.bug(format!("invalid FP format: {:?}", instr)),
        }
    }

    fn fp_binary<T: FpFmt, F>(&mut self, instr: &Instruction, f: F)
        where F: Fn(T, T) -> T
    {
        let a = T::read_fpr(&self.reg_fpr[instr.fs()]);
        let b = T::read_fpr(&self.reg_fpr[instr.ft()]);
        let res = f(a, b);
        dprintln!(self, "{} $f{:02} :  {:16.8}", INDENT, instr.fs(), a);
        dprintln!(self, "{} $f{:02} :  {:16.8}", INDENT, instr.ft(), b);
        dprintln!(self, "{} $f{:02} <- {:16.8}", INDENT, instr.fd(), res);
        T::write_fpr(&mut self.reg_fpr[instr.fd()], res);
    }

    fn fp_binary_2way<F, G>(&mut self, instr: &Instruction, f: F, g: G)
        where F: Fn(f32, f32) -> f32, G: Fn(f64, f64) -> f64
    {
        match instr.fp_fmt() {
            FMT_S => self.fp_binary(&instr, f),
            FMT_D => self.fp_binary(&instr, g),
            _     => self.bug(format!("invalid FP format: {:?}", instr)),
        }
    }

    fn fp_convert<T: FpFmt, U: FpFmt, F>(&mut self, instr: &Instruction, f: F)
        where F: Fn(T) -> U
    {
        let value = T::read_fpr(&self.reg_fpr[instr.fs()]);
        let res = f(value);
        dprintln!(self, "{} $f{:02} :  {:16.8}", INDENT, instr.fs(), value);
        dprintln!(self, "{} $f{:02} <- {:16.8}", INDENT, instr.fd(), res);
        U::write_fpr(&mut self.reg_fpr[instr.fd()], res);
    }

    fn fp_convert_2way<U: FpFmt, F, G>(&mut self, instr: &Instruction, f: F, g: G)
        where F: Fn(f32) -> U, G: Fn(f64) -> U
    {
        match instr.fp_fmt() {
            FMT_S => self.fp_convert(&instr, f),
            FMT_D => self.fp_convert(&instr, g),
            _     => self.bug(format!("invalid FP format: {:?}", instr)),
        }
    }

    fn fp_compare<T: FpFmt, F>(&mut self, instr: &Instruction, signal: bool, f: F)
        where F: Fn(FpOrd) -> bool
    {
        let a = T::read_fpr(&self.reg_fpr[instr.fs()]);
        let b = T::read_fpr(&self.reg_fpr[instr.ft()]);
        if signal && (a.is_nan() || b.is_nan()) {
            // TODO: set FCR31 cause bit "invalid operation"
            self.flag_exception(Exception::FloatingPointException);
            return;
        }
        let cond = f(FpOrd::from(a, b));
        dprintln!(self, "{} $f{:02} :  {:16.8}", INDENT, instr.fs(), a);
        dprintln!(self, "{} $f{:02} :  {:16.8}", INDENT, instr.ft(), b);
        dprintln!(self, "{} cond <- {}", INDENT, cond);
        self.reg_fcr31 = (self.reg_fcr31 & 0xff7f_ffff) | ((cond as u32) << 23);
    }

    fn fp_compare_2way<F>(&mut self, instr: &Instruction, signal: bool, f: F)
        where F: Fn(FpOrd) -> bool
    {
        match instr.fp_fmt() {
            FMT_S => self.fp_compare::<f32, _>(&instr, signal, f),
            FMT_D => self.fp_compare::<f64, _>(&instr, signal, f),
            _     => self.bug(format!("invalid FP format: {:?}", instr)),
        }
    }

    fn mem_load_fp<'c, T: FpFmt + MemFmt<'c, Self>>(&mut self, bus: &CpuBus<'c>, instr: &Instruction) {
        let addr = self.aligned_offset(&instr, T::get_align());
        let data = self.load_mem(bus, addr);
        dprintln!(self, "{} $f{:02} <- {:16.8} :  mem @ {:#x}",
                  INDENT, instr.ft(), data, addr);
        let reg = instr.ft();
        if self.cp0.reg_status.additional_fp_regs {
            T::write_fpr(&mut self.reg_fpr[reg], data);
        } else {
            if reg & 1 != 0 {
                // fill the high bytes of the even-numbered register
                T::write_fpr_hi(&mut self.reg_fpr[reg & !1], data);
            } else {
                T::write_fpr(&mut self.reg_fpr[reg], data);
            }
        }
    }

    fn mem_store_fp<'c, T: FpFmt + MemFmt<'c, Self>>(&mut self, bus: &mut CpuBus<'c>, instr: &Instruction) {
        let addr = self.aligned_offset(&instr, T::get_align());
        let reg = instr.ft();
        let data = if self.cp0.reg_status.additional_fp_regs {
            T::read_fpr(&self.reg_fpr[reg])
        } else {
            if reg & 1 != 0 {
                // read the high bytes of the even-numbered register
                T::read_fpr_hi(&self.reg_fpr[reg & !1])
            } else {
                T::read_fpr(&self.reg_fpr[reg])
            }
        };
        dprintln!(self, "{}         {:16.8} -> mem @ {:#x}", INDENT, data, addr);
        self.store_mem(bus, addr, data);
    }

    fn reg_load_fp<T: FpFmt, F>(&mut self, instr: &Instruction, func: F)
        where F: Fn(u64) -> T
    {
        let data = func(self.read_gpr(instr.rt()));
        dprintln!(self, "{} $f{:02} <- {:16.8}", INDENT, instr.fs(), data);
        let reg = instr.fs();
        if self.cp0.reg_status.additional_fp_regs {
            T::write_fpr(&mut self.reg_fpr[reg], data);
        } else {
            if reg & 1 != 0 {
                // fill the high bytes of the even-numbered register
                T::write_fpr_hi(&mut self.reg_fpr[reg & !1], data);
            } else {
                T::write_fpr(&mut self.reg_fpr[reg], data);
            }
        }
    }

    fn reg_store_fp<T: FpFmt, F>(&mut self, instr: &Instruction, func: F)
        where F: Fn(T) -> u64
    {
        let reg = instr.fs();
        let data = if self.cp0.reg_status.additional_fp_regs {
            T::read_fpr(&self.reg_fpr[reg])
        } else {
            if reg & 1 != 0 {
                // read the high bytes of the even-numbered register
                T::read_fpr_hi(&self.reg_fpr[reg & !1])
            } else {
                T::read_fpr(&self.reg_fpr[reg])
            }
        };
        dprintln!(self, "{} {} <- {:16.8}", INDENT, REG_NAMES[instr.rt()], data);
        self.write_gpr(instr.rt(), func(data));
    }
}


#[derive(PartialEq, Eq)]
pub enum FpOrd {
    Eq,
    Gt,
    Lt,
    No
}

impl FpOrd {
    pub fn from<T: PartialOrd>(a: T, b: T) -> Self {
        match a.partial_cmp(&b) {
            None                    => FpOrd::No,
            Some(Ordering::Equal)   => FpOrd::Eq,
            Some(Ordering::Greater) => FpOrd::Gt,
            Some(Ordering::Less)    => FpOrd::Lt,
        }
    }
}

// TODO: verify this is the correct offset for half-width
const LO: usize = 4;

pub trait FpFmt: Copy + fmt::Display + PartialOrd {
    fn read_fpr(reg: &[u8; 8]) -> Self;
    fn read_fpr_hi(reg: &[u8; 8]) -> Self { Self::read_fpr(reg) }
    fn write_fpr(reg: &mut [u8; 8], value: Self);
    fn write_fpr_hi(reg: &mut [u8; 8], value: Self) { Self::write_fpr(reg, value); }
    fn is_nan(&self) -> bool { false }
}

impl FpFmt for f32 {
    fn read_fpr(reg: &[u8; 8]) -> f32 {
        BigEndian::read_f32(&reg[LO..])
    }
    fn read_fpr_hi(reg: &[u8; 8]) -> f32 {
        BigEndian::read_f32(&reg[..])
    }
    fn write_fpr(reg: &mut [u8; 8], value: f32) {
        BigEndian::write_f32(&mut reg[LO..], value);
    }
    fn write_fpr_hi(reg: &mut [u8; 8], value: f32) {
        BigEndian::write_f32(&mut reg[..], value);
    }
    fn is_nan(&self) -> bool { f32::is_nan(*self) }
}

impl FpFmt for f64 {
    fn read_fpr(reg: &[u8; 8]) -> f64 {
        BigEndian::read_f64(reg)
    }
    fn write_fpr(reg: &mut [u8; 8], value: f64) {
        BigEndian::write_f64(reg, value);
    }
    fn is_nan(&self) -> bool { f64::is_nan(*self) }
}

impl FpFmt for i32 {
    fn read_fpr(reg: &[u8; 8]) -> i32 {
        BigEndian::read_i32(&reg[LO..])
    }
    fn read_fpr_hi(reg: &[u8; 8]) -> i32 {
        BigEndian::read_i32(&reg[..])
    }
    fn write_fpr(reg: &mut [u8; 8], value: i32) {
        BigEndian::write_i32(&mut reg[LO..], value);
    }
    fn write_fpr_hi(reg: &mut [u8; 8], value: i32) {
        BigEndian::write_i32(&mut reg[..], value);
    }
}

impl FpFmt for i64 {
    fn read_fpr(reg: &[u8; 8]) -> i64 {
        BigEndian::read_i64(reg)
    }
    fn write_fpr(reg: &mut [u8; 8], value: i64) {
        BigEndian::write_i64(reg, value);
    }
}

// For memory loads/stores.
impl FpFmt for u32 {
    fn read_fpr(reg: &[u8; 8]) -> u32 {
        BigEndian::read_u32(&reg[LO..])
    }
    fn read_fpr_hi(reg: &[u8; 8]) -> u32 {
        BigEndian::read_u32(&reg[..])
    }
    fn write_fpr(reg: &mut [u8; 8], value: u32) {
        BigEndian::write_u32(&mut reg[LO..], value);
    }
    fn write_fpr_hi(reg: &mut [u8; 8], value: u32) {
        BigEndian::write_u32(&mut reg[..], value);
    }
}

impl FpFmt for u64 {
    fn read_fpr(reg: &[u8; 8]) -> u64 {
        BigEndian::read_u64(reg)
    }
    fn write_fpr(reg: &mut [u8; 8], value: u64) {
        BigEndian::write_u64(reg, value);
    }
}

pub trait FpRoundExt {
    fn round_to_even(&self) -> Self;
}

macro_rules! impl_round {
    ($ty:ty) => {
        impl FpRoundExt for $ty {
            /// Very naive implementation of round-to-even.
            fn round_to_even(&self) -> Self {
                let i = self.floor();
                let r = self - i;
                match r.partial_cmp(&0.5) {
                    Some(Ordering::Less)                    => i,
                    Some(Ordering::Greater)                 => i + 1.0,
                    Some(Ordering::Equal) if i % 2.0 == 0.0 => i,
                    Some(Ordering::Equal)                   => i + 1.0,
                    None                                    => *self,
                }
            }
        }
    }
}

impl_round!(f32);
impl_round!(f64);
