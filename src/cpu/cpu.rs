use std::fmt;
use std::collections::VecDeque;
use ansi_term::Colour;

use super::instruction::*;
use super::exception::*;
use super::cp0::Cp0;
use super::types::*;
use mem_map::*;
use interconnect;
use debug::{Debugger, DebugSpecList};
use util::{mult_64_64_unsigned, mult_64_64_signed};

const NUM_GPR: usize = 32;

pub struct Cpu {
    instr_counter:      u32,
    debug_instrs_until: u32,
    debug_instrs:       bool,

    reg_gpr:    [u64; NUM_GPR],
    reg_fpr:    [[u8; 8]; NUM_GPR],

    reg_pc:     u64,
    last_pc:    u64,
    last_instr: u32,

    reg_hi:     u64,
    reg_lo:     u64,
    reg_llbit:  bool,
    reg_fcr31:  u32,

    in_branch_delay: bool,
    exc_pending: VecDeque<Exception>,

    cp0: Cp0,
    interconnect: interconnect::Interconnect
}

macro_rules! bug {
    ($cpu:expr, $($args:expr),+) => { $cpu.bug(format!($($args),+)); }
}

macro_rules! dprintln {
    ($cpu:expr, $($args:expr),+) => { if $cpu.debug_instrs {
        println!("{}", Colour::Blue.paint(format!($($args),+)));
    } }
    //($cpu:expr, $($args:expr),+) => { }
}


impl fmt::Debug for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for row in 0..8 {
            for col in 0..4 {
                let i = row + col * 8;
                try!(write!(f, "  {:2}:{} = {:016x}", i, REG_NAMES[i], self.reg_gpr[i]));
            }
            try!(write!(f, "\n"));
        }
        try!(write!(f, "     pc = {:016x}", self.reg_pc));
        try!(write!(f, "     hi = {:016x}     lo = {:016x}", self.reg_hi, self.reg_lo));
        write!(f, "     ll = {:16}\n", self.reg_llbit)
    }
}

pub const INDENT: &'static str = "                                       ";

impl Cpu {
    pub fn new(interconnect: interconnect::Interconnect) -> Cpu {
        Cpu {
            instr_counter:      0,
            debug_instrs_until: 0,
            debug_instrs:       false,

            in_branch_delay: false,
            exc_pending:     VecDeque::new(),

            reg_gpr:    [0; NUM_GPR],
            reg_fpr:    [[0; 8]; NUM_GPR],

            reg_pc:     0,
            last_pc:    0,
            last_instr: 0,

            reg_hi:     0,
            reg_lo:     0,
            reg_llbit:  false,
            reg_fcr31:  0,

            cp0: Cp0::default(),
            interconnect: interconnect,
        }
    }

    pub fn power_on_reset(&mut self) {
        self.cp0.power_on_reset();
        self.interconnect.power_on_reset();

        self.reg_pc = RESET_VECTOR;
    }

    // TODO: Different interface
    pub fn run(&mut self) {
        loop {
            self.run_instruction();
        }
    }

    pub fn run_branch_delay_slot(&mut self) {
        if self.in_branch_delay {
            bug!(self, "Branching in branch delay slot -- check semantics!");
        }
        self.in_branch_delay = true;
        self.run_instruction();
        self.in_branch_delay = false;
    }

    pub fn run_instruction(&mut self) {
        // Timer interrupt?
        self.cp0.reg_count = self.cp0.reg_count.wrapping_add(1);
        if self.cp0.reg_compare == self.cp0.reg_count {
            if self.cp0.reg_status.interrupts_enabled &&
                !self.cp0.reg_status.interrupt_mask.timer_interrupt &&
                !self.cp0.reg_status.exception_level
            {
                self.flag_exception(ExcType::Interrupt(Intr::Timer));
            }
        }

        // Do we need to process an exception?
        if let Some(exc) = self.exc_pending.pop_front() {
            println!("processing exception: {:?}", exc);
            self.cp0.reg_epc = self.last_pc;
            if self.in_branch_delay {
                self.cp0.reg_epc -= 4;
            }
            self.cp0.reg_cause.exc_in_delay_slot = self.in_branch_delay;
            self.cp0.reg_cause.coprocessor = exc.coprocessor();
            self.cp0.reg_cause.exception_code = exc.code();
            self.cp0.reg_cause.interrupts_pending = exc.interrupt_mask();
            self.cp0.reg_status.exception_level = true;
            self.reg_pc = exc.vector_location(self.cp0.reg_status.is_bootstrap());
        }

        self.instr_counter += 1;
        self.last_pc = self.reg_pc;
        let pc = self.reg_pc;
        self.last_instr = self.read_word(pc, true);
        let instr = Instruction(self.last_instr);

        // Debug stuff. This might not really belong here?
        let (debug_for, dump_here, break_here) =
            self.interconnect.debug_specs.check_instr(pc, &instr, &self.reg_gpr);
        if break_here {
            println!("{}", Colour::Red.paint(
                format!("at: {:#10x}   {:?}", pc as u32, instr)));
            if dump_here {
                println!("{:?}", self);
            }
            let mut debugger = Debugger::new();
            debugger.run_loop(self);
        } else if dump_here {
            println!("{}", Colour::Red.paint(
                format!("at: {:#10x}   {:?}", pc as u32, instr)));
            println!("{:?}", self);
        }
        if debug_for > 0 {
            self.debug_instrs = true;
            if debug_for > 1 {
                self.debug_instrs_until = self.instr_counter + debug_for;
            }
        } else if self.debug_instrs && self.instr_counter > self.debug_instrs_until {
            self.debug_instrs = false;
        }
        dprintln!(self, "op: {:#10x}   {:?}", pc as u32, instr);

        self.dispatch_instr(&instr);

        self.reg_pc += 4;
    }

    #[inline(always)]
    fn dispatch_instr(&mut self, instr: &Instruction) {
        match instr.opcode() {
            LUI   => {
                let val = instr.imm_sign_extended() << 16;
                dprintln!(self, "{} {} <- {:#x}", INDENT, REG_NAMES[instr.rt()], val);
                self.write_gpr(instr.rt(), val);
            }
            LW    => self.mem_load (instr, false, |word: u32| word as i32 as u64),
            LWU   => self.mem_load (instr, false, |word: u32| word as u64),
            SW    => self.mem_store(instr, false, |data| data as u32),
            // TODO: overflow exception
            ADDI  => self.binary_imm(instr, |rs| rs.wrapping_add(instr.imm_sign_extended()) as i32 as u64),
            ADDIU => self.binary_imm(instr, |rs| rs.wrapping_add(instr.imm_sign_extended()) as i32 as u64),
            ANDI  => self.binary_imm(instr, |rs| rs & instr.imm()),
            ORI   => self.binary_imm(instr, |rs| rs | instr.imm()),
            XORI  => self.binary_imm(instr, |rs| rs ^ instr.imm()),
            SLTI  => self.binary_imm(instr, |rs| ((rs as i64) < instr.imm_sign_extended() as i64) as u64),
            SLTIU => self.binary_imm(instr, |rs| (rs < instr.imm_sign_extended()) as u64),
            J     => {
                let addr = ((self.reg_pc + 4) & 0xffff_ffff_c000_0000) | (instr.j_target() << 2);
                self.jump(addr, 0);
            }
            JAL   => {
                let addr = ((self.reg_pc + 4) & 0xffff_ffff_c000_0000) | (instr.j_target() << 2);
                self.jump(addr, 31);
            }
            BEQ   => self.branch(instr, false, false, |cpu|
                                 cpu.read_gpr(instr.rs()) == cpu.read_gpr(instr.rt())),
            BEQL  => self.branch(instr, true, false, |cpu|
                                 cpu.read_gpr(instr.rs()) == cpu.read_gpr(instr.rt())),
            BNE   => self.branch(instr, false, false, |cpu|
                                 cpu.read_gpr(instr.rs()) != cpu.read_gpr(instr.rt())),
            BNEL  => self.branch(instr, true, false, |cpu|
                                 cpu.read_gpr(instr.rs()) != cpu.read_gpr(instr.rt())),
            BGTZ  => self.branch(instr, false, false, |cpu| {
                                 let v = cpu.read_gpr(instr.rs()); v != 0 && (v >> 63) == 0 }),
            BGTZL => self.branch(instr, true, false, |cpu| {
                                 let v = cpu.read_gpr(instr.rs()); v != 0 && (v >> 63) == 0 }),
            BLEZ  => self.branch(instr, false, false, |cpu| {
                                 let v = cpu.read_gpr(instr.rs()); v == 0 || (v >> 63) != 0 }),
            BLEZL => self.branch(instr, true, false, |cpu| {
                                 let v = cpu.read_gpr(instr.rs()); v == 0 || (v >> 63) != 0 }),
            // TODO: overflow exception
            DADDI => self.binary_imm(instr, |rs| rs.wrapping_add(instr.imm_sign_extended())),
            DADDIU => self.binary_imm(instr, |rs| rs.wrapping_add(instr.imm_sign_extended())),
            LB    => self.mem_load (instr, false, |byte: u8| byte as i8 as u64),
            LBU   => self.mem_load (instr, false, |byte: u8| byte as u64),
            LH    => self.mem_load (instr, false, |hword: u16| hword as i16 as u64),
            LHU   => self.mem_load (instr, false, |hword: u16| hword as u64),
            LD    => self.mem_load (instr, false, |dword: u64| dword),
            SB    => self.mem_store(instr, false, |data| data as u8),
            SH    => self.mem_store(instr, false, |data| data as u16),
            SD    => self.mem_store(instr, false, |data| data),
            LWL   => self.mem_load_unaligned (instr, false, |data: u32| data as i32 as u64),
            LWR   => self.mem_load_unaligned (instr, true,  |data: u32| data as i32 as u64),
            LDL   => self.mem_load_unaligned (instr, false, |data: u64| data),
            LDR   => self.mem_load_unaligned (instr, true,  |data: u64| data),
            SWL   => self.mem_store_unaligned(instr, false, |mask, data: u32, reg|
                                              ((data as u64 & !mask) | (reg & mask)) as u32),
            SWR   => self.mem_store_unaligned(instr, true,  |mask, data: u32, reg|
                                              ((data as u64 & !mask) | (reg & mask)) as u32),
            SDL   => self.mem_store_unaligned(instr, false, |mask, data: u64, reg|
                                              (data & !mask) | (reg & mask)),
            SDR   => self.mem_store_unaligned(instr, true,  |mask, data: u64, reg|
                                              (data & !mask) | (reg & mask)),
            LL    => self.mem_load (instr, true, |data: u32| data as i32 as u64),
            LLD   => self.mem_load (instr, true, |data: u64| data),
            SC    => self.mem_store(instr, true, |data| data as u32),
            SCD   => self.mem_store(instr, true, |data| data),
            CACHE => {
                // TODO: Check if we need to implement this
            }
            SPECIAL => match instr.special_op() {
                JR   => { let addr = self.read_gpr(instr.rs()); self.jump(addr, 0); }
                JALR => { let addr = self.read_gpr(instr.rs()); self.jump(addr, instr.rd()); }
                // TODO: Overflow exception
                ADD  => self.binary(instr, |rs, rt| (rs as i32).wrapping_add(rt as i32) as i32 as u64),
                ADDU => self.binary(instr, |rs, rt| (rs as u32).wrapping_add(rt as u32) as i32 as u64),
                // TODO: Overflow exception
                SUB  => self.binary(instr, |rs, rt| (rs as i32).wrapping_sub(rt as i32) as i32 as u64),
                SUBU => self.binary(instr, |rs, rt| (rs as u32).wrapping_sub(rt as u32) as i32 as u64),
                AND  => self.binary(instr, |rs, rt| rs & rt),
                OR   => self.binary(instr, |rs, rt| rs | rt),
                XOR  => self.binary(instr, |rs, rt| rs ^ rt),
                NOR  => self.binary(instr, |rs, rt| !(rs | rt)),
                SLT  => self.binary(instr, |rs, rt| ((rs as i64) < rt as i64) as u64),
                SLTU => self.binary(instr, |rs, rt| (rs < rt) as u64),
                SLLV => self.binary(instr, |rs, rt| (rt << (rs & 0b11111)) as i32 as u64),
                SRAV => self.binary(instr, |rs, rt| (rt as i32 >> (rs & 0b11111)) as u64),
                SRLV => self.binary(instr, |rs, rt| (rt as u32 >> (rs & 0b11111)) as i32 as u64),
                SLL  => if instr.sa() != 0 { self.unary(instr, |rt| (rt << instr.sa()) as i32 as u64) },
                SRA  => self.unary(instr, |rt| (rt as i32 >> instr.sa()) as i32 as u64),
                SRL  => self.unary(instr, |rt| (rt as u32 >> instr.sa()) as i32 as u64),
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
                SYNC  => { }
                // SYSCALL
                _ => bug!(self, "#UD: I {:#b} -- {:?}", instr.0, instr)
            },
            REGIMM => match instr.regimm_op() {
                BGEZ    => self.branch(instr, false, false, |cpu| (cpu.read_gpr(instr.rs()) >> 63) == 0),
                BGEZL   => self.branch(instr, true,  false, |cpu| (cpu.read_gpr(instr.rs()) >> 63) == 0),
                BGEZAL  => self.branch(instr, false, true,  |cpu| (cpu.read_gpr(instr.rs()) >> 63) == 0),
                BGEZALL => self.branch(instr, true,  true,  |cpu| (cpu.read_gpr(instr.rs()) >> 63) == 0),
                BLTZ    => self.branch(instr, false, false, |cpu| (cpu.read_gpr(instr.rs()) >> 63) != 0),
                BLTZL   => self.branch(instr, true,  false, |cpu| (cpu.read_gpr(instr.rs()) >> 63) != 0),
                BLTZAL  => self.branch(instr, false, true,  |cpu| (cpu.read_gpr(instr.rs()) >> 63) != 0),
                BLTZALL => self.branch(instr, true,  true,  |cpu| (cpu.read_gpr(instr.rs()) >> 63) != 0),
                // TEQI, TGEI, TGEIU, TLTI, TLTIU, TNEI
                _ => bug!(self, "#UD: I {:#b} -- {:?}", instr.0, instr)
            },
            COP0 => match instr.cop_op() {
                // TODO: do these really transfer 64 bits?
                MF => {
                    let data = self.cp0.read_reg(instr.rd());
                    dprintln!(self, "{} cp0[{:2}] :  {:#18x}, {:#066b}",
                              INDENT, instr.rd(), data, data);
                    self.write_gpr(instr.rt(), data);
                }
                MT => {
                    let data = self.read_gpr(instr.rt());
                    // println!("{} cp0[{:2}] <- {:#066b}", INDENT, instr.rd(), data);
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
                            self.reg_pc = self.cp0.reg_error_epc - 4;
                            self.cp0.reg_status.error_level = false;
                        } else if self.cp0.reg_status.exception_level {
                            dprintln!(self, "{} return from exception to {:#x}",
                                      INDENT, self.cp0.reg_epc);
                            self.reg_pc = self.cp0.reg_epc - 4;
                            self.cp0.reg_status.exception_level = false;
                        } else {
                            bug!(self, "ERET without error/exception bit set");
                        }
                    }
                    TLBP  => { }
                    TLBR  => { }
                    TLBWI => { }
                    TLBWR => { }
                    _ => bug!(self, "#UD: I {:#b} -- {:?}", instr.0, instr)
                },
                _ => bug!(self, "#UD: I {:#b} -- {:?}", instr.0, instr)
            },
            COP1 => match instr.cop_op() {
                MF  => self.reg_store_fp(instr, |v: i32| v as u64),
                DMF => self.reg_store_fp(instr, |v: u64| v),
                MT  => self.reg_load_fp (instr, |v| v as u32),
                DMT => self.reg_load_fp (instr, |v| v),
                CF  => {
                    let data = match instr.fs() {
                        31 => self.reg_fcr31,
                        0  => 0xB << 8 | 0,  // TODO: constant
                        _  => bug!(self, "#RG: invalid read fp control reg {}", instr.fs())
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
                        bug!(self, "#RG: invalid write fp control reg {}", instr.fs());
                    }
                }
                BC  => match instr.regimm_op() {
                    BCF  => self.branch(instr, false, false, |cpu| cpu.reg_fcr31 & 0x80_0000 == 0),
                    BCFL => self.branch(instr, true,  false, |cpu| cpu.reg_fcr31 & 0x80_0000 == 0),
                    BCT  => self.branch(instr, false, false, |cpu| cpu.reg_fcr31 & 0x80_0000 != 0),
                    BCTL => self.branch(instr, true,  false, |cpu| cpu.reg_fcr31 & 0x80_0000 != 0),
                    _    => bug!(self, "#UD: {:#b} -- {:?}", instr.0, instr)
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
                        _     => bug!(self, "invalid FP source format: {:?}", instr)
                    },
                    FCVTD   => match instr.fp_fmt() {
                        FMT_S => self.fp_convert::<f32, _, _>(instr, |a| a as f64),
                        FMT_W => self.fp_convert::<i32, _, _>(instr, |a| a as f64),
                        FMT_L => self.fp_convert::<i64, _, _>(instr, |a| a as f64),
                        _     => bug!(self, "invalid FP source format: {:?}", instr)
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
                    _       => bug!(self, "#UD: I {:#b} -- {:?}", instr.0, instr)
                }
            },
            LDC1 => self.mem_load_fp::<u64>(instr),
            LWC1 => self.mem_load_fp::<u32>(instr),
            SDC1 => self.mem_store_fp::<u64>(instr),
            SWC1 => self.mem_store_fp::<u32>(instr),
            _    => bug!(self, "#UD: I {:#b} -- {:?}", instr.0, instr)
        }
    }

    fn mem_load<T: MemFmt, F>(&mut self, instr: &Instruction, linked: bool, func: F)
        where F: Fn(T) -> u64
    {
        let addr = self.aligned_addr(instr, T::get_align());
        if linked {
            self.cp0.reg_lladdr = (self.virt_addr_to_phys_addr(addr) as u32) & !0xF;
            self.reg_llbit = true;
        }
        let data = func(T::load_from(self, addr));
        dprintln!(self, "{} {} <- {:#18x} :  mem @ {:#x}",
                  INDENT, REG_NAMES[instr.rt()], data, addr);
        self.write_gpr(instr.rt(), data);
    }

    fn mem_store<T: MemFmt, F>(&mut self, instr: &Instruction, linked: bool, func: F)
        where F: Fn(u64) -> T
    {
        let addr = self.aligned_addr(instr, T::get_align());
        let data = self.read_gpr(instr.rt());
        let data = func(data);
        dprintln!(self, "{}       {:#18x} -> mem @ {:#x}", INDENT, data, addr);
        if linked {
            let llbit = self.reg_llbit;
            if llbit {
                T::store_to(self, addr, data);
            }
            self.write_gpr(instr.rt(), llbit as u64);
        } else {
            T::store_to(self, addr, data);
        }
    }

    fn mem_load_unaligned<T: MemFmt, F>(&mut self, instr: &Instruction, right: bool, func: F)
        where F: Fn(T) -> u64
    {
        let addr = self.aligned_addr(&instr, 1);
        let align = T::get_align();
        let aligned_addr = addr & !(align - 1);
        let offset = addr & (align - 1);
        let data = func(T::load_from(self, aligned_addr));
        let shift = if right { (3 - offset) << 3 } else { offset << 3 };
        let sh_data = if right { data >> shift } else { data << shift };
        let mask = if right { ((1 << (8 * align)) - 1) >> shift } else { !0 << shift };
        let orig_reg = self.read_gpr(instr.rt());
        let reg = (orig_reg & !mask) | (sh_data & mask);
        dprintln!(self, "{}       {:#18x} :  mem @ {:#x}", INDENT, data, aligned_addr);
        dprintln!(self, "{} {} <- {:#18x} :  mem @ {:#x}",
                  INDENT, REG_NAMES[instr.rt()], reg, addr);
        self.write_gpr(instr.rt(), reg);
    }

    fn mem_store_unaligned<T: MemFmt, F>(&mut self, instr: &Instruction, right: bool, func: F)
        where F: Fn(u64, T, u64) -> T
    {
        let addr = self.aligned_addr(&instr, 1);
        let align = T::get_align();
        let aligned_addr = addr & !(align - 1);
        let offset = addr & (align - 1);
        let reg = self.read_gpr(instr.rt());
        let shift = if right { (3 - offset) << 3 } else { offset << 3 };
        let sh_reg = if right { reg << shift } else { reg >> shift };
        let mask = if right { !0 << shift } else { !0 >> shift };
        let orig_data = T::load_from(self, aligned_addr);
        let data = func(mask, orig_data, sh_reg);
        dprintln!(self, "{}       {:#18x} :  mem @ {:#x}",
                  INDENT, orig_data, aligned_addr);
        dprintln!(self, "{}       {:#18x} -> mem @ {:#x}", INDENT, data, addr);
        T::store_to(self, aligned_addr, data);
    }

    fn binary_imm<F>(&mut self, instr: &Instruction, func: F)
        where F: Fn(u64) -> u64
    {
        let res = func(self.read_gpr(instr.rs()));
        dprintln!(self, "{} {} :  {:#18x}", INDENT, REG_NAMES[instr.rs()], self.read_gpr(instr.rs()));
        dprintln!(self, "{} {} <- {:#18x}", INDENT, REG_NAMES[instr.rt()], res);
        self.write_gpr(instr.rt(), res);
    }

    fn binary<F>(&mut self, instr: &Instruction, func: F)
        where F: Fn(u64, u64) -> u64
    {
        let res = func(self.read_gpr(instr.rs()), self.read_gpr(instr.rt()));
        dprintln!(self, "{} {} :  {:#18x}", INDENT, REG_NAMES[instr.rs()], self.read_gpr(instr.rs()));
        dprintln!(self, "{} {} :  {:#18x}", INDENT, REG_NAMES[instr.rt()], self.read_gpr(instr.rt()));
        dprintln!(self, "{} {} <- {:#18x}", INDENT, REG_NAMES[instr.rd()], res);
        self.write_gpr(instr.rd(), res);
    }

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

    fn unary<F>(&mut self, instr: &Instruction, func: F)
        where F: Fn(u64) -> u64
    {
        let res = func(self.read_gpr(instr.rt()));
        dprintln!(self, "{} {} :  {:#18x}", INDENT, REG_NAMES[instr.rt()], self.read_gpr(instr.rt()));
        dprintln!(self, "{} {} <- {:#18x}", INDENT, REG_NAMES[instr.rd()], res);
        self.write_gpr(instr.rd(), res);
    }

    fn jump(&mut self, addr: u64, link_reg: usize) {
        if addr & 0b11 != 0 {
            bug!(self, "Unaligned address in jump: {:#x}", addr);
        }
        if link_reg > 0 {
            let return_addr = self.reg_pc + 8;
            self.write_gpr(link_reg, return_addr);
            dprintln!(self, "{} {} <- {:#18x}", INDENT, REG_NAMES[link_reg], return_addr);
        }
        self.reg_pc += 4;
        self.run_branch_delay_slot();
        self.reg_pc = addr - 4;  // compensate for += 4 at the end of run_instruction
    }

    fn branch<P>(&mut self, instr: &Instruction, likely: bool, link: bool, mut predicate: P)
        where P: FnMut(&mut Self) -> bool
    {
        // Offset is supposed to be relative to the delay slot, but reg_pc points to
        // the branch instruction.  This is correct since we add 4 to reg_pc at the
        // end of the run_instruction() function.
        let addr = (instr.imm_sign_extended() << 2).wrapping_add(self.reg_pc);
        let take = predicate(self);
        if link {
            let return_addr = self.reg_pc + 8;
            self.write_gpr(31, return_addr);
        }
        dprintln!(self, "{} branch: {}", INDENT, if take { "taken" } else { "not taken" });
        // Run the delay slot
        self.reg_pc += 4;
        if take || !likely {
            self.run_branch_delay_slot();
            if take {
                self.reg_pc = addr;
            } else {
                self.reg_pc -= 4;
            }
        }
    }

    fn aligned_addr(&self, instr: &Instruction, align: u64) -> u64 {
        let addr = self.read_gpr(instr.base()).wrapping_add(instr.imm_sign_extended());
        if addr & (align - 1) != 0 {
            bug!(self, "Address not aligned to {} bytes: {:#x}", align, addr);
        }
        addr
    }

    fn fp_unary<T: FpFmt, F>(&mut self, instr: &Instruction, f: F)
        where F: Fn(T) -> T
    {
        let a = T::read_fpr(&self.reg_fpr[instr.fs()]);
        let res = f(a);
        dprintln!(self, "{} $f{} :  {:16.8}", INDENT, instr.fs(), a);
        dprintln!(self, "{} $f{} <- {:16.8}", INDENT, instr.fd(), res);
        T::write_fpr(&mut self.reg_fpr[instr.fd()], res);
    }

    fn fp_unary_2way<F, G>(&mut self, instr: &Instruction, f: F, g: G)
        where F: Fn(f32) -> f32, G: Fn(f64) -> f64
    {
        match instr.fp_fmt() {
            FMT_S => self.fp_unary(&instr, f),
            FMT_D => self.fp_unary(&instr, g),
            _     => { bug!(self, "invalid FP format: {:?}", instr); }
        }
    }

    fn fp_binary<T: FpFmt, F>(&mut self, instr: &Instruction, f: F)
        where F: Fn(T, T) -> T
    {
        let a = T::read_fpr(&self.reg_fpr[instr.fs()]);
        let b = T::read_fpr(&self.reg_fpr[instr.ft()]);
        let res = f(a, b);
        dprintln!(self, "{} $f{} :  {:16.8}", INDENT, instr.fs(), a);
        dprintln!(self, "{} $f{} :  {:16.8}", INDENT, instr.ft(), b);
        dprintln!(self, "{} $f{} <- {:16.8}", INDENT, instr.fd(), res);
        T::write_fpr(&mut self.reg_fpr[instr.fd()], res);
    }

    fn fp_binary_2way<F, G>(&mut self, instr: &Instruction, f: F, g: G)
        where F: Fn(f32, f32) -> f32, G: Fn(f64, f64) -> f64
    {
        match instr.fp_fmt() {
            FMT_S => self.fp_binary(&instr, f),
            FMT_D => self.fp_binary(&instr, g),
            _     => { bug!(self, "invalid FP format: {:?}", instr); }
        }
    }

    fn fp_convert<T: FpFmt, U: FpFmt, F>(&mut self, instr: &Instruction, f: F)
        where F: Fn(T) -> U
    {
        let value = T::read_fpr(&self.reg_fpr[instr.fs()]);
        let res = f(value);
        dprintln!(self, "{} $f{} :  {:16.8}", INDENT, instr.fs(), value);
        dprintln!(self, "{} $f{} <- {:16.8}", INDENT, instr.fd(), res);
        U::write_fpr(&mut self.reg_fpr[instr.fd()], res);
    }

    fn fp_convert_2way<U: FpFmt, F, G>(&mut self, instr: &Instruction, f: F, g: G)
        where F: Fn(f32) -> U, G: Fn(f64) -> U
    {
        match instr.fp_fmt() {
            FMT_S => self.fp_convert(&instr, f),
            FMT_D => self.fp_convert(&instr, g),
            _     => { bug!(self, "invalid FP format: {:?}", instr); }
        }
    }

    fn fp_compare<T: FpFmt, F>(&mut self, instr: &Instruction, signal: bool, f: F)
        where F: Fn(FpOrd) -> bool
    {
        let a = T::read_fpr(&self.reg_fpr[instr.fs()]);
        let b = T::read_fpr(&self.reg_fpr[instr.ft()]);
        if signal && (a.is_nan() || b.is_nan()) {
            // TODO: set FCR31 cause bit "invalid operation"
            self.flag_exception(ExcType::FloatingPointException);
            return;
        }
        let cond = f(FpOrd::from(a, b));
        dprintln!(self, "{} $f{} :  {:16.8}", INDENT, instr.fs(), a);
        dprintln!(self, "{} $f{} :  {:16.8}", INDENT, instr.ft(), b);
        dprintln!(self, "{} cond <- {}", INDENT, cond);
        self.reg_fcr31 = (self.reg_fcr31 & 0xff7f_ffff) | ((cond as u32) << 23);
    }

    fn fp_compare_2way<F>(&mut self, instr: &Instruction, signal: bool, f: F)
        where F: Fn(FpOrd) -> bool
    {
        match instr.fp_fmt() {
            FMT_S => self.fp_compare::<f32, _>(&instr, signal, f),
            FMT_D => self.fp_compare::<f64, _>(&instr, signal, f),
            _     => { bug!(self, "invalid FP format: {:?}", instr); }
        }
    }

    fn mem_load_fp<T: FpFmt + MemFmt>(&mut self, instr: &Instruction) {
        let addr = self.aligned_addr(&instr, T::get_align());
        let data = T::load_from(self, addr);
        dprintln!(self, "{} $f{} <- {:16.8} :  mem @ {:#x}",
                  INDENT, instr.ft(), data, addr);
        T::write_fpr(&mut self.reg_fpr[instr.ft()], data);
    }

    fn mem_store_fp<T: FpFmt + MemFmt>(&mut self, instr: &Instruction) {
        let addr = self.aligned_addr(&instr, T::get_align());
        let data = T::read_fpr(&self.reg_fpr[instr.ft()]);
        dprintln!(self, "{}         {:16.8} -> mem @ {:#x}", INDENT, data, addr);
        T::store_to(self, addr, data);
    }

    fn reg_load_fp<T: FpFmt, F>(&mut self, instr: &Instruction, func: F)
        where F: Fn(u64) -> T
    {
        let value = func(self.read_gpr(instr.rt()));
        dprintln!(self, "{} $f{} <- {:16.8}", INDENT, instr.fs(), value);
        T::write_fpr(&mut self.reg_fpr[instr.fs()], value);
    }

    fn reg_store_fp<T: FpFmt, F>(&mut self, instr: &Instruction, func: F)
        where F: Fn(T) -> u64
    {
        let value = T::read_fpr(&self.reg_fpr[instr.fs()]);
        dprintln!(self, "{} {} <- {:16.8}", INDENT, REG_NAMES[instr.rt()], value);
        self.write_gpr(instr.rt(), func(value));
    }

    pub fn read_word(&mut self, virt_addr: u64, load_instr: bool) -> u32 {
        let phys_addr = self.virt_addr_to_phys_addr(virt_addr);
        match self.interconnect.read_word(phys_addr as u32) {
            Ok(res) => {
                if !load_instr && self.debug_specs().matches_mem(phys_addr as u64, false) {
                    println!("Bus read:  {:#10x} :  {:#10x}", phys_addr, res);
                }
                res
            }
            Err(desc) => {
                bug!(self, "{}: ({:#x}) {:#x}", desc, virt_addr, phys_addr);
            }
        }
    }

    pub fn write_word(&mut self, virt_addr: u64, word: u32) {
        let phys_addr = self.virt_addr_to_phys_addr(virt_addr);
        if (phys_addr as u32) & !0xF == self.cp0.reg_lladdr {
            self.reg_llbit = false;
        }
        if self.debug_specs().matches_mem(phys_addr as u64, true) {
            println!("Bus write: {:#10x} <- {:#10x}", phys_addr, word);
        }
        if let Err(desc) = self.interconnect.write_word(phys_addr as u32, word) {
            bug!(self, "{}: ({:#x}) {:#x}", desc, virt_addr, phys_addr);
        }
    }

    pub fn read_dword(&mut self, virt_addr: u64) -> u64 {
        (self.read_word(virt_addr, false) as u64) << 32 |
        self.read_word(virt_addr + 4, false) as u64
    }

    pub fn write_dword(&mut self, virt_addr: u64, dword: u64) {
        self.write_word(virt_addr, (dword >> 32) as u32);
        self.write_word(virt_addr + 4, dword as u32);
    }

    fn virt_addr_to_phys_addr(&self, virt_addr: u64) -> u64 {
        // See Table 5-3 in the VR4300 User's Manual
        let addr_bit_values = (virt_addr >> 29) & 0b111;

        if addr_bit_values == 0b101 {
            // kseg1
            virt_addr - KSEG1_START
        } else if addr_bit_values == 0b100 {
            // kseg0 (cached)
            virt_addr - KSEG0_START
        } else {
            // TODO
            bug!(self, "Unrecognized virtual address: {:#x}", virt_addr);
        }
    }

    fn read_gpr(&self, index: usize) -> u64 {
        // Reg 0 is always 0 since we never write it
        self.reg_gpr[index]
    }

    fn write_gpr(&mut self, index: usize, value: u64) {
        if index != 0 {
            self.reg_gpr[index] = value;
        }
    }

    fn flag_exception(&mut self, exc_type: ExcType) {
        self.exc_pending.push_back(Exception { exc_type: exc_type });
    }

    #[cold]
    fn bug(&self, msg: String) -> ! {
        //println!("{:#?}", $cpu.interconnect);
        println!("\nCPU dump:\n{:?}", self);
        println!("last instr was:    {:?}", Instruction(self.last_instr));
        println!("#instrs executed:  {}", self.instr_counter);
        panic!(msg);
    }

    pub fn read_pc(&self) -> u64 {
        self.reg_pc
    }

    pub fn debug_specs(&mut self) -> &mut DebugSpecList {
        &mut self.interconnect.debug_specs
    }

    pub fn cp0_dump(&self) {
        println!("{:#?}", self.cp0);
    }

    pub fn cp1_dump(&self) {
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
}
