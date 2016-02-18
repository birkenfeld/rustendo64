use std::cmp::Ordering;
use std::fmt;
use std::sync::RwLock;
use byteorder::{BigEndian, ByteOrder};

use bus::Bus;
use bus::mem_map::*;
use cpu::cp0::Cp0;
use cpu::exception::*;
use vr4k::instruction::*;
use vr4k::types::*;
use debug::DebugSpecList;
use util::{mult_64_64_unsigned, mult_64_64_signed};

const NUM_GPR: usize = 32;

#[derive(Default)]
pub struct Cpu {
    // Debugging info
    pub debug_specs:    DebugSpecList,
    instr_counter:      u64,
    last_instr:         Instruction,
    #[cfg(debug_assertions)] debug_print: bool,
    #[cfg(debug_assertions)] debug_until: u64,

    // Helpers
    in_branch_delay:    bool,
    next_pc:            Option<u64>,

    // Subcomponents
    cp0:                Cp0,

    // Registers
    reg_gpr:            [u64; NUM_GPR],
    reg_fpr:            [[u8; 8]; NUM_GPR],
    reg_pc:             u64,
    reg_hi:             u64,
    reg_lo:             u64,
    reg_llbit:          bool,
    reg_fcr31:          u32,
}

pub type CpuBus<'c> = Bus<'c, &'c mut [u32], &'c RwLock<Box<[u32]>>>;

#[cfg(debug_assertions)]
macro_rules! dprintln {
    ($cpu:expr, $($args:expr),+) => {
        if $cpu.debug_print {
            use ansi_term::Colour;
            println!("{}", Colour::Blue.paint(format!($($args),+)));
        }
    }
}

#[cfg(debug_assertions)]
pub const INDENT: &'static str = "                                       ";

#[cfg(not(debug_assertions))]
macro_rules! dprintln {
    ($cpu:expr, $($args:expr),+) => { }
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

impl Cpu {
    pub fn new(debug: DebugSpecList) -> Cpu {
        Cpu {
            debug_specs: debug, .. Cpu::default()
        }
    }

    pub fn power_on_reset(&mut self) {
        self.cp0.power_on_reset();
        self.reg_pc = RESET_VECTOR;
    }

    pub fn run_instruction(&mut self, bus: &mut CpuBus) {
        // Process interrupts from interconnect.
        if bus.has_interrupt() {
            self.flag_exception(Exception::Interrupt(Intr::Ext(0)));
        }

        // Timer interrupt?
        self.cp0.reg_count = self.cp0.reg_count.wrapping_add(1);
        if self.cp0.reg_compare == self.cp0.reg_count {
            self.flag_exception(Exception::Interrupt(Intr::Timer));
        }

        // Read next instruction.
        let pc = self.reg_pc;
        self.last_instr = Instruction(self.read_word(bus, pc, true));
        let instr = self.last_instr;

        // Maybe process some debug stuff.
        self.handle_debug(bus, pc, &instr);

        // Dispatch.
        dprintln!(self, "op: {:#10x}   {:?}", pc as u32, instr);
        self.dispatch_instr(bus, &instr);

        // Go to next instruction if current instruction didn't jump
        // (or set an exception vector, etc.)
        self.reg_pc = self.next_pc.take().unwrap_or(self.reg_pc + 4);
    }

    pub fn run_branch_delay_slot(&mut self, bus: &mut CpuBus, addr: u64) {
        if self.in_branch_delay {
            self.bug(format!("Branching in branch delay slot -- check semantics!"));
        }
        self.in_branch_delay = true;
        self.reg_pc += 4;
        self.next_pc = Some(addr); // prepare jump
        self.run_instruction(bus);
        self.next_pc = Some(self.reg_pc); // no adjustment to pc
        self.in_branch_delay = false;
    }

    #[cfg(debug_assertions)]
    fn handle_debug(&mut self, bus: &mut CpuBus, pc: u64, instr: &Instruction) {
        use std::u64;
        use ansi_term::Colour;
        use debug::Debugger;

        let (debug_for, dump_here, break_here) =
            self.debug_specs.check_instr(pc, instr, &self.reg_gpr);
        self.instr_counter += 1;
        if break_here {
            println!("{}", Colour::Red.paint(
                format!("at: {:#10x}   {:?}", pc as u32, instr)));
            if dump_here {
                println!("{:?}", self);
            }
            let mut debugger = Debugger::new(self, bus);
            debugger.run_loop();
        } else if dump_here {
            println!("{}", Colour::Red.paint(
                format!("at: {:#10x}   {:?}", pc as u32, instr)));
            println!("{:?}", self);
        }
        if debug_for > 0 {
            self.debug_print = true;
            if debug_for == u64::MAX {
                self.debug_until = u64::MAX;
            } else if debug_for > 1 {
                self.debug_until = self.instr_counter + debug_for;
            }
        } else if self.debug_print && self.instr_counter > self.debug_until {
            self.debug_print = false;
        }
    }

    #[cfg(not(debug_assertions))]
    fn handle_debug(&mut self, _: &mut CpuBus, _: u64, _: &Instruction) { }

    #[inline(always)]
    fn dispatch_instr(&mut self, bus: &mut CpuBus, instr: &Instruction) {
        match instr.opcode() {
            LUI   => {
                let val = instr.imm_sign_ext() << 16;
                dprintln!(self, "{} {} <- {:#x}", INDENT, REG_NAMES[instr.rt()], val);
                self.write_gpr(instr.rt(), val);
            }
            LW    => self.mem_load (bus, instr, false, |word: u32| word as i32 as u64),
            LWU   => self.mem_load (bus, instr, false, |word: u32| word as u64),
            SW    => self.mem_store(bus, instr, false, |data| data as u32),
            // TODO: overflow exception
            ADDI  => self.binary_imm(instr, |rs| rs.wrapping_add(instr.imm_sign_ext()) as i32 as u64),
            ADDIU => self.binary_imm(instr, |rs| rs.wrapping_add(instr.imm_sign_ext()) as i32 as u64),
            ANDI  => self.binary_imm(instr, |rs| rs & instr.imm()),
            ORI   => self.binary_imm(instr, |rs| rs | instr.imm()),
            XORI  => self.binary_imm(instr, |rs| rs ^ instr.imm()),
            SLTI  => self.binary_imm(instr, |rs| ((rs as i64) < instr.imm_sign_ext() as i64) as u64),
            SLTIU => self.binary_imm(instr, |rs| (rs < instr.imm_sign_ext()) as u64),
            J     => {
                let addr = ((self.reg_pc + 4) & 0xffff_ffff_c000_0000) | (instr.j_target() << 2);
                self.jump(bus, addr, 0);
            }
            JAL   => {
                let addr = ((self.reg_pc + 4) & 0xffff_ffff_c000_0000) | (instr.j_target() << 2);
                self.jump(bus, addr, 31);
            }
            BEQ   => self.branch(bus, instr, false, false, |cpu|
                                 cpu.read_gpr(instr.rs()) == cpu.read_gpr(instr.rt())),
            BEQL  => self.branch(bus, instr, true, false, |cpu|
                                 cpu.read_gpr(instr.rs()) == cpu.read_gpr(instr.rt())),
            BNE   => self.branch(bus, instr, false, false, |cpu|
                                 cpu.read_gpr(instr.rs()) != cpu.read_gpr(instr.rt())),
            BNEL  => self.branch(bus, instr, true, false, |cpu|
                                 cpu.read_gpr(instr.rs()) != cpu.read_gpr(instr.rt())),
            BGTZ  => self.branch(bus, instr, false, false, |cpu| {
                                 let v = cpu.read_gpr(instr.rs()); v != 0 && (v >> 63) == 0 }),
            BGTZL => self.branch(bus, instr, true, false, |cpu| {
                                 let v = cpu.read_gpr(instr.rs()); v != 0 && (v >> 63) == 0 }),
            BLEZ  => self.branch(bus, instr, false, false, |cpu| {
                                 let v = cpu.read_gpr(instr.rs()); v == 0 || (v >> 63) != 0 }),
            BLEZL => self.branch(bus, instr, true, false, |cpu| {
                                 let v = cpu.read_gpr(instr.rs()); v == 0 || (v >> 63) != 0 }),
            // TODO: overflow exception
            DADDI => self.binary_imm(instr, |rs| rs.wrapping_add(instr.imm_sign_ext())),
            DADDIU => self.binary_imm(instr, |rs| rs.wrapping_add(instr.imm_sign_ext())),
            LB    => self.mem_load (bus, instr, false, |byte: u8| byte as i8 as u64),
            LBU   => self.mem_load (bus, instr, false, |byte: u8| byte as u64),
            LH    => self.mem_load (bus, instr, false, |hword: u16| hword as i16 as u64),
            LHU   => self.mem_load (bus, instr, false, |hword: u16| hword as u64),
            LD    => self.mem_load (bus, instr, false, |dword: u64| dword),
            SB    => self.mem_store(bus, instr, false, |data| data as u8),
            SH    => self.mem_store(bus, instr, false, |data| data as u16),
            SD    => self.mem_store(bus, instr, false, |data| data),
            LWL   => self.mem_load_unaligned (bus, instr, false, |data: u32| data as i32 as u64),
            LWR   => self.mem_load_unaligned (bus, instr, true,  |data: u32| data as i32 as u64),
            LDL   => self.mem_load_unaligned (bus, instr, false, |data: u64| data),
            LDR   => self.mem_load_unaligned (bus, instr, true,  |data: u64| data),
            SWL   => self.mem_store_unaligned(bus, instr, false, |mask, data: u32, reg|
                                              ((data as u64 & !mask) | (reg & mask)) as u32),
            SWR   => self.mem_store_unaligned(bus, instr, true,  |mask, data: u32, reg|
                                              ((data as u64 & !mask) | (reg & mask)) as u32),
            SDL   => self.mem_store_unaligned(bus, instr, false, |mask, data: u64, reg|
                                              (data & !mask) | (reg & mask)),
            SDR   => self.mem_store_unaligned(bus, instr, true,  |mask, data: u64, reg|
                                              (data & !mask) | (reg & mask)),
            LL    => self.mem_load (bus, instr, true, |data: u32| data as i32 as u64),
            LLD   => self.mem_load (bus, instr, true, |data: u64| data),
            SC    => self.mem_store(bus, instr, true, |data| data as u32),
            SCD   => self.mem_store(bus, instr, true, |data| data),
            CACHE => {
                /* TODO: do we need to implement this? */
            }
            SPECIAL => match instr.special_op() {
                JR   => { let addr = self.read_gpr(instr.rs()); self.jump(bus, addr, 0); }
                JALR => { let addr = self.read_gpr(instr.rs()); self.jump(bus, addr, instr.rd()); }
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
                _ => self.bug(format!("#UD: I {:#b} -- {:?}", instr.0, instr))
            },
            REGIMM => match instr.regimm_op() {
                BGEZ    => self.branch(bus, instr, false, false,
                                       |cpu| (cpu.read_gpr(instr.rs()) >> 63) == 0),
                BGEZL   => self.branch(bus, instr, true,  false,
                                       |cpu| (cpu.read_gpr(instr.rs()) >> 63) == 0),
                BGEZAL  => self.branch(bus, instr, false, true,
                                       |cpu| (cpu.read_gpr(instr.rs()) >> 63) == 0),
                BGEZALL => self.branch(bus, instr, true,  true,
                                       |cpu| (cpu.read_gpr(instr.rs()) >> 63) == 0),
                BLTZ    => self.branch(bus, instr, false, false,
                                       |cpu| (cpu.read_gpr(instr.rs()) >> 63) != 0),
                BLTZL   => self.branch(bus, instr, true,  false,
                                       |cpu| (cpu.read_gpr(instr.rs()) >> 63) != 0),
                BLTZAL  => self.branch(bus, instr, false, true,
                                       |cpu| (cpu.read_gpr(instr.rs()) >> 63) != 0),
                BLTZALL => self.branch(bus, instr, true,  true,
                                       |cpu| (cpu.read_gpr(instr.rs()) >> 63) != 0),
                // TEQI, TGEI, TGEIU, TLTI, TLTIU, TNEI
                _ => self.bug(format!("#UD: I {:#b} -- {:?}", instr.0, instr))
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
                            self.next_pc = Some(self.cp0.reg_error_epc);
                            self.cp0.reg_status.error_level = false;
                        } else if self.cp0.reg_status.exception_level {
                            dprintln!(self, "{} return from exception to {:#x}",
                                      INDENT, self.cp0.reg_epc);
                            self.next_pc = Some(self.cp0.reg_epc);
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
            },
            LDC1 => self.mem_load_fp::<u64> (bus, instr),
            LWC1 => self.mem_load_fp::<u32> (bus, instr),
            SDC1 => self.mem_store_fp::<u64>(bus, instr),
            SWC1 => self.mem_store_fp::<u32>(bus, instr),
            _    => self.bug(format!("#UD: I {:#b} -- {:?}", instr.0, instr))
        }
    }

    fn mem_load<'c, T: MemFmt<'c, Self>, F>(&mut self, bus: &CpuBus<'c>, instr: &Instruction,
                                            linked: bool, func: F) where F: Fn(T) -> u64
    {
        let addr = self.aligned_addr(instr, T::get_align());
        if linked {
            self.cp0.reg_lladdr = (self.virt_addr_to_phys_addr(addr) as u32) & !0xF;
            self.reg_llbit = true;
        }
        let data = func(T::load_from(self, bus, addr));
        dprintln!(self, "{} {} <- {:#18x} :  mem @ {:#x}",
                  INDENT, REG_NAMES[instr.rt()], data, addr);
        self.write_gpr(instr.rt(), data);
    }

    fn mem_store<'c, T: MemFmt<'c, Self>, F>(&mut self, bus: &mut CpuBus<'c>, instr: &Instruction,
                                     linked: bool, func: F) where F: Fn(u64) -> T
    {
        let addr = self.aligned_addr(instr, T::get_align());
        let data = self.read_gpr(instr.rt());
        let data = func(data);
        dprintln!(self, "{}       {:#18x} -> mem @ {:#x}", INDENT, data, addr);
        if linked {
            let llbit = self.reg_llbit;
            if llbit {
                T::store_to(self, bus, addr, data);
            }
            self.write_gpr(instr.rt(), llbit as u64);
        } else {
            T::store_to(self, bus, addr, data);
        }
    }

    fn mem_load_unaligned<'c, T: MemFmt<'c, Self>, F>(&mut self, bus: &CpuBus<'c>, instr: &Instruction,
                                              right: bool, func: F) where F: Fn(T) -> u64
    {
        let addr = self.aligned_addr(&instr, 1);
        let align = T::get_align();
        let amask = align - 1;
        let aligned_addr = addr & !amask;
        let offset = addr & amask;
        let shift = if right { (amask - offset) * 8 } else { offset * 8 };
        let data = func(T::load_from(self, bus, aligned_addr));
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

    fn mem_store_unaligned<'c, T: MemFmt<'c, Self>, F>(&mut self, bus: &mut CpuBus<'c>, instr: &Instruction,
                                               right: bool, func: F) where F: Fn(u64, T, u64) -> T
    {
        let addr = self.aligned_addr(&instr, 1);
        let align = T::get_align();
        let amask = align - 1;
        let aligned_addr = addr & !amask;
        let offset = addr & amask;
        let shift = if right { (amask - offset) * 8 } else { offset * 8 };
        let reg = self.read_gpr(instr.rt());
        let sh_reg = if right { reg << shift } else { reg >> shift };
        let mask = if right { !0 << shift } else { !0 >> shift };
        let orig_data = T::load_from(self, bus, aligned_addr);
        let data = func(mask, orig_data, sh_reg);
        dprintln!(self, "{}       {:#18x} :  mem @ {:#x}",
                  INDENT, orig_data, aligned_addr);
        dprintln!(self, "{}       {:#18x} -> mem @ {:#x}", INDENT, data, addr);
        T::store_to(self, bus, aligned_addr, data);
    }

    fn binary_imm<F>(&mut self, instr: &Instruction, func: F)
        where F: Fn(u64) -> u64
    {
        let res = func(self.read_gpr(instr.rs()));
        dprintln!(self, "{} {} :  {:#18x}", INDENT, REG_NAMES[instr.rs()],
                  self.read_gpr(instr.rs()));
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
        dprintln!(self, "{} {} :  {:#18x}", INDENT, REG_NAMES[instr.rt()],
                  self.read_gpr(instr.rt()));
        dprintln!(self, "{} {} <- {:#18x}", INDENT, REG_NAMES[instr.rd()], res);
        self.write_gpr(instr.rd(), res);
    }

    fn jump(&mut self, bus: &mut CpuBus, addr: u64, link_reg: usize) {
        if addr & 0b11 != 0 {
            self.bug(format!("Unaligned address in jump: {:#x}", addr));
        }
        if link_reg > 0 {
            let return_addr = self.reg_pc + 8;
            self.write_gpr(link_reg, return_addr);
            dprintln!(self, "{} {} <- {:#18x}", INDENT, REG_NAMES[link_reg],
                      return_addr);
        }
        // TODO: do this differently (now we execute 2 instructions for one run_instr)
        self.run_branch_delay_slot(bus, addr);
    }

    fn branch<P>(&mut self, bus: &mut CpuBus, instr: &Instruction, likely: bool,
                 link: bool, mut predicate: P) where P: FnMut(&mut Self) -> bool
    {
        // Offset is relative to the delay slot.
        let addr = (instr.imm_sign_ext() << 2).wrapping_add(self.reg_pc + 4);
        let take = predicate(self);
        let next_instr = self.reg_pc + 8;
        if link {
            self.write_gpr(31, next_instr);
            dprintln!(self, "{} ra <- {:#18x}", INDENT, next_instr);
        }
        dprintln!(self, "{} branch: {}", INDENT,
                  if take { "taken" } else { "not taken" });
        // Run the delay slot (or not, for not-taken likely branches)
        if take {
            self.run_branch_delay_slot(bus, addr);
        } else if !likely {
            self.run_branch_delay_slot(bus, next_instr);
        } else {
            self.next_pc = Some(next_instr);
        }
    }

    fn aligned_addr(&self, instr: &Instruction, align: u64) -> u64 {
        let addr = self.read_gpr(instr.base()).wrapping_add(instr.imm_sign_ext());
        if addr & (align - 1) != 0 {
            self.bug(format!("Address not aligned to {} bytes: {:#x}", align, addr));
        }
        addr
    }

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
        let addr = self.aligned_addr(&instr, T::get_align());
        let data = T::load_from(self, bus, addr);
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
        let addr = self.aligned_addr(&instr, T::get_align());
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
        T::store_to(self, bus, addr, data);
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

    #[cfg(debug_assertions)]
    fn debug_read(&self, phys_addr: u64, load_instr: bool, res: u32) {
        if !load_instr && self.debug_specs.matches_mem(phys_addr, false) {
            println!("   {:#10x}   Bus read:  {:#10x} :  {:#10x}",
                     self.reg_pc as u32, phys_addr, res);
        }
    }

    #[cfg(debug_assertions)]
    fn debug_write(&self, phys_addr: u64, word: u32) {
        if self.debug_specs.matches_mem(phys_addr, true) {
            println!("   {:#10x}   Bus write: {:#10x} <- {:#10x}",
                     self.reg_pc as u32, phys_addr, word);
        }
    }

    #[cfg(not(debug_assertions))]
    fn debug_read(&self, _: u64, _: bool, _: u32) { }

    #[cfg(not(debug_assertions))]
    fn debug_write(&self, _: u64, _: u32) { }

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
            self.bug(format!("Unrecognized virtual address: {:#x}", virt_addr));
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

    fn flag_exception(&mut self, exc: Exception) {
        if !self.cp0.reg_status.exception_level && exc.is_enabled(&self.cp0) {
            dprintln!(self, "Starting exception processing: {:?}", exc);
            self.cp0.reg_epc = self.reg_pc;
            if self.in_branch_delay {
                self.cp0.reg_epc -= 4;
            }
            self.cp0.reg_cause.exc_in_delay_slot = self.in_branch_delay;
            self.cp0.reg_cause.coprocessor = exc.coprocessor();
            self.cp0.reg_cause.exception_code = exc.code();
            self.cp0.reg_cause.interrupts_pending = exc.interrupt_mask();
            self.cp0.reg_status.exception_level = true;
            self.reg_pc = exc.vector_location(self.cp0.reg_status.is_bootstrap());
            self.next_pc = Some(self.reg_pc);
        }
    }

    #[cold]
    fn bug(&self, msg: String) -> ! {
        //println!("{:#?}", $cpu.interconnect);
        println!("\nCPU dump:\n{:?}", self);
        println!("last instr was:    {:?}", self.last_instr);
        println!("#instrs executed:  {}", self.instr_counter);
        panic!(msg);
    }

    #[cfg(debug_assertions)]
    pub fn read_pc(&self) -> u64 {
        self.reg_pc
    }

    #[cfg(debug_assertions)]
    pub fn cp0_dump(&self) {
        println!("{:#?}", self.cp0);
    }

    #[cfg(debug_assertions)]
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

impl<'c> R4300<'c> for Cpu {
    type Bus = CpuBus<'c>;

    fn read_word(&self, bus: &CpuBus, virt_addr: u64, load_instr: bool) -> u32 {
        let phys_addr = self.virt_addr_to_phys_addr(virt_addr);
        match bus.read_word(phys_addr as u32) {
            Ok(res) => {
                self.debug_read(phys_addr, load_instr, res);
                res
            }
            Err(desc) => {
                self.bug(format!("{}: ({:#x}) {:#x}", desc, virt_addr, phys_addr));
            }
        }
    }

    fn write_word(&mut self, bus: &mut CpuBus, virt_addr: u64, word: u32) {
        let phys_addr = self.virt_addr_to_phys_addr(virt_addr);
        if (phys_addr as u32) & !0xF == self.cp0.reg_lladdr {
            self.reg_llbit = false;
        }
        self.debug_write(phys_addr, word);
        if let Err(desc) = bus.write_word(phys_addr as u32, word) {
            self.bug(format!("{}: ({:#x}) {:#x}", desc, virt_addr, phys_addr));
        }
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
