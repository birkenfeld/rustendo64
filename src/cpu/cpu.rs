use std::fmt;
use std::collections::VecDeque;
use std::cmp::Ordering;
use byteorder::{BigEndian, ByteOrder};

use super::instruction::*;
use super::exception::*;
use super::cp0::Cp0;
use interconnect;
use util::mult_64_64;

const NUM_GPR: usize = 32;

pub struct Cpu {
    instr_counter: u32,
    debug_instrs:  bool,

    reg_gpr:    [u64; NUM_GPR],
    reg_fpr:    [[u8; 8]; NUM_GPR],

    reg_pc:     u64,
    last_pc:    u64,
    last_instr: u32,

    reg_hi:     u64,
    reg_lo:     u64,
    reg_llbit:  bool, // TODO: Enum type
    reg_fcr31:  u32,

    in_branch_delay: bool,
    exc_pending: VecDeque<Exception>,

    cp0: Cp0,
    interconnect: interconnect::Interconnect
}

macro_rules! bug {
    ($cpu:expr, $($args:expr),+) => {
        //println!("{:#?}", $cpu.interconnect);
        println!("{:?}", $cpu);
        println!("last instr was:    {:?}", Instruction($cpu.last_instr));
        println!("#instrs executed:  {}", $cpu.instr_counter);
        panic!($($args),+);
    }
}

macro_rules! dprintln {
    ($cpu:expr, $($args:expr),+) => { if $cpu.debug_instrs { println!($($args),+) } }
    //($cpu:expr, $($args:expr),+) => { }
}


impl fmt::Debug for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "\nCPU dump:\n"));
        for row in 0..8 {
            for col in 0..4 {
                let i = row + col * 8;
                try!(write!(f, "  {:2}:{} = {:016x}", i, REG_NAMES[i], self.reg_gpr[i]));
            }
            try!(write!(f, "\n"));
        }
        try!(write!(f, "\n"));
        for row in 0..8 {
            for col in 0..4 {
                let i = row + col * 8;
                try!(write!(f, "  fpr{:-2} = {:016x}", i, BigEndian::read_u64(&self.reg_fpr[i])));
            }
            try!(write!(f, "\n"));
        }
        try!(write!(f, "     pc = {:016x}", self.reg_pc));
        try!(write!(f, "     hi = {:016x}     lo = {:016x}", self.reg_hi, self.reg_lo));
        write!(f, "     ll = {}\n", self.reg_llbit)
    }
}

pub const INDENT: &'static str = "                                       ";

impl Cpu {
    pub fn new(interconnect: interconnect::Interconnect) -> Cpu {
        Cpu {
            instr_counter: 0,
            debug_instrs: false,
            in_branch_delay: false,
            exc_pending: VecDeque::new(),

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

        self.reg_pc = 0xffff_ffff_bfc0_0000; // TODO: Move to const
    }

    // TODO: Different interface
    pub fn run(&mut self) {
        loop {
            self.run_instruction();
        }
    }

    pub fn run_branch_delay_slot(&mut self) {
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
                self.exc_pending.push_back(
                    Exception { exc_type: ExcType::Interrupt(Intr::Timer) });
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
        // if pc > 0xffff_ffff_8020_0000 && pc < 0xffff_ffff_9000_0000 {
        // if pc == 0xffff_ffff_8020_0188 {
        //     self.debug_instrs = true;
        // }
        self.last_instr = self.read_word(pc);
        let instr = Instruction(self.last_instr);
        dprintln!(self, "op: {:#x}   {:?}", self.reg_pc & 0xffff_ffff, instr);

        match instr.opcode() {
            LUI   => {
                let val = instr.imm_sign_extended() << 16;
                dprintln!(self, "{} {} <- {:#x}", INDENT, REG_NAMES[instr.rt()], val);
                self.write_gpr(instr.rt(), val);
            }
            LW    => {
                let addr = self.aligned_addr(&instr, 4);
                let word = self.read_word(addr) as i32 as u64;
                dprintln!(self, "{} {} <- {:#18x} :  mem @ {:#x}",
                         INDENT, REG_NAMES[instr.rt()], word, addr);
                self.write_gpr(instr.rt(), word);
            }
            LWU   => {
                let addr = self.aligned_addr(&instr, 4);
                let word = self.read_word(addr) as u64;
                dprintln!(self, "{} {} <- {:#18x} :  mem @ {:#x}",
                          INDENT, REG_NAMES[instr.rt()], word, addr);
                self.write_gpr(instr.rt(), word);
            }
            SW    => {
                let addr = self.aligned_addr(&instr, 4);
                let word = self.read_gpr(instr.rt());
                dprintln!(self, "{}       {:#18x} -> mem @ {:#x}",
                          INDENT, word, addr);
                self.write_word(addr, word as u32);
            }
            // TODO: overflow exception
            ADDI  => self.binary_imm(&instr, |rs| rs.wrapping_add(instr.imm_sign_extended()) as i32 as u64),
            ADDIU => self.binary_imm(&instr, |rs| rs.wrapping_add(instr.imm_sign_extended()) as i32 as u64),
            ANDI  => self.binary_imm(&instr, |rs| rs & instr.imm()),
            ORI   => self.binary_imm(&instr, |rs| rs | instr.imm()),
            XORI  => self.binary_imm(&instr, |rs| rs ^ instr.imm()),
            SLTI  => self.binary_imm(&instr, |rs| ((rs as i64) < instr.imm_sign_extended() as i64) as u64),
            SLTIU => self.binary_imm(&instr, |rs| (rs < instr.imm_sign_extended()) as u64),
            J     => {
                let addr = ((self.reg_pc + 4) & 0xffff_ffff_c000_0000) | (instr.j_target() << 2);
                self.jump(addr, 0);
            }
            JAL   => {
                let addr = ((self.reg_pc + 4) & 0xffff_ffff_c000_0000) | (instr.j_target() << 2);
                self.jump(addr, 31);
            }
            BEQ   => self.branch(&instr, false, false, |cpu|
                                 cpu.read_gpr(instr.rs()) == cpu.read_gpr(instr.rt())),
            BEQL  => self.branch(&instr, true, false, |cpu|
                                 cpu.read_gpr(instr.rs()) == cpu.read_gpr(instr.rt())),
            BNE   => self.branch(&instr, false, false, |cpu|
                                 cpu.read_gpr(instr.rs()) != cpu.read_gpr(instr.rt())),
            BNEL  => self.branch(&instr, true, false, |cpu|
                                 cpu.read_gpr(instr.rs()) != cpu.read_gpr(instr.rt())),
            BGTZ  => self.branch(&instr, false, false, |cpu| {
                                 let v = cpu.read_gpr(instr.rs()); v != 0 && (v >> 63) == 0 }),
            BGTZL => self.branch(&instr, true, false, |cpu| {
                                 let v = cpu.read_gpr(instr.rs()); v != 0 && (v >> 63) == 0 }),
            BLEZ  => self.branch(&instr, false, false, |cpu| {
                                 let v = cpu.read_gpr(instr.rs()); v == 0 || (v >> 63) != 0 }),
            BLEZL => self.branch(&instr, true, false, |cpu| {
                                 let v = cpu.read_gpr(instr.rs()); v == 0 || (v >> 63) != 0 }),
            // TODO: overflow exception
            DADDI => self.binary_imm(&instr, |rs| rs.wrapping_add(instr.imm_sign_extended())),
            DADDIU => self.binary_imm(&instr, |rs| rs.wrapping_add(instr.imm_sign_extended())),
            LB    => {
                let addr = self.aligned_addr(&instr, 1);
                let word_addr = addr & !0b11;
                let byte_offset = 0b11 - (addr & 0b11);  // big endian
                let word = self.read_word(word_addr);
                let byte = (word >> (8 * byte_offset)) & 0xFF;
                dprintln!(self, "{} {} <- {:#18x}({:#4x}) :  mem @ {:#x}",
                         INDENT, REG_NAMES[instr.rt()], word, byte, addr);
                self.write_gpr(instr.rt(), byte as i8 as u64);
            }
            LBU   => {
                let addr = self.aligned_addr(&instr, 1);
                let word_addr = addr & !0b11;
                let byte_offset = 0b11 - (addr & 0b11);
                let word = self.read_word(word_addr) as u64;
                let byte = (word >> (8 * byte_offset)) & 0xFF;
                dprintln!(self, "{} {} <- {:#18x}({:#4x}) :  mem @ {:#x}",
                         INDENT, REG_NAMES[instr.rt()], word, byte, addr);
                self.write_gpr(instr.rt(), byte);
            }
            LH    => {
                let addr = self.aligned_addr(&instr, 2);
                let word_addr = addr & !0b1;
                let hword_offset = 0b1 - ((addr & 0b10) >> 1);  // big endian
                let word = self.read_word(word_addr);
                let hword = (word >> (16 * hword_offset)) & 0xFFFF;
                dprintln!(self, "{} {} <- {:#18x}({:#6x}) :  mem @ {:#x}",
                         INDENT, REG_NAMES[instr.rt()], word, hword, addr);
                self.write_gpr(instr.rt(), hword as i16 as u64);
            }
            LHU   => {
                let addr = self.aligned_addr(&instr, 2);
                let word_addr = addr & !0b1;
                let hword_offset = 0b1 - ((addr & 0b10) >> 1);  // big endian
                let word = self.read_word(word_addr) as u64;
                let hword = (word >> (16 * hword_offset)) & 0xFFFF;
                dprintln!(self, "{} {} <- {:#18x}({:#6x}) :  mem @ {:#x}",
                         INDENT, REG_NAMES[instr.rt()], word, hword, addr);
                self.write_gpr(instr.rt(), hword);
            }
            LD    => {
                let addr = self.aligned_addr(&instr, 8);
                let dword = (self.read_word(addr) as u64) << 32 | self.read_word(addr + 4) as u64;
                dprintln!(self, "{} {} <- {:#18x} :  mem @ {:#x}",
                         INDENT, REG_NAMES[instr.rt()], dword, addr);
                self.write_gpr(instr.rt(), dword);
            }
            // LDL, LDR, LWL, LWR
            SB    => {
                let addr = self.aligned_addr(&instr, 1);
                let word_addr = addr & !0b11;
                let byte_offset = 0b11 - (addr & 0b11);  // big endian
                let byte = (self.read_gpr(instr.rt()) as u32 & 0xFF) << (8 * byte_offset);
                let mask = !(0xFFu32 << (8 * byte_offset));
                let mut word = self.read_word(word_addr);
                word = (word & mask) | byte;
                dprintln!(self, "{}       {:#18x} -> mem @ {:#x}",
                          INDENT, word, word_addr);
                self.write_word(word_addr, word);
            }
            SH    => {
                let addr = self.aligned_addr(&instr, 2);
                let word_addr = addr & !0b11;
                let hword_offset = 0b1 - ((addr & 0b10) >> 1);  // big endian
                let hword = (self.read_gpr(instr.rt()) as u32 & 0xFFFF) << (16 * hword_offset);
                let mask = !(0xFFFFu32 << (16 * hword_offset));
                let mut word = self.read_word(word_addr);
                word = (word & mask) | hword;
                dprintln!(self, "{}       {:#18x} -> mem @ {:#x}",
                          INDENT, word, word_addr);
                self.write_word(word_addr, word);
            }
            SD    => {
                let addr = self.aligned_addr(&instr, 8);
                let dword = self.read_gpr(instr.rt());
                dprintln!(self, "{}       {:#18x} -> mem @ {:#x}",
                          INDENT, dword, addr);
                self.write_word(addr, (dword >> 32) as u32);
                self.write_word(addr + 4, dword as u32);
            }
            // SDL, SDR, SWL, SWR
            // LL, LLD, SC
            CACHE => {
                // TODO: Check if we need to implement this
            }
            SPECIAL => match instr.special_op() {
                JR   => { let addr = self.read_gpr(instr.rs()); self.jump(addr, 0); }
                JALR => { let addr = self.read_gpr(instr.rs()); self.jump(addr, instr.rd()); }
                // TODO: Overflow exception
                ADD  => self.binary(&instr, |rs, rt| (rs as i32).wrapping_add(rt as i32) as i32 as u64),
                ADDU => self.binary(&instr, |rs, rt| (rs as u32).wrapping_add(rt as u32) as i32 as u64),
                // TODO: Overflow exception
                SUB  => self.binary(&instr, |rs, rt| (rs as i32).wrapping_sub(rt as i32) as i32 as u64),
                SUBU => self.binary(&instr, |rs, rt| (rs as u32).wrapping_sub(rt as u32) as i32 as u64),
                AND  => self.binary(&instr, |rs, rt| rs & rt),
                OR   => self.binary(&instr, |rs, rt| rs | rt),
                XOR  => self.binary(&instr, |rs, rt| rs ^ rt),
                NOR  => self.binary(&instr, |rs, rt| !(rs | rt)),
                SLT  => self.binary(&instr, |rs, rt| ((rs as i64) < rt as i64) as u64),
                SLTU => self.binary(&instr, |rs, rt| (rs < rt) as u64),
                SLLV => self.binary(&instr, |rs, rt| (rt << (rs & 0b11111)) as i32 as u64),
                SRAV => self.binary(&instr, |rs, rt| (rt as i32 >> (rs & 0b11111)) as u64),
                SRLV => self.binary(&instr, |rs, rt| (rt as u32 >> (rs & 0b11111)) as i32 as u64),
                SLL  => if instr.sa() != 0 {
                    self.unary(&instr, |rt| (rt << instr.sa()) as i32 as u64)
                },
                SRA  => self.unary(&instr, |rt| (rt as i32 >> instr.sa()) as i32 as u64),
                SRL  => self.unary(&instr, |rt| (rt as u32 >> instr.sa()) as i32 as u64),
                // TODO: Overflow exception
                DADD   => self.binary(&instr, |rs, rt| rs.wrapping_add(rt)),
                DADDU  => self.binary(&instr, |rs, rt| rs.wrapping_add(rt)),
                // TODO: Overflow exception
                DSUB   => self.binary(&instr, |rs, rt| rs.wrapping_sub(rt)),
                DSUBU  => self.binary(&instr, |rs, rt| rs.wrapping_sub(rt)),
                DSLLV  => self.binary(&instr, |rs, rt| rt << (rs & 0b111111)),
                DSRAV  => self.binary(&instr, |rs, rt| (rt as i64 >> (rs & 0b111111)) as u64),
                DSRLV  => self.binary(&instr, |rs, rt| rt >> (rs & 0b111111)),
                DSLL   => self.unary(&instr, |rt| rt << instr.sa()),
                DSLL32 => self.unary(&instr, |rt| rt << (instr.sa() + 32)),
                DSRA   => self.unary(&instr, |rt| (rt as i64 >> instr.sa()) as u64),
                DSRA32 => self.unary(&instr, |rt| (rt as i64 >> (instr.sa() + 32)) as u64),
                DSRL   => self.unary(&instr, |rt| rt >> instr.sa()),
                DSRL32 => self.unary(&instr, |rt| rt >> (instr.sa() + 32)),
                MFHI   => { let val = self.reg_hi; self.write_gpr(instr.rd(), val); }
                MFLO   => { let val = self.reg_lo; self.write_gpr(instr.rd(), val); }
                MTHI   => { let val = self.read_gpr(instr.rs()); self.reg_hi = val; }
                MTLO   => { let val = self.read_gpr(instr.rs()); self.reg_lo = val; }
                MULT   => {
                    let mut mult_result = (self.read_gpr(instr.rs()) as i32) as i64 *
                        (self.read_gpr(instr.rt()) as i32) as i64;
                    self.reg_lo = mult_result as i32 as u64;
                    mult_result >>= 32;
                    self.reg_hi = mult_result as i32 as u64;
                }
                MULTU  => {
                    let mut mult_result = (self.read_gpr(instr.rs()) & 0xffff_ffff) *
                        (self.read_gpr(instr.rt()) & 0xffff_ffff);
                    self.reg_lo = mult_result as i32 as u64;
                    mult_result >>= 32;
                    self.reg_hi = mult_result as i32 as u64;
                }
                DIV    => {
                    let a = self.read_gpr(instr.rs()) as i32;
                    let b = self.read_gpr(instr.rt()) as i32;
                    self.reg_lo = (a / b) as u64;
                    self.reg_hi = (a % b) as u64;
                }
                DIVU   => {
                    let a = self.read_gpr(instr.rs()) as u32;
                    let b = self.read_gpr(instr.rt()) as u32;
                    self.reg_lo = (a / b) as u64;
                    self.reg_hi = (a % b) as u64;
                }
                DMULT  => {
                    let mut a = self.read_gpr(instr.rs());
                    let mut b = self.read_gpr(instr.rt());
                    let neg = (a & 0x8000_0000_0000_0000 != 0) ^ (b & 0x8000_0000_0000_0000 != 0);
                    a &= 0x7FFF_FFFF_FFFF_FFFF;
                    b &= 0x7FFF_FFFF_FFFF_FFFF;
                    let (rl, rh) = mult_64_64(a, b);
                    self.reg_lo = rl;
                    self.reg_hi = rh | (neg as u64) << 63;
                }
                DMULTU => {
                    let (rl, rh) = mult_64_64(self.read_gpr(instr.rs()),
                                              self.read_gpr(instr.rt()));
                    self.reg_lo = rl;
                    self.reg_hi = rh;
                }
                DDIV   => {
                    let a = self.read_gpr(instr.rs()) as i64;
                    let b = self.read_gpr(instr.rt()) as i64;
                    self.reg_lo = (a / b) as u64;
                    self.reg_hi = (a % b) as u64;
                }
                DDIVU  => {
                    let a = self.read_gpr(instr.rs());
                    let b = self.read_gpr(instr.rt());
                    self.reg_lo = a / b;
                    self.reg_hi = a % b;
                }
                // TEQ, TGE, TGEU, TLT, TLTU, TNE
                SYNC  => { }
                // SYSCALL
                _ => { bug!(self, "#UD: I {:#b} -- {:?}", instr.0, instr); }
            },
            REGIMM => match instr.regimm_op() {
                BGEZ    => self.branch(&instr, false, false, |cpu| (cpu.read_gpr(instr.rs()) >> 63) == 0),
                BGEZL   => self.branch(&instr, true,  false, |cpu| (cpu.read_gpr(instr.rs()) >> 63) == 0),
                BGEZAL  => self.branch(&instr, false, true,  |cpu| (cpu.read_gpr(instr.rs()) >> 63) == 0),
                BGEZALL => self.branch(&instr, true,  true,  |cpu| (cpu.read_gpr(instr.rs()) >> 63) == 0),
                BLTZ    => self.branch(&instr, false, false, |cpu| (cpu.read_gpr(instr.rs()) >> 63) != 0),
                BLTZL   => self.branch(&instr, true,  false, |cpu| (cpu.read_gpr(instr.rs()) >> 63) != 0),
                BLTZAL  => self.branch(&instr, false, true,  |cpu| (cpu.read_gpr(instr.rs()) >> 63) != 0),
                BLTZALL => self.branch(&instr, true,  true,  |cpu| (cpu.read_gpr(instr.rs()) >> 63) != 0),
                // TEQI, TGEI, TGEIU, TLTI, TLTIU, TNEI
                _ => { bug!(self, "#UD: I {:#b} -- {:?}", instr.0, instr); }
            },
            COP0 => match instr.cop_op() {
                // TODO: do these really transfer 64 bits?
                MF => {
                    let data = self.cp0.read_reg(instr.rd());
                    dprintln!(self, "{} cp0[{:2}] :  {:#066b}", INDENT, instr.rd(), data);
                    self.write_gpr(instr.rt(), data);
                }
                MT => {
                    let data = self.read_gpr(instr.rt());
                    println!("{} cp0[{:2}] <- {:#066b}", INDENT, instr.rd(), data);
                    dprintln!(self, "{} cp0[{:2}] <- {:#066b}", INDENT, instr.rd(), data);
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
                    _ => { bug!(self, "#UD: I {:#b} -- {:?}", instr.0, instr); }
                },
                _ => { bug!(self, "#UD: I {:#b} -- {:?}", instr.0, instr); }
            },
            COP1 => match instr.cop_op() {
                MF  => {
                    let value = i32::read_fpr(&self.reg_fpr[instr.fs()]);
                    self.write_gpr(instr.rt(), value as u64);
                }
                MT  => {
                    let value = self.read_gpr(instr.rt()) as u32;
                    u32::write_fpr(&mut self.reg_fpr[instr.fs()], value);
                }
                DMF => {
                    let value = u64::read_fpr(&self.reg_fpr[instr.fs()]);
                    self.write_gpr(instr.rt(), value);
                }
                DMT => {
                    let value = self.read_gpr(instr.rt());
                    u64::write_fpr(&mut self.reg_fpr[instr.fs()], value);
                }
                CF  => {
                    let data = match instr.fs() {
                        31 => self.reg_fcr31,
                        0  => 0xB << 8 | 0,  // TODO: constant
                        _  => { bug!(self, "#RG: invalid read fp control reg {}", instr.fs()); },
                    };
                    self.write_gpr(instr.rt(), data as u64);
                }
                CT  => {
                    let data = self.read_gpr(instr.rt());
                    if instr.fs() == 31 {
                        self.reg_fcr31 = data as u32;
                    } else {
                        bug!(self, "#RG: invalid write fp control reg {}", instr.fs());
                    }
                }
                BC  => match instr.regimm_op() {
                    BCF  => self.branch(&instr, false, false, |cpu| cpu.reg_fcr31 & 0x80_0000 == 0),
                    BCFL => self.branch(&instr, true,  false, |cpu| cpu.reg_fcr31 & 0x80_0000 == 0),
                    BCT  => self.branch(&instr, false, false, |cpu| cpu.reg_fcr31 & 0x80_0000 != 0),
                    BCTL => self.branch(&instr, true,  false, |cpu| cpu.reg_fcr31 & 0x80_0000 != 0),
                    _    => { bug!(self, "#UD: {:#b} -- {:?}", instr.0, instr); }
                },
                _   => match instr.fp_op() {
                    // TODO: exceptions
                    // Binary operations
                    FADD    => self.fp_binary_2way(&instr, |a, b| a + b, |a, b| a + b),
                    FSUB    => self.fp_binary_2way(&instr, |a, b| a - b, |a, b| a - b),
                    FMUL    => self.fp_binary_2way(&instr, |a, b| a * b, |a, b| a * b),
                    FDIV    => self.fp_binary_2way(&instr, |a, b| a / b, |a, b| a / b),
                    // Unary operations
                    FMOV    => self.fp_unary_2way(&instr, |a| a,        |a| a),
                    FNEG    => self.fp_unary_2way(&instr, |a| -a,       |a| -a),
                    FABS    => self.fp_unary_2way(&instr, |a| a.abs(),  |a| a.abs()),
                    FSQRT   => self.fp_unary_2way(&instr, |a| a.sqrt(), |a| a.sqrt()),
                    // Conversions
                    FCVTS   => match instr.fp_fmt() {
                        FMT_D => self.fp_convert::<f64, _, _>(&instr, |a| a as f32),
                        FMT_W => self.fp_convert::<i32, _, _>(&instr, |a| a as f32),
                        FMT_L => self.fp_convert::<i64, _, _>(&instr, |a| a as f32),
                        _     => { bug!(self, "invalid FP source format: {:?}", instr); }
                    },
                    FCVTD   => match instr.fp_fmt() {
                        FMT_S => self.fp_convert::<f32, _, _>(&instr, |a| a as f64),
                        FMT_W => self.fp_convert::<i32, _, _>(&instr, |a| a as f64),
                        FMT_L => self.fp_convert::<i64, _, _>(&instr, |a| a as f64),
                        _     => { bug!(self, "invalid FP source format: {:?}", instr); }
                    },
                    FCVTW   => self.fp_convert_2way(&instr, |a| a.round() as i32, |a| a.round() as i32),
                    FCVTL   => self.fp_convert_2way(&instr, |a| a.round() as i64, |a| a.round() as i64),
                    FCEILW  => self.fp_convert_2way(&instr, |a| a.ceil()  as i32, |a| a.ceil()  as i32),
                    FCEILL  => self.fp_convert_2way(&instr, |a| a.ceil()  as i64, |a| a.ceil()  as i64),
                    FFLOORW => self.fp_convert_2way(&instr, |a| a.floor() as i32, |a| a.floor() as i32),
                    FFLOORL => self.fp_convert_2way(&instr, |a| a.floor() as i64, |a| a.floor() as i64),
                    FROUNDW => self.fp_convert_2way(&instr, |a| a.round() as i32, |a| a.round() as i32),
                    FROUNDL => self.fp_convert_2way(&instr, |a| a.round() as i64, |a| a.round() as i64),
                    FTRUNCW => self.fp_convert_2way(&instr, |a| a.trunc() as i32, |a| a.trunc() as i32),
                    FTRUNCL => self.fp_convert_2way(&instr, |a| a.trunc() as i64, |a| a.trunc() as i64),
                    // Compares
                    FCF     => self.fp_compare_2way(&instr, |_| false),
                    FCUN    => self.fp_compare_2way(&instr, |r| r == FpOrd::No),
                    FCEQ    => self.fp_compare_2way(&instr, |r| r == FpOrd::Eq),
                    FCUEQ   => self.fp_compare_2way(&instr, |r| r == FpOrd::Eq || r == FpOrd::No),
                    FCOLT   => self.fp_compare_2way(&instr, |r| r == FpOrd::Lt),
                    FCULT   => self.fp_compare_2way(&instr, |r| r == FpOrd::Lt || r == FpOrd::No),
                    FCOLE   => self.fp_compare_2way(&instr, |r| r != FpOrd::Gt && r != FpOrd::No),
                    FCULE   => self.fp_compare_2way(&instr, |r| r != FpOrd::Gt),
                    // TODO: exception on No
                    FCSF    => self.fp_compare_2way(&instr, |_| false),
                    FCNGLE  => self.fp_compare_2way(&instr, |r| r == FpOrd::No),
                    FCSEQ   => self.fp_compare_2way(&instr, |r| r == FpOrd::Eq),
                    FCNGL   => self.fp_compare_2way(&instr, |r| r == FpOrd::Eq || r == FpOrd::No),
                    FCLT    => self.fp_compare_2way(&instr, |r| r == FpOrd::Lt),
                    FCNGE   => self.fp_compare_2way(&instr, |r| r == FpOrd::Lt || r == FpOrd::No),
                    FCLE    => self.fp_compare_2way(&instr, |r| r != FpOrd::Gt && r != FpOrd::No),
                    FCNGT   => self.fp_compare_2way(&instr, |r| r != FpOrd::Gt),
                    _ => { bug!(self, "#UD: I {:#b} -- {:?}", instr.0, instr); }
                }
            },
            LDC1 => {
                let addr = self.aligned_addr(&instr, 8);
                let word1 = self.read_word(addr) as u64;
                let word2 = self.read_word(addr + 4) as u64;
                let dword = (word1 << 32) | word2;
                u64::write_fpr(&mut self.reg_fpr[instr.ft()], dword);
            },
            LWC1 => {
                let addr = self.aligned_addr(&instr, 4);
                let word = self.read_word(addr);
                u32::write_fpr(&mut self.reg_fpr[instr.ft()], word);
            },
            SDC1 => {
                let addr = self.aligned_addr(&instr, 8);
                let value = u64::read_fpr(&self.reg_fpr[instr.ft()]);
                self.write_word(addr, (value >> 32) as u32);
                self.write_word(addr + 4, value as u32);
            },
            SWC1 => {
                let addr = self.aligned_addr(&instr, 4);
                let value = u32::read_fpr(&self.reg_fpr[instr.ft()]);
                self.write_word(addr, value);
            },
            _ => { bug!(self, "#UD: I {:#b} -- {:?}", instr.0, instr); }
        }

        self.reg_pc += 4;
    }

    #[inline]
    fn binary_imm<F>(&mut self, instr: &Instruction, func: F)
        where F: Fn(u64) -> u64
    {
        let res = func(self.read_gpr(instr.rs()));
        dprintln!(self, "{} {} :  {:#18x}", INDENT, REG_NAMES[instr.rs()], self.read_gpr(instr.rs()));
        dprintln!(self, "{} {} <- {:#18x}", INDENT, REG_NAMES[instr.rt()], res);
        self.write_gpr(instr.rt(), res);
    }

    #[inline]
    fn binary<F>(&mut self, instr: &Instruction, func: F)
        where F: Fn(u64, u64) -> u64
    {
        let res = func(self.read_gpr(instr.rs()), self.read_gpr(instr.rt()));
        dprintln!(self, "{} {} :  {:#18x}", INDENT, REG_NAMES[instr.rs()], self.read_gpr(instr.rs()));
        dprintln!(self, "{} {} :  {:#18x}", INDENT, REG_NAMES[instr.rt()], self.read_gpr(instr.rt()));
        dprintln!(self, "{} {} <- {:#18x}", INDENT, REG_NAMES[instr.rd()], res);
        self.write_gpr(instr.rd(), res);
    }

    #[inline]
    fn unary<F>(&mut self, instr: &Instruction, func: F)
        where F: Fn(u64) -> u64
    {
        let res = func(self.read_gpr(instr.rt()));
        dprintln!(self, "{} {} :  {:#18x}", INDENT, REG_NAMES[instr.rt()], self.read_gpr(instr.rt()));
        dprintln!(self, "{} {} <- {:#18x}", INDENT, REG_NAMES[instr.rd()], res);
        self.write_gpr(instr.rd(), res);
    }

    #[inline]
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

    #[inline]
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
        // Run the delay slot (XXX what happens if the delay slot is
        // another branch?)
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

    #[inline]
    fn aligned_addr(&self, instr: &Instruction, align: u64) -> u64 {
        let addr = self.read_gpr(instr.base()).wrapping_add(instr.imm_sign_extended());
        if addr & (align - 1) != 0 {
            bug!(self, "Address not aligned to {} bytes: {:#x}", align, addr);
        }
        addr
    }

    #[inline]
    fn fp_unary<T: FpFmt, F>(&mut self, instr: &Instruction, f: F)
        where F: Fn(T) -> T
    {
        let a = T::read_fpr(&self.reg_fpr[instr.fs()]);
        T::write_fpr(&mut self.reg_fpr[instr.fd()], f(a));
    }

    #[inline]
    fn fp_unary_2way<F, G>(&mut self, instr: &Instruction, f: F, g: G)
        where F: Fn(f32) -> f32, G: Fn(f64) -> f64
    {
        match instr.fp_fmt() {
            FMT_S => self.fp_unary(&instr, f),
            FMT_D => self.fp_unary(&instr, g),
            _     => { bug!(self, "invalid FP format: {:?}", instr); }
        }
    }

    #[inline]
    fn fp_binary<T: FpFmt, F>(&mut self, instr: &Instruction, f: F)
        where F: Fn(T, T) -> T
    {
        let a = T::read_fpr(&self.reg_fpr[instr.fs()]);
        let b = T::read_fpr(&self.reg_fpr[instr.ft()]);
        T::write_fpr(&mut self.reg_fpr[instr.fd()], f(a, b));
    }

    #[inline]
    fn fp_binary_2way<F, G>(&mut self, instr: &Instruction, f: F, g: G)
        where F: Fn(f32, f32) -> f32, G: Fn(f64, f64) -> f64
    {
        match instr.fp_fmt() {
            FMT_S => self.fp_binary(&instr, f),
            FMT_D => self.fp_binary(&instr, g),
            _     => { bug!(self, "invalid FP format: {:?}", instr); }
        }
    }

    #[inline]
    fn fp_convert<T: FpFmt, U: FpFmt, F>(&mut self, instr: &Instruction, f: F)
        where F: Fn(T) -> U
    {
        let value = T::read_fpr(&self.reg_fpr[instr.fs()]);
        U::write_fpr(&mut self.reg_fpr[instr.fd()], f(value));
    }

    #[inline]
    fn fp_convert_2way<U: FpFmt, F, G>(&mut self, instr: &Instruction, f: F, g: G)
        where F: Fn(f32) -> U, G: Fn(f64) -> U
    {
        match instr.fp_fmt() {
            FMT_S => self.fp_convert(&instr, f),
            FMT_D => self.fp_convert(&instr, g),
            _     => { bug!(self, "invalid FP format: {:?}", instr); }
        }
    }

    #[inline]
    fn fp_compare<T: FpFmt, F>(&mut self, instr: &Instruction, f: F)
        where F: Fn(FpOrd) -> bool
    {
        let a = T::read_fpr(&self.reg_fpr[instr.fs()]);
        let b = T::read_fpr(&self.reg_fpr[instr.ft()]);
        let comp = match a.partial_cmp(&b) {
            None                    => FpOrd::No,
            Some(Ordering::Equal)   => FpOrd::Eq,
            Some(Ordering::Greater) => FpOrd::Gt,
            Some(Ordering::Less)    => FpOrd::Lt,
        };
        let cond = f(comp);
        self.reg_fcr31 = (self.reg_fcr31 & 0xff7f_ffff) | ((cond as u32) << 23);
    }

    #[inline]
    fn fp_compare_2way<F>(&mut self, instr: &Instruction, f: F)
        where F: Fn(FpOrd) -> bool
    {
        match instr.fp_fmt() {
            FMT_S => self.fp_compare::<f32, _>(&instr, f),
            FMT_D => self.fp_compare::<f64, _>(&instr, f),
            _     => { bug!(self, "invalid FP format: {:?}", instr); }
        }
    }

    #[inline]
    fn read_word(&mut self, virt_addr: u64) -> u32 {
        let phys_addr = self.virt_addr_to_phys_addr(virt_addr);
        match self.interconnect.read_word(phys_addr as u32) {
            Ok(res) => res,
            Err(desc) => {
                bug!(self, "{}: ({:#x}) {:#x}", desc, virt_addr, phys_addr);
            }
        }
    }

    #[inline]
    fn write_word(&mut self, virt_addr: u64, word: u32) {
        let phys_addr = self.virt_addr_to_phys_addr(virt_addr);
        if let Err(desc) = self.interconnect.write_word(phys_addr as u32, word) {
            bug!(self, "{}: ({:#x}) {:#x}", desc, virt_addr, phys_addr);
        }
    }

    #[inline]
    fn virt_addr_to_phys_addr(&self, virt_addr: u64) -> u64 {
        // See Table 5-3 in the VR4300 User's Manual
        let addr_bit_values = (virt_addr >> 29) & 0b111;

        if addr_bit_values == 0b101 {
            // kseg1
            virt_addr - 0xffff_ffff_a000_0000
        } else if addr_bit_values == 0b100 {
            // kseg0 (cached)
            virt_addr - 0xffff_ffff_8000_0000
        } else {
            // TODO
            bug!(self, "Unrecognized virtual address: {:#x}", virt_addr);
        }
    }

    #[inline]
    fn read_gpr(&self, index: usize) -> u64 {
        // Reg 0 is always 0 since we never write it
        self.reg_gpr[index]
    }

    #[inline]
    fn write_gpr(&mut self, index: usize, value: u64) {
        if index != 0 {
            self.reg_gpr[index] = value;
        }
    }
}

#[derive(PartialEq, Eq)]
enum FpOrd {
    Eq,
    Gt,
    Lt,
    No
}

// TODO: verify this is the correct offset for half-width
const LO: usize = 4;

trait FpFmt: PartialOrd {
    fn read_fpr(&[u8; 8]) -> Self;
    fn write_fpr(&mut [u8; 8], value: Self);
}

impl FpFmt for f32 {
    fn read_fpr(reg: &[u8; 8]) -> f32 {
        BigEndian::read_f32(&reg[LO..])
    }
    fn write_fpr(reg: &mut [u8; 8], value: f32) {
        BigEndian::write_f32(&mut reg[LO..], value);
    }
}

impl FpFmt for f64 {
    fn read_fpr(reg: &[u8; 8]) -> f64 {
        BigEndian::read_f64(reg)
    }
    fn write_fpr(reg: &mut [u8; 8], value: f64) {
        BigEndian::write_f64(reg, value);
    }
}

impl FpFmt for i32 {
    fn read_fpr(reg: &[u8; 8]) -> i32 {
        BigEndian::read_i32(&reg[LO..])
    }
    fn write_fpr(reg: &mut [u8; 8], value: i32) {
        BigEndian::write_i32(&mut reg[LO..], value);
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
    fn write_fpr(reg: &mut [u8; 8], value: u32) {
        BigEndian::write_u32(&mut reg[LO..], value);
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
