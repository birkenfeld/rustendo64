use std::fmt;
use std::collections::VecDeque;

use super::instruction::*;
use super::exception::*;
use super::cp0::Cp0;
use interconnect;
use util::mult_64_64;

const NUM_GPR: usize = 32;

pub struct Cpu {
    instr_counter: u32,
    debug_instrs: bool,

    reg_gpr: [u64; NUM_GPR],
    reg_fpr: [f64; NUM_GPR],

    reg_pc:  u64,
    last_pc: u64,

    reg_hi: u64,
    reg_lo: u64,

    reg_llbit: bool, // TODO: Enum type

    reg_fcr31: u32,

    in_branch_delay: bool,
    exc_pending: VecDeque<Exception>,

    cp0: Cp0,
    interconnect: interconnect::Interconnect
}

macro_rules! bug {
    ($cpu:expr, $($args:expr),+) => {
        //println!("{:#?}", $cpu.interconnect);
        println!("{:?}", $cpu);
        println!("#instrs executed: {}", $cpu.instr_counter);
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
        write!(f, "     pc = {:016x}", self.reg_pc);
        write!(f, "     hi = {:016x}     lo = {:016x}", self.reg_hi, self.reg_lo);
        Ok(())
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

            reg_gpr:   [0; NUM_GPR],
            reg_fpr:   [0.0; NUM_GPR],

            reg_pc:    0,
            last_pc:   0,

            reg_hi:    0,
            reg_lo:    0,
            reg_llbit: false,
            reg_fcr31: 0,

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
            if !self.cp0.reg_status.interrupt_mask.timer_interrupt &&
                self.cp0.reg_status.interrupts_enabled
            {
                self.exc_pending.push_back(
                    Exception { exc_type: ExcType::Interrupt(Intr::Timer) });
            }
        }

        // Can we start exception processing?
        if !self.cp0.reg_status.exception_level {
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
        }

        self.instr_counter += 1;
        self.last_pc = self.reg_pc;
        let pc = self.reg_pc;
        // if pc > 0xffff_ffff_8000_5000 && pc < 0xffff_ffff_9000_0000 {
        //      self.debug_instrs = true;
        // }
        let instr = Instruction(self.read_word(pc));
        dprintln!(self, "op: {:#x}   {:?}", self.reg_pc & 0xffff_ffff, instr);

        match instr.opcode() {
            LUI   => {
                let val = instr.imm_sign_extended() << 16;
                dprintln!(self, "{} {} <- {:#x}", INDENT, REG_NAMES[instr.rt()], val);
                self.write_gpr(instr.rt(), val);
            }
            LW    => {
                let addr = self.read_gpr(instr.base()).wrapping_add(instr.imm_sign_extended());
                if addr & 0b11 != 0 {
                    bug!(self, "Unaligned address in LW: {:#x}", addr);
                }
                let word = self.read_word(addr) as i32 as u64;
                dprintln!(self, "{} {} <- {:#18x} :  mem @ {:#x}",
                         INDENT, REG_NAMES[instr.rt()], word, addr);
                self.write_gpr(instr.rt(), word);
            }
            LWU   => {
                let addr = self.read_gpr(instr.base()).wrapping_add(instr.imm_sign_extended());
                if addr & 0b11 != 0 {
                    bug!(self, "Unaligned address in LWU: {:#x}", addr);
                }
                let word = self.read_word(addr);
                dprintln!(self, "{} {} <- {:#18x} :  mem @ {:#x}",
                          INDENT, REG_NAMES[instr.rt()], word, addr);
                self.write_gpr(instr.rt(), word as u64);
            }
            SW    => {
                let addr = self.read_gpr(instr.base()).wrapping_add(instr.imm_sign_extended());
                if addr & 0b11 != 0 {
                    bug!(self, "Unaligned address in SW: {:#x}", addr);
                }
                let word = self.read_gpr(instr.rt());
                dprintln!(self, "{}       {:#18x} -> mem @ {:#x}",
                          INDENT, word, addr);
                self.write_word(addr, word as u32);
            }
            // TODO: overflow exception
            ADDI  => self.arithi(&instr, |rs| rs.wrapping_add(instr.imm_sign_extended()) as i32 as u64),
            ADDIU => self.arithi(&instr, |rs| rs.wrapping_add(instr.imm_sign_extended()) as i32 as u64),
            ANDI  => self.arithi(&instr, |rs| rs & instr.imm()),
            ORI   => self.arithi(&instr, |rs| rs | instr.imm()),
            XORI  => self.arithi(&instr, |rs| rs ^ instr.imm()),
            SLTI  => self.arithi(&instr, |rs| ((rs as i64) < instr.imm_sign_extended() as i64) as u64),
            SLTIU => self.arithi(&instr, |rs| (rs < instr.imm_sign_extended()) as u64),
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
            DADDI => self.arithi(&instr, |rs| rs.wrapping_add(instr.imm_sign_extended())),
            DADDIU => self.arithi(&instr, |rs| rs.wrapping_add(instr.imm_sign_extended())),
            LB    => {
                // TODO: dedicated method?
                let addr = self.read_gpr(instr.base()).wrapping_add(instr.imm_sign_extended());
                let word_addr = addr & !0b11;
                let byte_offset = 0b11 - (addr & 0b11);  // big endian
                let word = self.read_word(word_addr);
                let byte = (word >> (8 * byte_offset)) & 0xFF;
                dprintln!(self, "{} {} <- {:#18x}({:#4x}) :  mem @ {:#x}",
                         INDENT, REG_NAMES[instr.rt()], word, byte, addr);
                self.write_gpr(instr.rt(), byte as i8 as u64);
            }
            LBU   => {
                // TODO: dedicated method?
                let addr = self.read_gpr(instr.base()).wrapping_add(instr.imm_sign_extended());
                let word_addr = addr & !0b11;
                let byte_offset = 0b11 - (addr & 0b11);
                let word = self.read_word(word_addr) as u64;
                let byte = (word >> (8 * byte_offset)) & 0xFF;
                dprintln!(self, "{} {} <- {:#18x}({:#4x}) :  mem @ {:#x}",
                         INDENT, REG_NAMES[instr.rt()], word, byte, addr);
                self.write_gpr(instr.rt(), byte);
            }
            LH    => {
                // TODO: dedicated method?
                let addr = self.read_gpr(instr.base()).wrapping_add(instr.imm_sign_extended());
                if addr & 0b1 != 0 {
                    bug!(self, "Unaligned address in LH: {:#x}", addr);
                }
                let word_addr = addr & !0b1;
                let hword_offset = 0b1 - ((addr & 0b10) >> 1);  // big endian
                let word = self.read_word(word_addr);
                let hword = (word >> (16 * hword_offset)) & 0xFFFF;
                dprintln!(self, "{} {} <- {:#18x}({:#6x}) :  mem @ {:#x}",
                         INDENT, REG_NAMES[instr.rt()], word, hword, addr);
                self.write_gpr(instr.rt(), hword as i16 as u64);
            }
            LHU   => {
                // TODO: dedicated method?
                let addr = self.read_gpr(instr.base()).wrapping_add(instr.imm_sign_extended());
                if addr & 0b1 != 0 {
                    bug!(self, "Unaligned address in LHU: {:#x}", addr);
                }
                let word_addr = addr & !0b1;
                let hword_offset = 0b1 - ((addr & 0b10) >> 1);  // big endian
                let word = self.read_word(word_addr) as u64;
                let hword = (word >> (16 * hword_offset)) & 0xFFFF;
                dprintln!(self, "{} {} <- {:#18x}({:#6x}) :  mem @ {:#x}",
                         INDENT, REG_NAMES[instr.rt()], word, hword, addr);
                self.write_gpr(instr.rt(), hword);
            }
            LD    => {
                let addr = self.read_gpr(instr.base()).wrapping_add(instr.imm_sign_extended());
                if addr & 0b111 != 0 {
                    bug!(self, "Unaligned address in LD: {:#x}", addr);
                }
                let dword = (self.read_word(addr) as u64) << 32 | self.read_word(addr + 4) as u64;
                dprintln!(self, "{} {} <- {:#18x} :  mem @ {:#x}",
                         INDENT, REG_NAMES[instr.rt()], dword, addr);
                self.write_gpr(instr.rt(), dword);
            }
            // LDL, LDR, LWL, LWR
            SB    => {
                // TODO: dedicated method?
                let addr = self.read_gpr(instr.base()).wrapping_add(instr.imm_sign_extended());
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
                // TODO: dedicated method?
                let addr = self.read_gpr(instr.base()).wrapping_add(instr.imm_sign_extended());
                if addr & 0b1 != 0 {
                    bug!(self, "Unaligned address in SH: {:#x}", addr);
                }
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
                let addr = self.read_gpr(instr.base()).wrapping_add(instr.imm_sign_extended());
                if addr & 0b111 != 0 {
                    bug!(self, "Unaligned address in SD: {:#x}", addr);
                }
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
                ADD  => self.arithr(&instr, |rs, rt| (rs as i32).wrapping_add(rt as i32) as i32 as u64),
                ADDU => self.arithr(&instr, |rs, rt| (rs as u32).wrapping_add(rt as u32) as i32 as u64),
                // TODO: Overflow exception
                SUB  => self.arithr(&instr, |rs, rt| (rs as i32).wrapping_sub(rt as i32) as i32 as u64),
                SUBU => self.arithr(&instr, |rs, rt| (rs as u32).wrapping_sub(rt as u32) as i32 as u64),
                AND  => self.arithr(&instr, |rs, rt| rs & rt),
                OR   => self.arithr(&instr, |rs, rt| rs | rt),
                XOR  => self.arithr(&instr, |rs, rt| rs ^ rt),
                NOR  => self.arithr(&instr, |rs, rt| !(rs | rt)),
                SLT  => self.arithr(&instr, |rs, rt| ((rs as i64) < rt as i64) as u64),
                SLTU => self.arithr(&instr, |rs, rt| (rs < rt) as u64),
                SLLV => self.arithr(&instr, |rs, rt| (rt << (rs & 0b11111)) as i32 as u64),
                SRAV => self.arithr(&instr, |rs, rt| (rt as i32 >> (rs & 0b11111)) as u64),
                SRLV => self.arithr(&instr, |rs, rt| (rt as u32 >> (rs & 0b11111)) as i32 as u64),
                SLL  => if instr.sa() != 0 {
                    self.ariths(&instr, |rt| (rt << instr.sa()) as i32 as u64)
                },
                SRA  => self.ariths(&instr, |rt| (rt as i32 >> instr.sa()) as i32 as u64),
                SRL  => self.ariths(&instr, |rt| (rt as u32 >> instr.sa()) as i32 as u64),
                // TODO: Overflow exception
                DADD   => self.arithr(&instr, |rs, rt| rs.wrapping_add(rt)),
                DADDU  => self.arithr(&instr, |rs, rt| rs.wrapping_add(rt)),
                // TODO: Overflow exception
                DSUB   => self.arithr(&instr, |rs, rt| rs.wrapping_sub(rt)),
                DSUBU  => self.arithr(&instr, |rs, rt| rs.wrapping_sub(rt)),
                DSLLV  => self.arithr(&instr, |rs, rt| rt << (rs & 0b111111)),
                DSRAV  => self.arithr(&instr, |rs, rt| (rt as i64 >> (rs & 0b111111)) as u64),
                DSRLV  => self.arithr(&instr, |rs, rt| rt >> (rs & 0b111111)),
                DSLL   => self.ariths(&instr, |rt| rt << instr.sa()),
                DSLL32 => self.ariths(&instr, |rt| rt << (instr.sa() + 32)),
                DSRA   => self.ariths(&instr, |rt| (rt as i64 >> instr.sa()) as u64),
                DSRA32 => self.ariths(&instr, |rt| (rt as i64 >> (instr.sa() + 32)) as u64),
                DSRL   => self.ariths(&instr, |rt| rt >> instr.sa()),
                DSRL32 => self.ariths(&instr, |rt| rt >> (instr.sa() + 32)),
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
                    let (rl, rh) = mult_64_64(self.read_gpr(instr.rs()),
                                              self.read_gpr(instr.rt()));
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
                _ => {
                    bug!(self, "#UD: I {:#b} -- {:?}", instr.0, instr);
                }
            },
            REGIMM => match instr.regimm_op() {
                BGEZ    => self.branch(&instr, false, false, |cpu|
                                       (cpu.read_gpr(instr.rs()) >> 63) == 0),
                BGEZL   => self.branch(&instr, true, false, |cpu|
                                       (cpu.read_gpr(instr.rs()) >> 63) == 0),
                BGEZAL  => self.branch(&instr, false, true, |cpu|
                                       (cpu.read_gpr(instr.rs()) >> 63) == 0),
                BGEZALL => self.branch(&instr, true, true, |cpu|
                                       (cpu.read_gpr(instr.rs()) >> 63) == 0),
                BLTZ    => self.branch(&instr, false, false, |cpu|
                                       (cpu.read_gpr(instr.rs()) >> 63) != 0),
                BLTZL   => self.branch(&instr, true, false, |cpu|
                                       (cpu.read_gpr(instr.rs()) >> 63) != 0),
                BLTZAL  => self.branch(&instr, false, true, |cpu|
                                       (cpu.read_gpr(instr.rs()) >> 63) != 0),
                BLTZALL => self.branch(&instr, true, true, |cpu|
                                       (cpu.read_gpr(instr.rs()) >> 63) != 0),
                // TEQI, TGEI, TGEIU, TLTI, TLTIU, TNEI
                _ => {
                    bug!(self, "#UD: I {:#b} -- {:?}", instr.0, instr);
                }
            },
            COP0 => match instr.cop_op() {
                MF => {
                    let data = self.cp0.read_reg(instr.rd());
                    dprintln!(self, "{} cp0[{:2}] :  {:#066b}", INDENT, instr.rd(), data);
                    self.write_gpr(instr.rt(), data);
                }
                MT => {
                    let data = self.read_gpr(instr.rt());
                    dprintln!(self, "{} cp0[{:2}] <- {:#066b}", INDENT, instr.rd(), data);
                    self.cp0.write_reg(instr.rd(), data);
                }
                // DMF, DMT
                // BC...
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
                    _ => {
                        bug!(self, "#UD: I {:#b} -- {:?}", instr.0, instr);
                    }
                },
                _ => {
                    bug!(self, "#UD: I {:#b} -- {:?}", instr.0, instr);
                }
            },
            COP1 => match instr.cop_op() {
                // MF, MT
                // DMF, DMT
                CF => {
                    let data = match instr.fs() {
                        31 => self.reg_fcr31,
                        0  => 0xB << 8 | 0,  // TODO: constant
                        _  => { bug!(self, "#RG: invalid read fp control reg {}", instr.fs()); },
                    };
                    self.write_gpr(instr.rt(), data as u64);
                }
                CT => {
                    let data = self.read_gpr(instr.rt());
                    if instr.fs() == 31 {
                        self.reg_fcr31 = data as u32;
                    } else {
                        bug!(self, "#RG: invalid write fp control reg {}", instr.fs());
                    }
                }
                // BC...
                _  => match instr.fp_op() {
                    // all the special fp ops
                    _ => {
                        bug!(self, "#UD: I {:#b} -- {:?}", instr.0, instr);
                    }
                }
            },
            _ => {
                bug!(self, "#UD: I {:#b} -- {:?}", instr.0, instr);
            }
        }

        self.reg_pc += 4;
    }

    #[inline]
    fn arithi<F>(&mut self, instr: &Instruction, func: F)
        where F: Fn(u64) -> u64
    {
        let res = func(self.read_gpr(instr.rs()));
        dprintln!(self, "{} {} :  {:#18x}", INDENT, REG_NAMES[instr.rs()], self.read_gpr(instr.rs()));
        dprintln!(self, "{} {} <- {:#18x}", INDENT, REG_NAMES[instr.rt()], res);
        self.write_gpr(instr.rt(), res);
    }

    #[inline]
    fn arithr<F>(&mut self, instr: &Instruction, func: F)
        where F: Fn(u64, u64) -> u64
    {
        let res = func(self.read_gpr(instr.rs()), self.read_gpr(instr.rt()));
        dprintln!(self, "{} {} :  {:#18x}", INDENT, REG_NAMES[instr.rs()], self.read_gpr(instr.rs()));
        dprintln!(self, "{} {} :  {:#18x}", INDENT, REG_NAMES[instr.rt()], self.read_gpr(instr.rt()));
        dprintln!(self, "{} {} <- {:#18x}", INDENT, REG_NAMES[instr.rd()], res);
        self.write_gpr(instr.rd(), res);
    }

    #[inline]
    fn ariths<F>(&mut self, instr: &Instruction, func: F)
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

    fn read_word(&mut self, virt_addr: u64) -> u32 {
        let phys_addr = self.virt_addr_to_phys_addr(virt_addr);
        match self.interconnect.read_word(phys_addr as u32) {
            Ok(res) => res,
            Err(desc) => {
                bug!(self, "{}: ({:#x}) {:#x}", desc, virt_addr, phys_addr);
            }
        }
    }

    fn write_word(&mut self, virt_addr: u64, word: u32) {
        let phys_addr = self.virt_addr_to_phys_addr(virt_addr);
        if let Err(desc) = self.interconnect.write_word(phys_addr as u32, word) {
            bug!(self, "{}: ({:#x}) {:#x}", desc, virt_addr, phys_addr);
        }
    }

    fn virt_addr_to_phys_addr(&self, virt_addr: u64) -> u64 {
        // See Table 5-3 in the VR4300 User's Manual
        let addr_bit_values = (virt_addr >> 29) & 0b111;

        if addr_bit_values == 0b101 {
            // kseg1
            virt_addr - 0xffff_ffff_a000_0000
        } else if addr_bit_values == 0b100 {
            // kseg0
            virt_addr - 0xffff_ffff_8000_0000
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

    // fn read_fpr(&self, index: usize, fmt: u32) -> {
    // }
}
