use std::fmt;

use super::instr::*;
use super::cp0::Cp0;
use interconnect;

const NUM_GPR: usize = 32;

pub struct Cpu {
    reg_gpr: [u64; NUM_GPR],
    reg_fpr: [f64; NUM_GPR],

    reg_pc: u64,

    reg_hi: u64,
    reg_lo: u64,

    reg_llbit: bool, // TODO: Enum type

    reg_fcr0: u32,
    reg_fcr31: u32,

    cp0: Cp0,
    tmp_i: u32,

    interconnect: interconnect::Interconnect
}

macro_rules! unimpl {
    ($cpu:expr, $($args:expr),+) => {
        println!("{:?}", $cpu);
        panic!($($args),+);
    }
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
        Ok(())
    }
}

impl Cpu {
    pub fn new(interconnect: interconnect::Interconnect) -> Cpu {
        Cpu {
            reg_gpr: [0; NUM_GPR],
            reg_fpr: [0.0; NUM_GPR],

            reg_pc: 0,

            reg_hi: 0,
            reg_lo: 0,

            reg_llbit: false,

            reg_fcr0: 0,
            reg_fcr31: 0,

            cp0: Cp0::default(),
            tmp_i: 0,

            interconnect: interconnect
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

    pub fn run_instruction(&mut self) {
        let instr = Instr(self.read_word_q(self.reg_pc));
        println!("op: {:#x}   {:?}", self.reg_pc & 0xffff_ffff, instr);
        if self.reg_pc == 0xffff_ffff_a400_15f8 {
            if self.tmp_i >= 30 {
                unimpl!(self, "...");
            }
            self.tmp_i += 1;
        }

        match instr.opcode() {
            LUI   => self.write_gpr(instr.rt(), instr.imm_se() << 16),
            LW    => {
                let addr = self.read_gpr(instr.base()).wrapping_add(instr.imm_se());
                if addr & 0b11 != 0 {
                    unimpl!(self, "Unaligned address in LW: {:#x}", addr);
                }
                let word = self.read_word(addr);
                self.write_gpr(instr.rt(), word as i32 as u64);
            }
            LWU   => {
                let addr = self.read_gpr(instr.base()).wrapping_add(instr.imm_se());
                if addr & 0b11 != 0 {
                    unimpl!(self, "Unaligned address in LWU: {:#x}", addr);
                }
                let word = self.read_word(addr);
                self.write_gpr(instr.rt(), word as u64);
            }
            SW    => {
                let addr = self.read_gpr(instr.base()).wrapping_add(instr.imm_se());
                if addr & 0b11 != 0 {
                    unimpl!(self, "Unaligned address in SW: {:#x}", addr);
                }
                let word = self.read_gpr(instr.rt());
                self.write_word(addr, word as u32);
            }
            // TODO: overflow exception
            ADDI  => self.arithi(&instr, |rs| rs.wrapping_add(instr.imm_se()) as i32 as u64),
            ADDIU => self.arithi(&instr, |rs| rs.wrapping_add(instr.imm_se()) as i32 as u64),
            ANDI  => self.arithi(&instr, |rs| rs & instr.imm()),
            ORI   => self.arithi(&instr, |rs| rs | instr.imm()),
            XORI  => self.arithi(&instr, |rs| rs ^ instr.imm()),
            SLTI  => self.arithi(&instr, |rs| ((rs as i64) < instr.imm_se() as i64) as u64),
            SLTIU => self.arithi(&instr, |rs| (rs < instr.imm_se()) as u64),
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
            // LB, LBU, LH, LHU
            LD    => {
                let addr = self.read_gpr(instr.base()).wrapping_add(instr.imm_se());
                if addr & 0b111 != 0 {
                    unimpl!(self, "Unaligned address in SD: {:#x}", addr);
                }
                // TODO: Check endianness
                let dword = (self.read_word(addr) as u64) << 32 | self.read_word(addr + 4) as u64;
                self.write_gpr(instr.rt(), dword);
            }
            // SB, SH
            SD    => {
                let addr = self.read_gpr(instr.base()).wrapping_add(instr.imm_se());
                if addr & 0b111 != 0 {
                    unimpl!(self, "Unaligned address in SD: {:#x}", addr);
                }
                let dword = self.read_gpr(instr.rt());
                // TODO: Check endianness
                self.write_word(addr, (dword >> 32) as u32);
                self.write_word(addr + 4, dword as u32);
            }
            SPECIAL => match instr.special_op() {
                JR => {
                    let addr = self.read_gpr(instr.rs());
                    if addr & 0b11 != 0 {
                        unimpl!(self, "Unaligned address in JR: {:#x}", addr);
                    }
                    self.reg_pc += 4;
                    self.run_instruction();
                    self.reg_pc = addr - 4;  // absolute jump
                }
                JALR => {
                    let addr = self.read_gpr(instr.rs());
                    if addr & 0b11 != 0 {
                        unimpl!(self, "Unaligned address in JALR: {:#x}", addr);
                    }
                    let return_addr = self.reg_pc + 8;
                    self.write_gpr(instr.rd(), return_addr);  // usually rd = 31
                    self.reg_pc += 4;
                    self.run_instruction();
                    self.reg_pc = addr - 4;  // absolute jump
                }
                // TODO: Overflow exception
                ADD  => self.arithr(&instr, |rs, rt| rs.wrapping_add(rt)),
                ADDU => self.arithr(&instr, |rs, rt| rs.wrapping_add(rt)),
                // TODO: Overflow exception
                SUB  => self.arithr(&instr, |rs, rt| rs.wrapping_sub(rt)),
                SUBU => self.arithr(&instr, |rs, rt| rs.wrapping_sub(rt)),
                AND  => self.arithr(&instr, |rs, rt| rs & rt),
                OR   => self.arithr(&instr, |rs, rt| rs | rt),
                XOR  => self.arithr(&instr, |rs, rt| rs ^ rt),
                NOR  => self.arithr(&instr, |rs, rt| !(rs | rt)),
                SLT  => self.arithr(&instr, |rs, rt| ((rs as i64) < rt as i64) as u64),
                SLTU => self.arithr(&instr, |rs, rt| (rs < rt) as u64),
                SRLV => self.arithr(&instr, |rs, rt| (rt >> (rs & 0b1111)) as i32 as u64),
                SRAV => self.arithr(&instr, |rs, rt| (rt as i64 >> (rs & 0b1111)) as i32 as u64),
                SLLV => self.arithr(&instr, |rs, rt| (rt << (rs & 0b1111)) as i32 as u64),
                SRL  => self.ariths(&instr, |rt| (rt >> instr.sa()) as i32 as u64),
                SRA  => self.ariths(&instr, |rt| (rt as i64 >> instr.sa()) as i32 as u64),
                SLL  => self.ariths(&instr, |rt| (rt << instr.sa()) as i32 as u64),
                MFHI => { let val = self.reg_hi; self.write_gpr(instr.rd(), val); }
                MFLO => { let val = self.reg_lo; self.write_gpr(instr.rd(), val); }
                MTHI => { let val = self.read_gpr(instr.rs()); self.reg_hi = val; }
                MTLO => { let val = self.read_gpr(instr.rs()); self.reg_lo = val; }
                MULT => {
                    let mut mult_result = (self.read_gpr(instr.rs()) as i32) as i64 *
                        (self.read_gpr(instr.rt()) as i32) as i64;
                    self.reg_lo = mult_result as i32 as u64;
                    mult_result >>= 32;
                    self.reg_hi = mult_result as i32 as u64;
                }
                MULTU => {
                    let mut mult_result = (self.read_gpr(instr.rs()) & 0xffff_ffff) *
                        (self.read_gpr(instr.rt()) & 0xffff_ffff);
                    self.reg_lo = mult_result as i32 as u64;
                    mult_result >>= 32;
                    self.reg_hi = mult_result as i32 as u64;
                }
                // DIV, DIVU
                SYNC  => { }
                _ => {
                    unimpl!(self, "#UD at {:#x}: I {:#b} -- Op SPC -- Sub {:#08b}",
                            self.reg_pc, instr.0, instr.special_op());
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
                _ => {
                    unimpl!(self, "#UD at {:#x}: I {:#b} -- Op REGIMM -- Sub {:#08b}",
                            self.reg_pc, instr.0, instr.regimm_op());
                }
            },
            COP0 => {
                match instr.cop_op() {
                    MT => {
                        let data = self.read_gpr(instr.rt());
                        self.cp0.write_reg(instr.rd(), data);
                    }
                    // MF
                    // ERET
                    _ => {
                        unimpl!(self, "#UD at {:#x}: I {:#b} -- Op COP0 -- Sub {:#b}",
                                self.reg_pc, instr.0, instr.base());
                    }
                }
            },
            _ => {
                unimpl!(self, "#UD at {:#x}: I {:#b} -- Op {:#08b}",
                        self.reg_pc, instr.0, instr.opcode());
            }
        }

        self.reg_pc += 4;
    }

    #[inline]
    fn arithi<F>(&mut self, instr: &Instr, func: F)
        where F: Fn(u64) -> u64
    {
        let res = func(self.read_gpr(instr.rs()));
        self.write_gpr(instr.rt(), res);
    }

    #[inline]
    fn arithr<F>(&mut self, instr: &Instr, func: F)
        where F: Fn(u64, u64) -> u64
    {
        let res = func(self.read_gpr(instr.rs()), self.read_gpr(instr.rt()));
        self.write_gpr(instr.rd(), res);
    }

    #[inline]
    fn ariths<F>(&mut self, instr: &Instr, func: F)
        where F: Fn(u64) -> u64
    {
        let res = func(self.read_gpr(instr.rt()));
        self.write_gpr(instr.rd(), res);
    }

    #[inline]
    fn branch<P>(&mut self, instr: &Instr, likely: bool, link: bool, mut predicate: P)
        where P: FnMut(&mut Self) -> bool
    {
        let addr = (instr.imm_se() << 2).wrapping_add(self.reg_pc);
        let take = predicate(self);
        if link {
            let return_addr = self.reg_pc + 8;
            self.write_gpr(31, return_addr);
        }
        println!("    branch: {}", if take { "taken" } else { "not taken" });
        // Run the delay slot (XXX what happens if the delay slot is
        // another branch?)
        self.reg_pc += 4;
        if take || !likely {
            self.run_instruction();
            if take {
                self.reg_pc = addr;
            } else {
                self.reg_pc -= 4;
            }
        }
    }

    fn read_word_q(&self, virt_addr: u64) -> u32 {
        let phys_addr = self.virt_addr_to_phys_addr(virt_addr);
        self.interconnect.read_word(phys_addr as u32)
    }

    fn read_word(&self, virt_addr: u64) -> u32 {
        let phys_addr = self.virt_addr_to_phys_addr(virt_addr);
        let res = self.interconnect.read_word(phys_addr as u32);
        println!("    mem: {:#x} v {:#010x} r= {:#x}", virt_addr, phys_addr, res);
        res
    }

    fn write_word(&mut self, virt_addr: u64, word: u32) {
        let phys_addr = self.virt_addr_to_phys_addr(virt_addr);
        println!("    mem: {:#x} v {:#010x} w= {:#x}", virt_addr, phys_addr, word);
        self.interconnect.write_word(phys_addr as u32, word);
    }

    fn virt_addr_to_phys_addr(&self, virt_addr: u64) -> u64 {
        // See Table 5-3 in the VR4300 User's Manual
        let addr_bit_values = (virt_addr >> 29) & 0b111;

        if addr_bit_values == 0b101 {
            // kseg1
            virt_addr - 0xffff_ffff_a000_0000
        } else {
            // TODO
            unimpl!(self, "Unrecognized virtual address: {:#x}", virt_addr);
        }
    }

    fn write_gpr(&mut self, index: usize, value: u64) {
        if index != 0 {
            self.reg_gpr[index] = value;
        }
    }

    fn read_gpr(&self, index: usize) -> u64 {
        // Reg 0 is always 0 since we never write it
        self.reg_gpr[index]
    }
}
