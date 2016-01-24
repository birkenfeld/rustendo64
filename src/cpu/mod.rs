mod cp0;
mod instr;

use std::fmt;

use self::instr::*;
use self::cp0::Cp0;
use super::interconnect;

const NUM_GPR: usize = 32;

pub const REG_NAMES: [&'static str; 32] = [
    "zz", "at", "v0", "v1", "a0", "a1", "a2", "a3",
    "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7",
    "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7",
    "t8", "t9", "k0", "k1", "gp", "sp", "fp", "ra"];

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
    (cpu, $args:expr,+) => {
        println!("{:?}", cpu);
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
        println!("op: {:#x}   {:?}", self.reg_pc, instr);
        if self.reg_pc == 0xffff_ffff_a400_15f8 {
            if self.tmp_i >= 30 {
                println!("{:?}", self);
                panic!("...");
            }
            self.tmp_i += 1;
        }

        match instr.opcode() {
            LUI => {
                // TODO: Check 32 vs 64 bit mode for sign extend
                // (currently 64 bit mode is assumed)
                self.write_gpr(instr.rt(), instr.imm_se() << 16);
            }
            ADDI => {
                // TODO: overflow exception
                let new_value = self.read_gpr(instr.rs()).wrapping_add(instr.imm_se());
                // result is 32-bit, sign-extended
                self.write_gpr(instr.rt(), new_value as i32 as u64);
            }
            ADDIU => {
                let new_value = self.read_gpr(instr.rs()).wrapping_add(instr.imm_se());
                // result is 32-bit, sign-extended
                self.write_gpr(instr.rt(), new_value as i32 as u64);
            }
            ANDI => {
                let new_value = self.read_gpr(instr.rs()) & instr.imm();
                self.write_gpr(instr.rt(), new_value);
            }
            ORI => {
                let new_value = self.read_gpr(instr.rs()) | instr.imm();
                self.write_gpr(instr.rt(), new_value);
            }
            XORI => {
                let new_value = self.read_gpr(instr.rs()) ^ instr.imm();
                self.write_gpr(instr.rt(), new_value);
            }
            LW => {
                let addr = self.read_gpr(instr.base()).wrapping_add(instr.imm_se());
                // TODO: Check alignment
                let word = self.read_word(addr);
                self.write_gpr(instr.rt(), word as i32 as u64);
            }
            SW => {
                let addr = self.read_gpr(instr.base()).wrapping_add(instr.imm_se());
                let word = self.read_gpr(instr.rt());
                self.write_word(addr, word as u32);
            }
            BEQ => {
                // TODO: combine branches
                // move PC to delay slot
                let addr = (instr.imm_se() << 2).wrapping_add(self.reg_pc);
                let is_eq = self.read_gpr(instr.rs()) == self.read_gpr(instr.rt());
                self.reg_pc += 4;
                self.run_instruction();
                if is_eq {
                    // Run the delay slot (XXX what happens if the delay slot is
                    // another branch?)
                    self.reg_pc = addr;
                } else {
                    self.reg_pc -= 4;
                }
            }
            BEQL => {
                let addr = (instr.imm_se() << 2).wrapping_add(self.reg_pc);
                let is_eq = self.read_gpr(instr.rs()) == self.read_gpr(instr.rt());
                self.reg_pc += 4;
                if is_eq {
                    self.run_instruction();
                    self.reg_pc = addr;
                }
            }
            BNE => {
                let addr = (instr.imm_se() << 2).wrapping_add(self.reg_pc);
                let is_neq = self.read_gpr(instr.rs()) != self.read_gpr(instr.rt());
                self.reg_pc += 4;
                self.run_instruction();
                if is_neq {
                    self.reg_pc = addr;
                } else {
                    self.reg_pc -= 4;
                }
            }
            BNEL => {
                let addr = (instr.imm_se() << 2).wrapping_add(self.reg_pc);
                let is_neq = self.read_gpr(instr.rs()) != self.read_gpr(instr.rt());
                self.reg_pc += 4;
                if is_neq {
                    self.run_instruction();
                    self.reg_pc = addr;
                }
            }
            SPECIAL => match instr.special_op() {
                JR => {
                    // TODO: Check alignment
                    let addr = self.read_gpr(instr.rs());
                    self.reg_pc += 4;
                    self.run_instruction();
                    self.reg_pc = addr - 4;  // absolute jump
                }
                AND => {
                    let new_value = self.read_gpr(instr.rs()) & self.read_gpr(instr.rt());
                    self.write_gpr(instr.rd(), new_value);
                }
                OR => {
                    let new_value = self.read_gpr(instr.rs()) | self.read_gpr(instr.rt());
                    self.write_gpr(instr.rd(), new_value);
                }
                XOR => {
                    let new_value = self.read_gpr(instr.rs()) ^ self.read_gpr(instr.rt());
                    self.write_gpr(instr.rd(), new_value);
                }
                ADDU => {
                    let new_value = self.read_gpr(instr.rs()).wrapping_add(self.read_gpr(instr.rt()));
                    self.write_gpr(instr.rd(), new_value);
                }
                SUBU => {
                    let new_value = self.read_gpr(instr.rs()).wrapping_sub(self.read_gpr(instr.rt()));
                    self.write_gpr(instr.rd(), new_value);
                }
                SLTU => {
                    let is_lt = self.read_gpr(instr.rs()) < self.read_gpr(instr.rt());
                    self.write_gpr(instr.rd(), is_lt as u64);
                }
                SRL => {
                    let new_value = self.read_gpr(instr.rt()) >> instr.sa();
                    self.write_gpr(instr.rd(), new_value as i32 as u64);
                }
                SLL => {
                    let new_value = self.read_gpr(instr.rt()) << instr.sa();
                    self.write_gpr(instr.rd(), new_value as i32 as u64);
                }
                SRLV => {
                    let bits = self.read_gpr(instr.rs()) & 0b11111;
                    let new_value = self.read_gpr(instr.rt()) >> bits;
                    self.write_gpr(instr.rd(), new_value as i32 as u64);
                }
                SLLV => {
                    let bits = self.read_gpr(instr.rs()) & 0b11111;
                    let new_value = self.read_gpr(instr.rt()) << bits;
                    self.write_gpr(instr.rd(), new_value as i32 as u64);
                }
                MULTU => {
                    let mut mult_result = self.read_gpr(instr.rs()) as u32 as u64 *
                        self.read_gpr(instr.rt()) as u32 as u64;
                    self.reg_lo = mult_result as i32 as u64;
                    mult_result >>= 32;
                    self.reg_hi = mult_result as i32 as u64;
                }
                MFHI => {
                    let new_value = self.reg_hi;
                    self.write_gpr(instr.rd(), new_value);
                }
                MFLO => {
                    let new_value = self.reg_lo;
                    self.write_gpr(instr.rd(), new_value);
                }
                _ => {
                    panic!("#UD at {:#x}: I {:#b} -- Op SPC -- Sub {:#08b}",
                       self.reg_pc, instr.0, instr.special_op());
                }
            },
            REGIMM => match instr.regimm_op() {
                BGEZAL => {
                    let addr = (instr.imm_se() << 2).wrapping_add(self.reg_pc);
                    let is_gez = (self.read_gpr(instr.rs()) >> 63) == 0;
                    let return_addr = self.reg_pc + 8;
                    self.write_gpr(31, return_addr);
                    self.reg_pc += 4;
                    self.run_instruction();
                    if is_gez {
                        self.reg_pc = addr;
                    } else {
                        self.reg_pc -= 4;
                    }
                }
                _ => {
                    panic!("#UD at {:#x}: I {:#b} -- Op REGIMM -- Sub {:#08b}",
                       self.reg_pc, instr.0, instr.regimm_op());
                }
            },
            COP0 => {
                match instr.cop_op() {
                    MT => {
                        // TODO: self.cp0...
                    }
                    _ => {
                        panic!("#UD at {:#x}: I {:#b} -- Op COP0 -- Sub {:#b}",
                               self.reg_pc, instr.0, instr.base());
                    }
                }
            },
            _ => {
                panic!("#UD at {:#x}: I {:#b} -- Op {:#08b}",
                       self.reg_pc, instr.0, instr.opcode());
            }
        }

        self.reg_pc += 4;
    }

    fn read_word_q(&self, virt_addr: u64) -> u32 {
        // TODO: Check endianness
        let phys_addr = self.virt_addr_to_phys_addr(virt_addr);
        self.interconnect.read_word(phys_addr as u32)
    }

    fn read_word(&self, virt_addr: u64) -> u32 {
        // TODO: Check endianness
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
            panic!("Unrecognized virtual address: {:#x}", virt_addr);
        }
    }

    fn write_gpr(&mut self, index: usize, value: u64) {
        if index != 0 {
            self.reg_gpr[index] = value;
        }
    }

    fn read_gpr(&self, index: usize) -> u64 {
        self.reg_gpr[index]
    }
}
