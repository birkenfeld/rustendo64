use std::fmt;

pub const REG_NAMES: [&'static str; 32] = [
    "zz", "at", "v0", "v1", "a0", "a1", "a2", "a3",
    "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7",
    "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7",
    "t8", "t9", "k0", "k1", "gp", "sp", "fp", "ra"];

pub const LUI:     u32 = 0b001111;
pub const LW:      u32 = 0b100011;
pub const LWU:     u32 = 0b100111;
pub const SW:      u32 = 0b101011;
pub const ADDI:    u32 = 0b001000;
pub const ADDIU:   u32 = 0b001001;
pub const ANDI:    u32 = 0b001100;
pub const ORI:     u32 = 0b001101;
pub const XORI:    u32 = 0b001110;
pub const SLTI:    u32 = 0b001010;
pub const SLTIU:   u32 = 0b001011;
pub const J:       u32 = 0b000010;
pub const JAL:     u32 = 0b000011;
pub const BEQ:     u32 = 0b000100;
pub const BEQL:    u32 = 0b010100;
pub const BNE:     u32 = 0b000101;
pub const BNEL:    u32 = 0b010101;
pub const BGTZ:    u32 = 0b000111;
pub const BGTZL:   u32 = 0b010111;
pub const BLEZ:    u32 = 0b000110;
pub const BLEZL:   u32 = 0b010110;
pub const LB:      u32 = 0b100000;
pub const LBU:     u32 = 0b100100;
pub const LH:      u32 = 0b100001;
pub const LHU:     u32 = 0b100101;
pub const LD:      u32 = 0b110111;
pub const SB:      u32 = 0b101000;
pub const SH:      u32 = 0b101001;
pub const SD:      u32 = 0b111111;
pub const SPECIAL: u32 = 0b000000;
pub const REGIMM:  u32 = 0b000001;
pub const COP0:    u32 = 0b010000;

pub const JR:      u32 = 0b001000;
pub const JALR:    u32 = 0b001001;
pub const ADD:     u32 = 0b100000;
pub const ADDU:    u32 = 0b100001;
pub const SUB:     u32 = 0b100010;
pub const SUBU:    u32 = 0b100011;
pub const AND:     u32 = 0b100100;
pub const OR:      u32 = 0b100101;
pub const XOR:     u32 = 0b100110;
pub const NOR:     u32 = 0b100111;
pub const SRLV:    u32 = 0b000110;
pub const SRAV:    u32 = 0b000111;
pub const SLLV:    u32 = 0b000100;
pub const SLT:     u32 = 0b101010;
pub const SLTU:    u32 = 0b101011;
pub const SRL:     u32 = 0b000010;
pub const SRA:     u32 = 0b000011;
pub const SLL:     u32 = 0b000000;
pub const MFHI:    u32 = 0b010000;
pub const MFLO:    u32 = 0b010010;
pub const MTHI:    u32 = 0b010001;
pub const MTLO:    u32 = 0b010011;
pub const MULT:    u32 = 0b011000;
pub const MULTU:   u32 = 0b011001;
pub const DIV:     u32 = 0b011010;
pub const DIVU:    u32 = 0b011011;
pub const SYNC:    u32 = 0b001111;

pub const BGEZ:    u32 = 0b00001;
pub const BGEZAL:  u32 = 0b10001;
pub const BGEZALL: u32 = 0b10011;
pub const BGEZL:   u32 = 0b00011;
pub const BLTZ:    u32 = 0b00000;
pub const BLTZAL:  u32 = 0b10000;
pub const BLTZALL: u32 = 0b10010;
pub const BLTZL:   u32 = 0b00010;

pub const MT:      u32 = 0b00100;
pub const MF:      u32 = 0b00000;
pub const ERET:    u32 = 0b10000;


#[derive(Clone, Copy)]
pub struct Instr(pub u32);

impl Instr {
    #[inline(always)]
    pub fn opcode(self) -> u32 {
        self.0 >> 26
    }

    #[inline(always)]
    pub fn rs(self) -> usize {
        (self.0 as usize >> 21) & 0b11111
    }

    #[inline(always)]
    pub fn rt(self) -> usize {
        (self.0 as usize >> 16) & 0b11111
    }

    #[inline(always)]
    pub fn rd(self) -> usize {
        (self.0 as usize >> 11) & 0b11111
    }

    #[inline(always)]
    pub fn mt_reg(self) -> usize {
        (self.0 as usize >> 11) & 0b11111
    }

    #[inline(always)]
    pub fn base(self) -> usize {
        (self.0 as usize >> 21) & 0b11111
    }

    #[inline(always)]
    pub fn cop_op(self) -> u32 {
        (self.0 as u32 >> 21) & 0b11111
    }

    #[inline(always)]
    pub fn special_op(self) -> u32 {
        self.0 & 0b111111
    }

    #[inline(always)]
    pub fn regimm_op(self) -> u32 {
        (self.0 >> 16) & 0b11111
    }

    #[inline(always)]
    pub fn sa(self) -> u64 {
        (self.0 as u64 >> 6) & 0b11111
    }

    #[inline(always)]
    pub fn imm(self) -> u64 {
        self.0 as u64 & 0xffff
    }

    #[inline(always)]
    pub fn imm_se(self) -> u64 {
        (self.0 & 0xffff) as i16 as u64
    }

    #[inline(always)]
    pub fn imm_signed(self) -> i64 {
        (self.0 & 0xffff) as i16 as i64
    }

    #[inline(always)]
    pub fn j_target(self) -> u64 {
        (self.0 & 0x3ff_ffff) as u64
    }
}

impl fmt::Debug for Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        macro_rules! iex {
            (rt)    => { REG_NAMES[self.rt()] };
            (rs)    => { REG_NAMES[self.rs()] };
            (rd)    => { REG_NAMES[self.rd()] };
            (base)  => { REG_NAMES[self.base()] };
            (imm)   => { self.imm() };
            (ims)   => { self.imm_signed() };
            (iof)   => { (self.imm_signed() + 1) << 2 };
            (mtreg) => { self.mt_reg() };
            (sa)    => { self.sa() };
            (targ)  => { self.j_target() << 2 };
        }
        macro_rules! ins1 {
            ($name:expr, $a1:tt) => { write!(f, "{:5} {}", $name, iex!($a1)) }
        }
        macro_rules! ins2 {
            ($name:expr, $a1:tt, $a2:tt) => { write!(f, "{:5} {}, {}", $name, iex!($a1), iex!($a2)) }
        }
        macro_rules! ins2x {
            ($name:expr, $a1:tt, $a2:tt) => { write!(f, "{:5} {}, {:#x}", $name, iex!($a1), iex!($a2)) }
        }
        macro_rules! ins2s {
            ($name:expr, $a1:tt, $a2:tt) => { write!(f, "{:5} {}, {:+}", $name, iex!($a1), iex!($a2)) }
        }
        macro_rules! ins3 {
            ($name:expr, $a1:tt, $a2:tt, $a3:tt) => {
                write!(f, "{:5} {}, {}, {}", $name, iex!($a1), iex!($a2), iex!($a3)) }
        }
        macro_rules! ins3x {
            ($name:expr, $a1:tt, $a2:tt, $a3:tt) => {
                write!(f, "{:5} {}, {}, {:#x}", $name, iex!($a1), iex!($a2), iex!($a3)) }
        }
        macro_rules! ins3s {
            ($name:expr, $a1:tt, $a2:tt, $a3:tt) => {
                write!(f, "{:5} {}, {}, {:+}", $name, iex!($a1), iex!($a2), iex!($a3)) }
        }
        macro_rules! ins3m {
            ($name:expr, $a1:tt, $a2:tt, $a3:tt) => {
                write!(f, "{:5} {}, {}({})", $name, iex!($a1), iex!($a2), iex!($a3)) }
        }
        macro_rules! insc {
            ($fmt:expr, $($a:tt),+) => { write!(f, $fmt, $(iex!($a)),+) }
        }

        match self.opcode() {
            LUI     => ins2x!("lui", rt, imm),
            LW      => ins3m!("lw", rt, ims, base),
            LWU     => ins3m!("lwu", rt, ims, base),
            SW      => ins3m!("sw", rt, ims, base),
            ADDI    => ins3! ("addi", rt, rs, ims),
            ADDIU   => if self.rs() == 0 { ins2!("li", rt, ims) } else { ins3!("addiu", rt, rs, ims) },
            ANDI    => ins3x!("andi", rt, rs, imm),
            ORI     => ins3x!("ori",  rt, rs, imm),
            XORI    => ins3x!("xori", rt, rs, imm),
            SLTI    => ins3x!("slti", rt, rs, imm),
            SLTIU   => ins3x!("sltiu", rt, rs, imm),
            J       => insc! ("j     {:#x}", targ),
            JAL     => insc! ("jal   {:#x}", targ),
            BEQ     => if self.rt() == 0 { ins2s!("beqz", rs, iof) } else { ins3s!("beq", rs, rt, iof) },
            BEQL    => if self.rt() == 0 { ins2s!("beqzl", rs, iof) } else { ins3s!("beql", rs, rt, iof) },
            BNE     => if self.rt() == 0 { ins2s!("bnez", rs, iof) } else { ins3s!("bne", rs, rt, iof) },
            BNEL    => if self.rt() == 0 { ins2s!("bnezl", rs, iof) } else { ins3s!("bnel", rs, rt, iof) },
            BGTZ    => ins2s!("bgtz", rs, iof),
            BGTZL   => ins2s!("bgtzl", rs, iof),
            BLEZ    => ins2s!("blez", rs, iof),
            BLEZL   => ins2s!("blezl", rs, iof),
            LB      => ins3m!("lb", rt, ims, base),
            LBU     => ins3m!("lbu", rt, ims, base),
            LH      => ins3m!("lh", rt, ims, base),
            LHU     => ins3m!("lhu", rt, ims, base),
            LD      => ins3m!("ld", rt, ims, base),
            SB      => ins3m!("sb", rt, ims, base),
            SH      => ins3m!("sh", rt, ims, base),
            SD      => ins3m!("sd", rt, ims, base),
            SPECIAL => match self.special_op() {
                JR      => ins1!("jr", rs),
                JALR    => if self.rd() == 31 { ins1!("jalr", rs) } else { ins2!("jalr", rd, rs) },
                AND     => ins3!("and", rd, rs, rt),
                OR      => if self.rt() == 0 { ins2!("move", rd, rs) } else { ins3!("or", rd, rs, rt) },
                XOR     => ins3!("xor", rd, rs, rt),
                NOR     => ins3!("nor", rd, rs, rt),
                ADDU    => ins3!("addu", rd, rs, rt),
                SUBU    => ins3!("subu", rd, rs, rt),
                SRLV    => ins3!("srlv", rd, rt, rs),
                SRAV    => ins3!("srav", rd, rt, rs),
                SLLV    => ins3!("sllv", rd, rt, rs),
                SLT     => ins3!("slt", rd, rs, rt),
                SLTU    => ins3!("sltu", rd, rs, rt),
                SRL     => ins3!("srl", rd, rt, sa),
                SRA     => ins3!("sra", rd, rt, sa),
                SLL     => if self.sa() == 0 { write!(f, "nop") } else { ins3!("sll", rd, rt, sa) },
                MFHI    => ins1!("mfhi", rd),
                MFLO    => ins1!("mflo", rd),
                MULT    => ins2!("mult", rs, rt),
                MULTU   => ins2!("multu", rs, rt),
                DIV     => ins2!("div", rs, rt),
                DIVU    => ins2!("divu", rs, rt),
                SYNC    => write!(f, "sync"),
                x       => panic!("unsupported special opcode for display: {:#08b}", x),
            },
            REGIMM  => match self.regimm_op() {
                BGEZ    => if self.rs() == 0 { ins1!("b", iof) } else { ins2!("bgez", rs, iof) },
                BGEZAL  => if self.rs() == 0 { ins1!("bal", iof) } else { ins2!("bgezal", rs, iof) },
                BGEZALL => if self.rs() == 0 { ins1!("ball", iof) } else { ins2!("bgezall", rs, iof) },
                BGEZL   => if self.rs() == 0 { ins1!("bl", iof) } else { ins2!("bgezl", rs, iof) },
                BLTZ    => ins2!("bltz", rs, iof),
                BLTZAL  => ins2!("bltzal", rs, iof),
                BLTZALL => ins2!("bltzall", rs, iof),
                BLTZL   => ins2!("bltzl", rs, iof),
                x       => panic!("unsupported reg-imm opcode for display: {:#07b}", x),
            },
            COP0    => match self.cop_op() {
                MT      => insc! ("mtc0  {}, ${}", rt, mtreg),
                MF      => insc! ("mfc0  {}, ${}", rt, mtreg),
                ERET    => write!(f, "eret"),
                x       => panic!("unsupported cop0 opcode for display: {:#07b}", x),
            },
            x => panic!("unsupported opcode for display: {:#08b}", x),
        }
    }
}
