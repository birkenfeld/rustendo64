use std::fmt;

pub const REG_NAMES: [&'static str; 32] = [
    "zz", "at", "v0", "v1", "a0", "a1", "a2", "a3",
    "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7",
    "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7",
    "t8", "t9", "k0", "k1", "gp", "sp", "fp", "ra"];

pub const FP_REG_NAMES: [&'static str; 32] = [
    "$f0", "$f1", "$f2", "$f3", "$f4", "$f5", "$f6", "$f7",
    "$f8", "$f9", "$f10", "$f11", "$f12", "$f13", "$f14", "$f15",
    "$f16", "$f17", "$f18", "$f19", "$f20", "$f21", "$f22", "$f23",
    "$f24", "$f25", "$f26", "$f27", "$f28", "$f29", "$f30", "$f31"];

pub const FP_FORMATS: [&'static str; 32] = [
    "?", "?", "?", "?", "?", "?", "?", "?",
    "?", "?", "?", "?", "?", "?", "?", "?",
    "s", "d", "?", "?", "w", "l", "?", "?",
    "?", "?", "?", "?", "?", "?", "?", "?"];

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
pub const DADDI:   u32 = 0b011000;
pub const DADDIU:  u32 = 0b011001;
pub const LB:      u32 = 0b100000;
pub const LBU:     u32 = 0b100100;
pub const LH:      u32 = 0b100001;
pub const LHU:     u32 = 0b100101;
pub const LD:      u32 = 0b110111;
pub const LDL:     u32 = 0b011010;
pub const LDR:     u32 = 0b011011;
pub const LWL:     u32 = 0b100010;
pub const LWR:     u32 = 0b100110;
pub const SB:      u32 = 0b101000;
pub const SH:      u32 = 0b101001;
pub const SD:      u32 = 0b111111;
pub const SDL:     u32 = 0b101100;
pub const SDR:     u32 = 0b101101;
pub const SWL:     u32 = 0b101010;
pub const SWR:     u32 = 0b101110;
pub const LL:      u32 = 0b110000;
pub const LLD:     u32 = 0b110100;
pub const SC:      u32 = 0b111000;
pub const CACHE:   u32 = 0b101111;
pub const SPECIAL: u32 = 0b000000;
pub const REGIMM:  u32 = 0b000001;
pub const COP0:    u32 = 0b010000;
pub const COP1:    u32 = 0b010001;
pub const LDC1:    u32 = 0b110101;
pub const LWC1:    u32 = 0b110001;
pub const SDC1:    u32 = 0b111101;
pub const SWC1:    u32 = 0b111001;

// SPECIAL ops
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
pub const DADD:    u32 = 0b101100;
pub const DADDU:   u32 = 0b101101;
pub const DSLL:    u32 = 0b111000;
pub const DSLLV:   u32 = 0b010100;
pub const DSLL32:  u32 = 0b111100;
pub const DSRA:    u32 = 0b111011;
pub const DSRAV:   u32 = 0b010111;
pub const DSRA32:  u32 = 0b111111;
pub const DSRL:    u32 = 0b111010;
pub const DSRLV:   u32 = 0b010110;
pub const DSRL32:  u32 = 0b111110;
pub const DSUB:    u32 = 0b101110;
pub const DSUBU:   u32 = 0b101111;
pub const MFHI:    u32 = 0b010000;
pub const MFLO:    u32 = 0b010010;
pub const MTHI:    u32 = 0b010001;
pub const MTLO:    u32 = 0b010011;
pub const MULT:    u32 = 0b011000;
pub const MULTU:   u32 = 0b011001;
pub const DIV:     u32 = 0b011010;
pub const DIVU:    u32 = 0b011011;
pub const DMULT:   u32 = 0b011100;
pub const DMULTU:  u32 = 0b011101;
pub const DDIV:    u32 = 0b011110;
pub const DDIVU:   u32 = 0b011111;
pub const TEQ:     u32 = 0b110100;
pub const TGE:     u32 = 0b110000;
pub const TGEU:    u32 = 0b110001;
pub const TLT:     u32 = 0b110010;
pub const TLTU:    u32 = 0b110011;
pub const TNE:     u32 = 0b110110;
pub const SYNC:    u32 = 0b001111;
pub const SYSCALL: u32 = 0b001100;

// REG-IMM ops
pub const BGEZ:    u32 = 0b00001;
pub const BGEZAL:  u32 = 0b10001;
pub const BGEZALL: u32 = 0b10011;
pub const BGEZL:   u32 = 0b00011;
pub const BLTZ:    u32 = 0b00000;
pub const BLTZAL:  u32 = 0b10000;
pub const BLTZALL: u32 = 0b10010;
pub const BLTZL:   u32 = 0b00010;
pub const TEQI:    u32 = 0b01100;
pub const TGEI:    u32 = 0b01000;
pub const TGEIU:   u32 = 0b01001;
pub const TLTI:    u32 = 0b01010;
pub const TLTIU:   u32 = 0b01011;
pub const TNEI:    u32 = 0b01110;

// Coprocessor ops common to all coprocessors
pub const MF:      u32 = 0b00000;
pub const DMF:     u32 = 0b00001;
pub const CF:      u32 = 0b00010;
pub const MT:      u32 = 0b00100;
pub const DMT:     u32 = 0b00101;
pub const CT:      u32 = 0b00110;
pub const BC:      u32 = 0b01000;
pub const CO:      u32 = 0b10000;

// BC sub-ops
pub const BCF:     u32 = 0b00000;
pub const BCFL:    u32 = 0b00010;
pub const BCT:     u32 = 0b00001;
pub const BCTL:    u32 = 0b00011;

// COP0 specific
pub const ERET:    u32 = 0b011000;
pub const TLBP:    u32 = 0b001000;
pub const TLBR:    u32 = 0b000001;
pub const TLBWI:   u32 = 0b000010;
pub const TLBWR:   u32 = 0b000110;

// COP1 FP functions
pub const FABS:    u32 = 0b000101;
pub const FADD:    u32 = 0b000000;
pub const FCEILL:  u32 = 0b001010;
pub const FCEILW:  u32 = 0b001110;
pub const FCVTD:   u32 = 0b100001;
pub const FCVTL:   u32 = 0b100101;
pub const FCVTS:   u32 = 0b100000;
pub const FCVTW:   u32 = 0b100100;
pub const FDIV:    u32 = 0b000011;
pub const FFLOORL: u32 = 0b001011;
pub const FFLOORW: u32 = 0b001111;
pub const FMOV:    u32 = 0b000110;
pub const FMUL:    u32 = 0b000010;
pub const FNEG:    u32 = 0b000111;
pub const FROUNDL: u32 = 0b001000;
pub const FROUNDW: u32 = 0b001100;
pub const FSQRT:   u32 = 0b000100;
pub const FSUB:    u32 = 0b000001;
pub const FTRUNCL: u32 = 0b001001;
pub const FTRUNCW: u32 = 0b001101;
pub const FCF:     u32 = 0b110000;
pub const FCUN:    u32 = 0b110001;
pub const FCEQ:    u32 = 0b110010;
pub const FCUEQ:   u32 = 0b110011;
pub const FCOLT:   u32 = 0b110100;
pub const FCULT:   u32 = 0b110101;
pub const FCOLE:   u32 = 0b110110;
pub const FCULE:   u32 = 0b110111;
pub const FCSF:    u32 = 0b111000;
pub const FCNGLE:  u32 = 0b111001;
pub const FCSEQ:   u32 = 0b111010;
pub const FCNGL:   u32 = 0b111011;
pub const FCLT:    u32 = 0b111100;
pub const FCNGE:   u32 = 0b111101;
pub const FCLE:    u32 = 0b111110;
pub const FCNGT:   u32 = 0b111111;

// FP formats
pub const FMT_S:   u32 = 0b010000;
pub const FMT_D:   u32 = 0b010001;
pub const FMT_W:   u32 = 0b010100;
pub const FMT_L:   u32 = 0b010101;


#[derive(Clone, Copy)]
pub struct Instr(pub u32);

impl Instr {
    /// Opcode field: bits 26-31 -- this is present in ALL instrs.
    #[inline(always)]
    pub fn opcode(self) -> u32 {
        self.0 >> 26
    }

    // Accessors for the bits 21-25

    /// Source register -- present in arithmetic and branch instrs.
    #[inline(always)]
    pub fn rs(self) -> usize {
        (self.0 as usize >> 21) & 0b11111
    }

    /// Base address register -- present in memory instrs.
    #[inline(always)]
    pub fn base(self) -> usize {
        (self.0 as usize >> 21) & 0b11111
    }

    /// Coprocessor sub-operation -- present in cop instrs.
    #[inline(always)]
    pub fn cop_op(self) -> u32 {
        (self.0 as u32 >> 21) & 0b11111
    }

    /// FP format -- present in most FP instrs.
    #[inline(always)]
    pub fn fp_fmt(self) -> usize {
        (self.0 as usize >> 21) & 0b11111
    }

    // Accessors for the bits 16-20

    /// Source2 register -- present in arithmetic and some branch instrs.
    #[inline(always)]
    pub fn rt(self) -> usize {
        (self.0 as usize >> 16) & 0b11111
    }

    /// REGIMM operation -- present in REGIMM branch instrs.
    #[inline(always)]
    pub fn regimm_op(self) -> u32 {
        (self.0 >> 16) & 0b11111
    }

    /// FP source2 -- present in FP instrs.
    #[inline(always)]
    pub fn ft(self) -> usize {
        (self.0 as usize >> 16) & 0b11111
    }

    // Accessors for the bits 11-15

    /// Destination register -- present in non-immediate arithmetic instrs.
    #[inline(always)]
    pub fn rd(self) -> usize {
        (self.0 as usize >> 11) & 0b11111
    }

    /// Coprocessor register -- present in cop move instrs.
    #[inline(always)]
    pub fn cop_reg(self) -> usize {
        (self.0 as usize >> 11) & 0b11111
    }

    /// FP source -- present in FP instrs.
    #[inline(always)]
    pub fn fs(self) -> usize {
        (self.0 as usize >> 11) & 0b11111
    }

    // Accessors for the bits 6-10

    /// Shift amount -- present in shift instrs.
    #[inline(always)]
    pub fn sa(self) -> u64 {
        (self.0 as u64 >> 6) & 0b11111
    }

    /// FP destination -- present in FP instrs.
    #[inline(always)]
    pub fn fd(self) -> usize {
        (self.0 as usize >> 6) & 0b11111
    }

    // Accessors for the bits 0-5

    /// SPECIAL operation -- present for opcode = SPECIAL.
    #[inline(always)]
    pub fn special_op(self) -> u32 {
        self.0 & 0b111111
    }

    /// FP operation -- present in FP instrs.
    #[inline(always)]
    pub fn fp_op(self) -> u32 {
        self.0 & 0b111111
    }

    // Accessors for the bits 0-15

    /// Unsigned, zero-extended immediate.
    #[inline(always)]
    pub fn imm(self) -> u64 {
        self.0 as u64 & 0xffff
    }

    /// Unsigned, sign-extended immediate.
    #[inline(always)]
    pub fn imm_se(self) -> u64 {
        (self.0 & 0xffff) as i16 as u64
    }

    /// Signed, sign-extended immediate.
    #[inline(always)]
    pub fn imm_signed(self) -> i64 {
        (self.0 & 0xffff) as i16 as i64
    }

    // Accessors for the bits 0-23

    /// Jump target
    #[inline(always)]
    pub fn j_target(self) -> u64 {
        (self.0 & 0x3ff_ffff) as u64
    }
}

impl fmt::Debug for Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        macro_rules! unknown {
            () => { write!(f, "{:7} {:#010x}", "????", self.0) }
        }
        macro_rules! iex {
            (rt)    => { REG_NAMES[self.rt()] };
            (rs)    => { REG_NAMES[self.rs()] };
            (rd)    => { REG_NAMES[self.rd()] };
            (base)  => { REG_NAMES[self.base()] };
            (imm)   => { self.imm() };
            (ims)   => { self.imm_signed() };
            (iof)   => { (self.imm_signed() + 1) << 2 };
            (cpreg) => { self.cop_reg() };
            (sa)    => { self.sa() };
            (sa32)  => { 32 + self.sa() };
            (targ)  => { self.j_target() << 2 };
            (cop)   => { self.rt() };
            (fpfmt) => { FP_FORMATS[self.fp_fmt()] };
            (ft)    => { FP_REG_NAMES[self.ft()] };
            (fd)    => { FP_REG_NAMES[self.fd()] };
            (fs)    => { FP_REG_NAMES[self.fs()] };
        }
        macro_rules! ins {
            ($fmt:expr, $($a:tt),+) => { write!(f, $fmt, $(iex!($a)),+) }
        }
        macro_rules! ins1 {
            ($name:expr, $a1:tt) => { write!(f, "{:7} {}", $name, iex!($a1)) }
        }
        macro_rules! ins2 {
            ($name:expr, $a1:tt, $a2:tt) => { write!(f, "{:7} {}, {}", $name, iex!($a1), iex!($a2)) }
        }
        macro_rules! ins2x {
            ($name:expr, $a1:tt, $a2:tt) => { write!(f, "{:7} {}, {:#x}", $name, iex!($a1), iex!($a2)) }
        }
        macro_rules! ins2s {
            ($name:expr, $a1:tt, $a2:tt) => { write!(f, "{:7} {}, {:+}", $name, iex!($a1), iex!($a2)) }
        }
        macro_rules! ins3 {
            ($name:expr, $a1:tt, $a2:tt, $a3:tt) => {
                write!(f, "{:7} {}, {}, {}", $name, iex!($a1), iex!($a2), iex!($a3)) }
        }
        macro_rules! ins3x {
            ($name:expr, $a1:tt, $a2:tt, $a3:tt) => {
                write!(f, "{:7} {}, {}, {:#x}", $name, iex!($a1), iex!($a2), iex!($a3)) }
        }
        macro_rules! ins3s {
            ($name:expr, $a1:tt, $a2:tt, $a3:tt) => {
                write!(f, "{:7} {}, {}, {:+}", $name, iex!($a1), iex!($a2), iex!($a3)) }
        }
        macro_rules! ins3m {
            ($name:expr, $a1:tt, $a2:tt, $a3:tt) => {
                write!(f, "{:7} {}, {}({})", $name, iex!($a1), iex!($a2), iex!($a3)) }
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
            J       => ins!  ("j       {:#x}", targ),
            JAL     => ins!  ("jal     {:#x}", targ),
            BEQ     => if self.rt() == 0 { ins2s!("beqz", rs, iof) } else { ins3s!("beq", rs, rt, iof) },
            BEQL    => if self.rt() == 0 { ins2s!("beqzl", rs, iof) } else { ins3s!("beql", rs, rt, iof) },
            BNE     => if self.rt() == 0 { ins2s!("bnez", rs, iof) } else { ins3s!("bne", rs, rt, iof) },
            BNEL    => if self.rt() == 0 { ins2s!("bnezl", rs, iof) } else { ins3s!("bnel", rs, rt, iof) },
            BGTZ    => ins2s!("bgtz", rs, iof),
            BGTZL   => ins2s!("bgtzl", rs, iof),
            BLEZ    => ins2s!("blez", rs, iof),
            BLEZL   => ins2s!("blezl", rs, iof),
            DADDI   => ins3x!("daddi", rt, rs, imm),
            DADDIU  => ins3x!("daddiu", rt, rs, imm),
            LB      => ins3m!("lb", rt, ims, base),
            LBU     => ins3m!("lbu", rt, ims, base),
            LH      => ins3m!("lh", rt, ims, base),
            LHU     => ins3m!("lhu", rt, ims, base),
            LD      => ins3m!("ld", rt, ims, base),
            LDL     => ins3m!("ldl", rt, ims, base),
            LDR     => ins3m!("ldr", rt, ims, base),
            LWL     => ins3m!("lwl", rt, ims, base),
            LWR     => ins3m!("lwr", rt, ims, base),
            SB      => ins3m!("sb", rt, ims, base),
            SH      => ins3m!("sh", rt, ims, base),
            SD      => ins3m!("sd", rt, ims, base),
            SDL     => ins3m!("sdl", rt, ims, base),
            SDR     => ins3m!("sdr", rt, ims, base),
            SWL     => ins3m!("swl", rt, ims, base),
            SWR     => ins3m!("swr", rt, ims, base),
            LL      => ins3m!("ll", rt, ims, base),
            LLD     => ins3m!("lld", rt, ims, base),
            SC      => ins3m!("sc", rt, ims, base),
            CACHE   => ins3m!("cache", cop, ims, base),
            SPECIAL => match self.special_op() {
                JR      => ins1!("jr", rs),
                JALR    => if self.rd() == 31 { ins1!("jalr", rs) } else { ins2!("jalr", rd, rs) },
                ADD     => ins3!("add", rd, rs, rt),
                ADDU    => ins3!("addu", rd, rs, rt),
                SUB     => ins3!("sub", rd, rs, rt),
                SUBU    => ins3!("subu", rd, rs, rt),
                AND     => ins3!("and", rd, rs, rt),
                OR      => if self.rt() == 0 { ins2!("move", rd, rs) } else { ins3!("or", rd, rs, rt) },
                XOR     => ins3!("xor", rd, rs, rt),
                NOR     => ins3!("nor", rd, rs, rt),
                SLT     => ins3!("slt", rd, rs, rt),
                SLTU    => ins3!("sltu", rd, rs, rt),
                SLLV    => ins3!("sllv", rd, rt, rs),
                SRAV    => ins3!("srav", rd, rt, rs),
                SRLV    => ins3!("srlv", rd, rt, rs),
                SLL     => if self.sa() == 0 { write!(f, "nop") } else { ins3!("sll", rd, rt, sa) },
                SRA     => ins3!("sra", rd, rt, sa),
                SRL     => ins3!("srl", rd, rt, sa),
                DADD    => ins3!("dadd", rd, rs, rt),
                DADDU   => ins3!("daddu", rd, rs, rt),
                DSUB    => ins3!("sub", rd, rs, rt),
                DSUBU   => ins3!("subu", rd, rs, rt),
                DSLLV   => ins3!("dsllv", rd, rt, rs),
                DSRAV   => ins3!("dsrav", rd, rt, rs),
                DSRLV   => ins3!("dsrlv", rd, rt, rs),
                DSLL    => ins3!("dsll", rd, rt, sa),
                DSLL32  => ins3!("dsll", rd, rt, sa32),
                DSRA    => ins3!("dsra", rd, rt, sa),
                DSRA32  => ins3!("dsra", rd, rt, sa32),
                DSRL    => ins3!("dsrl", rd, rt, sa),
                DSRL32  => ins3!("dsrl", rd, rt, sa32),
                MFHI    => ins1!("mfhi", rd),
                MFLO    => ins1!("mflo", rd),
                MULT    => ins2!("mult", rs, rt),
                MULTU   => ins2!("multu", rs, rt),
                DIV     => ins2!("div", rs, rt),
                DIVU    => ins2!("divu", rs, rt),
                DMULT   => ins2!("dmult", rs, rt),
                DMULTU  => ins2!("dmultu", rs, rt),
                DDIV    => ins2!("ddiv", rs, rt),
                DDIVU   => ins2!("ddivu", rs, rt),
                TEQ     => ins2!("teq", rs, rt),
                TGE     => ins2!("tge", rs, rt),
                TGEU    => ins2!("tgeu", rs, rt),
                TLT     => ins2!("tlt", rs, rt),
                TLTU    => ins2!("tltu", rs, rt),
                TNE     => ins2!("tne", rs, rt),
                SYNC    => write!(f, "sync"),
                SYSCALL => write!(f, "syscall {:#x}", self.0 >> 6),
                _       => unknown!(),
            },
            REGIMM  => match self.regimm_op() {
                BGEZ    => if self.rs() == 0 { ins1!("b", iof) } else { ins2!("bgez", rs, iof) },
                BGEZAL  => if self.rs() == 0 { ins1!("bal", iof) } else { ins2!("bgezal", rs, iof) },
                BGEZALL => if self.rs() == 0 { ins1!("ball", iof) } else { ins2!("bgezall", rs, iof) },
                BGEZL   => if self.rs() == 0 { ins1!("bl", iof) } else { ins2!("bgezl", rs, iof) },
                BLTZ    => ins2! ("bltz", rs, iof),
                BLTZAL  => ins2! ("bltzal", rs, iof),
                BLTZALL => ins2! ("bltzall", rs, iof),
                BLTZL   => ins2! ("bltzl", rs, iof),
                TEQI    => ins2x!("teqi", rs, imm),
                TGEI    => ins2x!("tgei", rs, imm),
                TGEIU   => ins2x!("tgeiu", rs, imm),
                TLTI    => ins2x!("tlti", rs, imm),
                TLTIU   => ins2x!("tltiu", rs, imm),
                TNEI    => ins2x!("tnei", rs, imm),
                _       => unknown!(),
            },
            COP0    => match self.cop_op() {
                MF      => ins!("mfc0    {}, ${}", rt, cpreg),
                MT      => ins!("mtc0    {}, ${}", rt, cpreg),
                DMF     => ins!("dmfc0   {}, ${}", rt, cpreg),
                DMT     => ins!("dmtc0   {}, ${}", rt, cpreg),
                BC      => match self.regimm_op() {
                    BCF     => ins1!("bc0f", iof),
                    BCFL    => ins1!("bc0fl", iof),
                    BCT     => ins1!("bc0t", iof),
                    BCTL    => ins1!("bc0tl", iof),
                    _       => unknown!(),
                },
                CO      => match self.special_op() {
                    ERET    => write!(f, "eret"),
                    TLBP    => write!(f, "tlbp"),
                    TLBR    => write!(f, "tlbr"),
                    TLBWI   => write!(f, "tlbwi"),
                    TLBWR   => write!(f, "tlbwr"),
                    _       => unknown!(),
                },
                _       => unknown!(),
            },
            COP1    => match self.cop_op() {
                MF      => ins2!("mfc1", rt, fs),
                MT      => ins2!("mtc1", rt, fs),
                DMF     => ins2!("dmfc1", rt, fs),
                DMT     => ins2!("dmtc1", rt, fs),
                CF      => ins2!("cfc1", rt, fs),
                CT      => ins2!("ctc1", rt, fs),
                BC      => match self.regimm_op() {
                    BCF     => ins1!("bc1f", iof),
                    BCFL    => ins1!("bc1fl", iof),
                    BCT     => ins1!("bc1t", iof),
                    BCTL    => ins1!("bc1tl", iof),
                    _       => unknown!(),
                },
                _       => match self.fp_op() {
                    FABS    => ins!("abs.{}   {}, {}", fpfmt, fd, fs),
                    FADD    => ins!("add.{}   {}, {}, {}", fpfmt, fd, fs, ft),
                    FCEILL  => ins!("ceil.l.{} {}, {}", fpfmt, fd, fs),
                    FCEILW  => ins!("ceil.w.{} {}, {}", fpfmt, fd, fs),
                    FCVTD   => ins!("cvt.d.{} {}, {}", fpfmt, fd, fs),
                    FCVTL   => ins!("cvt.l.{} {}, {}", fpfmt, fd, fs),
                    FCVTS   => ins!("cvt.s.{} {}, {}", fpfmt, fd, fs),
                    FCVTW   => ins!("cvt.w.{} {}, {}", fpfmt, fd, fs),
                    FDIV    => ins!("div.{}   {}, {}, {}", fpfmt, fd, fs, ft),
                    FMOV    => ins!("mov.{}   {}, {}", fpfmt, fd, fs),
                    FMUL    => ins!("mul.{}   {}, {}", fpfmt, fd, fs),
                    FNEG    => ins!("neg.{}   {}, {}", fpfmt, fd, fs),
                    FROUNDL => ins!("round.l.{} {}, {}", fpfmt, fd, fs),
                    FROUNDW => ins!("round.w.{} {}, {}", fpfmt, fd, fs),
                    FSQRT   => ins!("sqrt.{}  {}, {}", fpfmt, fd, fs),
                    FSUB    => ins!("sub.{}   {}, {}, {}", fpfmt, fd, fs, ft),
                    FTRUNCL => ins!("trunc.l.{} {}, {}", fpfmt, fd, fs),
                    FTRUNCW => ins!("trunc.w.{} {}, {}", fpfmt, fd, fs),
                    FCF     => ins!("c.f.{}   {}, {}", fpfmt, fs, ft),
                    FCUN    => ins!("c.un.{}  {}, {}", fpfmt, fs, ft),
                    FCEQ    => ins!("c.eq.{}  {}, {}", fpfmt, fs, ft),
                    FCUEQ   => ins!("c.ueq.{} {}, {}", fpfmt, fs, ft),
                    FCOLT   => ins!("c.olt.{} {}, {}", fpfmt, fs, ft),
                    FCULT   => ins!("c.ult.{} {}, {}", fpfmt, fs, ft),
                    FCOLE   => ins!("c.ole.{} {}, {}", fpfmt, fs, ft),
                    FCULE   => ins!("c.ule.{} {}, {}", fpfmt, fs, ft),
                    FCSF    => ins!("c.sf.{}  {}, {}", fpfmt, fs, ft),
                    FCNGLE  => ins!("c.ngle.{} {}, {}", fpfmt, fs, ft),
                    FCSEQ   => ins!("c.seq.{} {}, {}", fpfmt, fs, ft),
                    FCNGL   => ins!("c.ngl.{} {}, {}", fpfmt, fs, ft),
                    FCLT    => ins!("c.lt.{}  {}, {}", fpfmt, fs, ft),
                    FCNGE   => ins!("c.nge.{} {}, {}", fpfmt, fs, ft),
                    FCLE    => ins!("c.cle.{} {}, {}", fpfmt, fs, ft),
                    FCNGT   => ins!("c.ngt.{} {}, {}", fpfmt, fs, ft),
                    _       => unknown!(),
                }
            },
            LDC1    => ins3m!("ldc1", ft, ims, base),
            LWC1    => ins3m!("lwc1", ft, ims, base),
            SDC1    => ins3m!("sdc1", ft, ims, base),
            SWC1    => ins3m!("swc1", ft, ims, base),
            _       => unknown!(),
        }
    }
}
