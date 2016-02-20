use std::fmt;
#[cfg(debug_assertions)]
use ansi_term;

use instruction::*;
use memory::MemFmt;
#[cfg(debug_assertions)]
use debug::DebugSpecList;

const NUM_GPR: usize = 32;

#[macro_export]
#[cfg(debug_assertions)]
macro_rules! dprintln {
    ($cpu:expr, $($args:expr),+) => {
        if $cpu.get_regs().debug_print {
            println!("{}", $cpu.get_debug_color().paint(format!($($args),+)));
        }
    }
}

#[cfg(debug_assertions)]
pub const INDENT: &'static str = "                                       ";

#[macro_export]
#[cfg(not(debug_assertions))]
macro_rules! dprintln {
    ($cpu:expr, $($args:expr),+) => { }
}


#[derive(Default)]
pub struct R4300Common {
    // Debugging info
    #[cfg(debug_assertions)] pub debug_specs: DebugSpecList,
    #[cfg(debug_assertions)] pub debug_print: bool,
    #[cfg(debug_assertions)] pub debug_until: u64,
                             pub instr_ctr:   u64,  // TODO

    // Some more informational
    pub last_instr:      Instruction,

    // Registers
    pub gpr:             [u64; NUM_GPR],
    pub pc:              u64,

    // Helpers
    pub in_branch_delay: bool,
    pub next_pc:         Option<u64>,
}

impl R4300Common {
    // implemented here because we need to split-borrow self
    #[cfg(debug_assertions)]
    fn check_debug_instr(&mut self, pc: u64, instr: &Instruction) -> (u64, bool, bool) {
        self.debug_specs.check_instr(pc, instr, &self.gpr)
    }

    #[cfg(debug_assertions)]
    fn check_debug_mem(&self, addr: u64, store: bool) -> bool {
        self.debug_specs.matches_mem(addr, store)
    }
}

/// Main trait for a VR4300-like processor.
///
/// Most functions are default-implemented, others have to be supplied by the
/// two implementors (Cpu and Rsp).
pub trait R4300<'c> where Self: Sized + fmt::Debug {
    type Bus;

    // FUNCTIONS TO IMPLEMENT

    /// Read an instruction word from memory.
    fn read_instr(&self, &Self::Bus, u64) -> u32;
    /// Read a word from memory.
    fn read_word(&self, &Self::Bus, u64) -> u32;
    /// Write a word to memory.
    fn write_word(&mut self, &mut Self::Bus, u64, u32);
    /// Translate address.
    fn translate_addr(&self, virt_addr: u64) -> u64;

    /// Interrupt handling before an instruction.
    fn check_interrupts(&mut self, &mut Self::Bus);
    /// Linked-load/store handlers.
    fn ll_handler(&mut self, virt_addr: u64);
    fn sc_handler<T: MemFmt<'c, Self>>(&mut self, &mut Self::Bus,
                                       &Instruction, virt_addr: u64, T);

    /// Get the color to use for debug output.
    #[cfg(debug_assertions)]
    fn get_debug_color(&self) -> ansi_term::Colour;
    #[cfg(debug_assertions)]
    fn cp0_dump(&self);
    #[cfg(debug_assertions)]
    fn cp1_dump(&self);
    #[cfg(debug_assertions)]
    fn cp2_dump(&self);

    /// Get mask for PC references.
    fn get_pc_mask(&self) -> u64;
    /// Get references to the common-register struct.
    fn get_regs(&self) -> &R4300Common;
    fn mut_regs(&mut self) -> &mut R4300Common;
    /// Get description for debug output.
    fn get_desc(&self) -> &'static str;

    /// Handlers for non-common ops.
    fn dispatch_op(&mut self, &mut Self::Bus, &Instruction);
    fn dispatch_special_op(&mut self, &mut Self::Bus, &Instruction);

    /// Handlers for coprocessor ops.
    fn dispatch_cop0_op(&mut self, &mut Self::Bus, &Instruction);
    fn dispatch_cop1_op(&mut self, &mut Self::Bus, &Instruction);
    fn dispatch_cop2_op(&mut self, &mut Self::Bus, &Instruction);

    // MAIN FUNCTIONS

    fn run_instruction(&mut self, bus: &mut Self::Bus) {
        // Handle pending interrupts.
        self.check_interrupts(bus);

        // Read next instruction.
        let pc = self.get_regs().pc;
        let instr = Instruction(self.read_instr(bus, pc));
        self.mut_regs().last_instr = instr;

        // Maybe process some debug stuff.
        self.handle_debug(bus, pc, &instr);

        // Dispatch.
        dprintln!(self, "op: {:#10x}   {:?}", pc as u32, instr);
        self.dispatch_instr(bus, &instr);

        // Go to next instruction if current instruction didn't jump
        // (or set an exception vector, etc.)
        let next_pc = self.mut_regs().next_pc.take().unwrap_or(self.get_regs().pc + 4);
        self.mut_regs().pc = next_pc & self.get_pc_mask();
    }

    fn run_branch_delay_slot(&mut self, bus: &mut Self::Bus, addr: u64) {
        if self.get_regs().in_branch_delay {
            self.bug(format!("Branching in branch delay slot -- check semantics!"));
        }
        self.mut_regs().in_branch_delay = true;
        self.mut_regs().pc += 4;
        self.mut_regs().next_pc = Some(addr); // prepare jump
        self.run_instruction(bus);
        self.mut_regs().next_pc = Some(self.get_regs().pc); // no adjustment to pc
        self.mut_regs().in_branch_delay = false;
    }

    #[inline(always)]
    fn dispatch_instr(&mut self, bus: &mut Self::Bus, instr: &Instruction) {
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
                let addr = ((self.get_regs().pc + 4) & 0xffff_ffff_c000_0000) | (instr.j_target() << 2);
                self.jump(bus, addr, 0);
            }
            JAL   => {
                let addr = ((self.get_regs().pc + 4) & 0xffff_ffff_c000_0000) | (instr.j_target() << 2);
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
            LB    => self.mem_load (bus, instr, false, |byte: u8| byte as i8 as u64),
            LBU   => self.mem_load (bus, instr, false, |byte: u8| byte as u64),
            LH    => self.mem_load (bus, instr, false, |hword: u16| hword as i16 as u64),
            LHU   => self.mem_load (bus, instr, false, |hword: u16| hword as u64),
            LD    => self.mem_load (bus, instr, false, |dword: u64| dword),
            SB    => self.mem_store(bus, instr, false, |data| data as u8),
            SH    => self.mem_store(bus, instr, false, |data| data as u16),
            SD    => self.mem_store(bus, instr, false, |data| data),
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
                _    => self.dispatch_special_op(bus, instr),
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
            COP0 => self.dispatch_cop0_op(bus, &instr),
            COP1 => self.dispatch_cop1_op(bus, &instr),
            COP2 => self.dispatch_cop2_op(bus, &instr),
            _    => self.dispatch_op(bus, instr),
        }
    }

    // HELPERS

    fn read_dword(&self, bus: &Self::Bus, virt_addr: u64) -> u64 {
        (self.read_word(bus, virt_addr) as u64) << 32 |
        self.read_word(bus, virt_addr + 4) as u64
    }

    fn write_dword(&mut self, bus: &mut Self::Bus, virt_addr: u64, dword: u64) {
        self.write_word(bus, virt_addr, (dword >> 32) as u32);
        self.write_word(bus, virt_addr + 4, dword as u32);
    }

    fn read_gpr(&self, index: usize) -> u64 {
        // Reg 0 is always 0 since we never write it
        self.get_regs().gpr[index]
    }

    fn write_gpr(&mut self, index: usize, value: u64) {
        if index != 0 {
            self.mut_regs().gpr[index] = value;
        }
    }

    fn aligned_addr(&self, instr: &Instruction, align: u64) -> u64 {
        let addr = self.read_gpr(instr.base()).wrapping_add(instr.imm_sign_ext());
        if addr & (align - 1) != 0 {
            self.bug(format!("Address not aligned to {} bytes: {:#x}", align, addr));
        }
        addr
    }

    #[cold]
    fn bug(&self, msg: String) -> ! {
        println!("\nBug in {}! Processor dump:\n{:?}", self.get_desc(), self);
        println!("last instr was:    {:?}", self.get_regs().last_instr);
        println!("#instrs executed:  {}", self.get_regs().instr_ctr);
        panic!(msg);
    }

    // OPS

    fn mem_load<T: MemFmt<'c, Self>, F>(&mut self, bus: &Self::Bus, instr: &Instruction,
                                        linked: bool, func: F) where F: Fn(T) -> u64
    {
        let addr = self.aligned_addr(instr, T::get_align());
        if linked {
            self.ll_handler(addr);
        }

        let data = func(T::load_from(self, bus, addr));
        dprintln!(self, "{} {} <- {:#18x} :  mem @ {:#x}",
                  INDENT, REG_NAMES[instr.rt()], data, addr);
        self.write_gpr(instr.rt(), data);
    }

    fn mem_store<T: MemFmt<'c, Self>, F>(&mut self, bus: &mut Self::Bus, instr: &Instruction,
                                          linked: bool, func: F) where F: Fn(u64) -> T
    {
        let addr = self.aligned_addr(instr, T::get_align());
        let data = self.read_gpr(instr.rt());
        let data = func(data);
        dprintln!(self, "{}       {:#18x} -> mem @ {:#x}", INDENT, data, addr);
        if linked {
            self.sc_handler(bus, instr, addr, data);
        } else {
            T::store_to(self, bus, addr, data);
        }
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

    fn unary<F>(&mut self, instr: &Instruction, func: F)
        where F: Fn(u64) -> u64
    {
        let res = func(self.read_gpr(instr.rt()));
        dprintln!(self, "{} {} :  {:#18x}", INDENT, REG_NAMES[instr.rt()],
                  self.read_gpr(instr.rt()));
        dprintln!(self, "{} {} <- {:#18x}", INDENT, REG_NAMES[instr.rd()], res);
        self.write_gpr(instr.rd(), res);
    }

    fn jump(&mut self, bus: &mut Self::Bus, addr: u64, link_reg: usize) {
        if addr & 0b11 != 0 {
            self.bug(format!("Unaligned address in jump: {:#x}", addr));
        }
        if link_reg > 0 {
            let return_addr = self.get_regs().pc + 8;
            self.write_gpr(link_reg, return_addr);
            dprintln!(self, "{} {} <- {:#18x}", INDENT, REG_NAMES[link_reg],
                      return_addr);
        }
        // TODO: do this differently (now we execute 2 instructions for one run_instr)
        self.run_branch_delay_slot(bus, addr);
    }

    fn branch<P>(&mut self, bus: &mut Self::Bus, instr: &Instruction, likely: bool,
                 link: bool, mut predicate: P) where P: FnMut(&mut Self) -> bool
    {
        // Offset is relative to the delay slot.
        let addr = (instr.imm_sign_ext() << 2).wrapping_add(self.get_regs().pc + 4);
        let take = predicate(self);
        let next_instr = self.get_regs().pc + 8;
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
            self.mut_regs().next_pc = Some(next_instr);
        }
    }

    // DEBUGGING

    #[cfg(debug_assertions)]
    fn get_debug_specs(&mut self) -> &mut DebugSpecList {
        &mut self.mut_regs().debug_specs
    }

    #[cfg(debug_assertions)]
    fn handle_debug(&mut self, bus: &mut Self::Bus, pc: u64, instr: &Instruction) {
        use std::u64;
        use ansi_term::Colour;
        use debug::Debugger;

        let (debug_for, dump_here, break_here) = self.mut_regs().check_debug_instr(pc, instr);
        self.mut_regs().instr_ctr += 1;
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
            self.mut_regs().debug_print = true;
            if debug_for == u64::MAX {
                self.mut_regs().debug_until = u64::MAX;
            } else if debug_for > 1 {
                self.mut_regs().debug_until = self.get_regs().instr_ctr + debug_for;
            }
        } else if self.get_regs().debug_print && self.get_regs().instr_ctr > self.get_regs().debug_until {
            self.mut_regs().debug_print = false;
        }
    }

    #[cfg(debug_assertions)]
    fn debug_read(&self, phys_addr: u64, res: u32) {
        if self.get_regs().check_debug_mem(phys_addr, false) {
            println!("{}", self.get_debug_color().paint(
                format!("   {:#10x}   Bus read:  {:#10x} :  {:#10x}",
                        self.get_regs().pc as u32, phys_addr, res)));
        }
    }

    #[cfg(debug_assertions)]
    fn debug_write(&self, phys_addr: u64, word: u32) {
        if self.get_regs().check_debug_mem(phys_addr, true) {
            println!("{}", self.get_debug_color().paint(
                format!("   {:#10x}   Bus write: {:#10x} <- {:#10x}",
                        self.get_regs().pc as u32, phys_addr, word)));
        }
    }

    #[cfg(debug_assertions)]
    fn read_pc(&self) -> u64 {
        self.get_regs().pc
    }

    // NON-DEBUG STUBS

    #[cfg(not(debug_assertions))]
    fn handle_debug(&mut self, _: &mut Self::Bus, _: u64, _: &Instruction) { }

    #[cfg(not(debug_assertions))]
    fn debug_read(&self, _: u64, _: u32) { }

    #[cfg(not(debug_assertions))]
    fn debug_write(&self, _: u64, _: u32) { }
}
