// Debug options
//
// at program start: set debug conditions
//
// - debug insns at PC range x..y
// - debug insns from PC x (N insns or forever)
// - debug memory access at range x..y
// - debug memory access for peripheral x (translated to range)
// - break at PC x
//
// in debugger prompt:
//
// - add/remove debug condition
// - single-step
// - continue

use std::cmp::max;
use std::env;
use std::fmt;
use std::str::FromStr;
use std::str;
use std::u64;
use std::process;
use std::path::PathBuf;
use std::sync::atomic::Ordering;
use rustyline::Editor;
use nom::IResult;
use nom::{eof, hex_u32};

use cpu::Cpu;
use cpu::instruction::*;
use INTR;

#[derive(Clone)]
pub struct MemAccess(bool, bool); // read, write

impl fmt::Debug for MemAccess {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (self.0, self.1) {
            (true, true)  => write!(f, "r/w"),
            (true, false) => write!(f, "r"),
            (false, true) => write!(f, "w"),
            _ => write!(f, "no")
        }
    }
}

#[derive(Clone)]
pub enum DebugSpec {
    MemRange(MemAccess, u64, u64),
    InsnRange(u64, u64),
    InsnFrom(u64, u64),
    StateAt(u64),
    BreakAt(u64, bool),  // if true, remove breakpoint after hit
    MemBreak(MemAccess, u64),
    BreakIn(u64),
}

named!(dec_u32<u32>, map_res!(
       map_res!(is_a!(b"0123456789"), str::from_utf8),
       |s| { u32::from_str_radix(s, 10) }
));

named!(integer<u64>, map!(
    alt!( complete!(preceded!(tag!("0x"), hex_u32)) |
          dec_u32 ),
    |s| s as i32 as u64
));

named!(addr_range<(u64, u64)>, chain!(
    a1: integer ~
    a2: opt!(complete!(preceded!(tag!(":"), integer))),
    || { (a1, a2.unwrap_or(a1)) }
));

named!(breakat<DebugSpec>, preceded!(
    tag!("b:"), map!(integer, |a| DebugSpec::BreakAt(a, false))
));

named!(stateat<DebugSpec>, preceded!(
    tag!("d:"), map!(integer, DebugSpec::StateAt)
));

named!(membreak<DebugSpec>, chain!(
    ac: alt!(tag!("bm:")  => { |_| MemAccess(true, true) }   |
             tag!("bmr:") => { |_| MemAccess(true, false) }  |
             tag!("bmw:") => { |_| MemAccess(false, true) }) ~
    ad: integer ,
    || { DebugSpec::MemBreak(ac, ad) }
));

named!(memrange<DebugSpec>, chain!(
    ac: alt!(tag!("m:")  => { |_| MemAccess(true, true) }   |
             tag!("mr:") => { |_| MemAccess(true, false) }  |
             tag!("mw:") => { |_| MemAccess(false, true) }) ~
    ad: addr_range ,
    || { DebugSpec::MemRange(ac, ad.0, ad.1) }
));

named!(insnfrom<DebugSpec>, chain!(
        tag!("ix:") ~
    ad: integer     ~
    ct: opt!(complete!(preceded!(tag!(":"), integer))),
    || { DebugSpec::InsnFrom(ad, ct.unwrap_or(u64::MAX)) }
));

named!(insn<DebugSpec>, preceded!(
    tag!("i:"), map!(addr_range, |(a1, a2)| { DebugSpec::InsnRange(a1, a2) })
));

named!(debugspec<DebugSpec>, terminated!(
    alt!(insn | insnfrom | memrange | breakat | membreak | stateat),
    eof));

impl FromStr for DebugSpec {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match debugspec(s.as_bytes()) {
            IResult::Done(_, o) => Ok(o),
            _                   => Err(())
        }
    }
}

impl DebugSpec {
    pub fn is_dump(&self) -> bool {
        match *self {
            DebugSpec::BreakAt(..) | DebugSpec::BreakIn(..) |
                DebugSpec::MemBreak(..) => false,
            _ => true
        }
    }
}

impl fmt::Debug for DebugSpec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            DebugSpec::BreakAt(a, temp) =>
                write!(f, "break at {:#x}{}", a, if temp {" once"} else {""}),
            DebugSpec::BreakIn(n)       =>
                write!(f, "break in {} instrs", n),
            DebugSpec::MemBreak(ref acc, a) =>
                write!(f, "break at {:?} memory access at {:#x}", acc, a),
            DebugSpec::InsnFrom(a, n)   =>
                write!(f, "print {} insns from pc {:#x}", n, a),
            DebugSpec::InsnRange(a, b)  =>
                write!(f, "print insns from pc {:#x} to pc {:#x}", a, b),
            DebugSpec::StateAt(a)       =>
                write!(f, "print CPU state at pc {:#x}", a),
            DebugSpec::MemRange(ref acc, a, b) =>
                write!(f, "print {:?} memory access from {:#x} to {:#x}", acc, a, b),
        }
    }
}

pub struct DebugSpecList(Vec<DebugSpec>);

impl DebugSpecList {
    pub fn from_args(args: Vec<String>) -> DebugSpecList {
        let specs = args.into_iter().filter_map(|arg| {
            match arg.parse::<DebugSpec>() {
                Ok(v)  => Some(v),
                Err(_) => {
                    println!("Warning: unparseable debug spec {} ignored", arg);
                    None
                }
            }
        }).collect();
        DebugSpecList(specs)
    }

    pub fn add_spec(&mut self, spec: DebugSpec) {
        self.0.push(spec);
    }

    pub fn specs(&mut self) -> &mut Vec<DebugSpec> {
        &mut self.0
    }

    pub fn check_instr(&mut self, pc: u64, instr: &Instruction, gprs: &[u64])
                       -> (u32, bool, bool) {
        let mut debug_for = 0;
        let mut dump = false;
        let mut breakpt = false;
        if INTR.load(Ordering::Relaxed) {
            INTR.store(false, Ordering::Relaxed);
            debug_for = 1;
            dump = true;
            breakpt = true;
        }
        for c in &mut self.0 {
            match *c {
                DebugSpec::InsnRange(a, b) if a <= pc && pc <= b => {
                    debug_for = max(debug_for, 1);
                }
                DebugSpec::InsnFrom(a, duration) if pc == a => {
                    debug_for = max(debug_for, duration);
                }
                DebugSpec::StateAt(a) if a == pc => {
                    dump = true;
                }
                DebugSpec::BreakAt(a, _) if a == pc => {
                    debug_for = max(debug_for, 1);
                    dump = true;
                    breakpt = true;
                }
                DebugSpec::BreakIn(ref mut n) => {
                    *n -= 1;
                    if *n < 10 {
                        debug_for = max(debug_for, 1);
                    }
                    if *n == 0 {
                        breakpt = true;
                    }
                }
                DebugSpec::MemBreak(ref acc, addr) => {
                    let hit = match instr.opcode() {
                        LW | LWU | LB | LBU | LH | LHU | LD |
                        LWL | LWR | LDL | LL | LLD | LDC1 | LWC1 => {
                            acc.0
                        }
                        SW | SB | SH | SD | SWL | SWR | SDL | SDR |
                        SC | SCD | SDC1 | SWC1 => {
                            acc.1
                        }
                        _ => false
                    };
                    // TODO: currently these are virtual addrs!
                    if hit {
                        let op_addr = gprs[instr.base()]
                            .wrapping_add(instr.imm_sign_extended()) & !0b11;
                        if op_addr == addr {
                            debug_for = max(debug_for, 1);
                            dump = true;
                            breakpt = true;
                        }
                    }
                }
                _ => {}
            }
        }
        self.0.retain(|v| match *v {
            DebugSpec::BreakAt(a, true) if a == pc  => false,
            DebugSpec::BreakIn(0)                   => false,
            _ => true
        });
        (debug_for as u32, dump, breakpt)
    }

    pub fn matches_mem(&self, addr: u64, write: bool) -> bool {
        for c in &self.0 {
            match *c {
                DebugSpec::MemRange(ref ac, a, b) => {
                    if a <= addr && addr <= b {
                        if !write && ac.0 { return true; }
                        if write && ac.1 { return true; }
                    }
                }
                _ => {}
            }
        }
        false
    }
}

pub enum DebuggerResult {
    Step,
    Continue,
    Quit,
}

pub struct Debugger<'c> {
    histfile: Option<PathBuf>,
    editor: Editor<'c>,
}

impl<'c> Debugger<'c> {
    pub fn new<'a>() -> Debugger<'a> {
        let mut editor = Editor::new();
        let histfile = env::home_dir().map(|p| p.join(".rustendo64dbg"));
        if let Some(ref fp) = histfile {
            let _ = editor.load_history(fp);
        }
        Debugger {
            editor: editor,
            histfile: histfile,
        }
    }

    pub fn run_loop(&mut self, cpu: &mut Cpu) {
        loop {
            match self.editor.readline("- ") {
                Err(_) => {
                    println!("Interrupt/Quit.");
                    process::exit(1);
                }
                Ok(input) => {
                    if input.len() > 0 {
                        self.editor.add_history_entry(&input);
                    }
                    if self.dispatch(cpu, input) {
                        if let Some(ref fp) = self.histfile {
                            let _ = self.editor.save_history(fp);
                        }
                        return;
                    }
                }
            }
        }
    }

    fn dispatch(&mut self, cpu: &mut Cpu, input: String) -> bool {
        let parts = input.split_whitespace().collect::<Vec<_>>();
        let int_arg = |n| parts.get(n).and_then(|v|
                match integer(v.as_bytes()) {
                    IResult::Done(_, o) => Some(o),
                    _                   => None
                });
        if parts.len() == 0 {
            return self.repeat_last(cpu);
        }
        match parts[0] {
            "q"  => { println!("Quit."); process::exit(1); },
            "c"  => self.cont(cpu, int_arg(1)),
            "s"  => self.step(cpu, int_arg(1)),
            "b"  => self.add_break(cpu, int_arg(1)),
            "bm" => self.add_mem_break(cpu, parts.get(1), int_arg(2)),
            "bd" => self.del_break(cpu, int_arg(1)),
            "bl" => self.list_breaks(cpu),
            "r"  => self.read_mem(cpu, int_arg(1), int_arg(2)),
            "w"  => self.write_mem(cpu, int_arg(1), int_arg(2)),
            "sa" => self.add_spec(cpu, parts.get(1)),
            "sl" => self.list_specs(cpu),
            "l"  => self.list(cpu, int_arg(1), int_arg(2)),
            "d"  => self.dump(cpu, true, false, false),
            "dm" => self.dump(cpu, false, true, false),
            "df" => self.dump(cpu, false, false, true),
            "da" => self.dump(cpu, true, true, true),
            "?" | "h" => self.help(),
            _    => { println!("unrecognized debugger command"); false },
        }
    }

    fn repeat_last(&mut self, cpu: &mut Cpu) -> bool {
        let last_line = {
            let hist = self.editor.get_history();
            if hist.len() == 0 {
                return false;
            }
            hist.get(hist.len() - 1).unwrap().to_owned()
        };
        self.dispatch(cpu, last_line)
    }

    fn cont(&self, cpu: &mut Cpu, until: Option<u64>) -> bool {
        if let Some(addr) = until {
            cpu.debug_specs().add_spec(DebugSpec::BreakAt(addr, true));
        }
        true
    }

    fn step(&self, cpu: &mut Cpu, n: Option<u64>) -> bool {
        cpu.debug_specs().add_spec(DebugSpec::BreakIn(n.unwrap_or(1)));
        true
    }

    fn read_mem(&self, cpu: &mut Cpu, addr: Option<u64>, n: Option<u64>) -> bool {
        if let Some(addr) = addr {
            for i in 0..n.unwrap_or(1) {
                // TODO: use a non-panicking version
                let word = cpu.read_word(addr + 4*i, false);
                println!("{:#10x}  {:#10x}", addr + 4*i, word);
            }
        } else {
            println!("Need an address to read from.");
        }
        false
    }

    fn write_mem(&self, cpu: &mut Cpu, addr: Option<u64>, word: Option<u64>) -> bool {
        if let Some(addr) = addr {
            if let Some(word) = word {
                // TODO: use a non-panicking version
                cpu.write_word(addr, word as u32);
            } else {
                println!("Need a word to write.");
            }
        } else {
            println!("Need an address to write to.");
        }
        false
    }

    fn add_break(&self, cpu: &mut Cpu, addr: Option<u64>) -> bool {
        if let Some(addr) = addr {
            cpu.debug_specs().add_spec(DebugSpec::BreakAt(addr, false));
            println!("Added breakpoint.");
        } else {
            println!("Need an address to break at.");
        }
        false
    }

    fn add_mem_break(&self, cpu: &mut Cpu, rw: Option<&&str>, addr: Option<u64>) -> bool {
        if let Some(rw) = rw {
            let acc = if *rw == "r" {
                MemAccess(true, false)
            } else if *rw == "w" {
                MemAccess(false, true)
            } else if *rw == "rw" {
                MemAccess(true, true)
            } else {
                println!("Assuming read/write for {}.", rw);
                MemAccess(true, true)
            };
            if let Some(addr) = addr {
                cpu.debug_specs().add_spec(DebugSpec::MemBreak(acc, addr));
                println!("Added breakpoint.");
            } else {
                println!("Need an address to break at.");
            }
        } else {
            println!("Need r, w or rw as first argument.");
        }
        false
    }

    fn del_break(&self, cpu: &mut Cpu, n: Option<u64>) -> bool {
        if let Some(n) = n {
            let mut i = 0;
            let mut removed = false;
            cpu.debug_specs().specs().retain(|c| match *c {
                DebugSpec::BreakAt(_, false) |
                DebugSpec::MemBreak(..) => {
                    i += 1;
                    if i == n + 1 {
                        removed = true;
                        false
                    } else {
                        true
                    }
                },
                _ => true
            });
            if removed {
                println!("Removed breakpoint.");
            } else {
                println!("Breakpoint not found.")
            }
        } else {
            println!("Need a breakpoint number (listed by bl).");
        }
        false
    }

    fn list_breaks(&self, cpu: &mut Cpu) -> bool {
        println!("#   type address      instruction/access");
        println!("--- ---- ------------ -------------------------");
        let specs = cpu.debug_specs().specs().iter().filter_map(|spec| {
            match *spec {
                DebugSpec::BreakAt(_, false) => Some(spec.clone()),
                DebugSpec::MemBreak(..) => Some(spec.clone()),
                _ => None
            }
        }).collect::<Vec<_>>();
        for (i, spec) in specs.into_iter().enumerate() {
            match spec {
                DebugSpec::BreakAt(addr, _) =>
                    println!("{:3} pc   {:#10x}   {:?}", i, addr as u32,
                             Instruction(cpu.read_word(addr, true))),
                DebugSpec::MemBreak(ref acc, addr) =>
                    println!("{:3} mem  {:#10x}   {:?}", i, addr as u32, acc),
                _ => {}
            }
        }
        false
    }

    fn add_spec(&self, cpu: &mut Cpu, spec: Option<&&str>) -> bool {
        if let Some(spec) = spec {
            if let Ok(spec) = DebugSpec::from_str(spec) {
                cpu.debug_specs().add_spec(spec);
            } else {
                println!("Debug spec {} not understood.", spec);
            }
        } else {
            println!("Need a debug spec to add.");
        }
        false
    }

    fn list_specs(&self, cpu: &mut Cpu) -> bool {
        for spec in cpu.debug_specs().specs().iter() {
            if spec.is_dump() {
                println!("{:?}", spec);
            }
        }
        println!("");
        false
    }

    fn dump(&self, cpu: &Cpu, gpr: bool, cp0: bool, cp1: bool) -> bool {
        if gpr {
            println!("CPU dump:\n{:?}", cpu);
        }
        if cp0 {
            println!("CP0 dump:");
            cpu.cp0_dump();
        }
        if cp1 {
            println!("FPU dump:");
            cpu.cp1_dump();
        }
        false
    }

    fn list(&self, cpu: &mut Cpu, n: Option<u64>, addr: Option<u64>) -> bool {
        let base_addr = addr.unwrap_or(cpu.read_pc());
        for i in 0..n.unwrap_or(10) {
            let addr = base_addr + 4 * i;
            let instr = Instruction(cpu.read_word(addr, true));
            println!(" {} {:#10x}   {:?}",
                     if i == 0 { "->" } else { "  " }, addr as u32, instr);
        }
        false
    }

    fn help(&self) -> bool {
        println!("Debugger commands:
c [addr]     - continue until pc = addr (or forever)
s [n]        - single step over n (=1) instrs

b addr       - set breakpoint at addr
bm rw addr   - set memory breakpoint at addr (rw can be r, w, or rw)
bd num       - remove breakpoint #num
bl           - list all breakpoints

r addr [n]   - read n (=1) words from memory starting at addr
w addr val   - write word val to memory at addr

sa spec      - add a debug spec
sl           - list currently active debug specs

l [n] [addr] - list n (=10) instructions from addr (or pc)
d            - dump CPU state
dm           - dump CP0 (MMU) state
df           - dump CP1 (FPU) state
da           - dump everything

q            - quit
");
        false
    }
}
