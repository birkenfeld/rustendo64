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
use std::str::FromStr;
use std::str;
use std::u32;
use std::process;
use std::path::PathBuf;
use rustyline::Editor;
use nom::IResult;
use nom::{eof, hex_u32};

use cpu::{Cpu, Instruction};

#[derive(Debug)]
pub struct MemAccess(bool, bool); // read, write

#[derive(Debug)]
pub enum DebugCond {
    MemRange(MemAccess, u64, u64),
    InsnRange(u64, u64),
    InsnFrom(u64, u32),
    InsnFor(u64),
    StateAt(u64),
    BreakAt(u64, bool),  // if true, remove breakpoint after hit
    BreakIn(u64),
}

pub struct DebugCondParseErr;

named!(hex_addr<u64>, map!(hex_u32, |v| v as i32 as u64));

named!(dec_u32<u32>, map_res!(
       map_res!(is_a!(b"0123456789"), str::from_utf8),
       |s| { u32::from_str_radix(s, 10) }
));

named!(addr_range<(u64, u64)>, chain!(
    a1: hex_addr ~
    a2: opt!(complete!(preceded!(tag!(":"), hex_addr))),
    || { (a1, a2.unwrap_or(a1)) }
));

named!(breakat<DebugCond>, preceded!(
    tag!("b:"), map!(hex_addr, |a| DebugCond::BreakAt(a, false))
));

named!(dumpat<DebugCond>, preceded!(
    tag!("d:"), map!(hex_addr, DebugCond::StateAt)
));

named!(memrange<DebugCond>, chain!(
    ac: alt!(tag!("m:")  => { |_| MemAccess(true, true) }   |
             tag!("mr:") => { |_| MemAccess(true, false) }  |
             tag!("mw:") => { |_| MemAccess(false, true) }) ~
    ad: addr_range ,
    || { DebugCond::MemRange(ac, ad.0, ad.1) }
));

named!(insnfrom<DebugCond>, chain!(
        tag!("i+:") ~
    ad: hex_addr     ~
    ct: opt!(complete!(preceded!(tag!(":"), dec_u32))),
    || { DebugCond::InsnFrom(ad, ct.unwrap_or(u32::MAX)) }
));

named!(insn<DebugCond>, preceded!(
    tag!("i:"), map!(addr_range, |(a1, a2)| { DebugCond::InsnRange(a1, a2) })
));

named!(debugspec<DebugCond>, terminated!(
    alt!(insn | insnfrom | memrange | breakat | dumpat),
    eof));

impl FromStr for DebugCond {
    type Err = DebugCondParseErr;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match debugspec(s.as_bytes()) {
            IResult::Done(_, o) => Ok(o),
            _                   => Err(DebugCondParseErr)
        }
    }
}

pub struct DebugCondList(pub Vec<DebugCond>);

impl DebugCondList {
    pub fn add_condition(&mut self, cond: DebugCond) {
        self.0.push(cond);
    }

    pub fn conditions(&mut self) -> &mut Vec<DebugCond> {
        &mut self.0
    }

    pub fn check_instr(&mut self, pc: u64) -> (u32, bool, bool) {
        let mut debug_for = 0;
        let mut dump = false;
        let mut breakpt = false;
        for c in &mut self.0 {
            match *c {
                DebugCond::InsnRange(a, b) if a <= pc && pc <= b => {
                    debug_for = max(debug_for, 1);
                }
                DebugCond::InsnFrom(a, duration) if pc == a => {
                    debug_for = max(debug_for, duration);
                }
                DebugCond::StateAt(a) if a == pc => {
                    dump = true;
                }
                DebugCond::BreakAt(a, _) if a == pc => {
                    debug_for = max(debug_for, 1);
                    dump = true;
                    breakpt = true;
                }
                DebugCond::BreakIn(ref mut n) => {
                    *n -= 1;
                    debug_for = max(debug_for, 1);
                    if *n == 0 {
                        breakpt = true;
                    }
                }
                _ => {}
            }
        }
        self.0.retain(|v| match *v {
            DebugCond::BreakAt(a, true) if a == pc  => false,
            DebugCond::BreakIn(0)                   => false,
            _ => true
        });
        (debug_for, dump, breakpt)
    }

    pub fn matches_mem(&self, addr: u64, write: bool) -> bool {
        for c in &self.0 {
            match *c {
                DebugCond::MemRange(ref ac, a, b) => {
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
                    println!("Quit/Interrupted.");
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
        let optional_addr = |n| parts.get(n).and_then(|v| u64::from_str_radix(v, 16).ok());
        let optional_count = |n| parts.get(n).and_then(|v| u64::from_str_radix(v, 10).ok());
        if parts.len() == 0 {
            return self.repeat_last(cpu);
        }
        match parts[0] {
            "q"  => { println!("Quit."); process::exit(1); },
            "c"  => self.cont(cpu, optional_addr(1)),
            "s"  => self.step(cpu, optional_count(1)),
            "b"  => self.add_break(cpu, optional_addr(1)),
            "bd" => self.del_break(cpu, optional_addr(1)),
            "d"  => self.dump(cpu, true, false, false),
            "dm" => self.dump(cpu, false, true, false),
            "df" => self.dump(cpu, false, false, true),
            "da" => self.dump(cpu, true, true, true),
            "l"  => self.list(cpu, optional_count(1)),
            "?" | "h" => self.help(),
            _         => { println!("unrecognized debugger command"); false },
            // TODO:
            // - write word
            // - read word
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
            cpu.debug_conds().add_condition(DebugCond::BreakAt(addr, true));
        }
        true
    }

    fn step(&self, cpu: &mut Cpu, n: Option<u64>) -> bool {
        cpu.debug_conds().add_condition(DebugCond::BreakIn(n.unwrap_or(1)));
        true
    }

    fn add_break(&self, cpu: &mut Cpu, addr: Option<u64>) -> bool {
        if let Some(addr) = addr {
            cpu.debug_conds().add_condition(DebugCond::BreakAt(addr, false));
            println!("Added breakpoint.");
        } else {
            println!("Need an address to break at.");
        }
        false
    }

    fn del_break(&self, cpu: &mut Cpu, addr: Option<u64>) -> bool {
        if let Some(addr) = addr {
            let mut removed = false;
            cpu.debug_conds().conditions().retain(|c| match *c {
                DebugCond::BreakAt(a, false) if a == addr => {
                    removed = true;
                    false
                },
                _ => true
            });
            if removed {
                println!("Removed breakpoint.");
            } else {
                println!("Breakpoint not found.")
            }
        } else {
            println!("Need an address to break at.");
        }
        false
    }

    fn dump(&self, cpu: &Cpu, gpr: bool, cp0: bool, cp1: bool) -> bool {
        if gpr {
            println!("CPU dump:\n{:?}", cpu);
        }
        // TODO: cp0, cp1
        false
    }

    fn list(&self, cpu: &mut Cpu, n: Option<u64>) -> bool {
        for i in 0..n.unwrap_or(10) {
            let addr = cpu.read_pc() + 4 * i;
            let instr = Instruction(cpu.read_word(addr));
            println!(" {} {:#10x}   {:?}",
                     if i == 0 { "->" } else { "  " }, addr as u32, instr);
        }
        false
    }

    fn help(&self) -> bool {
        println!("Debugger commands:
c [addr]  - continue [until pc = addr]
s [n]     - single step [over n instrs]
d         - dump CPU state
dm        - dump CP0 (MMU) state
df        - dump CP1 (FPU) state
da        - dump everything
l [n]     - list n instructions from pc
b addr    - set breakpoint at addr
bd addr   - remove breakpoint at addr
bl        - list all breakpoints
q         - quit
");
        false
    }
}
