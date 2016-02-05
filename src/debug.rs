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

use std::str::FromStr;
use std::str;
use rustyline::Editor;
use nom::IResult;
use nom::{eof, hex_u32};

use cpu::Cpu;

#[derive(Debug)]
pub struct MemAccess(bool, bool); // read, write

#[derive(Debug)]
pub enum DebugCond {
    InsnRange(u64, u64),
    InsnFrom(u64, Option<u32>),
    MemRange(MemAccess, u64, u64),
    DumpAt(u64),
    BreakAt(u64),
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
    tag!("b:"), map!(hex_addr, DebugCond::BreakAt)
));

named!(dumpat<DebugCond>, preceded!(
    tag!("d:"), map!(hex_addr, DebugCond::DumpAt)
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
    || { DebugCond::InsnFrom(ad, ct) }
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
    pub fn check_pc(&self, pc: u64) -> (u32, bool, bool) {
        let mut debug_for = 0;
        let mut dump = false;
        let mut breakpoint = false;
        for c in &self.0 {
            match *c {
                DebugCond::InsnRange(a, b) if a <= pc && pc <= b => debug_for = 1,
                DebugCond::InsnFrom(a, ref howlong) if pc == a => {
                    if let &Some(duration) = howlong {
                        debug_for = duration;
                    } else {
                        debug_for = 1;
                    }
                },
                DebugCond::DumpAt(a) if a == pc => dump = true,
                DebugCond::BreakAt(a) if a == pc => {
                    dump = true;
                    breakpoint = true;
                },
                _ => {}
            }
        }
        (debug_for, dump, breakpoint)
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
    editor: Editor<'c>,
}

impl<'c> Debugger<'c> {
    pub fn new<'a>() -> Debugger<'a> {
        Debugger {
            editor: Editor::new(),
        }
    }

    pub fn run_loop(&mut self, cpu: &mut Cpu) -> DebuggerResult {
        loop {
            match self.editor.readline("- ") {
                Err(_) => return DebuggerResult::Quit,
                Ok(input) => {
                    match &input[..] {
                        "c"  => return DebuggerResult::Continue,
                        "q"  => return DebuggerResult::Quit,
                        "s"  => return DebuggerResult::Step,
                        "d"  => println!("CPU dump:\n{:?}", cpu),
                        _    => println!("unrecognized debugger command"),
                        // TODO:
                        // - dump FPR
                        // - dump CP0
                        // - disassemble around PC
                        // - write word
                        // - read word
                    }
                }
            }
        }
    }
}
