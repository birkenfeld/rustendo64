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

fn paddr(s: &str) -> Result<u64, ::std::num::ParseIntError> {
    u32::from_str_radix(s, 16).map(|v| v as i32 as u64)
}

impl FromStr for DebugCond {
    type Err = DebugCondParseErr;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts = s.split(':').collect::<Vec<_>>();
        match parts[0] {
            "i" => {
                if parts.len() == 3 {
                    if let (Ok(a), Ok(b)) = (paddr(parts[1]), paddr(parts[2])) {
                        return Ok(DebugCond::InsnRange(a, b));
                    }
                } else if parts.len() == 2 {
                    if let Ok(a) = paddr(parts[1]) {
                        return Ok(DebugCond::InsnFrom(a, None));
                    }
                }
            },
            "i+" => {
                if parts.len() == 3 {
                    if let (Ok(a), Ok(b)) = (paddr(parts[1]), parts[2].parse()) {
                        return Ok(DebugCond::InsnFrom(a, Some(b)));
                    }
                }
            }
            "m" | "mw" | "mr" => {
                let access = if parts[0] == "m" { MemAccess(true, true) } else
                    if parts[0] == "mw" { MemAccess(false, true) } else
                    { MemAccess(true, false) };
                if parts.len() == 3 {
                    if let (Ok(a), Ok(b)) = (paddr(parts[1]), paddr(parts[2])) {
                        return Ok(DebugCond::MemRange(access, a, b));
                    }
                } else if parts.len() == 2 {
                    if let Ok(a) = paddr(parts[1]) {
                        return Ok(DebugCond::MemRange(access, a, a));
                    }
                }
            },
            "d" => {
                if parts.len() == 2 {
                    if let Ok(a) = paddr(parts[1]) {
                        return Ok(DebugCond::DumpAt(a));
                    }
                }
            },
            "b" => {
                if parts.len() == 2 {
                    if let Ok(a) = paddr(parts[1]) {
                        return Ok(DebugCond::BreakAt(a));
                    }
                }
            },
            _ => {}
        }
        Err(DebugCondParseErr)
    }
}

pub struct DebugCondList(pub Vec<DebugCond>);

impl DebugCondList {
    pub fn matches_pc(&self, pc: u64) -> u32 {
        for c in &self.0 {
            match *c {
                DebugCond::InsnRange(a, b) if a <= pc && pc <= b => return 1,
                DebugCond::InsnFrom(a, ref howlong) if pc == a => {
                    if let &Some(duration) = howlong {
                        return duration;
                    }
                    return 1;
                }
                _ => {}
            }
        }
        0
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
