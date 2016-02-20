//! This crate abstracts anything that's common to the CPU and the RSP.

#[macro_use]
extern crate nom;
extern crate ansi_term;
extern crate rustyline;

pub mod debug;
pub mod instruction;
mod processor;
mod memory;
pub use processor::{R4300, R4300Common};
pub use memory::MemFmt;
