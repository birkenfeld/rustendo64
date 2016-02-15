#![allow(dead_code)]

#[macro_use] extern crate clap;
#[macro_use] extern crate nom;
#[macro_use] extern crate enum_primitive;
extern crate num;
extern crate minifb;
extern crate byteorder;
extern crate rustyline;
extern crate ansi_term;
extern crate chan_signal;
extern crate crossbeam;

mod n64;
mod cpu;
mod rsp;
mod bus;
mod util;
mod ui;
mod debug;

use std::process;
use std::thread;
use std::sync::atomic::{AtomicBool, ATOMIC_BOOL_INIT};
use clap::{App, Arg, ArgMatches};
use chan_signal::{notify, Signal};

/// Set to true when SIGINT is caught.
pub static CAUGHT_SIGINT: AtomicBool = ATOMIC_BOOL_INIT;

/// Main entry point for the emulator.
fn main() {
    let arguments = get_arguments();
    let pif_file_name = arguments.value_of("pif").unwrap();
    let rom_file_name = arguments.value_of("rom").unwrap();
    let debug_specs = debug::DebugSpecList::from_args(
        arguments.values_of_lossy("debug").unwrap_or_default());

    let pif_data = util::read_bin(pif_file_name);
    let rom_data = util::read_bin(rom_file_name);

    setup_signal_handler();

    let mut n64 = n64::N64::new(pif_data, rom_data, debug_specs);
    n64.power_on_reset();
    n64.run();
}

/// Parse the command line with clap.
fn get_arguments<'a>() -> ArgMatches<'a> {
    App::new("rustendo64")
        .version(crate_version!())
        .author("ferris <jake@fusetools.com>")
        .about("Livecoding a Nintendo 64 emulator in Rust :D")
        .arg(Arg::with_name("debug")
                 .short("d")
                 .long("debug")
                 .takes_value(true)
                 .number_of_values(1)
                 .multiple(true))
        .arg(Arg::with_name("pif")
                 .help("Sets the PIF ROM needed for booting")
                 .takes_value(true)
                 .required(true))
        .arg(Arg::with_name("rom")
                 .help("Sets the ROM to run")
                 .takes_value(true)
                 .required(true))
        .get_matches()
}

#[cfg(debug_assertions)]
fn sigint_handler() {
    use std::sync::atomic::Ordering;
    CAUGHT_SIGINT.store(true, Ordering::Relaxed);
}

#[cfg(not(debug_assertions))]
fn sigint_handler() {
    println!("Exiting on keyboard interrupt.");
    process::exit(2);
}

/// Set up a signal handler to break into the debugger on Ctrl-C.
fn setup_signal_handler() {
    let sig = notify(&[Signal::INT, Signal::TERM, Signal::QUIT]);
    thread::spawn(move || {
        while let Some(sig) = sig.recv() {
            match sig {
                Signal::INT  => sigint_handler(),
                // We need to handle these, currently chan-signal does
                // block them otherwise.
                Signal::TERM => process::exit(2),
                Signal::QUIT => process::exit(2),
                _ => {}
            }
        }
    });
}
