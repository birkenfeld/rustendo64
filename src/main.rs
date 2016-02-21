#[macro_use]
extern crate clap;
extern crate chan_signal;
extern crate crossbeam;
#[macro_use]
extern crate rustendo64_r4k as r4k;
extern crate rustendo64_bus as bus;
extern crate rustendo64_cpu as cpu;
extern crate rustendo64_rsp as rsp;
extern crate rustendo64_rdp as rdp;
extern crate rustendo64_ui as ui;
extern crate rustendo64_util as util;

mod n64;

use std::{process, thread};
use clap::{App, Arg, ArgMatches};
use chan_signal::{notify, Signal};

#[cfg(debug_assertions)]
use r4k::debug::CAUGHT_SIGINT;
use r4k::debug::DebugSpecList;

/// Main entry point for the emulator.
fn main() {
    let arguments = get_arguments();
    let pif_file_name = arguments.value_of("pif").unwrap();
    let rom_file_name = arguments.value_of("rom").unwrap();
    let debug_cpu = DebugSpecList::from_args(
        arguments.values_of_lossy("debug").unwrap_or_default());
    let debug_rsp = DebugSpecList::from_args(
        arguments.values_of_lossy("debug_rsp").unwrap_or_default());

    let pif_data = util::read_bin(pif_file_name);
    let rom_data = util::read_bin(rom_file_name);
    let options = ui::Options {
        win_title: util::get_rom_name(&rom_data, rom_file_name),
        mute_audio: arguments.is_present("mute")
    };

    setup_signal_handler();

    let mut n64 = n64::N64::new(options, pif_data, rom_data, debug_cpu, debug_rsp);
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
        .arg(Arg::with_name("debug_rsp")
                 .short("r")
                 .long("debug-rsp")
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
        .arg(Arg::with_name("mute")
                 .short("m")
                 .long("mute")
                 .help("Mute audio initially"))
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
