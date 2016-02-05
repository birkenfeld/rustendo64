#![allow(dead_code)]

#[macro_use] extern crate clap;
#[macro_use] extern crate nom;
#[macro_use] extern crate enum_primitive;
extern crate num;
extern crate minifb;
extern crate byteorder;
extern crate rustyline;

mod n64;
mod cpu;
mod rsp;
mod interconnect;
mod mem_map;
mod cic;
mod util;
mod ui;
mod debug;

use std::fs;
use std::io::Read;
use std::path::Path;
use clap::{App, Arg, ArgMatches};

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

fn main() {
    let arguments = get_arguments();
    let pif_file_name = arguments.value_of("pif").unwrap();
    let rom_file_name = arguments.value_of("rom").unwrap();
    let debug = if let Some(args) = arguments.values_of("debug") {
        args.filter_map(|arg| match arg.parse::<debug::DebugCond>() {
            Ok(v)  => Some(v),
            Err(_) => {
                println!("Warning: ignoring unrecognized debug arg {}", arg);
                None
            }
        }).collect()
    } else { vec![] };

    let pif = read_bin(pif_file_name);
    let rom = read_bin(rom_file_name);

    let mut n64 = n64::N64::new(pif, rom, debug::DebugCondList(debug));
    n64.power_on_reset();
    n64.run();
}

fn read_bin<P: AsRef<Path>>(path: P) -> Vec<u8> {
    let mut file = fs::File::open(path).unwrap();
    let mut file_buf = Vec::new();
    file.read_to_end(&mut file_buf).unwrap();
    file_buf
}
