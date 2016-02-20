extern crate minifb as minifb_crate;
extern crate cpal;

use std::sync::Arc;
use std::sync::mpsc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::process;
use std::thread;

pub mod minifb;

pub enum UiOutput {
    SetMode(usize, usize, usize),  // width, height, pixelsize
    Update(Vec<u32>),
    Audio(u32, Vec<u32>),
}

#[derive(Clone)]
pub struct UiChannel {
    sender: mpsc::Sender<UiOutput>,
    input: Arc<AtomicUsize>,
    pending_audio: Arc<AtomicUsize>,
}

impl UiChannel {
    pub fn send(&self, out: UiOutput) {
        if let Err(_) = self.sender.send(out) {
            // The GUI was closed.
            process::exit(0);
        }
    }

    pub fn get_input_state(&self) -> u32 {
        self.input.load(Ordering::Relaxed) as u32
    }

    pub fn get_pending_audio(&self) -> usize {
        self.pending_audio.load(Ordering::Relaxed)
    }
}

pub struct Options {
    pub mute_audio: bool,
}

pub trait Interface {
    fn new(Options, mpsc::Receiver<UiOutput>, Arc<AtomicUsize>, Arc<AtomicUsize>) -> Self;
    fn run(&mut self);
}

pub fn init_ui<T: Interface>(opts: Options) -> UiChannel {
    let input = Arc::new(AtomicUsize::new(0));
    let pending_audio = Arc::new(AtomicUsize::new(0));
    let (outsend, outrecv) = mpsc::channel();
    let input_clone = input.clone();
    let pa_clone = pending_audio.clone();
    thread::spawn(move || T::new(opts, outrecv, input_clone, pa_clone).run());
    UiChannel { sender: outsend, input: input, pending_audio: pending_audio }
}
