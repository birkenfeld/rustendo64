use std::sync::atomic::{AtomicUsize, ATOMIC_USIZE_INIT, Ordering};
use std::sync::mpsc;
use std::process;
use std::thread;

pub mod minifb;

pub enum UiOutput {
    SetMode(usize, usize, usize),  // width, height, pixelsize
    Update(Vec<u32>),
}

static CONTROLLER: AtomicUsize = ATOMIC_USIZE_INIT;

pub struct UiChannel(mpsc::Sender<UiOutput>);

impl UiChannel {
    pub fn send(&self, out: UiOutput) {
        if let Err(_) = self.0.send(out) {
            // The GUI was closed.
            process::exit(0);
        }
    }

    pub fn get_input_state(&self) -> u32 {
        CONTROLLER.load(Ordering::Relaxed) as u32
    }

    pub fn clone(&self) -> UiChannel {
        UiChannel(self.0.clone())
    }
}

pub trait Interface {
    fn new(mpsc::Receiver<UiOutput>) -> Self;
    fn run(&mut self);
}

pub fn init_ui<T: Interface>() -> UiChannel {
    let (outsend, outrecv) = mpsc::channel();
    thread::spawn(|| T::new(outrecv).run());
    UiChannel(outsend)
}
