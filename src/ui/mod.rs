use std::sync::Arc;
use std::sync::mpsc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::process;
use std::thread;

pub mod minifb;

pub enum UiOutput {
    SetMode(usize, usize, usize),  // width, height, pixelsize
    Update(Vec<u32>),
}

#[derive(Clone)]
pub struct UiChannel {
    sender: mpsc::Sender<UiOutput>,
    input: Arc<AtomicUsize>,
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
}

pub trait Interface {
    fn new(mpsc::Receiver<UiOutput>, Arc<AtomicUsize>) -> Self;
    fn run(&mut self);
}

pub fn init_ui<T: Interface>() -> UiChannel {
    let input = Arc::new(AtomicUsize::new(0));
    let (outsend, outrecv) = mpsc::channel();
    let input_clone = input.clone();
    thread::spawn(move || T::new(outrecv, input_clone).run());
    UiChannel { sender: outsend, input: input }
}
