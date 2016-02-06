use std::sync::atomic::{AtomicUsize, ATOMIC_USIZE_INIT, Ordering};
use std::sync::mpsc;
use std::thread;

pub mod minifb;

pub enum IfOutput {
    SetSize(usize, usize),  // width, height
    SetMode(u32),
    //SetPixel(usize, u32),   // offset, value
    Update(Vec<u32>),
}

static CONTROLLER: AtomicUsize = ATOMIC_USIZE_INIT;

pub struct InterfaceChannel(mpsc::Sender<IfOutput>);

impl InterfaceChannel {
    pub fn send(&mut self, out: IfOutput) {
        self.0.send(out).unwrap();
    }

    pub fn get_input_state(&self) -> u32 {
        CONTROLLER.load(Ordering::Relaxed) as u32
    }
}

pub trait Interface {
    fn new(mpsc::Receiver<IfOutput>) -> Self;
    fn run(&mut self);
}

pub fn init_ui<T: Interface>() -> InterfaceChannel {
    let (outsend, outrecv) = mpsc::channel();
    thread::spawn(|| T::new(outrecv).run());
    InterfaceChannel(outsend)
}
