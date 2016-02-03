use std::sync::mpsc;
use std::thread;

pub mod minifb;

pub enum VideoMsg {
    SetMode(usize, usize),  // width, height
    SetPixel(usize, u32),   // offset, value
    Update,
}

pub trait Interface {
    fn new(receiver: mpsc::Receiver<VideoMsg>) -> Self;
    fn run(&mut self);
}

pub fn init_ui<T: Interface>() -> mpsc::Sender<VideoMsg> {
    let (sender, receiver) = mpsc::channel();
    thread::spawn(|| T::new(receiver).run());
    sender
}
