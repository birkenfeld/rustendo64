extern crate minifb as minifb_crate;
extern crate cpal;

use std::sync::Arc;
use std::sync::mpsc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::process;
use std::thread;

// Lives in this crate for now.
pub mod minifb;

/// Events sent to the user interface from the emulator threads.
pub enum UiMessage {
    SetMode(usize, usize, usize),  // width, height, pixelsize
    Update(Vec<u32>),
    Audio(u32, Vec<u32>),
}

/// Receiver for UI events, given to the UI plugin object.
pub struct UiReceiver {
    receiver: mpsc::Receiver<UiMessage>,
    input: Arc<AtomicUsize>,
    pending_audio: Arc<AtomicUsize>,
}

impl UiReceiver {
    pub fn recv(&self) -> Result<UiMessage, mpsc::RecvError> {
        self.receiver.recv()
    }

    pub fn set_input_state(&self, state: u32) {
        self.input.store(state as usize, Ordering::Relaxed)
    }

    pub fn set_pending_audio(&self, pending: usize) {
        self.pending_audio.store(pending, Ordering::Relaxed)
    }
}

/// Options passed to the interface from the command line.
pub struct Options {
    pub no_ui: bool,
    pub win_title: String,
    pub mute_audio: bool,
}

/// Trait for user interface plugins.
pub trait Interface {
    fn new(Options, UiReceiver) -> Self;
    fn run(&mut self);
}

/// An interface that does nothing (except to receive and discard messages,
/// which we have to do to avoid them accumulating in memory).
pub struct NullInterface {
    recv: mpsc::Receiver<UiMessage>,
}

impl Interface for NullInterface {
    fn new(_: Options, receiver: UiReceiver) -> Self {
        NullInterface { recv: receiver.receiver }
    }

    fn run(&mut self) {
        println!("Null interface selected: audio and video disabled.");
        while let Ok(_) = self.recv.recv() { }
    }
}

/// Initialize the selected UI and return the opened channel for communication.
pub fn init_ui<T: Interface>(opts: Options) -> UiSender {
    let input = Arc::new(AtomicUsize::new(0));
    let pend_audio = Arc::new(AtomicUsize::new(usize::max_value()));
    let (outsend, outrecv) = mpsc::channel();
    let receiver = UiReceiver {
        receiver: outrecv,
        input: input.clone(),
        pending_audio: pend_audio.clone(),
    };
    thread::spawn(move || T::new(opts, receiver).run());
    UiSender { sender: outsend, input: input, pending_audio: pend_audio }
}

/// This is the "producer" part for the UI; UiChannels are used by the emulator
/// objects to send events.  Can be cloned into each thread that needs them.
#[derive(Clone)]
pub struct UiSender {
    sender: mpsc::Sender<UiMessage>,
    input: Arc<AtomicUsize>,
    pending_audio: Arc<AtomicUsize>,
}

impl UiSender {
    pub fn send(&self, out: UiMessage) {
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
