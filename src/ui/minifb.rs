use std::sync::mpsc;

use minifb::{Window, WindowOptions, Scale};

use ui::{Interface, VideoMsg};

pub struct MinifbInterface {
    receiver: mpsc::Receiver<VideoMsg>,
    buffer: Vec<u32>,
    window: Option<Window>,
}

impl Interface for MinifbInterface {
    fn new(receiver: mpsc::Receiver<VideoMsg>) -> Self {
        MinifbInterface {
            receiver: receiver,
            buffer: Vec::new(),
            window: None,
        }
    }

    fn run(&mut self) {
        while let Ok(msg) = self.receiver.recv() {
            match msg {
                VideoMsg::SetMode(w, h) => self.setup(w, h),
                VideoMsg::SetPixel(o, p) => self.set(o, p),
                VideoMsg::Update => self.update(),
            }
        }
    }

}

impl MinifbInterface {
    fn setup(&mut self, w: usize, h: usize) {
        match Window::new(
            "Rustendo64_gb", w, h, WindowOptions {
                scale: Scale::X1, ..WindowOptions::default() }) {
            Ok(win) => {
                self.window = Some(win);
                self.buffer = vec![0; w * h];
            }
            Err(err) => {
                println!("Unable to create window: {}", err);
                return;
            }
        }
    }

    fn set(&mut self, o: usize, p: u32) {
        if o < self.buffer.len() {
            self.buffer[o] = p;
        }
    }

    fn update(&mut self) {
        if let Some(ref mut win) = self.window {
            win.update_with_buffer(&self.buffer);
        }
    }
}
