use std::sync::atomic::Ordering;
use std::sync::mpsc;

use minifb::{Window, WindowOptions, Scale, Key};

use ui::{Interface, IfOutput, CONTROLLER};

pub struct MinifbInterface {
    receiver: mpsc::Receiver<IfOutput>,
    buffer: Vec<u32>,
    window: Option<Window>,
}

impl Interface for MinifbInterface {
    fn new(outrecv: mpsc::Receiver<IfOutput>) -> Self {
        MinifbInterface {
            receiver: outrecv,
            buffer: Vec::new(),
            window: None,
        }
    }

    fn run(&mut self) {
        while let Ok(msg) = self.receiver.recv() {
            match msg {
                IfOutput::SetMode(w, h) => self.setup(w, h),
                IfOutput::SetPixel(o, p) => self.set(o, p),
                IfOutput::Update => self.update(),
            }
        }
    }

}

impl MinifbInterface {
    fn setup(&mut self, w: usize, h: usize) {
        match Window::new(
            "Rustendo64_gb", w, h, WindowOptions {
                scale: if w < 640 { Scale::X2 } else { Scale::X1 },
                ..WindowOptions::default() }) {
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
            if let Some(cstate) = win.get_keys().map(|keys| {
                keys.iter().fold(0, |a, &key| a | match key {
                    Key::LeftShift  => 1 << 15,  // A
                    Key::LeftCtrl   => 1 << 14,  // B
                    Key::Z          => 1 << 13,  // Z
                    Key::Enter      => 1 << 12,  // Start
                    Key::W          => 1 << 11,
                    Key::S          => 1 << 10,  // Joypad
                    Key::A          => 1 << 9,
                    Key::D          => 1 << 8,
                    Key::X          => 1 << 5,   // Left trigger
                    Key::C          => 1 << 4,   // Right trigger
                    Key::I          => 1 << 3,   // C-up
                    Key::K          => 1 << 2,   // C-down
                    Key::J          => 1 << 1,   // C-left
                    Key::L          => 1 << 0,   // C-right
                    Key::Up         => 0x7f << 24,
                    Key::Down       => 0x80 << 24,  // Analog pad
                    Key::Left       => 0x80 << 16,  // (full throttle)
                    Key::Right      => 0x7f << 16,
                    Key::Escape     => panic!("kbd quit"),
                    _ => 0
                })
            }) {
                CONTROLLER.store(cstate, Ordering::Relaxed);
            }
        }
    }
}
