use std::sync::atomic::Ordering;
use std::sync::mpsc;

use minifb::{Window, WindowOptions, Scale, Key};

use ui::{Interface, IfOutput, CONTROLLER};

pub struct MinifbInterface {
    receiver: mpsc::Receiver<IfOutput>,
    size: (usize, usize),
    mode: u32,
    window: Option<Window>,
}

impl Interface for MinifbInterface {
    fn new(outrecv: mpsc::Receiver<IfOutput>) -> Self {
        MinifbInterface {
            receiver: outrecv,
            window: None,
            mode: 0,
            size: (0, 0),
        }
    }

    fn run(&mut self) {
        while let Ok(msg) = self.receiver.recv() {
            match msg {
                IfOutput::SetSize(w, h) => self.setsize(w, h),
                IfOutput::SetMode(m) => self.setmode(m),
                IfOutput::Update(v) => self.update(v),
            }
        }
    }

}

impl MinifbInterface {
    fn setsize(&mut self, w: usize, h: usize) {
        match Window::new(
            "Rustendo64_gb", w, h, WindowOptions {
                scale: if w < 640 { Scale::X2 } else { Scale::X1 },
                ..WindowOptions::default() }) {
            Ok(win) => {
                self.size = (w, h);
                self.window = Some(win);
            }
            Err(err) => {
                println!("Unable to create window: {}", err);
                return;
            }
        }
    }

    fn setmode(&mut self, mode: u32) {
        self.mode = mode;
    }

    fn update(&mut self, mut buffer: Vec<u32>) {
        if let Some(ref mut win) = self.window {
            let pixelsize = self.mode & 0b11;
            if pixelsize == 0b11 {
                if buffer.len() == self.size.0 * self.size.1 {
                    for w in &mut buffer {
                        *w >>= 8;
                    }
                    win.update_with_buffer(&buffer);
                } else {
                    println!("strange buffer size?")
                }
            } else if pixelsize == 0b10 {
                if buffer.len() == self.size.0 * self.size.1 / 2 {
                    let mut buf32 = vec![0; buffer.len() * 2];
                    for i in 0..buffer.len() {
                        let pixel = buffer[i];
                        // convert 2 * 5-5-5-1 into 8-8-8
                        buf32[2*i]     = ((pixel >> 27) & 0b11111) << 19 |
                                         ((pixel >> 22) & 0b11111) << 11 |
                                         ((pixel >> 17) & 0b11111) << 3;
                        buf32[2*i + 1] = ((pixel >> 11) & 0b11111) << 19 |
                                         ((pixel >>  6) & 0b11111) << 11 |
                                         ((pixel >>  1) & 0b11111) << 3;
                    }
                    win.update_with_buffer(&buf32);
                }
            } // else it's blank
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
