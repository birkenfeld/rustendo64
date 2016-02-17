use std::sync::atomic::Ordering;
use std::sync::mpsc;

use minifb::{Window, WindowOptions, Scale, Key};

use ui::{Interface, UiOutput, CONTROLLER};

pub struct MinifbInterface {
    receiver: mpsc::Receiver<UiOutput>,
    size: (usize, usize),
    mode: usize,
    window: Option<Window>,
}

impl Interface for MinifbInterface {
    fn new(outrecv: mpsc::Receiver<UiOutput>) -> Self {
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
                UiOutput::SetMode(w, h, m) => self.set_mode(w, h, m),
                UiOutput::Update(v) => if self.update(v) { break; },
            }
        }
        println!("Interface closed.");
    }

}

impl MinifbInterface {
    fn set_mode(&mut self, w: usize, h: usize, mode: usize) {
        if (w, h) == self.size && mode == self.mode {
            return;
        }
        drop(self.window.take());
        if mode == 0 || w == 0 || h == 0 {
            return;
        }
        match Window::new(
            "Rustendo64_gb", w, h, WindowOptions {
                scale: if w < 640 { Scale::X2 } else { Scale::X1 },
                ..WindowOptions::default() }) {
            Ok(win) => {
                self.size = (w, h);
                self.mode = mode;
                self.window = Some(win);
            }
            Err(err) => {
                println!("Unable to create window: {}", err);
                return;
            }
        }
    }

    fn update(&mut self, mut buffer: Vec<u32>) -> bool {
        let mut quit = false;
        if let Some(ref mut win) = self.window {
            if self.mode == 4 {
                if buffer.len() == self.size.0 * self.size.1 {
                    for w in &mut buffer {
                        *w >>= 8;
                    }
                    win.update_with_buffer(&buffer);
                } else {
                    println!("strange buffer size?")
                }
            } else if self.mode == 2 {
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
                let mut a_x = 0_i8;
                let mut a_y = 0_i8;
                let mut a_throttle = 0_i8;
                let state = keys.iter().fold(0, |a, &key| a | match key {
                    Key::LeftShift  => 1 << 31,  // A
                    Key::LeftCtrl   => 1 << 30,  // B
                    Key::Z          => 1 << 29,  // Z
                    Key::Enter      => 1 << 28,  // Start
                    Key::W          => 1 << 27,
                    Key::S          => 1 << 26,  // Joypad
                    Key::A          => 1 << 25,
                    Key::D          => 1 << 24,
                    Key::X          => 1 << 21,  // Left trigger
                    Key::C          => 1 << 20,  // Right trigger
                    Key::I          => 1 << 19,  // C-up
                    Key::K          => 1 << 18,  // C-down
                    Key::J          => 1 << 17,  // C-left
                    Key::L          => 1 << 16,  // C-right
                    Key::Left       => { a_x -= 127; 0 },
                    Key::Right      => { a_x += 127; 0 },  // Analog pad
                    Key::Down       => { a_y -= 127; 0 },  // (L/R cancel out)
                    Key::Up         => { a_y += 127; 0 },
                    Key::RightShift => { a_throttle += 1; 0 },
                    Key::RightCtrl  => { a_throttle += 2; 0 },
                    Key::Escape     => { quit = true; 0 },
                    _ => 0
                });
                match a_throttle {
                    1 => { a_x = (a_x / 4) * 3; a_y = (a_y / 4) * 3; }
                    2 => { a_x /= 2; a_y /= 2; }
                    3 => { a_x /= 4; a_y /= 4; }
                    _ => { }
                }
                state | ((a_x as u8 as u32) << 8) | (a_y as u8 as u32)
            }) {
                CONTROLLER.store(cstate as usize, Ordering::Relaxed);
            }
        }
        quit
    }
}
