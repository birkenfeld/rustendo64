use std::ops::DerefMut;
use std::thread;
use std::time;

use minifb_crate::{Window, WindowOptions, Scale, Key, KeyRepeat};
use cpal;

use {Interface, UiMessage, UiReceiver, UiSender, Options, VideoMode, Depth};

pub struct MinifbInterface {
    receiver: UiReceiver,
    title: String,
    video: Option<Window>,
    video_mode: VideoMode,
    video_reopen: bool,
    audio_dev: Option<cpal::Endpoint>,
    audio_src: Option<cpal::Voice>,
    audio_mute: bool,
    audio_mute_key: bool,
    audio_rate: u32,
    audio_underflowed: bool,
}

impl Interface for MinifbInterface {
    fn new(opts: Options, receiver: UiReceiver, sender: UiSender) -> Self {
        thread::spawn(move || MinifbInterface::ping_thread(sender));
        MinifbInterface {
            receiver: receiver,
            title: format!("Rustendo64_gb: {}", opts.win_title),
            video: None,
            video_mode: VideoMode::default(),
            video_reopen: false,
            audio_dev: cpal::get_default_endpoint(),
            audio_src: None,
            audio_mute: opts.mute_audio,
            audio_mute_key: false,
            audio_rate: 0,
            audio_underflowed: false,
        }
    }

    fn run(&mut self) {
        while let Ok(msg) = self.receiver.recv() {
            match msg {
                UiMessage::Update => if self.update() { break; },
                UiMessage::VideoMode(vm) => self.set_mode(vm),
                UiMessage::Video(v) => self.display(v),
                UiMessage::Audio(f, v) => self.play(f, v),
            }
        }
        println!("Interface closed.");
    }
}

impl MinifbInterface {
    fn ping_thread(sender: UiSender) {
        // Just ping the UI every now and then to update the input state.
        loop {
            thread::sleep(time::Duration::from_millis(10));
            // This will exit when the channel is broken.
            sender.send(UiMessage::Update);
        }
    }

    fn update(&mut self) -> bool {
        // Update pending audio samples.
        if let Some(ref voice) = self.audio_src {
            let samples = voice.get_pending_samples();
            self.receiver.set_pending_audio(samples);
            if voice.underflowed() && !self.audio_underflowed {
                println!("Audio: underflow detected!");
                self.audio_underflowed = true;
            }
        }

        let mut quit = false;

        // Update controller button state.
        if let Some(ref mut win) = self.video {
            if win.is_key_pressed(Key::Escape, KeyRepeat::No) {
                quit = true;
            }
            if win.is_key_pressed(Key::M, KeyRepeat::No) {
                if !self.audio_mute_key {
                    self.audio_mute = !self.audio_mute;
                    println!("Audio: now {}.",
                             if self.audio_mute { "muted" } else { "unmuted" });
                    self.audio_mute_key = true;
                }
            } else {
                self.audio_mute_key = false;
            }
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
                    Key::Right      => { a_x += 127; 0 },  // Analog stick
                    Key::Down       => { a_y -= 127; 0 },  // (L/R cancel out)
                    Key::Up         => { a_y += 127; 0 },
                    Key::RightShift => { a_throttle += 1; 0 },
                    Key::RightCtrl  => { a_throttle += 2; 0 },
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
                self.receiver.set_input_state(cstate);
            }
        }
        quit
    }

    fn set_mode(&mut self, mode: VideoMode) {
        if mode == self.video_mode {
            return;
        }
        drop(self.video.take());
        if mode.width == 0 || mode.height == 0 || mode.depth == Depth::Blank {
            self.video_reopen = false;
        } else {
            self.video_reopen = true;
        }
        self.video_mode = mode;
    }

    fn display(&mut self, mut buffer: Vec<u32>) {
        if self.video_reopen {
            self.video_reopen = false;
            let mode = &self.video_mode;
            let opts = WindowOptions {
                scale: if mode.width < 640 { Scale::X2 } else { Scale::X1 },
                ..WindowOptions::default()
            };
            match Window::new(&self.title, mode.width, mode.height, opts) {
                Ok(win) => {
                    println!("Video: new window with resolution {}x{}, {:?}",
                             mode.width, mode.height, mode.depth);
                    self.video = Some(win);
                }
                Err(err) => {
                    println!("Unable to create window: {}", err);
                    return;
                }
            }
        }

        if let Some(ref mut win) = self.video {
            if self.video_mode.depth == Depth::Rgb32 {
                if buffer.len() == self.video_mode.width * self.video_mode.height {
                    for w in &mut buffer {
                        *w >>= 8;
                    }
                    win.update_with_buffer(&buffer);
                } else {
                    println!("Video: frame buffer size != screen size.");
                }
            } else if self.video_mode.depth == Depth::Rgb16 {
                if buffer.len() == self.video_mode.width * self.video_mode.height / 2 {
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
                } else {
                    println!("Video: frame buffer size != screen size.");
                }
            } // else it's blank
        }
    }

    fn play(&mut self, rate: u32, data: Vec<u32>) {
        if let Some(ref endpoint) = self.audio_dev {
            if self.audio_rate != rate {
                /* TODO: clear this mess up */
                let clean_rate = match rate {
                    0...7000       => 5512,
                    7001...9000    => 8000,
                    9001...13000   => 11025,
                    13001...19000  => 16000,
                    19001...26000  => 22050,
                    26001...38000  => 32000,
                    38001...46000  => 44100,
                    _              => 48000
                };
                self.audio_src = cpal::Voice::new(&endpoint, &cpal::Format {
                    channels: vec![cpal::ChannelPosition::FrontLeft,
                                   cpal::ChannelPosition::FrontRight],
                    samples_rate: cpal::SamplesRate(clean_rate),
                    data_type: cpal::SampleFormat::I16
                }).ok();
                self.audio_rate = rate;
                self.audio_underflowed = false;
                println!("Audio: opened new source with {} ({}) Hz", clean_rate, rate);
            }
            if let Some(ref mut voice) = self.audio_src {
                let nsamples = 2 * data.len();
                match voice.append_data(nsamples) {
                    cpal::UnknownTypeBuffer::F32(_) |
                    cpal::UnknownTypeBuffer::U16(_) => { },
                    cpal::UnknownTypeBuffer::I16(mut buf) => {
                        if !self.audio_mute {
                            let max = buf.deref_mut().len() / 2;
                            for (p, i) in data.iter().zip(0..max) {
                                buf[2*i] = (p >> 16) as i16;
                                buf[2*i+1] = *p as i16;
                            }
                        }
                    }
                }
                // println!("Audio: buffer filled, now pending: {}",
                //          voice.get_pending_samples());
                voice.play();
            }
        }
    }
}
